# ---- Shiny + DuckDB + Two Maps (fit-to-screen layout) ----
suppressPackageStartupMessages({
  library(shiny); library(shinyjs)
  library(DBI); library(duckdb); library(DT); library(dplyr)
  library(sf); library(leaflet); library(terra); library(stringr); library(foreign)
})

# ---------------- Configuration ----------------
DB_PATH            <- "TESM.duckdb"
MAIN_TABLE         <- "CROP_PROPERTY_COUNT_MASTER"
CROP_MASTER        <- "CROP_MASTER"
ROW_LIMIT_DEFAULT  <- 1000
REG_TABLE          <- "USER_REGISTRATIONS"

# --- Static cutout assets (for the right-hand maps) ---
csv_path      <- "parcel_crop_suitability_counts_master.csv"  # CSV with GEOMETRY + FULL_ADDRESS
esm_dir       <- "ESM_Tiff/ESM_Tiff"                          # folder containing <crop>.tif
initial_crs   <- 28355                                        # EPSG of WKT in CSV
zoom_buffer_m <- 150

# --- LIST WMS ---
wms_url <- "https://services.thelist.tas.gov.au/arcgis/services/Public/EnterpriseSuitabilityMapping2018/MapServer/WMSServer"

canon <- function(s) tolower(gsub("\\s+", " ", trimws(s)))
esm_layers <- c(
  "Barley"="0","Blueberries [nhb]"="1","Blueberries [shb]"="2","Carrots"="3","Carrot seed"="4",
  "Cherries"="5","Cocksfoot Continental"="6","Cocksfoot Mediterranean"="7","Eucalyptus Globulus"="8",
  "Eucalyptus Nitens"="9","Hazelnuts"="10","Industrial Hemp"="11","Linseed"="12","Lucerne"="13",
  "Olives"="14","Onions"="15","Phalaris"="16","Pinus Radiata"="17","Poppies"="18","Potatoes"="19",
  "Pyrethrum"="20","Raspberries"="21","Red Clover"="22","Rye Grass"="23","Sparkling Wine Grapes"="24",
  "Strawberries"="25","Strawberry Clover"="26","Table Wine Grapes"="27","Tall Fescue Continental"="28",
  "Tall Fescue Mediterranean"="29","Wheat"="30","White Clover"="31","Enterprise Versatility Index"="32",
  "Enterprise Versatility Index - Pastures"="33"
)
aliases <- c(
  "carrot_seed"="Carrot seed","cocksfootcontinental"="Cocksfoot Continental",
  "cocksfootmediterranean"="Cocksfoot Mediterranean","eucglobulus"="Eucalyptus Globulus",
  "eucnitens"="Eucalyptus Nitens","industrialhemp"="Industrial Hemp","ryegrass"="Rye Grass",
  "sparklingwine"="Sparkling Wine Grapes","strawberryclover"="Strawberry Clover",
  "tablewine"="Table Wine Grapes","tallfescuecontinental"="Tall Fescue Continental",
  "tallfescuemediterranean"="Tall Fescue Mediterranean","redclover"="Red Clover","whiteclover"="White Clover"
)
resolve_wms_layer <- function(crop, layers = esm_layers, alias_map = aliases) {
  k <- canon(crop)
  title <- if (!is.null(alias_map) && length(alias_map) && k %in% names(alias_map)) alias_map[[k]] else crop
  idx <- match(canon(title), canon(names(layers)))
  if (is.na(idx)) stop("No WMS layer id for crop label: '", crop, "'.")
  unname(layers[idx])
}

# ---------------- Helpers ---------------------
safe_query <- function(con, sql, params = list()) {
  if (length(params)) {
    qry <- do.call(DBI::sqlInterpolate, c(list(conn = con, sql = sql), params))
    DBI::dbGetQuery(con, qry)
  } else DBI::dbGetQuery(con, sql)
}
is_present <- function(x) !is.null(x) && nzchar(as.character(x))
selected_not_all <- function(x) !is.null(x) && nzchar(as.character(x)) && x != "All"
tas_fallback <- function(proxy) proxy |> setView(lng=146.7, lat=-41.7, zoom=7)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x  # null-coalescer

# ---------- Modal helper (visible × and dismiss buttons) ----------
show_blocking_modal <- function(
    title = "Preferences required",
    msg   = "Please fill at least TWO preference fields (crop, municipality, class label, or price range)."
) {
  showModal(
    modalDialog(
      title = tagList(
        span(title),
        tags$button(
          type  = "button",
          class = "custom-close-x",
          `aria-label` = "Close",
          `data-dismiss` = "modal",
          `data-bs-dismiss` = "modal",
          HTML("&times;")
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        tags$button(
          type  = "button",
          class = "btn btn-primary",
          `data-dismiss` = "modal",
          `data-bs-dismiss` = "modal",
          "OK, got it"
        )
      ),
      size = "m",
      tags$p(msg, style = "margin-bottom:0;")
    )
  )
}

# ---------------- UI ---------------------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # -------- CSS (WMS + table dynamic; static plot fixed) --------
    tags$style(HTML("
      :root { --mapH1: 360px; --tableH: 420px; }

      .app-banner-strip {
        margin: 0 -15px 8px -15px;
        height: 60px;
        background-image: url('agrisuit_logo3.png'), url('agrisuit_backdrop.png');
        background-repeat: no-repeat, repeat-x;
        background-position: 24px center, left top;
        background-size: auto 60%, auto 100%;
        background-color: #e0e0e0;
      }
      @media (max-height: 900px) { .app-banner-strip { height: 48px; } }

      .filter-bar {
        display: grid;
        grid-template-columns: repeat(6, minmax(220px, 1fr));
        column-gap: 16px; row-gap: 10px; align-items: end;
      }
      @media (max-width:1400px){ .filter-bar { grid-template-columns: repeat(4, minmax(220px,1fr)); } }
      @media (max-width: 992px){ .filter-bar { grid-template-columns: repeat(2, minmax(220px,1fr)); } }
      @media (max-width: 576px){ .filter-bar { grid-template-columns: 1fr; } }
      .filter-bar .form-group, .filter-bar .shiny-input-container { margin-bottom: 4px; }
      .filter-bar input, .filter-bar .selectize-input { min-height: 30px; }
      #addr { min-width: 260px; }  #limit { width: 120px; }

      #map_wms { height: var(--mapH1) !important; min-height: 300px; }
      #tbl-wrap .dataTables_scrollBody { height: var(--tableH) !important; max-height: none !important; }

      div.dt-buttons { float: right; margin-top: 10px; }
      table.dataTable tbody td { cursor: pointer; }
      table.dataTable tbody { user-select: none; -webkit-user-select: none; }

      .modal-header .close, .modal-header button.close {
        display: block !important; opacity: 1 !important; position: relative; z-index: 2051;
      }
      .modal-header .close span { font-size: 22px; line-height: 1; }
      .modal-header .custom-close-x {
        position: absolute; right: 12px; top: 8px; font-size: 26px; line-height: 1;
        border: 0; background: transparent; color: #333; opacity: .65; cursor: pointer;
      }
      .modal-header .custom-close-x:hover { opacity: .95; }

      /* ===== Full-screen spinner overlay ===== */
      #screen-spinner {
        display: none;                   /* default hidden */
        position: fixed; inset: 0;       /* top:0; left:0; right:0; bottom:0 */
        background: rgba(255,255,255,.75);
        z-index: 4000;
        align-items: center; justify-content: center; flex-direction: column;
      }
      .spinner-ring {
        width: 64px; height: 64px; border-radius: 50%;
        border: 6px solid #999; border-top-color: transparent; animation: spin 0.8s linear infinite;
        margin-bottom: 10px;
      }
      @keyframes spin { from{transform: rotate(0)} to{transform: rotate(360deg)} }
    ")),
    
    # -------- JS: dynamic heights & spinner toggler --------
    tags$script(HTML("
      (function() {
        function fitHeights() {
          var vh = window.innerHeight || document.documentElement.clientHeight || 900;
          var banner   = document.querySelector('.app-banner-strip');
          var controls = document.getElementById('controls');
          var b = banner   ? banner.getBoundingClientRect().height   : 0;
          var c = controls ? controls.getBoundingClientRect().height : 0;
          var buffer = 40, STATIC_H = 520;
          var avail  = Math.max(300, vh - b - c - buffer - STATIC_H);
          var wmsH   = Math.max(300, Math.floor(avail * 0.55));
          var tableH = Math.max(220, avail - wmsH);
          document.documentElement.style.setProperty('--mapH1',  wmsH + 'px');
          document.documentElement.style.setProperty('--tableH', tableH + 'px');
          if (window.dispatchEvent) window.dispatchEvent(new Event('resize'));
        }
        window.addEventListener('load', fitHeights);
        window.addEventListener('resize', fitHeights);
        if (window.Shiny && Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('reflow', function(_) { setTimeout(fitHeights, 0); });
          Shiny.addCustomMessageHandler('spin',   function(on) {
            var el = document.getElementById('screen-spinner');
            if (!el) return;
            el.style.display = on ? 'flex' : 'none';
          });
        }
      })();
    ")),
    
    # Reset → full reload
    tags$script(HTML("Shiny.addCustomMessageHandler('reload_page', function(x) { location.reload(); });"))
  ),
  
  div(class = "app-banner-strip", role = "img", `aria-label` = "AgriSuit"),
  
  # ===== Global full-screen spinner overlay (single place) =====
  div(id = "screen-spinner",
      div(class = "spinner-ring"),
      div(style="font-size:14px;color:#333;", "Working…")
  ),
  
  # ======================== Tabs ========================
  tabsetPanel(id = "tabs",
              
              # ---------- PAGE 1: Explore ----------
              tabPanel("Explore",
                       div(id = "controls",
                           div(class = "filter-bar",
                               uiOutput("cropcat_ui"),
                               uiOutput("crop_ui"),
                               uiOutput("class_ui"),
                               uiOutput("muni_ui"),
                               textInput("addr", "Full Address contains", placeholder = "e.g., 12 Smith St"),
                               numericInput("limit", "Max rows", ROW_LIMIT_DEFAULT, min = 100, step = 500),
                               actionButton("run", "Run", class = "btn btn-primary"),
                               actionButton("reset", "Reset", class = "btn btn-secondary")
                           ),
                           div(style="font-size:12px;color:#666;margin-top:6px;",
                               tags$b("Rule:"), tags$br(),
                               "• Maps render only when a specific Crop AND a Full Address are present.", tags$br(),
                               "• Click a row to auto-fill address & crop, then press Run to draw maps."
                           )
                       ),
                       fluidRow(
                         column(
                           width = 6,
                           div(id = "tbl-wrap", DTOutput("tbl")),
                           br()
                         ),
                         column(
                           width = 6,
                           h5("LIST WMS (crop layer)"), uiOutput("map_hint"),
                           leafletOutput("map_wms", height = 100),     # no per-map spinner
                           br(), h5("Static zoomed parcel"),
                           plotOutput("map_static", height = 520)      # no per-map spinner
                         )
                       )
              ),
              
              # ---------- PAGE 2: User Registration ----------
              tabPanel("User Registration",
                       fluidRow(
                         column(
                           width = 7,
                           h4("Subscribe for personalised notifications"),
                           tags$p("Enter your email and at least two preference fields so we can send relevant alerts."),
                           textInput("reg_email", "Email address *", placeholder = "name@example.com"),
                           textInput("reg_fullname", "Full name (optional)"),
                           textInput("reg_phone", "Phone (optional)"),
                           tags$hr(),
                           selectInput("reg_crop", "Preferred crop", choices = c(""), selected = ""),
                           selectInput("reg_muni", "Preferred municipality", choices = c(""), selected = ""),
                           selectInput("reg_class", "Class label",
                                       choices = c("", "1.0 Well suited","1.1 Well suited (with soil mgmt)","2.0 Suitable",
                                                   "2.1 Suitable (with soil mgmt)","3.0 Moderately suitable",
                                                   "3.1 Moderately suitable (with soil mgmt)","4.0 Unsuitable"),
                                       selected = ""
                           ),
                           fluidRow(
                             column(6, numericInput("reg_price_min", "Price min (AU$)", value = NA, min = 0, step = 10000)),
                             column(6, numericInput("reg_price_max", "Price max (AU$)", value = NA, min = 0, step = 10000))
                           ),
                           tags$hr(),
                           selectInput("reg_freq", "Alert frequency", choices = c("Daily","Weekly","Monthly"), selected = "Weekly"),
                           textAreaInput("reg_notes", "Notes (optional)", width = "100%", height = "80px",
                                         placeholder = "Any additional constraints or context..."),
                           tags$div(style="color:#666; font-size:12px; margin-top:6px;",
                                    "(*) Required. Please fill at least two preference fields: crop, municipality, class label, or price range."
                           ),
                           br(),
                           actionButton("reg_submit", "Save registration", class = "btn btn-success"),
                           span(id = "reg_status", style = "margin-left:10px; color:#2c7a7b;")
                         ),
                         column(
                           width = 5,
                           h5("Preview"),
                           verbatimTextOutput("reg_preview")
                         )
                       )
              ),
              
              # ---------- PAGE 3: Registrations (Admin) ----------
              tabPanel("Registrations",
                       fluidRow(
                         column(
                           width = 8,
                           h4("User registrations"),
                           div(
                             style = "margin-bottom:8px;",
                             actionButton("reg_refresh", "Refresh", class = "btn btn-secondary"),
                             actionButton("reg_delete",  "Delete selected", class = "btn btn-danger", style = "margin-left:6px;"),
                             downloadButton("reg_download", "Download CSV", class = "btn btn-success", style = "margin-left:6px;")
                           ),
                           DTOutput("reg_tbl")
                         ),
                         column(
                           width = 4,
                           h4("Summary"),
                           tags$div(style="font-size:12px;color:#666;margin-bottom:6px;",
                                    "Quick QA of registrations currently in the database."),
                           tableOutput("reg_summary_counts"),
                           br(),
                           tableOutput("reg_summary_freq")
                         )
                       )
              )
  )
)

# ---------------- Server -----------------------
server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = FALSE)
  onStop(function() try(dbDisconnect(con, shutdown = TRUE), silent = TRUE))
  
  # Spinner coordination
  rv <- reactiveValues(spin_wait = 0L)
  spin_show  <- function(n = 2L) { rv$spin_wait <- as.integer(n); session$sendCustomMessage('spin', TRUE) }
  spin_done  <- function() {
    rv$spin_wait <- max(0L, rv$spin_wait - 1L)
    if (rv$spin_wait <= 0L) session$sendCustomMessage('spin', FALSE)
  }
  spin_hide_now <- function() { rv$spin_wait <- 0L; session$sendCustomMessage('spin', FALSE) }
  
  # ---- Create user registration table if missing ----
  dbExecute(con, sprintf("
    CREATE TABLE IF NOT EXISTS %s (
      email           VARCHAR PRIMARY KEY,
      full_name       VARCHAR,
      phone           VARCHAR,
      crop            VARCHAR,
      municipality    VARCHAR,
      class_label     VARCHAR,
      price_min       DOUBLE,
      price_max       DOUBLE,
      alert_frequency VARCHAR,
      notes           VARCHAR,
      created_at      TIMESTAMP DEFAULT current_timestamp,
      updated_at      TIMESTAMP
    );
  ", DBI::dbQuoteIdentifier(con, REG_TABLE)))
  
  # ---- Choice builders (Explore) ----
  cropcat_choices <- reactive({
    req(dbExistsTable(con, CROP_MASTER))
    vals <- dbGetQuery(con, sprintf("SELECT DISTINCT CROP_CATEGORY FROM %s ORDER BY 1", dbQuoteIdentifier(con, CROP_MASTER)))
    c("All", vals[[1]])
  })
  output$cropcat_ui <- renderUI({
    selectInput("crop_category", "Crop Category", choices = cropcat_choices(), selected = "All")
  })
  
  crop_choices <- reactive({
    req(dbExistsTable(con, CROP_MASTER))
    if (!is.null(input$crop_category) && input$crop_category != "All") {
      vals <- dbGetQuery(
        con,
        sqlInterpolate(con,
                       "SELECT DISTINCT CROP_NAME FROM CROP_MASTER WHERE UPPER(CROP_CATEGORY) = UPPER(?cat) ORDER BY 1",
                       cat = input$crop_category
        )
      )
    } else {
      vals <- dbGetQuery(con, "SELECT DISTINCT CROP_NAME FROM CROP_MASTER ORDER BY 1")
    }
    c("All", vals[[1]])
  })
  output$crop_ui <- renderUI({
    selectInput("crop", "Crop", choices = crop_choices(), selected = "All")
  })
  
  class_choices <- reactive({
    req(dbExistsTable(con, MAIN_TABLE))
    vals <- dbGetQuery(con, sprintf("SELECT DISTINCT CLASS_LABEL FROM %s ORDER BY 1", dbQuoteIdentifier(con, MAIN_TABLE)))
    c("All", vals[[1]])
  })
  output$class_ui <- renderUI({
    selectInput("class", "Class label", choices = class_choices(), selected = "All")
  })
  
  muni_choices <- reactive({
    req(dbExistsTable(con, MAIN_TABLE))
    vals <- dbGetQuery(con, sprintf("SELECT DISTINCT MUNICIPALITY FROM %s ORDER BY 1", dbQuoteIdentifier(con, MAIN_TABLE)))
    vals[[1]]
  })
  output$muni_ui <- renderUI({
    selectInput("municipality", "Municipality (required if no address)", choices = c("", muni_choices()), selected = "")
  })
  
  # ---- Validation (Explore) ----
  has_loc_filter    <- reactive({ is_present(input$municipality) || is_present(input$addr) })
  has_target_filter <- reactive({ selected_not_all(input$crop) || selected_not_all(input$class) })
  
  # ---- Build SQL (Explore) ----
  build_sql <- reactive({
    req(input$limit)
    where <- c(); params <- list()
    
    if (selected_not_all(input$crop)) {
      where <- c(where, sprintf("UPPER(%s) = UPPER(?crop)", dbQuoteIdentifier(con, "CROP"))); params$crop <- input$crop
    } else if (!is.null(input$crop_category) && input$crop_category != "All") {
      where <- c(where, sprintf(
        "UPPER(%s) IN (SELECT UPPER(CROP_NAME) FROM %s WHERE UPPER(CROP_CATEGORY) = UPPER(?cat))",
        dbQuoteIdentifier(con, "CROP"), dbQuoteIdentifier(con, CROP_MASTER)
      )); params$cat <- input$crop_category
    }
    if (!is.null(input$class) && input$class != "All") {
      where <- c(where, sprintf("UPPER(%s) = UPPER(?class_label)", dbQuoteIdentifier(con, "CLASS_LABEL"))); params$class_label <- input$class
    }
    if (is_present(input$municipality)) {
      where <- c(where, sprintf("UPPER(%s) = UPPER(?municipality)", dbQuoteIdentifier(con, "MUNICIPALITY"))); params$municipality <- input$municipality
    }
    if (is_present(input$addr)) {
      where <- c(where, sprintf("UPPER(%s) ILIKE '%%' || UPPER(?addr) || '%%'", dbQuoteIdentifier(con, "FULL_ADDRESS"))); params$addr <- input$addr
    }
    
    where_sql   <- if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else ""
    select_cols <- paste(dbQuoteIdentifier(con, c("FULL_ADDRESS","MUNICIPALITY","CROP","CLASS_LABEL","PRICE","COUNT","WEBSITE")), collapse = ", ")
    
    sql <- sprintf("SELECT %s FROM %s %s ORDER BY FULL_ADDRESS LIMIT %d",
                   select_cols, dbQuoteIdentifier(con, MAIN_TABLE), where_sql, as.integer(input$limit))
    list(sql = sql, params = params)
  })
  output$sql_preview <- renderText({ build_sql()$sql })
  
  # ---- Run query (Explore) ----
  data_reactive <- eventReactive(input$run, {
    validate(
      need(has_loc_filter(),    "Please select a Municipality or enter a Full Address before running."),
      need(has_target_filter(), "Please select either a specific Crop or a Class label (Crop Category alone isn’t enough).")
    )
    b <- build_sql(); safe_query(con, b$sql, b$params)
  }, ignoreInit = TRUE)
  
  # ---- Table (Explore) ----
  output$tbl <- renderDT(server = FALSE, {
    df <- data_reactive(); req(df)
    
    # make WEBSITE column clickable
    if ("WEBSITE" %in% names(df)) {
      df$WEBSITE <- ifelse(
        !is.na(df$WEBSITE) & nzchar(df$WEBSITE),
        paste0("<a href='", trimws(df$WEBSITE), "' target='_blank'>View</a>"),
        NA
      )
    }
    
    datatable(
      df,
      escape = FALSE,   # <---- allow HTML links to render
      extensions = c("Buttons","Scroller"),
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scroller  = TRUE,
        scrollY   = TRUE,
        dom       = 'frtipB',
        drawCallback = JS(
          "function(){ setTimeout(function(){ if (window.Shiny) Shiny.onInputChange('tbl_draw', Date.now()); }, 0); }"
        ),
        initComplete = JS(
          "function(settings, json){",
          "  var $container = $(settings.nTableWrapper);",
          "  $container.find('div.dt-buttons').css({ float: 'right', marginTop: '10px' });",
          "}"
        ),
        buttons = list(
          list(extend = "copy",  className = "btn btn-secondary"),
          list(extend = "csv",   className = "btn btn-secondary"),
          list(extend = "excel", className = "btn btn-secondary")
        )
      ),
      rownames  = FALSE,
      filter    = "top",
      selection = "single"
    )
  })
  observe({ input$tbl_draw; session$sendCustomMessage('reflow', 0) })
  observeEvent(input$run, { session$sendCustomMessage('reflow', 0) })
  
  # ---- Row click → only fill addr & crop (NO auto-Run) ----
  observeEvent(input$tbl_rows_selected, ignoreInit = TRUE, {
    df <- isolate(data_reactive()); if (is.null(df)) return()
    sel <- input$tbl_rows_selected; if (length(sel) != 1) return()
    row <- df[sel, , drop = FALSE]
    updateTextInput(session, "addr", value = row$FULL_ADDRESS[1])
    if (is_present(row$CROP[1]) && row$CROP[1] %in% crop_choices()) {
      updateSelectInput(session, "crop", selected = row$CROP[1])
    }
  })
  
  # ---- Hints for maps ----
  output$map_hint <- renderUI({
    if (!selected_not_all(input$crop) || !is_present(input$addr)) {
      tags$div(style="color:#B35C00;font-size:12px;margin:2px 0 6px;",
               "Maps need a ", tags$strong("specific Crop"), " and a ",
               tags$strong("Full Address"), ". Click a row to auto-fill them, then press ",
               tags$strong("Run"), " to draw maps.")
    } else {
      tags$div(style="color:#444;font-size:12px;margin:2px 0 6px;",
               tags$strong("Address:"), paste0(input$addr))
    }
  })
  
  # ====================== MAPS ======================
  output$map_wms <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) |> addProviderTiles("CartoDB.Positron")
  })
  
  ready_for_maps <- reactive({ selected_not_all(input$crop) && is_present(input$addr) })
  map_params <- reactiveVal(NULL)   # governs static plot trigger
  
  # ---- Run → both maps (full-screen spinner only here) ----
  observeEvent(input$run, {
    if (!ready_for_maps()) return()
    
    # Show spinner and set tasks to wait for (WMS + static)
    spin_show(n = 2L)
    
    # Save params to trigger static map
    map_params(list(crop = input$crop, addr = input$addr, muni = input$municipality))
    
    # WMS overlay + fit to parcel
    try({
      proxy <- leafletProxy("map_wms") |> clearImages() |> clearShapes() |> clearControls()
      lyr <- tryCatch(resolve_wms_layer(input$crop), error = function(e) NULL)
      if (is.null(lyr)) { tas_fallback(proxy); stop("No WMS layer id.") }
      
      proxy <- proxy |> addWMSTiles(
        baseUrl = wms_url, layers = lyr,
        options = WMSTileOptions(version = "1.1.1", styles = "", format = "image/png",
                                 transparent = TRUE, tiled = TRUE),
        attribution = "© theLIST, State of Tasmania"
      )
      
      # parcel lookup + zoom
      if (!file.exists(csv_path)) { tas_fallback(proxy); stop("CSV missing.") }
      master <- tryCatch(read.csv(csv_path, check.names = FALSE), error = function(e) NULL)
      if (is.null(master) || !all(c("FULL_ADDRESS","GEOMETRY") %in% names(master))) { tas_fallback(proxy); stop("CSV columns.") }
      key <- stringr::str_squish(toupper(input$addr))
      hit <- master[stringr::str_squish(toupper(master$FULL_ADDRESS)) == key, , drop = FALSE]
      if (nrow(hit) < 1) { tas_fallback(proxy); stop("Address not found.") }
      
      parcel_sf <- st_as_sf(hit[1, c("FULL_ADDRESS","GEOMETRY")], wkt = "GEOMETRY", crs = initial_crs)
      if (!all(st_is_valid(parcel_sf))) parcel_sf <- st_make_valid(parcel_sf)
      parcel_4326 <- st_transform(parcel_sf, 4326)
      bb <- st_bbox(parcel_4326)
      
      proxy |>
        addPolygons(data = parcel_4326, color = "black", weight = 2, fill = FALSE, popup = ~FULL_ADDRESS) |>
        fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]),
                  as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
      
      session$sendCustomMessage('reflow', 0)
    }, silent = TRUE)
    
    # Mark WMS finished (success or fail)
    spin_done()
  }, ignoreInit = TRUE)
  
  # ---- Static cutout: depends ONLY on map_params() (set on Run) ----
  output$map_static <- renderPlot({
    params <- map_params()
    validate(need(!is.null(params), "Press Run after choosing Crop and entering Full Address to draw the maps."))
    
    # Ensure spinner decremented even if we bail out
    on.exit(spin_done(), add = TRUE)
    
    key_addr <- stringr::str_squish(toupper(params$addr))
    crop_nm  <- params$crop
    muni_in  <- params$muni
    
    master <- try(read.csv(csv_path, check.names = FALSE), silent = TRUE)
    if (inherits(master, "try-error")) { plot.new(); title("Failed to read CSV."); return(invisible(NULL)) }
    need_cols <- c("FULL_ADDRESS","GEOMETRY")
    if (!all(need_cols %in% names(master))) { plot.new(); title("CSV must have FULL_ADDRESS and GEOMETRY"); return(invisible(NULL)) }
    
    master$KEY <- stringr::str_squish(toupper(master$FULL_ADDRESS))
    cand <- master[master$KEY == key_addr, , drop = FALSE]
    if (nrow(cand) == 0) cand <- master[grepl(key_addr, master$KEY, fixed = TRUE), , drop = FALSE]
    if (nrow(cand) > 1 && "GEOMETRY" %in% names(cand)) cand <- cand[!duplicated(cand$GEOMETRY), , drop = FALSE]
    if (nrow(cand) > 1 && is_present(muni_in) && "MUNICIPALITY" %in% names(cand)) {
      cand_m <- cand[toupper(cand$MUNICIPALITY) == toupper(muni_in), , drop = FALSE]
      if (nrow(cand_m) >= 1) cand <- cand_m
    }
    if (nrow(cand) > 1) {
      sf_cand <- try(sf::st_as_sf(cand[, c("FULL_ADDRESS","GEOMETRY")], wkt = "GEOMETRY", crs = initial_crs), silent = TRUE)
      if (!inherits(sf_cand, "try-error")) {
        sf_cand <- sf::st_make_valid(sf_cand)
        cand <- cand[which.max(as.numeric(sf::st_area(sf_cand))), , drop = FALSE]
      } else cand <- cand[1, , drop = FALSE]
    }
    if (nrow(cand) != 1) { plot.new(); title("Parcel not found (or too many candidates) for this FULL_ADDRESS"); return(invisible(NULL)) }
    hit <- cand
    
    parcel_sf <- st_as_sf(hit[, c("FULL_ADDRESS","GEOMETRY")], wkt = "GEOMETRY", crs = initial_crs)
    if (!all(st_is_valid(parcel_sf))) parcel_sf <- st_make_valid(parcel_sf)
    
    tif_path <- file.path(esm_dir, paste0(crop_nm, ".tif"))
    validate(need(file.exists(tif_path), sprintf("Missing GeoTIFF: %s", tif_path)))
    r <- terra::rast(tif_path)
    
    bb <- st_bbox(st_transform(parcel_sf, st_crs(r)))
    ext_zoom <- terra::ext(as.numeric(bb["xmin"]) - zoom_buffer_m,
                           as.numeric(bb["xmax"]) + zoom_buffer_m,
                           as.numeric(bb["ymin"]) - zoom_buffer_m,
                           as.numeric(bb["ymax"]) + zoom_buffer_m)
    r_cut <- terra::crop(r, ext_zoom)
    
    vat_path <- paste0(tif_path, ".vat.dbf")
    cat_tbl <- NULL
    if (file.exists(vat_path)) {
      vat <- foreign::read.dbf(vat_path, as.is = TRUE)
      vcol <- grep("^value$", names(vat), ignore.case = TRUE, value = TRUE)
      if (length(vcol) == 1L) {
        lcol <- grep("(suitab|class|label|cat|legend|desc|category)", names(vat), ignore.case = TRUE, value = TRUE)
        if (!length(lcol)) lcol <- vcol
        cat_tbl <- data.frame(ID = vat[[vcol]], label = trimws(as.character(vat[[lcol]])))
        r_cut <- as.factor(r_cut)
        levels(r_cut) <- data.frame(ID = cat_tbl$ID, class = cat_tbl$label)
      }
    }
    
    classes <- c("1.0 Well suited","1.1 Well suited (with soil mgmt)","2.0 Suitable",
                 "2.1 Suitable (with soil mgmt)","3.0 Moderately suitable",
                 "3.1 Moderately suitable (with soil mgmt)","4.0 Unsuitable")
    cols <- c("#4B0082","#5A4CB2","#62C6D4","#8CD1B6","#58C39D","#A3D98A","#FFE550")
    canon2 <- function(s) { s <- trimws(s); s <- gsub('management','mgmt', s, ignore.case=TRUE); s <- gsub('\\s+',' ', s); tolower(s) }
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    layout(matrix(c(1,2), nrow = 1), widths = c(3, 1))
    par(mar = c(5,5,4,2) + 0.1, xpd = NA)
    if (!is.null(cat_tbl)) {
      idx <- match(canon2(cat_tbl$label), canon2(classes))
      pal <- rep("lightgray", nrow(cat_tbl)); pal[!is.na(idx)] <- cols[idx[!is.na(idx)]]
      terra::plot(r_cut, col = pal, legend = FALSE, main = paste(crop_nm, "suitability (zoomed on parcel)"))
    } else {
      terra::plot(r_cut, legend = FALSE, main = paste(crop_nm, "suitability (zoomed on parcel)"))
    }
    parcel_r <- st_transform(parcel_sf, st_crs(r_cut))
    plot(st_geometry(parcel_r), add = TRUE, border = "black", lwd = 2)
    plot(st_geometry(parcel_r), add = TRUE, col = grDevices::adjustcolor("black", alpha.f = 0.18), border = NA)
    pt <- st_point_on_surface(parcel_r); points(st_coordinates(pt), pch = 19, cex = 1.1)
    
    par(mar = c(5,1,4,2) + 0.1); plot.new()
    legend("left", legend = classes, fill = cols, bty = "n", cex = 0.9, title = "ESM")
  })
  
  # Reset → full reload (also ensure spinner off)
  observeEvent(input$reset, { spin_hide_now(); session$sendCustomMessage("reload_page", "now") })
  
  # ================== USER REGISTRATION (Page 2) ==================
  reg_crop_choices <- reactive({
    req(dbExistsTable(con, CROP_MASTER))
    vals <- dbGetQuery(con, sprintf("SELECT DISTINCT CROP_NAME FROM %s ORDER BY 1", dbQuoteIdentifier(con, CROP_MASTER)))
    c("", vals[[1]])
  })
  reg_muni_choices <- reactive({
    req(dbExistsTable(con, MAIN_TABLE))
    vals <- dbGetQuery(con, sprintf("SELECT DISTINCT MUNICIPALITY FROM %s ORDER BY 1", dbQuoteIdentifier(con, MAIN_TABLE)))
    c("", vals[[1]])
  })
  observe({
    updateSelectInput(session, "reg_crop", choices = reg_crop_choices())
    updateSelectInput(session, "reg_muni", choices = reg_muni_choices())
  })
  
  is_email <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(FALSE)
    grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", trimws(x))
  }
  count_filled_prefs <- function(crop, muni, class_lbl, pmin, pmax) {
    n <- 0L
    if (!is.null(crop)      && nzchar(trimws(crop))      && crop      != "") n <- n + 1L
    if (!is.null(muni)      && nzchar(trimws(muni))      && muni      != "") n <- n + 1L
    if (!is.null(class_lbl) && nzchar(trimws(class_lbl)) && class_lbl != "") n <- n + 1L
    if (!is.na(pmin) || !is.na(pmax)) n <- n + 1L
    n
  }
  
  output$reg_preview <- renderText({
    paste0(
      "Email: ", input$reg_email, "\n",
      "Name: ", input$reg_fullname, "\n",
      "Phone: ", input$reg_phone, "\n",
      "Preferences:\n",
      "  - Crop: ", input$reg_crop, "\n",
      "  - Municipality: ", input$reg_muni, "\n",
      "  - Class label: ", input$reg_class, "\n",
      "  - Price: ",
      ifelse(is.na(input$reg_price_min), "", paste0("min=", input$reg_price_min, " ")),
      ifelse(is.na(input$reg_price_max), "", paste0("max=", input$reg_price_max)), "\n",
      "Alert frequency: ", input$reg_freq, "\n",
      "Notes: ", input$reg_notes
    )
  })
  
  observeEvent(input$reg_submit, {
    # close any leftover modal/backdrop
    removeModal()
    shinyjs::html("reg_status", "Saving…", add = FALSE)
    
    tryCatch({
      # ---------- 1) VALIDATE ----------
      email <- trimws(input$reg_email %||% "")
      if (!is_email(email)) {
        showModal(modalDialog("Please enter a valid email.", easyClose = TRUE, footer = modalButton("OK")))
        stop("invalid_email")
      }
      
      n_prefs <- count_filled_prefs(input$reg_crop, input$reg_muni, input$reg_class,
                                    input$reg_price_min, input$reg_price_max)
      if (n_prefs < 2) {
        showModal(modalDialog(
          "Fill at least TWO preferences (crop, municipality, class label, or price range).",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        stop("too_few_prefs")
      }
      
      # numeric parsing (robust to blanks/strings)
      to_num_or_na <- function(x) {
        if (is.null(x) || identical(x, "") || is.na(x)) return(NA_real_)
        val <- suppressWarnings(as.numeric(x))
        if (is.na(val)) NA_real_ else val
      }
      pmin <- to_num_or_na(input$reg_price_min)
      pmax <- to_num_or_na(input$reg_price_max)
      
      if (!is.na(pmin) && !is.na(pmax) && pmin > pmax) {
        showModal(modalDialog("Price min must be ≤ price max.", easyClose = TRUE, footer = modalButton("OK")))
        stop("bad_price_range")
      }
      
      # ---------- 2) WRITE (transaction: DELETE → INSERT) ----------
      tbl_qid <- dbQuoteIdentifier(con, REG_TABLE)
      
      dbWithTransaction(con, {
        # delete any existing row for this email
        rows_del <- dbExecute(
          con,
          sqlInterpolate(con,
                         paste0("DELETE FROM ", tbl_qid, " WHERE email = ?email"),
                         email = email
          )
        )
        
        # insert new row
        rows_ins <- dbExecute(
          con,
          sqlInterpolate(
            con,
            paste0("
            INSERT INTO ", tbl_qid, " (
              email, full_name, phone, crop, municipality, class_label,
              price_min, price_max, alert_frequency, notes, updated_at
            )
            VALUES (?email, ?full_name, ?phone, ?crop, ?municipality, ?class_label,
                    ?price_min, ?price_max, ?alert_frequency, ?notes, now())
          "),
            email           = email,
            full_name       = (input$reg_fullname %||% NA),
            phone           = (input$reg_phone %||% NA),
            crop            = if (nzchar(input$reg_crop %||% "")) input$reg_crop else NA,
            municipality    = if (nzchar(input$reg_muni %||% "")) input$reg_muni else NA,
            class_label     = if (nzchar(input$reg_class %||% "")) input$reg_class else NA,
            price_min       = pmin,
            price_max       = pmax,
            alert_frequency = (input$reg_freq %||% NA),
            notes           = (input$reg_notes %||% NA)
          )
        )
        
        message(sprintf("[REG] rows_del=%s rows_ins=%s", rows_del, rows_ins))
        
        # sanity check: row should exist now
        peek <- dbGetQuery(
          con,
          sqlInterpolate(con,
                         paste0("SELECT 1 AS ok FROM ", tbl_qid, " WHERE email = ?email LIMIT 1"),
                         email = email
          )
        )
        if (nrow(peek) == 0) stop("insert_verification_failed")
      })
      
      # ---------- 3) DONE ----------
      shinyjs::html("reg_status", "Registration saved ✔", add = FALSE)
      
    }, error = function(e) {
      removeModal()
      shinyjs::html("reg_status", "Save failed.", add = FALSE)
      showModal(modalDialog(
        title = "Could not save registration",
        paste("Error:", conditionMessage(e)),
        easyClose = TRUE, footer = modalButton("OK")
      ))
      message("[REG][ERROR] ", conditionMessage(e))
    }, finally = {
      # always clear any accidental backdrops & tidy the status message
      removeModal()
      shinyjs::delay(3500, shinyjs::html("reg_status", ""))
    })
  })
  
  # ================== REGISTRATIONS ADMIN (Page 3) ==================
  load_regs <- function() {
    if (!dbExistsTable(con, REG_TABLE)) return(data.frame())
    DBI::dbGetQuery(con, sprintf("
      SELECT
        email, full_name, phone,
        crop, municipality, class_label,
        price_min, price_max, alert_frequency, notes,
        created_at, updated_at
      FROM %s
      ORDER BY COALESCE(updated_at, created_at) DESC
    ", dbQuoteIdentifier(con, REG_TABLE)))
  }
  regs_rv <- reactiveVal(load_regs())
  
  output$reg_tbl <- renderDT({
    df <- regs_rv()
    validate(need(nrow(df) > 0, "No registrations yet."))
    datatable(
      df,
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom       = 'Bfrtip',
        buttons   = c('copy','csv','excel'),
        deferRender = TRUE,
        scrollX   = TRUE,
        scroller  = TRUE,
        scrollY   = 400
      ),
      rownames  = FALSE,
      selection = "multiple",
      filter    = "top"
    )
  })
  
  observeEvent(input$reg_refresh, { regs_rv(load_regs()) })
  
  observeEvent(input$reg_delete, {
    df <- regs_rv()
    sel <- input$reg_tbl_rows_selected
    if (is.null(sel) || !length(sel)) return()
    emails <- df$email[sel]; emails <- emails[nzchar(emails)]
    if (!length(emails)) return()
    q <- sprintf("DELETE FROM %s WHERE email IN (%s)",
                 dbQuoteIdentifier(con, REG_TABLE),
                 paste(DBI::dbQuoteString(con, emails), collapse = ", "))
    DBI::dbExecute(con, q)
    regs_rv(load_regs())
  })
  
  output$reg_download <- downloadHandler(
    filename = function() sprintf("user_registrations_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) utils::write.csv(regs_rv(), file, row.names = FALSE, na = "")
  )
  
  output$reg_summary_counts <- renderTable({
    df <- regs_rv()
    if (!nrow(df)) return(NULL)
    data.frame(
      Metric = c("Total registrations",
                 "With crop",
                 "With municipality",
                 "With class label",
                 "With price range"),
      Count  = c(
        nrow(df),
        sum(nzchar(df$crop %||% "")),
        sum(nzchar(df$municipality %||% "")),
        sum(nzchar(df$class_label %||% "")),
        sum(!is.na(df$price_min) | !is.na(df$price_max))
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  output$reg_summary_freq <- renderTable({
    df <- regs_rv()
    if (!nrow(df)) return(NULL)
    as.data.frame(sort(table(df$alert_frequency), decreasing = TRUE)) |>
      `names<-`(c("Alert frequency", "Count"))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
}

options(shiny.launch.browser = TRUE)

shinyApp(ui, server)
