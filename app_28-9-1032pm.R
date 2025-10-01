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

# --- Static cutout assets (for the right-hand maps) ---
csv_path      <- "parcel_crop_suitability_counts_master.csv"  # CSV with GEOMETRY + FULL_ADDRESS
esm_dir       <- "ESM_Tiff"                                   # folder containing <crop>.tif
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

# ---------------- UI ---------------------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # -------- CSS (maps bigger; table fills remaining height) --------
    tags$style(HTML("
      :root { --mapH1: 360px; --mapH2: 260px; --tableH: 420px; } /* defaults before JS runs */

      .app-banner-strip {
        margin: 0 -15px 8px -15px;
        height: 60px;
        background-image: url('agrisuit_logo.png'), url('agrisuit_backdrop.png');
        background-repeat: no-repeat, repeat-x;
        background-position: 24px center, left top;
        background-size: auto 60%, auto 100%;
        background-color: #e0e0e0;
      }
      @media (max-height: 900px) { .app-banner-strip { height: 48px; } }

      /* Filters layout (responsive grid) */
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

      /* Fit-to-screen heights (JS sets the vars) */
      #map_wms    { height: var(--mapH1) !important; min-height: 360px; }
      #map_static { height: var(--mapH2) !important; min-height: 260px; }
      #tbl-wrap .dataTables_scrollBody { height: var(--tableH) !important; max-height: none !important; }

      /* DT export buttons bottom-right */
      div.dt-buttons { float: right; margin-top: 10px; }

      /* Table UX */
      table.dataTable tbody td { cursor: pointer; }
      table.dataTable tbody { user-select: none; -webkit-user-select: none; }
    ")),
    
    # -------- JS: compute available height & allocate map/table sizes --------
    tags$script(HTML("
      (function() {
        function fitHeights() {
          var vh = window.innerHeight || document.documentElement.clientHeight || 900;
          var banner   = document.querySelector('.app-banner-strip');
          var controls = document.getElementById('controls');
          var b = banner   ? banner.getBoundingClientRect().height   : 0;
          var c = controls ? controls.getBoundingClientRect().height : 0;
          var buffer = 40;
          var avail  = Math.max(300, vh - b - c - buffer); // space for table + maps

          // ~72% to maps (combined). Split 65/35 between WMS and Static.
          var mapsTotal = Math.max(420, Math.floor(avail * 0.72));
          var map1H     = Math.max(360, Math.floor(mapsTotal * 0.65)); // WMS (bigger)
          var map2H     = Math.max(260, mapsTotal - map1H);            // Static
          var tableH    = Math.max(220, avail - mapsTotal);

          document.documentElement.style.setProperty('--mapH1',  map1H + 'px');
          document.documentElement.style.setProperty('--mapH2',  map2H + 'px');
          document.documentElement.style.setProperty('--tableH', tableH + 'px');

          if (window.dispatchEvent) window.dispatchEvent(new Event('resize'));
        }
        window.addEventListener('load', fitHeights);
        window.addEventListener('resize', fitHeights);
        if (window.Shiny && Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('reflow', function(_) { setTimeout(fitHeights, 0); });
        }
      })();
    ")),
    
    # Reset → full reload
    tags$script(HTML("
      Shiny.addCustomMessageHandler('reload_page', function(x) {
        location.reload();
      });
    "))
  ),
  
  div(class = "app-banner-strip", role = "img", `aria-label` = "AgriSuit"),
  
  # ---------- Controls ----------
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
          "• Otherwise, only the table updates. Click a row to auto-fill address & crop, then maps will draw."
      )
  ),
  
  # ---------- Table & Maps (JS sets their heights) ----------
  fluidRow(
    column(
      width = 6,   # maps get more width (6/6 split)
      div(id = "tbl-wrap", DTOutput("tbl")),
      br()
    ),
    column(
      width = 6,
      h5("LIST WMS (crop layer)"), uiOutput("map_hint"),
      leafletOutput("map_wms", height = 100),  # placeholder; CSS overrides height
      br(), h5("Static zoomed parcel"),
      plotOutput("map_static", height = 100)   # placeholder; CSS overrides height
    )
  )
)

# ---------------- Server -----------------------
server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = FALSE)
  onStop(function() try(dbDisconnect(con, shutdown = TRUE), silent = TRUE))
  
  # ---- Choice builders ----
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
  
  # ---- Validation ----
  has_loc_filter    <- reactive({ is_present(input$municipality) || is_present(input$addr) })
  has_target_filter <- reactive({ selected_not_all(input$crop) || selected_not_all(input$class) })
  
  # ---- Build SQL (UPPER everywhere) ----
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
    select_cols <- paste(dbQuoteIdentifier(con, c("FULL_ADDRESS","MUNICIPALITY","CROP","CLASS_LABEL","COUNT")), collapse = ", ")
    
    sql <- sprintf("SELECT %s FROM %s %s ORDER BY FULL_ADDRESS LIMIT %d",
                   select_cols, dbQuoteIdentifier(con, MAIN_TABLE), where_sql, as.integer(input$limit))
    list(sql = sql, params = params)
  })
  
  # ---- Run query ----
  data_reactive <- eventReactive(input$run, {
    validate(
      need(has_loc_filter(),    "Please select a Municipality or enter a Full Address before running."),
      need(has_target_filter(), "Please select either a specific Crop or a Class label (Crop Category alone isn’t enough).")
    )
    b <- build_sql(); safe_query(con, b$sql, b$params)
  }, ignoreInit = TRUE)
  
  # ---- Table ----
  output$tbl <- renderDT(server = FALSE, {
    df <- data_reactive(); req(df)
    datatable(
      df,
      extensions = c("Buttons","Scroller"),
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scroller  = TRUE,
        scrollY   = 1,               # enable Scroller; CSS controls real height
        dom       = 'frtipB',        # buttons at the bottom
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
  
  # ---- Row click → auto-fill addr & crop, then press Run ----
  observeEvent(input$tbl_rows_selected, ignoreInit = TRUE, {
    df <- isolate(data_reactive()); if (is.null(df)) return()
    sel <- input$tbl_rows_selected; if (length(sel) != 1) return()
    row <- df[sel, , drop = FALSE]
    updateTextInput(session, "addr", value = row$FULL_ADDRESS[1])
    if (is_present(row$CROP[1]) && row$CROP[1] %in% crop_choices()) {
      updateSelectInput(session, "crop", selected = row$CROP[1])
    }
    shinyjs::click("run")
  })
  
  # ---- Hints for maps ----
  output$map_hint <- renderUI({
    if (!selected_not_all(input$crop) || !is_present(input$addr)) {
      tags$div(style="color:#B35C00;font-size:12px;margin:2px 0 6px;",
               "Maps need a ", tags$strong("specific Crop"), " and a ",
               tags$strong("Full Address"), ". Click a row to auto-fill them.")
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
  
  observeEvent(list(input$crop, input$addr, input$run), {
    if (!ready_for_maps()) return()
    
    proxy <- leafletProxy("map_wms") |> clearImages() |> clearShapes() |> clearControls()
    lyr <- tryCatch(resolve_wms_layer(input$crop), error = function(e) NULL)
    if (is.null(lyr)) { tas_fallback(proxy); return() }
    
    proxy <- proxy |> addWMSTiles(
      baseUrl = wms_url, layers = lyr,
      options = WMSTileOptions(version = "1.1.1", styles = "", format = "image/png",
                               transparent = TRUE, tiled = TRUE),
      attribution = "© theLIST, State of Tasmania"
    )
    
    if (!file.exists(csv_path)) { tas_fallback(proxy); return() }
    master <- tryCatch(read.csv(csv_path, check.names = FALSE), error = function(e) NULL)
    if (is.null(master) || !all(c("FULL_ADDRESS","GEOMETRY") %in% names(master))) { tas_fallback(proxy); return() }
    key <- stringr::str_squish(toupper(input$addr))
    hit <- master[stringr::str_squish(toupper(master$FULL_ADDRESS)) == key, , drop = FALSE]
    if (nrow(hit) < 1) { tas_fallback(proxy); return() }
    
    parcel_sf <- st_as_sf(hit[1, c("FULL_ADDRESS","GEOMETRY")], wkt = "GEOMETRY", crs = initial_crs)
    if (!all(st_is_valid(parcel_sf))) parcel_sf <- st_make_valid(parcel_sf)
    parcel_4326 <- st_transform(parcel_sf, 4326)
    bb <- st_bbox(parcel_4326)
    proxy |>
      addPolygons(data = parcel_4326, color = "black", weight = 2, fill = FALSE, popup = ~FULL_ADDRESS) |>
      fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]),
                as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
    session$sendCustomMessage('reflow', 0)
  }, ignoreInit = TRUE)
  
  output$map_static <- renderPlot({
    req(ready_for_maps())
    
    master <- try(read.csv(csv_path, check.names = FALSE), silent = TRUE)
    if (inherits(master, "try-error")) { plot.new(); title("Failed to read CSV."); return(invisible(NULL)) }
    need_cols <- c("FULL_ADDRESS","GEOMETRY")
    if (!all(need_cols %in% names(master))) { plot.new(); title("CSV must have FULL_ADDRESS and GEOMETRY"); return(invisible(NULL)) }
    
    key <- stringr::str_squish(toupper(input$addr))
    master$KEY <- stringr::str_squish(toupper(master$FULL_ADDRESS))
    cand <- master[master$KEY == key, , drop = FALSE]
    if (nrow(cand) == 0) cand <- master[grepl(key, master$KEY, fixed = TRUE), , drop = FALSE]
    if (nrow(cand) > 1 && "GEOMETRY" %in% names(cand)) cand <- cand[!duplicated(cand$GEOMETRY), , drop = FALSE]
    if (nrow(cand) > 1 && is_present(input$municipality) && "MUNICIPALITY" %in% names(cand)) {
      cand_m <- cand[toupper(cand$MUNICIPALITY) == toupper(input$municipality), , drop = FALSE]
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
    
    parcel_sf <- st_as_sf(hit[,c("FULL_ADDRESS","GEOMETRY")], wkt = "GEOMETRY", crs = initial_crs)
    if (!all(st_is_valid(parcel_sf))) parcel_sf <- st_make_valid(parcel_sf)
    
    tif_path <- file.path(esm_dir, paste0(input$crop, ".tif"))
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
      terra::plot(r_cut, col = pal, legend = FALSE, main = paste(input$crop, "suitability (zoomed on parcel)"))
    } else {
      terra::plot(r_cut, legend = FALSE, main = paste(input$crop, "suitability (zoomed on parcel)"))
    }
    parcel_r <- st_transform(parcel_sf, st_crs(r_cut))
    plot(st_geometry(parcel_r), add = TRUE, border = "black", lwd = 2)
    plot(st_geometry(parcel_r), add = TRUE, col = grDevices::adjustcolor("black", alpha.f = 0.18), border = NA)
    pt <- st_point_on_surface(parcel_r); points(st_coordinates(pt), pch = 19, cex = 1.1)
    
    par(mar = c(5,1,4,2) + 0.1); plot.new()
    legend("left", legend = classes, fill = cols, bty = "n", cex = 0.9, title = "ESM")
  })
  
  # Reset → full reload
  observeEvent(input$reset, { session$sendCustomMessage("reload_page", "now") })
}

shinyApp(ui, server)
