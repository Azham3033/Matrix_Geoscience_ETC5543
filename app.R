# ---- Shiny + DuckDB Dashboard (Tabular Output Only) ----
# app.R — tailored to your spec
#   • Data source: DuckDB file my_data.duckdb
#   • Table: final_data
#   • Filters: Crop (textbox search), Class label (dropdown, includes "All"),
#              Municipality (dropdown, mandatory unless Full Address filled),
#              Full Address (textbox). One of {Municipality, Full Address} must be provided.
#   • Output: Tabular (DT) with export buttons.
#
# install.packages(c("shiny","DBI","duckdb","DT","dplyr"))

library(shiny)
library(DBI)
library(duckdb)
library(DT)
library(dplyr)

# ---------------- Configuration ----------------
DB_PATH <- "my_data.duckdb"     # your DuckDB file (as given)
TABLE   <- "CROP_PROPERTY_COUNT_MASTER"         # your table (as given)
ROW_LIMIT_DEFAULT <- 1000        # safety cap

# Parameterised query helper to avoid SQL injection
safe_query <- function(con, sql, params = list()) {
  if (length(params)) {
    qry <- do.call(DBI::sqlInterpolate, c(list(conn = con, sql = sql), params))
    DBI::dbGetQuery(con, qry)
  } else {
    DBI::dbGetQuery(con, sql)
  }
}

ui <- fluidPage(
  titlePanel("Crop Suitability – Tabular Explorer (DuckDB)"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Filters"),
                 textInput("crop", "Crop (search)", placeholder = "e.g., Barley"),
                 uiOutput("class_ui"),
                 uiOutput("muni_ui"),
                 textInput("addr", "Full Address contains", placeholder = "e.g., 12 Smith St"),
                 hr(),
                 numericInput("limit", "Max rows", ROW_LIMIT_DEFAULT, min = 100, step = 500),
                 actionButton("run", "Run", class = "btn btn-primary"),
                 helpText("Tip: Either Municipality or Full Address must be provided before running.")
    ),
    mainPanel(
      DTOutput("tbl"),
      br(),
      verbatimTextOutput("sql_preview")
    )
  )
)

server <- function(input, output, session) {
  # Connect (honouring your connection details)
  con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = FALSE)
  onStop(function() try(dbDisconnect(con, shutdown = TRUE), silent = TRUE))
  
  # --- Schema-driven choices ---
  # Class label choices (prepend 'All')
  class_choices <- reactive({
    req(DBI::dbExistsTable(con, TABLE))
    vals <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT class_label FROM %s ORDER BY 1", DBI::dbQuoteIdentifier(con, TABLE)))
    c("All", vals[[1]])
  })
  
  # Municipality choices (no 'All' because one of muni or address must be given)
  muni_choices <- reactive({
    req(DBI::dbExistsTable(con, TABLE))
    vals <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT municipality FROM %s ORDER BY 1", DBI::dbQuoteIdentifier(con, "parcels_master")))
    vals[[1]]
  })
  
  output$class_ui <- renderUI({
    selectInput("class", "Class label", choices = class_choices(), selected = "All")
  })
  
  output$muni_ui <- renderUI({
    selectInput("municipality", "Municipality (required if no address)",
                choices = c("" , muni_choices()), selected = "")
  })
  
  # --- Validation: require at least one of municipality OR address ---
  has_key_filter <- reactive({
    nzchar(input$addr %||% "") || nzchar(input$municipality %||% "")
  })
  
  # Build SQL text (with placeholders for interpolation)
  build_sql <- reactive({
    req(input$limit)
    
    where_clauses <- c()
    params <- list()
    
    # Crop text search (ILIKE contains)
    if (nzchar(input$crop %||% "")) {
      where_clauses <- c(where_clauses, sprintf("%s ILIKE '%%' || ?crop || '%%'", DBI::dbQuoteIdentifier(con, "crop")))
      params$crop <- input$crop
    }
    
    # Class label exact unless 'All'
    if (!is.null(input$class) && input$class != "All") {
      where_clauses <- c(where_clauses, sprintf("%s = ?class_label", DBI::dbQuoteIdentifier(con, "class_label")))
      params$class_label <- input$class
    }
    
    # Municipality exact if provided
    if (nzchar(input$municipality %||% "")) {
      where_clauses <- c(where_clauses, sprintf("%s = ?municipality", DBI::dbQuoteIdentifier(con, "municipality")))
      params$municipality <- input$municipality
    }
    
    # Address contains search if provided
    if (nzchar(input$addr %||% "")) {
      where_clauses <- c(where_clauses, sprintf("%s ILIKE '%%' || ?addr || '%%'", DBI::dbQuoteIdentifier(con, "full_address")))
      params$addr <- input$addr
    }
    
    where_sql <- if (length(where_clauses)) paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
    
    sql <- sprintf("SELECT FULL_ADDRESS,CROP,CLASS_LABEL,COUNT FROM %s %s LIMIT %d",
                   DBI::dbQuoteIdentifier(con, TABLE), where_sql, as.integer(input$limit))
    
    list(sql = sql, params = params)
  })
  
  output$sql_preview <- renderText({
    b <- build_sql(); b$sql
  })
  
  # Execute only when clicked, with validation
  data_reactive <- eventReactive(input$run, {
    validate(need(has_key_filter(), "Please select a Municipality or enter a Full Address before running."))
    
    b <- build_sql()
    safe_query(con, b$sql, b$params)
  })
  
  output$tbl <- renderDT({
    df <- data_reactive()
    req(df)
    datatable(
      df,
      extensions = c("Buttons", "Scroller"),
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 500,
        scroller = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
}

# Run the app
shinyApp(ui, server)
