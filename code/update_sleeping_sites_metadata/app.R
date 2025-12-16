library(shiny)
library(leaflet)
library(DT)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%; margin: 0; padding: 0; overflow: hidden;
      }
      .container-fluid {
        height: 100%; display: flex; flex-direction: column;
        padding: 10px; box-sizing: border-box;
      }
      .title-and-button-container {
        flex-shrink: 0; margin-bottom: 10px;
      }
      .main-layout {
        flex-grow: 1; display: flex; flex-direction: row; overflow: hidden;
      }
      .sidebar, .main-content {
        flex: 1 1 50%; /* Equal width (50%) for sidebar and map */
        display: flex; flex-direction: column; overflow: hidden;
      }
      .sidebar {
        padding-right: 10px;
        background-color: #f9f9f9; box-shadow: 3px 0px 5px rgba(0, 0, 0, 0.1);
      }
      #table .dataTables_wrapper {
        flex-grow: 1; overflow: auto;
      }
      #map {
        flex-grow: 1; height: calc(100vh - 100px);
      }
      .row-selected {
        background-color: green !important; /* Highlight selected row in green */
        color: white !important;
      }
      .row-deselected {
        background-color: blue !important; /* Other rows remain blue */
        color: white !important;
      }
    "))
  ),
  div(
    class = "title-and-button-container",
    titlePanel("Interactive Map with Editable Popups"),
    actionButton("toggle_fullscreen_btn", "Toggle Fullscreen")
  ),
  div(
    class = "main-layout",
    div(
      class = "sidebar",
      fileInput("file_upload", "Upload CSV File", accept = ".csv"),
      helpText("Site Data:"),
      DTOutput("table"),
      br(),
      downloadButton("download_csv", "Download Current Data")
    ),
    div(
      class = "main-content",
      leafletOutput("map")
    )
  ),
  tags$script(HTML("
    function toggleFullScreen() {
      if (!document.fullscreenElement) {
        document.documentElement.requestFullscreen();
      } else {
        document.exitFullscreen();
      }
    }
    document.getElementById('toggle_fullscreen_btn').addEventListener('click', toggleFullScreen);
  "))
)

server <- function(input, output, session) {
  shinyjs::disable("download_csv")
  
  processed_data <- reactive({
    req(input$file_upload)
    tryCatch({
      df <- read.csv(input$file_upload$datapath)
      return(df)
    }, error = function(e) {
      shiny::showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  reactiveData <- reactiveVal(NULL)
  selectedRow <- reactiveVal(NULL)
  
  observeEvent(processed_data(), {
    if (!is.null(processed_data()) && nrow(processed_data()) > 0) {
      reactiveData(processed_data())
    } else {
      reactiveData(NULL)
    }
  })
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    if (!is.null(reactiveData())) {
      current_data <- reactiveData()
      original_class <- class(current_data[[info$col]])
      new_value <- tryCatch({
        as(info$value, original_class)
      }, error = function(e) {
        showNotification(paste("Invalid input type for column:", colnames(current_data)[info$col]), type = "error")
        return(current_data[info$row, info$col])
      })
      current_data[info$row, info$col] <- new_value
      reactiveData(current_data)
    }
  })
  
  observe({
    if (!is.null(reactiveData()) && nrow(reactiveData()) > 0) {
      shinyjs::enable("download_csv")
    } else {
      shinyjs::disable("download_csv")
    }
  })
  
  output$table <- renderDT({
    req(reactiveData())
    datatable(
      reactiveData(),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        paging = FALSE,
        deferRender = TRUE
      ),
      selection = 'single',
      editable = TRUE
    )
  })
  
  observeEvent(input$table_rows_selected, {
    selectedRow(input$table_rows_selected)
    if (!is.null(reactiveData()) && !is.null(input$table_rows_selected)) {
      selected_data <- reactiveData()[input$table_rows_selected, ]
      leafletProxy("map") %>% clearMarkers() %>%
        addCircleMarkers(
          lng = reactiveData()$avg_long,
          lat = reactiveData()$avg_lat,
          color = ifelse(seq_len(nrow(reactiveData())) == input$table_rows_selected, "red", "blue"),
          fillOpacity = 0.7,
          popup = reactiveData()$Location_Name
        ) %>%
        setView(lng = selected_data$avg_long, lat = selected_data$avg_lat, zoom = 15)
    }
  })
  
  output$map <- renderLeaflet({
    req(reactiveData())
    if ("avg_long" %in% names(reactiveData()) && "avg_lat" %in% names(reactiveData())) {
      leaflet(data = reactiveData()) %>%
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(~avg_long, ~avg_lat, popup = ~Location_Name, color = "blue", fillOpacity = 0.7)  %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      leaflet() %>% addTiles(group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = 0, lat = 0, zoom = 15)  %>%
        addLayersControl(
          baseGroups = c("OSM", "Topo", "Terrain"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("Mpala_Baboon_sleeping_sites_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      if (!is.null(reactiveData())) {
        write.csv(reactiveData(), file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)