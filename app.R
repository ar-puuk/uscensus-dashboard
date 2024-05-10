suppressMessages({
  # Install and load required packages if not already installed
  if (!require("tidyverse")) install.packages("tidyverse")
  if (!require("shinydashboard")) install.packages("shinydashboard")
  if (!require("leaflet")) install.packages("leaflet")
  if (!require("shinyWidgets")) install.packages("shinyWidgets")
  if (!require("shinythemes")) install.packages("shinythemes")
  if (!require("tigris")) install.packages("tigris")
  if (!require("tidycensus")) install.packages("tidycensus")
  if (!require("lehdr")) install.packages("lehdr")
  if (!require("sf")) install.packages("sf")
  if (!require("shinyjs")) install.packages("shinyjs")
  if (!require("DT")) install.packages("DT")
  if (!require("zip")) install.packages("zip")

  # Load packages
  library(tidyverse)
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(shinyWidgets)
  library(shinythemes)
  library(tigris)
  library(tidycensus)
  library(lehdr)
  library(sf)
  library(shinyjs)
  library(DT)
  library(zip)
})

options(tigris_use_cache = TRUE)

# Download US state boundaries data using tigris
sf_states <- tigris::states(cb = TRUE) %>% sf::st_transform(4326)
sf_counties <- tigris::counties(cb = TRUE) %>% sf::st_transform(4326)

# Create a function to download shapefiles as ZIP
write_sf_zip <- function(obj, zipfile, overwrite = FALSE) {
  stopifnot(tools::file_ext(zipfile) == "zip")

  if (file.exists(zipfile) && !overwrite) {
    stop(paste0(
      "File already exists: ", zipfile,
      "\nUse 'overwrite = TRUE' to overwrite the existing file."
    ))
  }

  tmp_zip <- basename(zipfile)
  shp_name <- paste0(tools::file_path_sans_ext(tmp_zip), ".shp")

  ## Temporary directory to write .shp file
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE))

  sf::write_sf(obj, file.path(tmp, shp_name), delete_layer = TRUE)
  withr::with_dir(tmp, zip(tmp_zip, list.files()))

  file.copy(file.path(tmp, tmp_zip), zipfile, overwrite = overwrite)
}

# Create the default map outside of renderLeaflet
default_map <- leaflet(options = leafletOptions(crs = leafletCRS())) %>%
  addProviderTiles("CartoDB.Voyager") %>% # CartoDB.Voyager, CartoDB.Positron, CartoDB.DarkMatter
  addPolygons(data = sf_states, color = "#222222", weight = 1, fillOpacity = 0.15) # Display all state boundaries

# American Community Survey (ACS)

## UI element
acsUI <- fluidPage(
  titlePanel(h4("American Community Survey 2009-2022")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("api_key_acs", label = h5("Census API Key"), value = ""),
      hr(),
      fluidRow(
        column(5, selectInput("year_acs", label = h5("Year"), choices = c(Choose = "", 2009:2022), selected = 2021)),
        column(7, selectInput("state_acs", label = h5("State"), choices = c(Choose = "", state.name), multiple = FALSE, selectize = TRUE))
      ),
      selectInput("county_acs", label = h5("Select County"), choices = NULL, multiple = TRUE, selectize = TRUE),
      radioGroupButtons(
        "level_acs",
        label = h5("Geographic Level of Estimates"),
        choices = c("Counties" = "county", "Tracts" = "tract", "Block Groups" = "block group"),
        selected = "tract",
        size = "sm",
        justified = TRUE
      ),
      br(),
      actionButton("varlist_acs", label = h5("Select Data Variable(s)"), align = "center"),
      hr(),
      fluidRow(
        column(6, shinyWidgets::switchInput(
          "geometry_acs",
          label = "Geometry",
          labelWidth = "80px",
          onLabel = "Yes",
          offLabel = "No"
        )),
        column(6, actionButton("fetch_button_acs", label = "Submit", icon = icon("search")))
      ),
      hr(),
      conditionalPanel(
        condition = "input.fetch_button_acs > 0",
        downloadButton("download_acs", label = "Download Data", icon = icon("download"), align = "center")
      ),
      hr(),
      em("Disclaimer: The U.S. Census Bureau logo® is a Federally registered trademark of the U.S. Census Bureau, U.S. Department of Commerce. The U.S. Census Bureau is not affiliated with this tool.", style = "font-size: 9.5pt;")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, solidHeader = TRUE,
            leafletOutput("map_acs", height = 450)
          ),
          box(
            width = NULL,
            DTOutput("dataframe_acs")
          )
        )
      )
    )
  )
)

# Decennial Census

## UI element
censusUI <- fluidPage(
  titlePanel(h4("Decennial Census 2000-2020")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("api_key_census", label = h5("Census API Key"), value = ""),
      hr(),
      fluidRow(
        column(5, selectInput("year_census", label = h5("Year"), choices = c(Choose = "", 2000, 2010, 2020), selected = 2020)),
        column(7, selectInput("state_census", label = h5("State"), choices = c(Choose = "", state.name), multiple = FALSE, selectize = TRUE))
      ),
      selectInput("county_census", label = h5("Select County"), choices = NULL, multiple = TRUE, selectize = TRUE),
      radioGroupButtons(
        "level_census",
        label = h5("Geographic Level of Estimates"),
        choices = c("Counties" = "county", "Tracts" = "tract", "Block Groups" = "block group", "Blocks" = "block"),
        selected = "tract",
        size = "sm",
        justified = TRUE
      ),
      br(),
      actionButton("varlist_census", label = h5("Select Data Variable(s)"), align = "center"),
      hr(),
      fluidRow(
        column(6, shinyWidgets::switchInput(
          "geometry_census",
          label = "Geometry",
          labelWidth = "80px",
          onLabel = "Yes",
          offLabel = "No"
        )),
        column(2, actionButton("fetch_button_census", label = "Submit", icon = icon("search")))
      ),
      hr(),
      conditionalPanel(
        condition = "input.fetch_button_census > 0",
        downloadButton("download_census", label = "Download Data", icon = icon("download"), align = "center")
      ),
      hr(),
      em("Disclaimer: The U.S. Census Bureau logo® is a Federally registered trademark of the U.S. Census Bureau, U.S. Department of Commerce. The U.S. Census Bureau is not affiliated with this tool.", style = "font-size: 9.5pt;")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, solidHeader = TRUE,
            leafletOutput("map_census", height = 450)
          ),
          box(
            width = NULL,
            DTOutput("dataframe_census")
          )
        )
      )
    )
  )
)

# Longitudinal Employer-Household Dynamics (LEHD)

## UI element
lehdUI <- fluidPage(
  titlePanel(h4("Longitudinal Employer-Household Dynamics 2002-2021")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "type_lehd",
        label = h5("Type of Data"),
        choices = c("Origin-Destination" = "od", "Residence Area Characteristics" = "rac", "Workplace Area Characteristics" = "wac"),
        selected = "od",
        multiple = FALSE, selectize = TRUE
      ),
      selectInput(
        "version_lehd",
        label = h5("Select LODES Version"),
        choices = c("LODES5", "LODES7", "LODES8"),
        selected = "LODES8",
        multiple = FALSE, selectize = TRUE
      ),
      hr(),
      fluidRow(
        column(5, selectInput("year_lehd", label = h5("Year"), choices = NULL, multiple = FALSE, selectize = TRUE)),
        column(7, selectInput("state_lehd", label = h5("State"), choices = c(Choose = "", state.name), multiple = FALSE, selectize = TRUE))
      ),
      # selectInput("county_lehd", label = h5("Select County"), choices = NULL, multiple = TRUE, selectize = TRUE),
      radioGroupButtons(
        "level_lehd",
        label = h5("Geographic Level of Estimates"),
        choices = c("Counties" = "county", "Tracts" = "tract", "Block Groups" = "bg", "Blocks" = "block"),
        selected = "tract",
        size = "sm",
        justified = TRUE
      ),
      br(),
      fluidRow(
        # column(6, shinyWidgets::switchInput(
        #   "geometry_lehd",
        #   label = "Geometry",
        #   labelWidth = "80px",
        #   onLabel = "Yes",
        #   offLabel = "No"
        # )),
        column(2, actionButton("fetch_button_lehd", label = "Submit", icon = icon("search")))
      ),
      hr(),
      conditionalPanel(
        condition = "input.fetch_button_lehd > 0",
        downloadButton("download_lehd", label = "Download Data", icon = icon("download"), align = "center")
      ),
      hr(),
      em("Disclaimer: The U.S. Census Bureau logo® is a Federally registered trademark of the U.S. Census Bureau, U.S. Department of Commerce. The U.S. Census Bureau is not affiliated with this tool.", style = "font-size: 9.5pt;")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, solidHeader = TRUE,
            leafletOutput("map_lehd", height = 450)
          ),
          box(
            width = NULL,
            DTOutput("dataframe_lehd")
          )
        )
      )
    )
  )
)


# Define server function
server <- function(input, output, session) {
  ## Server for ACS Data
  selected_variables_acs <- reactiveVal(character(0))

  variables_acs <- reactive({
    if (!is.null(input$year_acs)) {
      variables <- load_variables(year = as.numeric(input$year_acs), dataset = "acs5", cache = TRUE)
      if (input$level_acs == "county") {
        variables <- variables %>%
          select(-geography)
      } else if (input$level_acs == "tract") {
        variables <- variables %>%
          filter(geography %in% c("tract", "block group")) %>%
          select(-geography)
      } else if (input$level_acs == "block group") {
        variables <- variables %>%
          filter(geography == "block group") %>%
          select(-geography)
      }
      return(variables)
    }
  })

  observe({
    if (!is.null(input$state_acs) && input$state_acs != "") {
      counties_acs <- list_counties(state.abb[match(input$state_acs, state.name)])$county
      updateSelectInput(session, "county_acs",
        choices = counties_acs,
        selected = counties_acs[[1]]
      )
    } else {
      updateSelectInput(session, "county_acs", choices = NULL)
    }
  })

  var_modal_acs <- modalDialog(
    title = h4("Select Variable(s) from the List"),
    DTOutput("var_table_acs"),
    size = "l",
    easyClose = TRUE,
    footer = actionButton("selectVarButton_acs", "Select Variable(s)")
  )

  output$var_table_acs <- renderDT({
    req(variables_acs())
    datatable(data.frame(variables_acs()), options = list(
      pageLength = 15,
      selection = list(
        mode = "multiple",
        selected = NULL,
        target = "row",
        selectable = NULL
      )
    ))
  })

  observeEvent(input$varlist_acs, {
    showModal(var_modal_acs)
  })

  observeEvent(input$selectVarButton_acs, {
    selected_rows_acs <- input$var_table_acs_rows_selected
    if (length(selected_rows_acs) > 0) {
      selected_variables_acs(variables_acs()$name[selected_rows_acs])
    } else {
      selected_variables_acs(character(0))
    }
    removeModal() # Close the modal dialog
  })

  data_acs <- eventReactive(input$fetch_button_acs, {
    req(input$api_key_acs, input$year_acs, input$state_acs, input$county_acs, input$level_acs)
    if (!is.null(selected_variables_acs())) {
      result_acs <- tidycensus::get_acs(
        year = as.numeric(input$year_acs),
        survey = "acs5",
        geography = input$level_acs,
        variables = selected_variables_acs(),
        state = input$state_acs,
        county = input$county_acs,
        output = "wide",
        cb = FALSE,
        keep_zipped_shapefile = TRUE,
        key = input$api_key_acs,
        geometry = input$geometry_acs
      )

      result_acs <- result_acs %>%
        select(-matches("M$")) %>%
        rename_all(~ sub("E$", "", .))

      return(result_acs)
    } else {
      print("State not selected or selected variables are NULL. Returning NULL.")
      return(NULL)
    }
  })

  output$dataframe_acs <- DT::renderDT({
    if (!is.null(data_acs())) {
      DT::datatable(data_acs() %>% st_drop_geometry(), options = list(pageLength = 10, scrollX = TRUE))
    } else {
      data.frame()
    }
  })

  output$map_acs <- renderLeaflet({
    if (!is.null(input$state_acs) && input$state_acs != "") {
      selected_state_acs <- sf_states[sf_states$NAME == input$state_acs, ]
      state_counties_acs <- sf_counties[sf_counties$STATE_NAME == input$state_acs, ]
      selected_counties_acs <- state_counties_acs[state_counties_acs$NAME %in% input$county_acs, ]
      selected_bbox_acs <- sf::st_bbox(selected_state_acs)

      default_map %>%
        addPolygons(data = selected_state_acs, color = "#222222", weight = 4, fillOpacity = 0) %>%
        addPolygons(data = state_counties_acs, color = "#222222", weight = 1.5, fillOpacity = 0.35) %>%
        fitBounds(selected_bbox_acs[[1]], selected_bbox_acs[[2]], selected_bbox_acs[[3]], selected_bbox_acs[[4]]) %>%
        addPolygons(data = selected_counties_acs, color = "#222222", weight = 0, fillOpacity = 0.75)
    } else {
      default_map %>% setView(lng = -95.7129, lat = 37.0902, zoom = 4)
    }
  })

  output$download_acs <- downloadHandler(
    filename = function() {
      if (input$geometry_acs) {
        paste0("acs data_", input$state_acs, "_", input$year_acs, ".zip")
      } else {
        paste0("acs data_", input$state_acs, "_", input$year_acs, ".csv")
      }
    },
    content = function(file) {
      if (input$geometry_acs) {
        write_sf_zip(data_acs(), file, overwrite = TRUE)
      } else {
        readr::write_csv(data_acs() %>% st_drop_geometry(), file)
      }
    }
  )

  # # Disable download button if there is no data to download
  # observe({
  #   if (is.null(input$fetch_button_acs) || input$fetch_button_acs == 0 || is.null(data_acs()) || nrow(data_acs()) == 0) {
  #     shinyjs::disable("download_acs")
  #   } else {
  #     shinyjs::enable("download_acs")
  #   }
  # })

  ## Server for Decennial Census Data
  selected_variables_census <- reactiveVal(character(0))

  census_variables_census <- reactive({
    if (!is.null(input$year_census)) {
      load_variables(year = as.numeric(input$year_census), dataset = "pl", cache = TRUE)
    }
  })

  observe({
    if (!is.null(input$state_census) && input$state_census != "") {
      counties <- list_counties(state.abb[match(input$state_census, state.name)])$county
      updateSelectInput(session, "county_census",
        choices = counties,
        selected = counties[[1]]
      )
    } else {
      updateSelectInput(session, "county_census", choices = NULL)
    }
  })

  var_modal_census <- modalDialog(
    title = h4("Select Variable(s) from the List"),
    DTOutput("var_table_census"),
    size = "l",
    easyClose = TRUE,
    footer = actionButton("selectVarButton_census", "Select Variable(s)")
  )

  output$var_table_census <- renderDT({
    req(census_variables_census())
    datatable(data.frame(census_variables_census()), options = list(
      pageLength = 15,
      selection = list(
        mode = "multiple",
        selected = NULL,
        target = "row",
        selectable = NULL
      )
    ))
  })

  observeEvent(input$varlist_census, {
    showModal(var_modal_census)
  })

  observeEvent(input$selectVarButton_census, {
    selected_rows_census <- input$var_table_census_rows_selected
    if (length(selected_rows_census) > 0) {
      selected_variables_census(census_variables_census()$name[selected_rows_census])
    } else {
      selected_variables_census(character(0))
    }
    removeModal() # Close the modal dialog
  })

  data_census <- eventReactive(input$fetch_button_census, {
    # print("Fetching Decennial Census data...")
    req(input$api_key_census, input$year_census, input$state_census, input$county_census, input$level_census)

    if (!is.null(selected_variables_census())) {
      result_census <- tidycensus::get_decennial(
        year = as.numeric(input$year_census),
        # dataset = "pl",
        geography = input$level_census,
        variables = selected_variables_census(),
        state = input$state_census,
        county = input$county_census,
        output = "wide",
        cb = FALSE,
        keep_zipped_shapefile = TRUE,
        key = input$api_key_census,
        geometry = input$geometry_census
      )

      return(result_census)
    } else {
      print("State not selected or selected variables are NULL. Returning NULL.")
      return(NULL)
    }
  })

  # Initialize output$dataframe_census as an empty table
  output$dataframe_census <- DT::renderDT({
    # print("Rendering DT_census...")
    if (!is.null(data_census())) {
      # print("Rendering DT_census with ACS data.")
      DT::datatable(data_census() %>% st_drop_geometry(), options = list(pageLength = 10, scrollX = TRUE))
    } else {
      # print("Rendering DT_census with empty data frame.")
      data.frame()
    }
  })

  output$map_census <- renderLeaflet({
    # If a specific state is selected, modify the map to highlight it
    if (!is.null(input$state_census) && input$state_census != "") {
      selected_state_census <- sf_states[sf_states$NAME == input$state_census, ]
      state_counties_census <- sf_counties[sf_counties$STATE_NAME == input$state_census, ]
      selected_counties_census <- state_counties_census[state_counties_census$NAME %in% input$county_census, ]
      selected_bbox_census <- sf::st_bbox(selected_state_census)

      # Update the default map with the highlighted state
      default_map %>%
        addPolygons(data = selected_state_census, color = "#222222", weight = 4, fillOpacity = 0) %>%
        addPolygons(data = state_counties_census, color = "#222222", weight = 1.5, fillOpacity = 0.35) %>%
        fitBounds(selected_bbox_census[[1]], selected_bbox_census[[2]], selected_bbox_census[[3]], selected_bbox_census[[4]]) %>%
        addPolygons(data = selected_counties_census, color = "#222222", weight = 0, fillOpacity = 0.75)
    } else {
      # Return the default map if no state is selected
      default_map %>% setView(lng = -95.7129, lat = 37.0902, zoom = 4) # Centered at the United States
    }
  })

  output$download_census <- downloadHandler(
    filename = function() {
      if (input$geometry_census) {
        paste0("census data_", input$state_census, "_", input$year_census, ".zip")
      } else {
        paste0("census data_", input$state_census, "_", input$year_census, ".csv")
      }
    },
    content = function(file) {
      if (input$geometry_census) {
        write_sf_zip(data_census(), file, overwrite = TRUE)
      } else {
        # Save as CSV
        readr::write_csv(data_census() %>% st_drop_geometry(), file)
      }
    }
  )

  # # Disable download button if there is no data to download
  # observe({
  #   if (is.null(input$fetch_button_census) || input$fetch_button_census == 0 || is.null(data_census()) || nrow(data_census()) == 0) {
  #     shinyjs::disable("download_census")
  #   } else {
  #     shinyjs::enable("download_census")
  #   }
  # })

  ## Server for LEHD data

  observe({
    version <- if (!is.null(input$version_lehd)) input$version_lehd else "default"

    options <- if (version == "LODES5") {
      2002:2009
    } else if (version == "LODES7") {
      2002:2019
    } else {
      2002:2021
    }

    updateSelectInput(session, "year_lehd", choices = options, selected = max(options))
  })

  observe({
    if (!is.null(input$state_lehd) && input$state_lehd != "") {
      counties <- list_counties(state.abb[match(input$state_lehd, state.name)])$county
      updateSelectInput(session, "county_lehd",
        choices = counties,
        selected = counties[[1]]
      )
    } else {
      updateSelectInput(session, "county_lehd", choices = NULL)
    }
  })

  data_lehd <- eventReactive(input$fetch_button_lehd, {
    # print("Fetching LEHD data...")

    if (!is.null(input$state_lehd) && input$state_lehd != "" && !is.null(input$type_lehd)) {
      result_lehd <- lehdr::grab_lodes(
        version = input$version_lehd,
        state = tolower(state.abb[match(input$state_lehd, state.name)]),
        lodes_type = input$type_lehd,
        segment = "S000", job_type = "JT00",
        year = as.numeric(input$year_lehd),
        state_part = "main",
        agg_geo = input$level_lehd,
        use_cache = TRUE
      )

      # print("LEHD data fetched successfully.")

      return(result_lehd)
    } else {
      print("State not selected. Returning NULL.")
      return(NULL)
    }
  })

  # Initialize output$dataframe_lehd as an empty table
  output$dataframe_lehd <- DT::renderDT({
    # print("Rendering DT_lehd...")
    if (!is.null(data_lehd())) {
      # print("Rendering DT_lehd with ACS data.")
      DT::datatable(data_lehd() %>% st_drop_geometry(), options = list(pageLength = 10, scrollX = TRUE))
    } else {
      # print("Rendering DT_lehd with empty data frame.")
      data.frame()
    }
  })

  output$map_lehd <- renderLeaflet({
    # If a specific state is selected, modify the map to highlight it
    if (!is.null(input$state_lehd) && input$state_lehd != "") {
      selected_state <- sf_states[sf_states$NAME == input$state_lehd, ]
      state_counties <- sf_counties[sf_counties$STATE_NAME == input$state_lehd, ]
      selected_counties <- state_counties[state_counties$NAME %in% input$county_lehd, ]
      selected_bbox <- sf::st_bbox(selected_state)

      # Update the default map with the highlighted state
      default_map %>%
        addPolygons(data = selected_state, color = "#222222", weight = 4, fillOpacity = 0) %>%
        addPolygons(data = state_counties, color = "#222222", weight = 1.5, fillOpacity = 0.35) %>%
        fitBounds(selected_bbox[[1]], selected_bbox[[2]], selected_bbox[[3]], selected_bbox[[4]]) %>%
        addPolygons(data = selected_counties, color = "#222222", weight = 0, fillOpacity = 0.75)
    } else {
      # Return the default map if no state is selected
      default_map %>% setView(lng = -95.7129, lat = 37.0902, zoom = 4) # Centered at the United States
    }
  })

  output$download_lehd <- downloadHandler(
    filename = function() {
      paste0("lehd data_", input$state_lehd, "_", input$year_lehd, ".csv")
    },
    content = function(file) {
      # Save as CSV
      readr::write_csv(data_lehd() %>% st_drop_geometry(), file)
    }
  )

  # # Disable download button if there is no data to download
  # observe({
  #   if (is.null(input$fetch_button_lehd) || input$fetch_button_lehd == 0 || is.null(data_lehd()) || nrow(data_lehd()) == 0) {
  #     shinyjs::disable("download_lehd")
  #   } else {
  #     shinyjs::enable("download_lehd")
  #   }
  # })
} # server


# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    # theme = "darkly",  # <--- To use a theme, uncomment this
    div(
      img(src = "https://idser.utsa.edu/Images/census-logo.png", height = 50),
      strong("Data Explorer", style = "font-size: 24pt;")
    ),
    # ACS Tabpanel
    tabPanel(
      h4("ACS"),
      acsUI
    ),
    # Census Tabpanel
    tabPanel(
      h4("Census"),
      censusUI
    ),
    # LEHD Tabpanel
    tabPanel(
      h4("LEHD"),
      lehdUI
    )
  ) # navbarPage
) # fluidPage

# Create Shiny object
shinyApp(ui = ui, server = server)
