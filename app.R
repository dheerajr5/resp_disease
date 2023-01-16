options(warn = -1)
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(leaflet)
  library(plotly)
  library(dplyr)
  library(readr)
  library(rgdal)
  library(tidyr)
  library(glue)
  library(stringr)
  library(fresh)
})

mytheme <- create_theme(
  theme = "yeti",
  bs_vars_navbar(
    default_bg = "#75b8d1",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#75b8d1",
    default_link_active_bg = "#FFFFFF",
    default_link_hover_color = "firebrick"
  ),
  output_file = NULL
)


app_info <- function() {
  info <- tags$div(
    tags$h3("Respiratory disease app"),
    tags$p("This app is about the air pollution metric PM 2.5 and death rate due to respiratory diseases all over world."),
    tags$hr(),
    tags$h4("Dataset Info"),
    tags$ul(tags$li(
      tags$a(
        href = "https://data.worldbank.org/indicator/EN.ATM.PM25.MC.M3",
        "World bank"
      ),
      tags$li(tags$a(
        href = "https://stats.oecd.org/Index.aspx?DataSetCode=EXP_PM2_5",
        "OCED"
      )),
      tags$li(tags$a(
        href = "https://ourworldindata.org/grapher/respiratory-disease-death-rate",
        "Respiratory death rate over world"
      ))
    ))
  )
}


pivot_data <- function(file_path, pivot = TRUE) {
    data <- read_csv(
        file = file_path,
        col_names = TRUE, show_col_types = F
    )
    if (pivot) {
        data <- data %>%
            pivot_wider(names_from = year, values_from = value) %>%
            mutate(Country = str_replace_all(
                string = Country,
                pattern = "[[:punct:]]+",
                replacement = ""
            ))
    }
    data
}


get_countries <- function(data, shp_path = "data/shp/country.shp") {
    countries <- readOGR(verbose = F, dsn = shp_path)
    tmp <- inner_join(countries@data, data, by = c("name" = "Country"))
    sort(tmp$name)
}


plot_pm_by_year <- function(data, country, year_range) {
    start <- which(names(data) == year_range[1])
    end <- which(names(data) == year_range[2])

    data <- data %>% filter(Country %in% country) %>% select(Country, start:end)
    ch_data <- data %>%  pivot_longer(!Country, names_to = 'PM2_5')
    
    plot_ly(data = ch_data, x = ~PM2_5, y = ~value, type = "scatter", 
            linetype = ~Country, name = ~Country, mode = "lines+markers") 
}


plot_pm25_death_rate <- function(shp_path = "data/shp/country.shp", pm_data, year, resp_data) {
    # data fromat - country, years(1990, 1995...)
    countries <- readOGR(shp_path, verbose = FALSE)
    tmp <- coordinates(countries)

    world_shp <- data.frame(countries, long = as.numeric(tmp[, 1]), lat = as.numeric(tmp[, 2]))
    pm_data <- left_join(world_shp, pm_data, by = c("name" = "Country")) %>%
        select(name, value = which(names(.) == year), long, lat)
    countries@data <- full_join(pm_data, resp_data, by = c("name" = "Country")) %>%
        select(name, death_rate = which(names(.) == year), value, long, lat)

    leaflet(countries) %>%
        setView(78.14, 22.3230, zoom = 4) %>%
        addPolygons(
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5,
            label = ~ as.character(glue("Death rate for 100k people in {name} is {round(death_rate, 2)}")),
            fillColor = ~ colorQuantile("PuBu", value)(value),
            highlightOptions = highlightOptions(
                color = "white", weight = 2,
                bringToFront = F
            )
        ) %>%
        addCircleMarkers(
            radius = ~ sqrt(value)*2.5, lng = ~long, lat = ~lat, weight = 1,
            label = ~ as.character(glue("PM 2.5 for {name} is {round(value, 2)}"))
        )
}


dropdownUI <- function(id, label = "dropdown1") {
    ns <- NS(id)
    pickerInput(ns("Country"),
        label = label, choices = NULL,
        multiple = TRUE, selected = "India",
        options = list(
            `actions-box` = TRUE,
            size = 5,
            `selected-text-format` = "count > 3"
        )
    )
}


switchUI <- function(id, label = "switch1") {
    ns <- NS(id)
    tags$li(class = "dropdown", style = 'margin: 5px 100px; height: 30px;',
            switchInput(ns("dataSet"),  
                        label = label, value = TRUE,
                        onLabel = "OECD", offLabel = "World bank",
                        width = "50%", size = "mini"
            ) 
    )
}


sliderUI <- function(id, label = "slider1") {
    ns <- NS(id)
    sliderInput(
        inputId = ns("year"), label = label, min = 1990,
        max = 2010, value = 2000, step = 5, sep = "",
        ticks = TRUE
    )
}


checkBoxUI <- function(id, label = 'checkbox1') {
  ns <- NS(id)
  prettyCheckbox(
    inputId = ns('graphSwitch'), label = label,
    status = "success", outline = TRUE
  )
}

# UI ----
map_ui <- function(id) {
    title <- tags$img(src = "images/r.png", width = "10%", "Respiratory disease app")
                       
    dashboardPage(
        header = dashboardHeader(
            title = title, titleWidth = "300px",
            tags$li(class = 'dropdown',
                    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"), 
                    switchUI(id, label = "Select Dataset"),
                    actionBttn(inputId = NS(id, "infoApp"), label = '', block = F,
                               icon = icon('info'), style = 'material-circle',
                               size = 'xs', color = 'primary', no_outline = T)
                    # tags$i(id = NS(id, "infoApp"),
                    #   class = "glyphicon glyphicon-info-sign", 
                    #   style = "color:#0072B2;",
                    #   title = "App information "
                    # )
            )
        ),
        sidebar = dashboardSidebar(disable = TRUE),
        body = dashboardBody(
            fluidRow(
                column(
                    width = 4,
                    tags$h2("About"),
                    tags$p("The app gives a visual overview of PM2.5 air pollution for different countries over the years and its potential relationship to respiratory diseases and their prevalence."),
                    sliderUI(id, label = "Select Year"),
                    dropdownUI(id,
                        label = "Select Country"
                    )
                ),
                column(
                    width = 8,
                    tabBox(
                        width = "100%",
                        title = "Maps and Graphs",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Map", icon = icon('map'), leafletOutput(NS(id, "myMap"))),
                        tabPanel("Graph", icon = icon('line-chart'),
                                 checkBoxUI(id, label = "Switch Graph data"),
                                 plotlyOutput(NS(id, "myChart")))
                    )
                )
            )
        )
    )
}


map_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        prefix <- "data"
        dataset <- reactive(input$dataSet)
        selected <- reactive(input$Country)
        observeEvent(dataset(), {
          ds <- 'World bank'
          if (dataset()) ds <- 'OCED'
          countries <- get_countries(data = data_react()[["pm"]])
          updatePickerInput(session, 'Country', choices = countries,
                            selected = selected(),
                            label = glue('Select country for {ds} dataset'))
        })
        
        observeEvent(input$infoApp, {
          html_info <- app_info()
          showModal(modalDialog(title = 'App Info',
                                html_info,
                                easyClose = TRUE,
                                footer = NULL))
        })
        
        data_react <- reactive({
            resp_data <- pivot_data(file_path = paste0(c(prefix, "resp", "respiratory-disease-death-rate.csv"),
                collapse = .Platform$file.sep
            ))
            if (dataset()) {
                filepath <- paste0(c(prefix, "oecd", "pm25.csv"),
                    collapse = .Platform$file.sep
                )
                pm_data <- pivot_data(file_path = filepath)
            } else {
                filepath <- paste0(c(prefix, "worldbank", "world_bank_pm25.csv"),
                    collapse = .Platform$file.sep
                )
                pm_data <- pivot_data(file_path = filepath, pivot = FALSE)
            }
            list(pm = pm_data, resp = resp_data)
        })
        
        output$myMap <- renderLeaflet({
            plot_pm25_death_rate(
                pm_data = data_react()[["pm"]], year = input$year,
                resp_data = data_react()[["resp"]]
            )
            # plot_pm25_by_year(data = data_react(), country = input$Country, year_range = input$year)
        })

        output$myChart <- renderPlotly({
            contry_name <- input$Country
            year_range  <- c(1990, input$year)
            if (length(contry_name) > 1) {
                country_name <- "different Countries"
            }
            if (input$graphSwitch) {
                data <- data_react()[["pm"]]
                title <- glue("PM 2.5 for {country_name} between {year_range[1]} and {year_range[2]}")
                xaxis <- list(title = "Particulate Matter (PM) <=2.5 micron particles in air")
            } else {
                data <- data_react()[["resp"]]
                title <- glue("Death rate for {country_name} between {year_range[1]} and {year_range[2]}")
                xaxis <- list(title = "Death rate (per 100k)")
            }
            yaxis <- list(title = "Year")   
            p <- plot_pm_by_year(
                data = data,
                country = input$Country,
                year_range =  year_range)
            p %>% 
              layout(title = title, 
                     xaxis = xaxis, 
                     yaxis = yaxis)
        })
    })
}


# Run the application  options = list('launch.browser' = T)
mapApp <- function() {
    ui <- map_ui("x")
    server <- function(input, output) {
        map_server("x")
    }
    shinyApp(ui, server)
}


mapApp()
