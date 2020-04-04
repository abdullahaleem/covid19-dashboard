# Libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(leaflet)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(splitstackshape)
library(reshape2)
library(plotly)
library(httr)
library(jsonlite)
pdf(NULL)



# API Request Credentials
base <- "https://api.smartable.ai/coronavirus/stats/"
subscription_key <- "779078b2d73d453dae02f9461a612619"
cache_control <- "no-cache"

# Query Function. Returns JSON
getQueryResult <- function(region) {
  query_result <- GET(paste0(base, region, "?","Subscription-Key=", subscription_key, "&", "Cache-Control=", cache_control))
  query_result_text <- content(query_result, "text")
  query_result_json <- fromJSON(query_result_text, flatten = TRUE)
}

# Global Data
global_data <- getQueryResult("global")
global_breakdown <- as.data.frame(global_data$stats$breakdowns)
global_breakdown$Country <- global_breakdown$location.countryOrRegion
global_history <- as.data.frame(global_data$stats$history)
global_history$date <-  as.Date(global_history$date, "%Y-%m-%d")

countries_list = global_breakdown$Country
countries_with_history <- subset(global_breakdown, !is.na(location.isoCode))$Country

getCountryISO <- function(countryName) {
  subset(global_breakdown, Country == countryName)$location.isoCode
}


# United States Data
us_data <- getQueryResult("US")
us_breakdown <- as.data.frame(us_data$stats$breakdowns)
us_breakdown$State <- us_breakdown$location.provinceOrState
us_history <- as.data.frame(us_data$stats$history)
us_history$date <-  as.Date(us_history$date, "%Y-%m-%d")

states_list = us_breakdown$State


# States data with hospitalized
covidtracking_data = read_csv(url("https://covidtracking.com/api/states/daily.csv"))
covidtracking_data$date = as.Date(as.character(covidtracking_data$date), "%Y%m%d")

states_acronym = unique(covidtracking_data$state)



# reading map data for heatmap
data_map <- st_read("Boundaries/state_geometry.cpg.shp", stringsAsFactors = FALSE)
data_map$State = data_map$NAME

# colours for plots 
clrs = c('Diagnosed'='#52307c', 'Ailing'='#F4BC1C', 'Deaths'='#A91101', 'CaseFatality'='#A91101', 'Recovered'='#177245', 'Hospitalized'='#29b6f6')



# creating a shiny dashboard
ui <- dashboardPage(
	dashboardHeader(disable = TRUE),
	dashboardSidebar(disable = TRUE),
	dashboardBody(
    #fluidRow(align="center",
    #  h3(paste0("as of ", max(global_history$date)))
    #),
    fluidRow(align="center",
      h1("Global"),
      column(12,
             h2("Progression"),
             htmlOutput("choose_country"),
             withSpinner(plotOutput("progress"))
      )
    ),
    fluidRow(align="center",
      column(6, offset=3,
             h2("Case Fatality and Recovery"),
             column(12, withSpinner(plotOutput("barchart_world"))),
             htmlOutput("choose_multiple_countries")
      )
    ),
    fluidRow(align="center",
      hr(),hr(),
      h1("United States"),
      column(12,
             h2("Progression"),
             htmlOutput("choose_state"),
             withSpinner(plotOutput("progress_state"))
      )
    ),
    fluidRow(align="center",
      column(6,
             h2("Case Fatality and Recovery"),
             column(12, withSpinner(plotOutput("barchart_us"))),
             htmlOutput("choose_multiple_states")
      ),
      column(6,
        h2("Heatmap"),
        withSpinner(leafletOutput("heatmap_us")),
        radioButtons("countType", "Patient Condition", c("Diagnosed" = "Diagnosed", "Deaths" = "Deaths", "Recovered" = "Recovered"), inline=TRUE, selected = c("Diagnosed"))
      )
    ),
    fluidRow(align="center",
             hr(),
             p("Data for all graphs except united states progression chart was sourced from smartable.ai. Data from united states progression chart was sourced from covidtracking.com."),
             p("All ratios for bar charts were calculated based people with confirmed diagnosis. Ailing patients were computed by subtracting deaths and recovered from diagnosed.")
    )
  )
)


server <- function(input, output, session) { 

  # Change default font size
  theme_set(theme_light(base_size = 22))
  
  # Choose country
  output$choose_country = renderUI({
    selectInput(inputId = "country",
                label = "Select Region",
                choices = c(countries_with_history, "Global"),
                selected = "Global")
    
  })

  # Choose multiple countries
  output$choose_multiple_countries = renderUI({
    selectInput(inputId = "countries",
                label = "Select Countries",
                choices = countries_list,
                multiple = TRUE,
                selected = c("United States", "China", "Italy", "Spain", "India", "Iran", "Germany"))
    
  })

  # Choose state
  output$choose_state = renderUI({
    selectInput(inputId = "state",
                label = "Select State",
                choices = states_acronym,
                selected = "NY")
    
  })

  # Choose multiple states
  output$choose_multiple_states = renderUI({
    selectInput(inputId = "states",
                label = "Select States",
                choices = states_list,
                multiple = TRUE,
                selected = c("California", "Illinois", "Florida", "New York", "Minnesota", "Washington"))
    
  })

  # Returns country data based on country choosen
  country_progression <- reactive(
    if(input$country == "Global"){
      country_data <- getQueryResult("global")
    } else {
      country_data <- getQueryResult(getCountryISO(input$country))
    }
  )

  # Returns selected state
  state_select <- reactive(
    input$state
  )



  
  # Line Chart
  output$progress <- renderPlot({
    
    country_data <- country_progression()
    country_history <- as.data.frame(country_data$stats$history)
    country_history$date <-  as.Date(country_history$date, "%Y-%m-%d")
    country_history$ailing = country_history$confirmed - country_history$deaths - country_history$recovered
    
    progression_data <- country_history
    
    progressPlot <- ggplot() +
      labs(x = NULL, y = "", title = NULL)+ 
      theme_minimal(18)+
      scale_x_date(date_breaks = "1 week")+
      geom_line(aes(x=progression_data$date, y=progression_data$confirmed, color="Diagnosed"), size=1.25) +
      geom_line(aes(x=progression_data$date, y=progression_data$ailing, color="Ailing"), size=1.25) +
      geom_line(aes(x=progression_data$date, y=progression_data$deaths, color="Deaths"), size=1.25) +
      geom_line(aes(x=progression_data$date, y=progression_data$recovered, color="Recovered"), size=1.25) +
      scale_colour_manual(values=clrs) +
      theme(legend.position="bottom", legend.title = element_blank())
    
    progressPlot
  })

  
  
  # Bar chart World
  output$barchart_world <- renderPlot({

    countries = input$countries
    selected_countries_data = subset(global_breakdown, global_breakdown$Country %in% countries)
    selected_countries_data_df = data.frame(Country=selected_countries_data$Country,
                                            CaseFatality=selected_countries_data$totalDeaths,
                                            Recovered=selected_countries_data$totalRecoveredCases,
                                            Ailing=selected_countries_data$totalConfirmedCases-selected_countries_data$totalRecoveredCases-selected_countries_data$totalDeaths)
    selected_countries_data_melt <- melt(selected_countries_data_df, id.vars=1)
    
    
    barchart <- ggplot(selected_countries_data_melt, aes(fill=variable, x=Country, y=value)) +   
      geom_bar(position = "fill", stat="identity") + 
      theme_minimal(18) + theme(axis.line = element_blank()) + 
      theme(legend.position="bottom", legend.title = element_blank()) +
      labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
      scale_fill_manual(values = clrs)
    
    barchart
  })


  # Line Chart State
  output$progress_state <- renderPlot({
    
    state_choosen <- state_select()
    selected_state_data = subset(covidtracking_data, state == state_choosen)

    us_states_progression = data.frame(Date=selected_state_data$date,
                                      Diagnosed=selected_state_data$positive,
                                      Hospitalized=selected_state_data$hospitalized,
                                      Deaths=selected_state_data$death)

    progression_state <- ggplot() +
      labs(x = NULL, y = '', title = NULL)+ 
      scale_x_date(date_breaks = "3 day")+
      geom_line(aes(x=us_states_progression$Date, y=us_states_progression$Diagnosed, color="Diagnosed"), size=1.25) +
      geom_line(aes(x=us_states_progression$Date, y=us_states_progression$Hospitalized, color="Hospitalized"), size=1.25) +
      geom_line(aes(x=us_states_progression$Date, y=us_states_progression$Deaths, color="Deaths"), size=1.25) +
      scale_colour_manual(values=clrs) +
      theme_minimal(18)+
      theme(legend.position="bottom", legend.title = element_blank())

    progression_state
  })


  # Bar chart United States
  output$barchart_us <- renderPlot({

    states = input$states
    selected_states_data = subset(us_breakdown, us_breakdown$State %in% states)
    selected_states_data_df = data.frame(State=selected_states_data$State,
                                            CaseFatality=selected_states_data$totalDeaths,
                                            Recovered=selected_states_data$totalRecoveredCases,
                                            Ailing=selected_states_data$totalConfirmedCases-selected_states_data$totalRecoveredCases-selected_states_data$totalDeaths)
    selected_states_data_melt <- melt(selected_states_data_df, id.vars=1)
    
    
    barchartus <- ggplot(selected_states_data_melt, aes(fill=variable, x=State, y=value)) +   
      geom_bar(position = "fill", stat="identity") + 
      theme_minimal(18) + theme(axis.line = element_blank()) + 
      theme(legend.position="bottom", legend.title = element_blank()) +
      labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
      scale_fill_manual(values = clrs)
    
    barchartus
  })
  
  # Heat Map United States
  output$heatmap_us <- renderLeaflet({

    us_states_data = data.frame(State=us_breakdown$State,
                                Diagnosed=us_breakdown$totalConfirmedCases,
                                Recovered=us_breakdown$totalRecoveredCases,
                                Deaths=us_breakdown$totalDeaths)
    
    
    data <- left_join(data_map, us_states_data, by = c("State"))
    
    tmap_mode("view")
    map <- tm_shape(st_as_sf(data)) +
      tm_polygons(input$countType,
                  id="State",
                  style = "jenks",
                  palette = "BuPu",
                  border.col = "white",
                  border.alpha = 0.3,
                  popup.vars = c("State: " = "State", "Diagnosed: "= "Diagnosed", "Deaths: " = "Deaths", "Recovered: " = "Recovered")) + 
      tm_layout(inner.margins = c(0.10, 0.10, 0.10, 0.20)) +
      tm_shape(aggregate_map(st_as_sf(data_map), by = "State")) +
      tm_borders(col = "black", alpha = 0.4) +
      tm_view(basemaps = "OpenStreetMap",
              view.legend.position = c("right","bottom"),
              set.view = c(-97, 37, 3))
    
    tmap_leaflet(map)
    
  })

}

shinyApp(ui = ui, server = server)