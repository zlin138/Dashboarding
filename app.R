library(shiny)
library(shinydashboard)
library(here)
source("cogcc_helper.R")

ui <- dashboardPage(
    # --- SetUp ---------------  
    skin  = "black",
    
    dashboardHeader(title = strong("Pipeline Spill Report")),
    dashboardSidebar(
      width = 300, 
      sidebarMenu(
        menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("gauge")), 
        menuItem("Explore", tabName = "table", icon = icon("table")))
      ),
    
    dashboardBody(
      selectInput("v_date", strong("Select Date"),
                  c("This Year", "Last Year", "Last 5 Years", "All Time")),
      tabItems(
        tabItem(tabName = "tab_dashboard", 
                fluidRow(valueBoxOutput("avg_surface_area", width =3 ),
                         valueBoxOutput('nearby_building', width =3),
                         valueBoxOutput("nearby_waterwells", width =3 ),
                         valueBoxOutput("depth_groundwater", width =3 )),
                fluidRow(box(leafletOutput("map"), width = 6, solidHeader = T),
                         box(plotOutput("temperature"))), 
                fluidRow(box(plotOutput("spill_volume"), width = 6, solidHeader = TRUE, status = "primary"), 
                         box(plotOutput('failure_type'), width = 6, solidHeader = TRUE, status = "primary"))),
        tabItem(tabName = "table", 
                fluidRow(box(dataTableOutput("spill_table"), width =12)))
        )
    )
)


# box(plotlyOutput('failure_type'), width = 12, solidHeader = TRUE),

# Define server logic to render plots 
server <- function(input, output) {
  
  fetch_data <- reactive({
    req(input$v_date)
    current_year <- year(Sys.Date())
    date_range <- c()
    if(input$v_date == "This Year"){
      date_range <- date_range %>% append(current_year-1)
      date_range <- date_range %>% append(current_year)
    }
    else if(input$v_date == "Last Year"){
      date_range <- date_range %>% append(current_year -2)
      date_range <- date_range %>% append(current_year -1)
    }
    else if(input$v_date == "Last 5 Years"){
      date_range <- date_range %>% append(current_year-5)
      date_range <- date_range %>% append(current_year)
    }
    else{
      date_range <- date_range %>% append(spills_df$year_of_discovery %>% min())
      date_range <- date_range %>% append(spills_df$year_of_discovery %>% max())
    }
    spills_df %>% 
      filter(year_of_discovery > date_range[1] & year_of_discovery <= date_range[2])
  })
  
  output$avg_surface_area <- renderValueBox({
    avg_area <- fetch_data() %>% 
    select(surface_area_length, surface_area_width) %>% 
    mutate(surface_area = surface_area_length*surface_area_width) %>% 
    summarise(avg_surface_area = mean(surface_area, na.rm = T)) %>% 
      pluck(1)
    
    valueBox(
      value = round(avg_area), 
      subtitle = strong("Average Spill Surface Area(ft squared)"),
      color = "fuchsia",
      icon =icon("oil-well"))
  }
  )
  
  output$nearby_building <- renderValueBox({
    avg_distance <- fetch_data() %>% 
      summarise(avg_distance = mean(occupied_buildings, na.rm = T)) %>% 
      pluck(1)
    
    valueBox(
      value = round(avg_distance),
      subtitle = strong("Average Distance to Nearby Building(ft)"),
      color = "teal", 
      icon = icon("building"))
  })
  
  output$nearby_waterwells <- 
    renderValueBox({
      avg_waterwells <- fetch_data() %>% 
        summarise(water_well_avg = mean(water_wells_in_area, na.rm = T)) %>% 
        pluck(1)
      
      valueBox(
        value = round(avg_waterwells),
        subtitle = strong("Average Water Wells in Area"),
        color = "green", 
        icon = icon("droplet"))
    })
  
  output$depth_groundwater <- 
    renderValueBox({
      avg_depth <- fetch_data() %>% 
        summarise(depth_avg = mean(depth_to_groundwater, na.rm = T)) %>% 
        pluck(1)
      
      valueBox(
        value = round(avg_depth),
        subtitle = strong("Average Depth to GroundWater"),
        color = "orange", 
        icon = icon("fill-drip") )
    })
  
  
  output$spill_volume <- renderPlot({
    req(fetch_data())
    fetch_data() %>% 
      select(spilled_outside_of_berms:drilling_fluid_spill_volume)  %>% 
      select(contains('volume')) %>% 
      colnames() %>% 
      map( ~ 
             fetch_data() %>% 
             group_by(!!sym(.x)) %>% 
             count(name = "occurance") %>% 
             ungroup() %>%
             drop_na() %>% 
             ggplot(aes(x = fct_reorder(!!sym(.x), occurance),  y = occurance, fill = !!sym(.x))) + 
             geom_col() + 
             coord_flip() + 
             labs(title = .x %>% str_replace_all('_', ' ') %>% str_to_title(), x = '', y = 'Occurance')  +
             theme_pander() + 
             scale_fill_pander() + 
             theme(legend.position = "none")
      ) %>% 
      patchwork::wrap_plots(p, nrow = 3, ncol = 2)
  })
  
  
    output$failure_type <- renderPlot({
        req(fetch_data())
        fetch_data() %>% 
          select(human_error:other) %>% 
          mutate(missing_data = if_all(human_error:other, is.na)) %>% 
          mutate(entry_error = !(human_error | equipment_failure| historical_unkown | other)) %>% 
          mutate(across(-c(historical_unkown), ~ case_when(
            historical_unkown ~ FALSE, # If historical unknown all other categories should be false 
            TRUE ~. )))%>%
          gather() %>% 
          group_by(key) %>% 
          count(value) %>% 
          filter(value == TRUE) %>% 
          ungroup() %>% 
          mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
          mutate(key = fct_reorder(key, n)) %>% 
          ggplot(aes(x = key, y = n, color = key), alpha = 0.8) + 
          geom_point(size = 8) + 
          geom_segment(aes(x = key, xend = key, y = n, yend = 0)) + 
          theme_pander() + 
          scale_colour_tableau()+ 
          labs(title = "Pipeline failure types", x = " ", y = ' ') + 
          theme(legend.position =  'none') + 
          scale_x_discrete(guide = guide_axis(n.dodge = 2))
  })
    
  output$map <- renderLeaflet({
    req(fetch_data())
    fetch_data() %>% 
      leaflet() %>%
      addTiles() %>% 
      addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())
  })
  
  output$temperature <- renderPlot({
    
    
      fetch_data() %>% 
        select(weather_conditions) %>% 
        drop_na(weather_conditions) %>% 
        ggplot(aes(x= weather_conditions)) + 
      # bin tested using Scott and Freedman's rule 
        geom_histogram( color= "snow1", fill = "skyblue", bins = 27)  + 
        theme_pander() + 
        labs(title = "Temperature Distribution", y = "Spill Incidents", x = "Temperature")
  })
  
  output$spill_table <- renderDataTable({ 
    req(fetch_data())
    fetch_data() %>% 
    select(date_of_discovery, operator, facility_type, county, spill_description, root_cause, preventative_measures)  %>% 
    mutate(spill_description = str_trunc(spill_description, 100, "right")) %>% 
    mutate(root_cause = str_trunc(root_cause, 100, "right")) %>% 
    mutate(preventative_measures = str_trunc(preventative_measures, 100, "right")) 
  })
  
}

# Run the application 
shinyApp(ui, server)
