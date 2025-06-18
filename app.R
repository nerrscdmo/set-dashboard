library(shiny)
library(bslib)
library(bsicons)
library(here)
library(plotly)
library(leaflet)
library(leaflegend)
library(htmltools)
library(reactable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(waiter)


# setup ----
source("functions.R")
source("global.R")

# UI ----
ui <- page_fillable(
    useWaiter(),
    autoWaiter(html = spin_3(),
               color = transparent(0.5)),
    
    
    # css/html styling ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Header ----
    layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        
        # Header, github link, description
        div(
            # Header + GitHub link
            # modify h3 and github link for new dashboards
            div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                h3("NERRS Monitoring: PARAMETER HERE"),
                tags$a(
                    icon("github"), 
                    href = "https://github.com/nerrscdmo/dashboard-template", 
                    target = "_blank", 
                    style = "color: inherit;"
                )
            ),
            
            # Description
            # modify this for each parameter
            p("Choose a tab to explore PARAMETER nationally. Change options in the left sidebar, and click on a station to see more detail in a right sidebar.")
        ),
        
        # Value box
        value_box(
            title = "VALUE BOX TITLE",
            value = textOutput("station_count"),
            showcase = bsicons::bs_icon("graph-down-arrow"),
            showcase_layout = "left center",
            theme = "danger",
            max_height = "120px"
        )
    ),
    
    # main body
    navset_card_tab(
        full_screen = TRUE,
        
        # sidebar: stn info ----
        sidebar = sidebar(id = "stn_sidebar",
                          position = "right",
                          width = "50%",
                          height = "90vh",
                          open = FALSE,
                          uiOutput("station_section")
        ),
        
        
        
        # map tabs ----
        # panel 1: map 1 ----
        nav_panel(
            "PANEL 1",
            full_screen = TRUE,
            
            card_header("HEADER 1",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Information about calculation of what is shown on the map."
                        ), # end tooltip
            ), # end header
            
            
            
            # sidebar layout
            
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    div(
                        "Select OPTION to view: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Information on choices"
                        ),
                        radioButtons("panel1Param_sel", label = NULL,
                                     choiceNames = c("Option 1",
                                                     "Option 2",
                                                     "Option 3"),
                                     choiceValues = c("opt1",
                                                      "opt2",
                                                      "opt3"),
                                     selected = "opt1")
                    ),
                    
                    # choose which values to see
                    div(class = "two-col-checks",
                        checkboxGroupInput("trendShow_sel", "Select results to include:",
                                           choices = c("increasing",
                                                       "decreasing",
                                                       "no trend",
                                                       "not calculated"),
                                           selected = c("increasing",
                                                        "decreasing",
                                                        "no trend",
                                                        "not calculated"),
                                           inline = TRUE)
                    )
                ), # end sidebar
                
                # map
                leafletOutput("map1")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 1
        
        # panel 2: map 2 ----
        nav_panel(
            "PANEL 2",
            full_screen = TRUE,
            
            card_header("HEADER 2",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Info about what is on the map"
                        ), # end tooltip
            ), # end header
            
            
            
            # sidebar layout
            
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    div(
                        "Select OPTION to view: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Info about options."
                        ),
                        radioButtons("panel2Param_sel", label = NULL,
                                     choiceNames = c("Option 1",
                                                     "Option 2",
                                                     "Option 3"),
                                     choiceValues = c("opt1",
                                                      "opt2",
                                                      "opt3"),
                                     selected = "opt1")
                    ),
                    
                    
                ), # end sidebar
                
                # map
                leafletOutput("map2")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 2        
        
        
        # panel 3: Instructions ----
        nav_panel(
            "Using this dashboard",
            full_screen = FALSE,
            
            card_header("Tips and Tricks",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Info here"
                        ) # end tooltip
            ), # end header
            
            p("Put text here about different options"),
            p("...what each tab does, how to interact with the maps, what pops up in the sidebar and how you can interact with that"),
            p("...about SWMP data"),
            p("...text about dashboard development and for more information, etc.")
            
        ) # end nav-panel 3
        
    ) # end nav-panel layout
    
)  # end ui


# Server ----
server <- function(input, output, session) {
    # waiter new ----
    w <- Waiter$new(
        html = spin_3(),
        color = transparent(0.5)
    )
    
    # map1 setup----
    output$map1 <- renderLeaflet({
        # base map
        m <- leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        
        # in reality, add data, markers, and legends too
        
        # return the map
        m
        
    })
    
    # map2 setup----
    output$map2 <- renderLeaflet({
        # base map
        m <- leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        
        # in reality, add data, markers, and legends too
        
        # return the map
        m
        
    })
    

    # map1 update ----
    observe({
        
        # filter the dataset based on user selections
        
        # see if a station has been selected
        current_station_id <- selected_station()
        
        # if no station selected, normal map:
        if(is.null(current_station_id)){
            
            m <- leafletProxy("map1", data = FILTERED_DATA_ABOVE) |>
                clearMarkers() |>
                addCircleMarkers(
                    # stuff here
                )  |>
                clearControls()
            
        } else {
            # make the selected station stand out
            m <- leafletProxy("map1", data = FILTERED_DATA_ABOVE) |>
                clearMarkers() |>
                addCircleMarkers(
                    # stuff
                    # outline the point and make it heavier
                    color = ~ifelse(station == current_station_id, "#E67E22", "black"),
                    weight = ~ifelse(station == current_station_id, 4, 1),
                    # other point control stuff
                )  |>
                clearControls()
        }
        
        # do some other things here based on parameter inputs (make correct legends)
        
        m
        
    })
    
    
    # map2 update ----
    observe({
        # filter the dataset based on user selections
        
        # see if a station has been selected
        current_station_id <- selected_station()
        
        # if no station selected, normal map:
        if(is.null(current_station_id)){
            
            m <- leafletProxy("map2", data = FILTERED_DATA_ABOVE) |>
                clearMarkers() |>
                addCircleMarkers(
                    # stuff here
                )  |>
                clearControls()
            
        } else {
            # make the selected station stand out
            m <- leafletProxy("map2", data = FILTERED_DATA_ABOVE) |>
                clearMarkers() |>
                addCircleMarkers(
                    # stuff
                    # outline the point and make it heavier
                    color = ~ifelse(station == current_station_id, "#E67E22", "black"),
                    weight = ~ifelse(station == current_station_id, 4, 1),
                    # other point control stuff
                )  |>
                clearControls()
        }
        
        # do some other things here based on parameter inputs (make correct legends)
        
        m
        
    })
    
    # map clicks ----
    # Create reactive value to store selected station
    selected_station <- reactiveVal(NULL)
    
    # Add click observers to all maps
    observeEvent(input$map1_marker_click, {
        click <- input$map1_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        station_id <- click$id
        selected_station(station_id)
        
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
    })
    
    observeEvent(input$map2_marker_click, {
        click <- input$map2_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        
        station_id <- click$id
        selected_station(station_id)
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
        
    })
    

    # Plotly graphs----
    output$stn_timeSeries <- renderPlotly({
        
        # identify the station
        stn <- selected_station()
        
        # subset the data for that station
        
        # make a nice plotly graph with it
        
    })
    
    
    # reserve info ----
    output$reserve_info <- renderText({
        req(selected_station())
        
        res_inf <- stn_summaries |> 
            filter(station == selected_station()) |> 
            select(station, StationName, ReserveCode, ReserveName, ReserveState, ReserveWebsite, NERRAPage) |> 
            distinct()
        
        res_out <- res_inf |> 
            glue_data("<b>{station}</b> is the {StationName} monitoring station at {ReserveName} ({ReserveCode}) NERR in {ReserveState}. 
                      For more information about the reserve, please visit <a href='{ReserveWebsite}' target='_blank'>their website</a>.")
        
        HTML(res_out)
    })
    
    
    # station table ----
    output$stn_tbl <- renderReactable({
        req(selected_station())
        
        # filter some station information table; name it 'tbl'
        
        reactable(tbl,
                  sortable = FALSE,
                  columns = list(
                      Column1 = colDef(align = "center"),
                      Column2 = colDef(align = "center")
                  ))
    })
    
    # station sidebar ui ----
    # with accordion
    output$station_section <- renderUI({
        req(selected_station())
        
        accordion(
            id = "station_accordion",
            open = FALSE,
            
            h4(paste0("Selected Station: ", selected_station())),
            
            htmlOutput("reserve_info"),
            br(),
            br(),
            
            span("Station details:",
                 tooltip(
                     bsicons::bs_icon("info-circle"),
                     HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                 )),
            br(),
            
            
            # numeric outputs
            accordion_panel(
                title = "Numeric Summary",
                reactableOutput("stn_tbl")
            ),
            
            # Plotly graph, with accordioned options
            accordion_panel(
                title = "Time Series Graphs",
                tags$small("The graph section can be popped out to full screen from the bottom right corner."),
                
                card(
                    full_screen = TRUE,
                    height = "50vh",
                    
                    
                    # options popover
                    popover(
                        actionButton("btn", "Graph options", 
                                     icon = icon("sliders"),
                                     width = 200,
                                     class = "small-btn"),
                        div(
                            checkboxGroupInput("thresh_sel", "Select threshold(s) of interest:",
                                               choices = c("2 mg/L", "5 mg/L"),
                                               selected = c("2 mg/L", "5 mg/L")),
                            checkboxInput("var_sel", "Add middle 50% of values",
                                          value = FALSE),
                            checkboxInput("minmax_sel", label = "Add min/max",
                                          value = FALSE)
                        ),
                        title = "Graph Options",
                        placement = "right"
                    ),
                    
                    # graph
                    withWaiter(plotlyOutput("stn_timeSeries"))
                )
            )
        )
    })
    
    # value box ----
    output$station_count <- renderText({
        # calculate something here to show in the value box
    })
    
    
}

# Run the app
shinyApp(ui, server)
