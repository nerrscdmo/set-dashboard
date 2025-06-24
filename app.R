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
library(glue)
# library(SETr)


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
        col_widths = c(6, 3, 3),
        fill = FALSE,
        
        # Header, github link, description
        div(
            # Header + GitHub link
            div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                h3("NERRS Monitoring: Marsh Surface Elevation"),
                tags$a(
                    icon("github"), 
                    href = "https://github.com/nerrscdmo/set-dashboard", 
                    target = "_blank", 
                    style = "color: inherit;"
                )
            ),
            
            # Description
            # modify this for each parameter
            p("Choose a tab to explore marsh surface elevation change nationally. Change options in the left sidebar, and click on a station to see more detail in a right sidebar.")
        ),
        
        # Value box
        value_box(
            title = "# SETs keeping up with SLR",
            value = textOutput("station_count"),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            showcase_layout = "left center",
            theme = "success",
            max_height = "120px"
        ),
        
        # Value box 2
        value_box(
            title = "# SETs not keeping up with SLR",
            value = textOutput("station_count2"),
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
            "Reserve-level",
            # in this panel, show pie charts by reserve
            full_screen = TRUE,
            
            card_header("What is the split of SETs keeping up vs. not, by Reserve?",
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
                        "Compare reserves to: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Information on choices"
                        ),
                        radioButtons("panel1Param_sel", label = NULL,
                                     choiceNames = c("Long-term sea level rise",
                                                     "19-year water level change"),
                                     choiceValues = c("longterm",
                                                      "yr19"),
                                     selected = "longterm")
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
            "SET-level",
            # show all SETs here, with the up/down arrows
            full_screen = TRUE,
            
            card_header("How is elevation change spread within a reserve? (Zoom in!)",
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
                        "Compare SETs to: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Info about options."
                        ),
                        radioButtons("panel2Param_sel", label = NULL,
                                     choiceNames = c("Long-term sea level rise",
                                                     "19-year water level change"),
                                     choiceValues = c("longterm",
                                                      "yr19"),
                                     selected = "longterm")
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
    # map1 is the pie charts - reserve level
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
        
        # initially, render comparison to long-term slr
        
        m <- m |> 
            addMarkers(
                data = reserve_mappiness,
                lng = ~lon,
                lat = ~lat,
                layerId = ~reserve,
                icon = ~icons(iconUrl = pie_longterm,
                              iconWidth = 30,
                              iconHeight = 30) 
            ) |> 
            addLegend(colors = cols_slr,
                      labels = names(cols_slr),
                      position = "bottomleft",
                      opacity = 0.9,
                      title = "Are marshes keeping up with <br>long-term sea level rise?")
        
        # return the map
        m
        
    })
    
    # map2 setup----
    # map 2 is SET-level 
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
        
        # generate it with the longterm comparison
        m <- m |> 
            addCircleMarkers(
                data = set_details,
                lng = ~long,
                lat = ~lat,
                layerId = ~set_id,
                color = "black",
                weight = 1,
                fillColor = ~leaflet_colors(dir_slr),
                fillOpacity = 0.5,
                radius = 8
            ) |> 
            addLegend(colors = cols_slr,
                      labels = names(cols_slr),
                      position = "bottomleft",
                      opacity = 0.9,
                      title = "Are marshes keeping up with <br>long-term sea level rise?")    
        
        # return the map
        m
        
    })
    

    # map1 update ----
    observe({
        
        # waiter show ----
        # spinner while everything is recalculating
        w$show()
        on.exit(w$hide())
        
        comparison <- input$panel1Param_sel
        comp_dat <- reserve_mappiness |> 
            mutate(iconURL = case_when(comparison == "longterm" ~ pie_longterm,
                                       comparison == "yr19" ~ pie_19yr))
        
        legendtitle <- ifelse(comparison == "longterm",
                              "Are marshes keeping up with <br>long-term sea level rise?",
                              "Are marshes keeping up with <br>the rate of water level change <br>over the last 19 years?")
        
        
        m <- leafletProxy("map1") |> 
            clearMarkers() |> 
            clearControls() |> 
            addMarkers(
                data = comp_dat,
                lng = ~lon,
                lat = ~lat,
                layerId = ~reserve,
                icon = ~icons(iconUrl = iconURL,
                              iconWidth = 30,
                              iconHeight = 30) 
            ) |> 
            addLegend(colors = cols_slr,
                      labels = names(cols_slr),
                      position = "bottomleft",
                      opacity = 0.9,
                      title = legendtitle)
        
        m
        
    })
    
    
    # map2 update ----
    observe({
        
        # waiter show ----
        # spinner while everything is recalculating
        w$show()
        on.exit(w$hide())
        
        
        comparison <- input$panel2Param_sel
        comp_dat <- set_details |> 
            mutate(outcome = case_when(comparison == "longterm" ~ dir_slr,
                                       comparison == "yr19" ~ dir_19yr))
        
        legendtitle <- ifelse(comparison == "longterm",
                              "Are marshes keeping up with <br>long-term sea level rise?",
                              "Are marshes keeping up with <br>the rate of water level change <br>over the last 19 years?")
        
        
        m <- leafletProxy("map2") |> 
            clearMarkers() |> 
            clearControls() |> 
            addCircleMarkers(
                data = comp_dat,
                lng = ~long,
                lat = ~lat,
                layerId = ~set_id,
                color = "black",
                weight = 1,
                fillColor = ~leaflet_colors(outcome),
                fillOpacity = 0.5,
                radius = 8
            ) |> 
            addLegend(colors = cols_slr,
                      labels = names(cols_slr),
                      position = "bottomleft",
                      opacity = 0.9,
                      title = legendtitle)
        
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
        reserve <- set_details[which(set_details$set_id == station_id), "reserve"] 
        selected_station(reserve)
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
        
        res_inf <- reserve_details |> 
            filter(ReserveCode == selected_station()) |> 
            distinct()
        
        res_out <- res_inf |> 
            glue_data("<b>{ReserveCode}</b> is {ReserveName} NERR in {ReserveState}. 
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
        # req(selected_station())
        
        accordion(
            id = "station_accordion",
            open = FALSE,
            
            h5("There will be reserve and/or SET information in this sidebar"),
            p("at some point"),
            
            h4(paste0("Selected Reserve: ", selected_station())),

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
    
    # value box 2----
    output$station_count2 <- renderText({
        # calculate something here to show in the value box
    })
    
    
}

# Run the app
shinyApp(ui, server)
