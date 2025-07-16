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
library(patchwork)
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
        col_widths = c(6, 6),
        fill = FALSE,
        height = 140,
        
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
        
        # donut plots
        plotOutput("donuts_allSETs")
    ),
    
    # main body
    navset_card_tab(
        full_screen = TRUE,
        id = "tabs",
        
        # sidebar: reserve info ----
        sidebar = sidebar(id = "reserve_sidebar",
                          position = "right",
                          width = "50%",
                          height = "90vh",
                          open = FALSE,
                          uiOutput("reserve_section")
        ),
        
        # # sidebar: station info ----
        # sidebar = sidebar(id = "station_sidebar",
        #                   position = "right",
        #                   width = "50%",
        #                   height = "90vh",
        #                   open = FALSE,
        #                   uiOutput("station_section")
        # ),
        
        
        
        # map tabs ----
        # panel 1: pie chart map ----
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
                    )
                    
                    
                ), # end sidebar
                
                # map
                leafletOutput("map1")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 1
        
        # panel 2: SET-level ----
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
                    
                    # choose which values to see
                    # div(class = "two-col-checks",
                    #     checkboxGroupInput("trendShow_sel", "Select results to include:",
                    #                        choices = c("increasing",
                    #                                    "decreasing",
                    #                                    "no trend",
                    #                                    "not calculated"),
                    #                        selected = c("increasing",
                    #                                     "decreasing",
                    #                                     "no trend",
                    #                                     "not calculated"),
                    #                        inline = TRUE)
                    # )
                    
                    
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
                layerId = ~Res_SET,
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
                layerId = ~Res_SET,
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
    # Create reactive value to store selected reserve/station
    selected_reserve <- reactiveVal(NULL)
    selected_station <- reactiveVal(NULL)
    
    # Add click observers to all maps
    observeEvent(input$map1_marker_click, {
        click <- input$map1_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        reserve_id <- click$id
        selected_reserve(reserve_id)
        
        # Open the sidebar when a station is clicked
        sidebar_toggle("reserve_sidebar", open = TRUE)
        
    })
    
    observeEvent(input$map2_marker_click, {
        click <- input$map2_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        
        station_id <- click$id
        reserve <- set_details[which(set_details$Res_SET == station_id), "reserve"] 
        selected_reserve(reserve)
        selected_station(station_id)
        # Open the sidebar when a station is clicked
        sidebar_toggle("reserve_sidebar", open = TRUE)
        
    })
    
    
    # Plotly graphs----
    output$stn_timeSeries <- renderPlotly({
        
        # identify the station
        stn <- selected_station()
        
        # subset the data for that station
        toplo <- set_avgd_readings |> 
            filter(Res_SET == stn)
        
        # make a nice plotly graph with it
        p <- plot_cumu_set(toplo, height_cumu)
        
        ggplotly(p)
        
    })
    
    output$reserve_timeSeries <- renderPlotly({
        
        # identify the station
        res <- selected_reserve()
        
        # subset the data for that station
        toplo <- set_avgd_readings |> 
            filter(reserve == res)
        
        # make a nice plotly graph with it
        p <- plot_cumu_set(toplo, height_cumu)
        
        ggplotly(p)
        
    })
    
    
    # reserve list subsetting ----
    selected_reserve_list <- reactive({
        req(selected_reserve())
        reserves_sets_list[[selected_reserve()]]
    })
    
    # summary statement ----
    output$reserve_info <- renderText({
        req(selected_reserve())
        
        res_inf <- reserve_details |> 
            filter(ReserveCode == selected_reserve()) |> 
            distinct()
        
        # n_sets <- reserves_sets_list[[selected_station()]]$total$n
        n_sets <- selected_reserve_list()$total$n
        
        res_out <- res_inf |> 
            glue_data("<b>{ReserveCode}</b> is {ReserveName} NERR in {ReserveState}. 
            This reserve monitors <b>{n_sets} stations</b>; expand the sections below for more detail. <br><br>
            For more information about the reserve, please visit <a href='{ReserveWebsite}' target='_blank'>their website</a>.")
        
        HTML(res_out)
    })
    
    # SET summaries ----
    # dates installed
    output$sets_installed <- renderReactable({
        req(selected_reserve_list())
        
        selected_reserve_list()$installations |> 
            select(-reserve) |> 
            arrange(start_year) |> 
            reactable(
                columns = list(
                    start_year = colDef(name = "Year"),
                    n = colDef(name = "# SETs with first reading")
                )
            )
    })
    
    
    # SET types
    output$sets_types <- renderReactable({
        req(selected_reserve_list())
        
        selected_reserve_list()$types |> 
            select(-reserve) |> 
            arrange(set_type) |> 
            reactable(
                columns = list(
                    set_type = colDef(name = "SET type"),
                    n = colDef(name = "# monitored")
                )
            )
    })
    
    
    # dominant veg types
    output$sets_vegs <- renderReactable({
        req(selected_reserve_list())
        
        selected_reserve_list()$vegs |> 
            select(-reserve) |> 
            arrange(dominant_vegetation) |> 
            reactable(
                columns = list(
                    dominant_vegetation = colDef(name = "Dominant vegetation type(s)"),
                    n = colDef(name = "# SETs in this setting")
                )
            )
    })
    
    
    # salinities
    output$sets_salinities <- renderReactable({
        req(selected_reserve_list())
        
        selected_reserve_list()$salinities |> 
            select(-reserve) |> 
            reactable(
                columns = list(
                    general_salinity = colDef(name = "General salinity regime"),
                    n = colDef(name = "# SETs in this setting")
                )
            )
    })
    
    
    
    # station table ----
    output$stn_tbl <- renderReactable({
        req(selected_station())
        
        # filter some station information table; name it 'tbl'
        tbl <- set_details |> 
            filter(Res_SET == selected_station()) |> 
            mutate(rate2 = glue("{round(rate, 2)} (95% CI: {round(CI_low, 2)}, {round(CI_high, 2)})"),
                   start_yr = lubridate::year(start_date),
                   end_yr = lubridate::year(end_date),
                   time_series = glue("{round(ts_length, 1)} years ({start_yr} - {end_yr})")) |> 
            select(Reserve = reserve,
                   "SET Code" = set_id,
                   "SET Name" = user_friendly_set_name,
                   "SET Type" = set_type,
                   "Monitoring Time Period" = time_series,
                   "Rate of change (mm/yr)" = rate2,
                   "Keeping up with long-term SLR?" = dir_slr,
                   "Keeping up with 19-yr water level change?" = dir_19yr,
                   "Dominant vegetation" = dominant_vegetation,
                   "General Salinity" = general_salinity
            ) |> 
            mutate(across(everything(), as.character)) |> 
            pivot_longer(cols = everything(),
                         names_to = "Characteristic",
                         values_to = "Value")
        
        reactable(tbl,
                  sortable = FALSE
                  )
    })
    
    # sidebar ui ----
    # with accordion
    output$reserve_section <- renderUI({
        # req(selected_station())
        
        # reserve-level ----
        if(input$tabs == "Reserve-level"){
            accordion(
                id = "reserve_accordion",
                open = FALSE,
                
                h4(paste0("Selected Reserve: ", selected_reserve())),
                
                htmlOutput("reserve_info"),
                br(),
                br(),
                
                span(strong("More details about SETs at this reserve:"),
                     tooltip(
                         bsicons::bs_icon("info-circle"),
                         HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                     )),
                br(),
                
                
                # SET types
                accordion_panel(
                    title = "Types of SETs monitored",
                    reactableOutput("sets_types")
                ),
                
                # when installed/first read
                accordion_panel(
                    title = "Installation Dates",
                    reactableOutput("sets_installed")
                ),
                
                # dominant vegetation
                accordion_panel(
                    title = "Surrounding Vegetation",
                    reactableOutput("sets_vegs")
                ),
                
                # general salinity
                accordion_panel(
                    title = "Salinity Regime(s)",
                    reactableOutput("sets_salinities")
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
                                p(strong("THESE ARE NOT REAL OPTIONS AT THE MOMENT")),
                                br(),
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
                        withWaiter(plotlyOutput("reserve_timeSeries"))
                    )
                )
            )

            # station level ui ----
        } else if(input$tabs == "SET-level") {
            accordion(
                id = "reserve_accordion",
                open = FALSE,
                
                h4(paste0("Selected SET: ", selected_station())),
                
                htmlOutput("reserve_info"),
                br(),
                br(),
                
                span(strong("More details about SETs at this reserve:"),
                     tooltip(
                         bsicons::bs_icon("info-circle"),
                         HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                     )),
                br(),
                
                
                # SET types
                accordion_panel(
                    title = "SET summary",
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
                                p(strong("THESE ARE NOT REAL OPTIONS AT THE MOMENT")),
                                br(),
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
            
        } else {
            h3("Sidebar is useless in this tab")
        }
        
    })
    
    
   # donut charts ----
    output$donuts_allSETs <- renderPlot({
        plot_set_distn_pair(p_allSETs_longterm, p_allSETs_19yr,
                            legend.position = "right") +
            plot_annotation(subtitle = paste0("Compared to which water level change:\n(n = ", nrow(set_details), " SETs in this view)"),
                            theme = theme(plot.title.position = "panel",
                                          plot.subtitle = element_text(size = rel(0.9)))) &
            theme(legend.text = element_text(size = rel(0.8)),
                  legend.key.size = unit(0.8, "lines"))
    }, height = 140, res = 96)
    
    
}

# Run the app
shinyApp(ui, server)
