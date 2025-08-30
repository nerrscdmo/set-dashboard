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
source("explanations.R")

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
        
        
        # map tabs ----
        # panel 1: SET-level ----
        nav_panel(
            "Station-level",
            full_screen = TRUE,
            
            card_header("How is marsh elevation changing? (Zoom in to see multiple stations at a single reserve!)",
                        # details popover
                        popover(
                            actionButton("btnStations", "Map Details", 
                                         icon = icon("map-location-dot"),
                                         width = 175,
                                         class = "small-btn"),
                            p("This map shows how each station's elevation is changing, as compared to water level change."),
                            br(),
                            p(strong("What do we want to see?"), "We want to see more blue than red: we want marshes to be able to keep up with increasing water levels."),
                            br(),
                            p(strong("Clicking on a point"), "will open a sidebar with more details about the selected station: e.g., what is its name; how long has it been installed; is it keeping up with long- and near-term water level change."),
                            p(strong("Each point"), "represents a single station. A trend has been calculated for each station with enough data. Stations without enough data are labeled 'not calculated' and are represented by the color gray Each station with a calculated trend was categorized based on whether the rate exceeded or did not exceed the selected rate of water level change (long-term or near-term)."),
                            br(),
                            p("See the 'About' tab for more detail on data sources and calculations."),
                            title = "Station-level map details",
                            placement = "right"
                        ), # end popover
            ), # end header

            
            # sidebar layout
            
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    div(
                        "Compare marsh elevations to: ",
                        popover(
                            bsicons::bs_icon("info-circle"),
                            p("The rate of water level change is not constant. It varies by geographic location, and also by time period."),
                            p("Here, ", strong("'long-term'"), "refers to the entire period of record for the water level station nearest the selected reserve/station; generally >50 years."),
                            p(strong("'Near-term'"), "represents the rate of change for a 19-year period ending the year of the most recent marsh elevation measurements at the reserve/station.")
                        ),
                        radioButtons("panel2Param_sel", label = NULL,
                                     choiceNames = c("Long-term water level change",
                                                     "Near-term water level change"),
                                     choiceValues = c("longterm",
                                                      "yr19"),
                                     selected = "longterm")
                    ),
                    
                    
                ), # end sidebar
                
                # map
                leafletOutput("map2")  # originally this was the 2nd panel, so 2nd map
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 1    
        
        # panel 2: pie chart map ----
        nav_panel(
            "Reserve-level",
            # in this panel, show pie charts by reserve
            full_screen = TRUE,
            
            card_header("Pie charts represent the proportion of stations at a reserve falling into each category",
                        # details popover
                        popover(
                            actionButton("btnReserves", "Map Details", 
                                         icon = icon("map-location-dot"),
                                         width = 175,
                                         class = "small-btn"),
                            p("This map combines results from all included stations within each reserve to a single pie chart."),
                            br(),
                            p(strong("What do we want to see?"), "We want to see more blue than red: we want marshes to be able to keep up with increasing water levels."),
                            br(),
                            p(strong("Pie Charts"), "represent the collection of stations at a reserve. Clicking on a pie chart will open a sidebar with more information about the reserve. A trend has been calculated for each station with enough data. Stations without enough data are labeled 'not calculated' and are represented by the color gray. Each station with a calculated trend was categorized based on whether the rate exceeded or did not exceed the selected rate of water level change (long-term or near-term). The pie chart combines each of these results into one graphic."),
                            br(),
                            p("See the 'About' tab for more detail on data sources and calculations."),
                            title = "Reserve-level map details",
                            placement = "right"
                        ), # end popover
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
                        popover(
                            bsicons::bs_icon("info-circle"),
                            p("The rate of water level change is not constant. It varies by geographic location, and also by time period."),
                            p("Here, ", strong("'long-term'"), "refers to the entire period of record for the water level station nearest the selected reserve/station; generally >50 years."),
                            p(strong("'Near-term'"), "represents the rate of change for a 19-year period ending the year of the most recent marsh elevation measurements at the reserve/station.")
                        ),
                        radioButtons("panel1Param_sel", label = NULL,                # this was originally panel 1
                                     choiceNames = c("Long-term sea level change",
                                                     "Near-term water level change"),
                                     choiceValues = c("longterm",
                                                      "yr19"),
                                     selected = "longterm")
                    )
                    
                    
                ), # end sidebar
                
                # map
                leafletOutput("map1")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 2
        
        # panel 3: About ----
        # in another (sourced; see above) file to save space here
        about_ui
        
        
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
                radius = 7
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
        
        
        # set up the leaflet proxy piece
        m <- leafletProxy("map1") |> 
            clearMarkers() |> 
            clearControls() 
        
        current_reserve_id <- selected_reserve()
        
        m <- m |> 
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
        
        # if a reserve has been selected, make a circle to go around it
        # If a station is selected, add a highlight circle around its marker
        if(!is.null(current_reserve_id)) {
            # Find the selected station in the data
            selected_row <- which(comp_dat$reserve == current_reserve_id)
            
            if(length(selected_row) > 0) {
                # Add a circle marker underneath the icon for the selected station
                selected_data <- comp_dat[selected_row, ]
                
                m <- m |> addCircleMarkers(
                    data = selected_data,
                    lng = ~lon,
                    lat = ~lat,
                    radius = 15,  # Larger than the icon to create a highlight effect
                    color = "#E67E22",  # Orange outline
                    weight = 5,
                    opacity = 1.0,
                    fillOpacity = 0  # Transparent fill
                )
            }
        }
        
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
        
        # see if a station has been selected
        current_station_id <- selected_station()
        
        # if no station selected, normal map:
        if(is.null(current_station_id)){
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
                    radius = 7
                ) |>
                addLegend(colors = cols_slr,
                          labels = names(cols_slr),
                          position = "bottomleft",
                          opacity = 0.9,
                          title = legendtitle)
        } else {
            # make the selected station stand out
            m <- leafletProxy("map2") |> 
                clearMarkers() |> 
                clearControls() |> 
                addCircleMarkers(
                    data = comp_dat,
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~Res_SET,
                    fillColor = ~leaflet_colors(outcome),
                    color = ~ifelse(Res_SET == current_station_id, "#E67E22", "black"),
                    weight = ~ifelse(Res_SET == current_station_id, 4, 1),
                    opacity = ~ifelse(Res_SET == current_station_id, 1.0, 0.7),
                    fillOpacity = ~ifelse(Res_SET == current_station_id, 0.9, 0.5),
                    radius = ~ifelse(Res_SET == current_station_id, 10, 7)
                ) |>
                addLegend(colors = cols_slr,
                          labels = names(cols_slr),
                          position = "bottomleft",
                          opacity = 0.9,
                          title = legendtitle)
        }
        
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
        
        # Open the sidebar when a reserve pie chart is clicked
        sidebar_toggle("reserve_sidebar", open = TRUE)
        
        # browser()
        
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
        
        # browser()
        
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
    
    output$stn_grouptimeSeries <- renderPlotly({
        
        # identify the station
        stn <- selected_station()
        res <- selected_reserve()
        
        # subset the data for that station
        toplo_res <- set_avgd_readings |> 
            filter(reserve == res)
        
        toplo_set <- set_avgd_readings |> 
            filter(Res_SET == stn)
        
        # make a nice plotly graph with it
        p <- ggplot(toplo_res,
                    aes(x = date,
                        y = height_cumu,
                        group = set_id)) +
            geom_hline(yintercept = 0,
                       col = "gray20") +
            geom_line(col = "gray70") +
            geom_line(data = toplo_set,
                      col = "blue",
                      linewidth = 1.5) +
            theme_classic() +
            labs(title = "Elevation change since first reading\nblue line is selected station; \ngray lines are other stations from Reserve", 
                 subtitle = "compared to other Reserve stations",
                 x = "Date",
                 y = "Change since first reading (mm)")
        
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
            Data from <b>{n_sets} stations</b> from this reserve are included in this dashboard; expand the sections below for more detail. <br><br>
            For more information about the reserve, please visit <a href='{ReserveWebsite}' target='_blank'>their website</a>.")
        
        HTML(res_out)
    })
    
    # SET summaries ----
    # SLR summaries actually
    output$slr_rates <- renderReactable({
        req(selected_reserve_list())
        
        tmp <- selected_reserve_list()$water_comp_summaries |> 
            select(-Total)
        
        # Define the expected columns
        expected_cols <- c("Water_Comparison", "Rate",
                           "Yes, more confident", "Yes, less confident",
                           "No, less confident", "No, more confident")
        
        # Add any missing columns with 0
        missing_cols <- setdiff(expected_cols, names(tmp))
        if (length(missing_cols) > 0) {
            for (col in missing_cols) {
                tmp[[col]] <- 0
            }
        }
        
        # Reorder: expected columns first, then keep any extras (like "Not calculated")
        ordered_cols <- c(expected_cols, setdiff(names(tmp), expected_cols))
        tmp <- tmp[, ordered_cols, drop = FALSE]
        
        
        reactable(tmp,
                  columns = list(
                      Water_Comparison = colDef(name = "Description"),
                      Rate = colDef(name = "Rate (95% CI)"),
                      `Yes, more confident` = colDef(name = "More Confident"),
                      `Yes, less confident` = colDef(name = "Less Confident"),
                      `No, less confident` = colDef(name = "Less Confident"),
                      `No, more confident` = colDef(name = "More Confident")
                  ),
                  columnGroups = list(
                      colGroup("Water Level Change", columns = c("Water_Comparison", "Rate")),
                      colGroup("# Keeping Up", columns = c("Yes, more confident", "Yes, less confident")),
                      colGroup("# Not Keeping Up", columns = c("No, less confident", "No, more confident"))
                  )
        )
        
    })
    
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
            mutate(rate2 = glue("{round(slr_rate, 2)} (95% CI: {round(slr_CI_low, 2)}, {round(slr_CI_high, 2)})"),
                   rate19yr = glue("{round(slr_19yr, 2)} (95% CI: {round(yr19_CI_low, 2)}, {round(yr19_CI_high, 2)})"),
                   start_yr = lubridate::year(start_date),
                   end_yr = lubridate::year(end_date),
                   time_series = glue("{round(ts_length, 1)} years ({start_yr} - {end_yr})")) |> 
            select(Reserve = reserve,
                   "SET Code" = set_id,
                   "SET Name" = user_friendly_set_name,
                   "SET Type" = set_type,
                   "Monitoring Time Period" = time_series,
                   "Rate of water level change, long-term (mm/yr)" = rate2,
                   "Keeping up with long-term water level change?" = dir_slr,
                   "Rate of water level change, near-term (mm/yr)" = rate19yr,
                   "Keeping up with near-term water level change?" = dir_19yr,
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
                
                # Comparisons to SLR
                accordion_panel(
                  title = "Comparisons to Water Level Change",
                  reactableOutput("slr_rates")
                ),
                
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
        } else if(input$tabs == "Station-level") {
            accordion(
                id = "reserve_accordion",
                open = FALSE,
                
                h4(paste0("Selected Station: ", selected_station())),
                
                htmlOutput("reserve_info"),
                br(),
                br(),
                
                span(strong("More details about this station:"),
                     tooltip(
                         bsicons::bs_icon("info-circle"),
                         HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                     )),
                br(),
                
                
                # SET types
                accordion_panel(
                    title = "Station summary",
                    reactableOutput("stn_tbl")
                ),
                
                # Plotly graph, with accordioned options
                accordion_panel(
                    title = "Time Series Graph",
                    tags$small("The graph section can be popped out to full screen from the bottom right corner."),
                    
                    card(
                        full_screen = TRUE,
                        height = "50vh",
                        
                        
                        withWaiter(plotlyOutput("stn_grouptimeSeries"))
                        
                    )
                )
            )
            
        } else {
            h5("The sidebar does not have additional information for this tab.")
            
        }
        
    })
    
    
    # filter sets based on user choices
    filtered_sets <- reactive({
        set_details |> 
            filter(dir_slr != "Not calculated",
                   dir_19yr != "Not calculated")
    })
    
   # donut charts ----
    output$donuts_allSETs <- renderPlot({
        req(filtered_sets())
        
        p_allSETs_longterm <- plot_set_distn(filtered_sets(), dir_slr, 
                                             type = "donut", label = "none",
                                             hsize = 1.6) +
            annotate("text",
                     label = "long-\nterm",
                     size = 3,
                     col = "gray20",
                     fontface = "bold",
                     x = 0.2,
                     y = 0)
        
        p_allSETs_19yr <- plot_set_distn(filtered_sets(), dir_19yr, 
                                         type = "donut", label = "none",
                                         hsize = 1.6) +
            annotate("text",
                     label = "near-\nterm",
                     size = 3,
                     col = "gray20",
                     fontface = "bold",
                     x = 0.2,
                     y = 0)
        
        plot_set_distn_pair(p_allSETs_longterm, p_allSETs_19yr,
                            legend.position = "right") +
            plot_annotation(subtitle = paste0("Compared to which water level change:\n(n = ", nrow(filtered_sets()), " stations)"),
                            theme = theme(plot.title.position = "panel",
                                          plot.subtitle = element_text(size = rel(0.9)))) &
            theme(legend.text = element_text(size = rel(0.8)),
                  legend.key.size = unit(0.8, "lines"))
    }, height = 140, res = 96)
    
    
}

# Run the app
shinyApp(ui, server)
