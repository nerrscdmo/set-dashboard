

# about description ----

about_ui <- nav_panel(
    "About",
    full_screen = FALSE,
    
    h5("Expand a section below for more detail."),
    
    accordion(
        id = "explanation_accordion",
        open = FALSE,
        
        accordion_panel(
            title = "Using the Maps",
            p("Just about everything in these maps is interactive - click something interesting and see what happens! Some tips are below."),
            tags$ul(
                tags$li(strong("Choose a tab."), "The default view ('Station-level' tab) shows one point per station. The 'Reserve-level' tab shows a pie chart compiling results of all stations within a single reserve."),
                tags$li(strong("Click on the 'Map Details' button"), "for more information about what is on the map."),
                tags$li(strong("Adjust options in the left sidebar."), "Info buttons can be clicked on for more detail about certain options. In this dashboard, you can select which rate of water level change you want to compare each station's rate of change to."),
                tags$li(strong("Click on a point"), "and a sidebar on the right will open up with more detail. Expand any of the sections by clicking on it. The sidebar can be closed again by clicking on the arrow in the upper left corner of the sidebar."),
                tags$li(strong("Sidebars and expandable information"), "can be closed again by clicking on the arrows at the top of them."),
                tags$li(strong("Graphs in the sidebar"), "can be popped out by clicking on the symbol in the bottom right corner of the graph pane. The entire tab's contents can also be popped out, so if you don't see what you want, try looking for the symbol in a slightly different place."),
                tags$li(strong("Zoom in and out"), "using the buttons in the upper left corner of the map."),
                tags$li(strong("Change the map background"), "if desired, by using the layers button in the upper right corner of the map.")
                
            )
        ),
        
        accordion_panel(
            title = "Data source(s) and Definitions",
            
            h5("Data Sources"),

            p("Marsh elevation data files contributed by participating reserves during the",
              tags$a("SETr project", href = "https://nerrssciencecollaborative.org/project/Cressman18", target = "_blank"),
              "were processed using code in this", tags$a("set-data-processing repository", href = "https://github.com/nerrscdmo/set-data-processing", target = "_blank"), "."),
            p("The time period included in this dashboard varies by reserve but the most recent data is from ~2017."),
            p("Data for water level change calculations were obtained from the nearest", 
              tags$a("NOAA NWLON station", href = "https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels", target = "_blank"), 
              "identified by each reserve."),

            h5("Variable and Category definitions"),
            p(strong(em("Station")), "in this dashboard represents an individual Surface Elevation Table, or SET."),
            p(strong("Water Level Change categories."), "Water level change is not constant, and is in most areas increasing. Shorter time periods may be more relevant to marsh changes. 
              For these reasons, we compare marsh elevation change to two separate rates of water level change."),
            tags$ul(
                tags$li(strong(em("Long-term")), "refers to the entire period of record at a NOAA NWLON station. For most stations in this app, the period of record is at least 50 years."),
                tags$li(strong(em("Near-term")), "refers to a period of 19 years, ending the last year of measurements at the nearest marsh elevation station.
                        19 years was chosen because water levels undergo an approximately 19.6 year cycle (the 'metonic cycle'), 
                        and trends over shorter time periods than this cycle may be misleading, as they may really only describe a part of the normal cycle.")
            ),

            p(strong("Categories in comparison to water-level change."), "Because trends for marsh elevation and water level change were calculated using different methods, there is not a direct method to test whether they are different at a p<0.05 level. 
              So comparisons here were made based on whether the confidence intervals overlapped."),
            tags$ul(
                tags$li(strong(em("More Confident")), "means that the confidence interval of marsh elevation change at the station did NOT overlap with the confidence interval of water level change."),
                tags$li(strong(em("Less Confident")), "means that the confidence interval of marsh elevation change at the station DID overlap with the confidence interval of water level change.")
            )
        ),
        
        accordion_panel(
            title = "Trend Calculations and Comparisons to Water Level Change",
            p("Marsh elevation trends at a station were calculated, via linear mixed model (see below for more detail), only if that station had at least 5 recorded measurements over at least 4.5 years. Otherwise a station was assigned the result category of 'not calculated' and represented on the maps in gray."),
            p("Water level trends were generated based on the nearest NOAA NWLON station to each station. Two rates of change were calculated for water levels: long-term, or the entire period of record; and near-term, or a 19-year period ending with the most recent year of marsh elevation measurements at a station."),
            p("Rates of elevation change at each station were compared to rates of water level change by identifying whether the 95% confidence intervals of each rate overlapped. This method of comparison was chosen because different methods were used to calculate rates for water level change and marsh elevation change, using data from different sources."),
            p("Each individual interval has 95% confidence associated with it, and conclusions that are made based on pairwise comparison of these intervals", strong("are not equivalent to conducting a formal hypothesis test"), "for a difference at the 5% level (Schenker and Gentleman, 2001)."),

            
            h5("Additional Detail"),
            p("Rates of elevation change at each station were generated using random-intercept linear mixed models. Pin height was the response variable; date was used as a numeric fixed effect; and pin nested within arm was the random effect."),
            p("For this analysis, models were fit in R, using the `lme()` function in the `nlme` package (Pinheiro et al. 2019). Confidence intervals were generated using the `intervals()` function, also in the `nlme` package."),
            p("All calculations generated output in *mm/day* and these rates were converted to *mm/yr* by multiplying by 365.25, to account for leap years. "),

            p("Water level trends were calculated using an AR1 model on detrended monthly water level data, as per NOAA methodology. NOAA does not calculate trends on a time period less than the period of record, so we checked that long-term estimates from our calculations matched what NOAA provided for a station, then subsetted the same data to 19 yrs and calculated near-term change using the same methods.")
            
        )
        
    ),  # end accordion
    
    
    hr(),
    div(class = "footer-text",
        p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
        p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), "under a subaward from NOAA [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
        p("For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
    ) 
)

