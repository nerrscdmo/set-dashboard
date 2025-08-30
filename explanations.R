

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
            br(),
            p("Water level change rates were also calculated during the SETr project. Data for water level change calculations were obtained from the nearest", 
              tags$a("NOAA NWLON station", href = "https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels", target = "_blank"), 
              "identified by each reserve."),
            br(),
            
            h5("Variable and Category definitions"),
            p("Station is a SET"),
            p("long-term/near-term distinction; the 'more-confident/less-confident' definitions")
        ),
        
        accordion_panel(
            title = "Trend Calculations",
            p("only calculated when >4.5 yrs of data; linear mixed models; link to SETr for more details"),
            p("water level trends calculated using AR1 model, as NOAA uses. Made sure long-term estimates matched what was on the NWLON station's page, subsetted to 19 yrs and calculated near-term change using the same methods."),
            
            br(),
            
            h5("When trends were calculated"),
            p("trends only calculated when a station had 5 or more measurements over 4.5 years or longer."),
            br(),
            
            h5("Comparisons to water level change"),
            p("Rates of elevation change at each SET are compared to rates of water level change (SLR = long-term sea level rise; 19yr = water level change over a 19 year period) by investigating whether confidence intervals overlap. This method of comparison was chosen because different methods were used to calculate rates for sea level rise (ARIMA) and SET elevation change (LMMs), using data from different sources. We note that each individual interval has 95% confidence associated with it, and conclusions that are made based on pairwise comparison of these intervals will not necessarily be equivalent to conducting a formal hypothesis test for a difference at the 5% level (Schenker and Gentleman, 2001)."),
            br(),
            
            h5("How trends were calculated"),
            p("Rates of elevation change at each SET were generated using random-intercept linear mixed models. See Zuur et al. (2009) and Cahoon et al. (2019) for details."),
            p("Data for each SET is analyzed separately using pin height as the response variable; arm and pin (nested in arm) are treated as random effects; and date is considered a numeric covariate."),
            p("For this analysis, models were fit in R, using the `lme()` function in the `nlme` package (Pinheiro et al. 2019). Confidence intervals were generated using the `intervals()` function, also in the `nlme` package."),
            p("All calculations generated output in *mm/day* and these rates were converted to *mm/yr* by multiplying by 365.25, to account for leap years. ")
            
        )
        
    ),  # end accordion
    
    
    hr(),
    p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
    p("Funding was provided by NOAA under a subaward from [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
    p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), ". For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")

)

