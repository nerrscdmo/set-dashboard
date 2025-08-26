# SET Dashboard  

This is the code behind an interactive dashboard of NERRS Surface Elevation Table data. The dashboard can be found on the [Wetlands and Water Levels page](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/wetlands-and-water-levels?authuser=0) of [this NERRS monitoring website](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/home).  


## To update in the future:  

This dashboard imported data and trends that had already been calculated as part of the [SETr project](https://nerrssciencecollaborative.org/project/Cressman18). Processing scripts are in the [SET Data Processing Repository](https://github.com/nerrscdmo/set-data-processing). Follow instructions in that readme to update processed data. The following items need to be copied from that processing directory into this `set-dashboard` project:  

-  `images` folder, from root of processing directory to root of dashboard directory. This folder contains the pie charts, arrows, and color palettes needed for the interactive maps.  
-  `data/04_rates/SET_rates_and_details.RData`, into the `data/` folder of the dashboard project.   

