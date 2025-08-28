# SET Dashboard  

This is the code behind an interactive dashboard of NERRS Surface Elevation Table data. The dashboard can be found on the [Wetlands and Water Levels page](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/wetlands-and-water-levels?authuser=0) of [this NERRS monitoring website](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/home).  


## To update in the future:  

This dashboard imported data and trends that had already been calculated as part of the [SETr project](https://nerrssciencecollaborative.org/project/Cressman18). Processing scripts are in the [SET Data Processing Repository](https://github.com/nerrscdmo/set-data-processing). Follow instructions in that readme to update processed data. The following items need to be copied from that processing directory into this `set-dashboard` project:  

-  `images` folder, from root of processing directory to root of dashboard directory. This folder contains the pie charts, arrows, and color palettes needed for the interactive maps.  
-  `data/04_rates/SET_rates_and_details.RData`, into the `data/` folder of the dashboard project.   


### Colors and symbols  

For this dashboard, the color palette for maps and pie charts is defined in the `set-data-processing` repo, in `R_scripts/03_construct_map_graphics.R`. If the color palette is changed, the entire script needs to be run and the `images` folder copied over into this `set-dashboard` directory.  


### Factor order  

Outcomes of comparison to water level change are defined as a factor, in a specifically defined order to determine how they show up in the legend, and with specific names that are more understandable than the names originally used in SETr. Changes to the names or order of this factor can be made in *Chunk 3* of `02_rate_generation_all_sets.Rmd` in the `set-data-processing-repo`, and those changes will propagate to the places the factor is used (which are chunks 23 & 24 of that script, and in the names for the color palette in `03_construct_map_graphics.R`).  




