# read in data ----
# bring in data frames (calculations have been made outside the app)
load(here::here("data",
                "SET_rates_and_details.RData"))
# read in color palette for legends
load(here::here("images",
                "color_palette.RDS"))


# set up central coordinates and file paths for reserves
reserve_mappiness <- set_details |> 
    summarize(.by = reserve,
              lat = mean(lat, na.rm = TRUE),
              lon = mean(long, na.rm = TRUE)) |> 
    mutate(pie_longterm = file.path("images", "pie_charts", paste0(reserve,
                                                                    "_longterm.svg")),
           pie_19yr = file.path("images", "pie_charts", paste0(reserve,
                                                               "_19yr.svg")))
