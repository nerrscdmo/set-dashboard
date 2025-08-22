# read in data ----
# bring in data frames (calculations have been made outside the app)
load(here::here("data",
                "SET_rates_and_details.RData"))
# read in color palette for legends ----
load(here::here("images",
                "color_palette.RDS"))
# make that work with leaflet legend too
leaflet_colors <- colorFactor(palette = unname(cols_slr),
                              domain = set_details$dir_slr)

# # Check the color palette
# data.frame(
#     level = levels(set_details$dir_slr),
#     color = leaflet_colors(levels(set_details$dir_slr))
# )


# use factors - match names of color palette ----
# note, when changing the order of the factor, need to also change it in:
# set-data-processing, 03_construct_map_graphics, lines 9-22
# set-data-processing, 02_rate_generation_all_sets, chunk 23, starting line 331
reserve_sets <- reserve_sets |> 
    mutate(outcome = factor(outcome, 
                            levels = c("dec_sig", "dec_nonsig", "inc_nonsig", "inc_sig", "not_enough_info"),
                            labels = c("No, more confident", "No, less confident",
                                       "Yes, less confident", "Yes, more confident", "Not calculated")))


# set up central coordinates and file paths for reserves ----
reserve_mappiness <- set_details |> 
    summarize(.by = reserve,
              lat = mean(lat, na.rm = TRUE),
              lon = mean(long, na.rm = TRUE)) |> 
    mutate(pie_longterm = file.path("images", "pie_charts", paste0(reserve,
                                                                    "_longterm.svg")),
           pie_19yr = file.path("images", "pie_charts", paste0(reserve,
                                                               "_19yr.svg")))

set_details <- set_details |> 
    mutate(Res_SET = paste(reserve, set_id, sep = "_"))

set_avgd_readings <- set_avgd_readings |> 
    mutate(Res_SET = paste(reserve, set_id, sep = "_"))




