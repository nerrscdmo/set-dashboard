# read in data ----
# bring in data frames (calculations have been made outside the app)
load(here::here("data",
                "SET_rates_and_details.RData"))
# read in color palette for legends ----
load(here::here("images",
                "color_palette.RDS"))
# make that work with leaflet legend too
leaflet_colors <- colorFactor(palette = cols_slr,
                              domain = names(cols_slr))


# use factors - match names of color palette ----
reserve_sets <- reserve_sets |> 
    mutate(outcome = factor(outcome, 
                            levels = c("dec_sig", "dec_nonsig", "not_enough_info", "inc_nonsig", "inc_sig"),
                            labels = c("No, more confident", "No, less confident", "Not enough info",
                                       "Yes, less confident", "Yes, more confident")))

# this is now done in processing step
# set_details <- set_details |>
#     mutate(dir_19yr = factor(dir_19yr,
#                             levels = c("dec_sig", "dec_nonsig", "not_enough_info", "inc_nonsig", "inc_sig"),
#                             labels = c("No, more confident", "No, less confident", "Not enough info",
#                                        "Yes, less confident", "Yes, more confident")),
#            dir_slr = factor(dir_slr,
#                             levels = c("dec_sig", "dec_nonsig", "not_enough_info", "inc_nonsig", "inc_sig"),
#                             labels = c("No, more confident", "No, less confident", "Not enough info",
#                                        "Yes, less confident", "Yes, more confident")))


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


# donut charts ----
p_allSETs_longterm <- plot_set_distn(set_details, dir_slr, 
                                     type = "donut", label = "none",
                                     hsize = 1.6) +
    annotate("text",
             label = "long-\nterm",
             size = 3,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0)
p_allSETs_19yr <- plot_set_distn(set_details, dir_19yr, 
                                 type = "donut", label = "none",
                                 hsize = 1.6) +
    annotate("text",
             label = "near-\nterm",
             size = 3,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0)

