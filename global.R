library(dplyr)

# read in data ----
# bring in data frames (calculations have been made outside the app)
# load(here::here("data_wq", "do_dataframes.RData"))

# bring in table of reserve websites etc.
res_mdat <- read.csv(here::here("NERR Websites.csv")) |> 
    mutate(ReserveCodeLower = tolower(ReserveCode))


# color palettes and shapes ----
# these are from the original DO dashboard
# palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#2166AC", "#B2182B"),  # from Tol's BuRd
                            levels = c(0, 1))

palette_time.hypoxic <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))

palette_median.mgl <- colorNumeric(palette = "YlGnBu", domain = c(0, 14))

palette_trnd.mgl <- colorFactor(palette = c("#2166AC", "#B2182B", "#FFEE99", "#7F7F7F"),  # from Tol's BuRd EXCEPT for 'not calcd' - need to check this
                                levels = c("increasing", "decreasing", "no trend", "not calculated"))

palette_trnd.thrsh <- colorFactor(palette = c("#762A83", "#1B7837", "#FFEE99", "#7F7F7F"),  # from Tol's PRGn
                                  levels = c("increasing", "decreasing", "no trend", "not calculated"))

# use circles for typical and squares for unusual - change here if desired
shape_assignment <- function(x){
    case_when(is.na(x) ~ "diamond",
              x == 0 ~ "circle",
              x == 1 ~ "rect",
              .default = "triangle")
}


# make necessary data frames ----
# leaving blank, as it will depend on the project 


