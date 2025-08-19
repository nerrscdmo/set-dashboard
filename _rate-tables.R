# run the app with browser() active on line 404
# where the map click happens
# then click on a reserve pie chart in the 2nd tab, and start here.

rts <- list(
    rates = selected_reserve_list()$slr_rates,
    results_slr = selected_reserve_list()$slr_comps,
    results_19yr = selected_reserve_list()$yr19_comps
)



rts$results_slr <- rts$results_slr |> 
    mutate(comp = "Long-term") |> 
    rename(Category = dir_slr,
           Rate = slr_rate)

rts$results_19yr <- rts$results_19yr |> 
    mutate(comp = "Near-term") |> 
    rename(Category = dir_19yr,
           Rate = slr_19yr)

full <- bind_rows(rts$results_slr, rts$results_19yr)
wide <- full |> 
    select(-Rate) |> 
    pivot_wider(names_from = comp,
                values_from = n) |> 
    arrange(desc(Category))

rates2 <- rts$rates |> 
    select(reserve, longterm, yr19) |> 
    pivot_longer(-reserve,
                 names_to = "comp",
                 values_to = "Rate") |> 
    mutate(comp = case_when(comp == "longterm" ~ "Long-term",
                            comp == "yr19" ~ "Near-term"))

wide2 <- full |> 
    select(-Rate) |> 
    arrange(desc(Category)) |> 
    pivot_wider(names_from = Category,
                values_from = n,
                values_fill = 0) |> 
    as.data.frame() |> 
    left_join(rates2) |> 
    relocate(comp, Rate) |> 
    select(-reserve)

reactable(wide2,
          columns = list(
              comp = colDef(name = "Description"),
              Rate = colDef(name = "Rate"),
              `Yes, more confident` = colDef(name = "More Confident"),
              `Yes, less confident` = colDef(name = "Less Confident"),
              `No, less confident` = colDef(name = "Less Confident"),
              `No, more confident` = colDef(name = "More Confident")
          ),
          columnGroups = list(
              colGroup("Water Level Change", columns = c("comp", "Rate")),
              colGroup("# Keeping Up", columns = c("Yes, more confident", "Yes, less confident")),
              colGroup("# Not Keeping Up", columns = c("No, less confident", "No, more confident"))
          )
          )
    
    

