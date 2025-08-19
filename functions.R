# Utility function to convert hex to rgba with alpha
hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- col2rgb(hex)
    sprintf("rgba(%d, %d, %d, %.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

# plotting functions ----
plot_set_distn <- function(data,
                           variable,
                           type = c("pie", "donut"),
                           label = c("none", "count", "percent"),
                           hsize = NULL){
    # data should be a data frame
    # in which each row represents a SET
    # this could be all SETs across the NERR system,
    # or just SETs within a reserve
    
    # variable should be a column name, and it should be
    # a character or ideally a factor
    
    # label_n means, should the counts go on the chart?
    
    # hsize is mostly just required if the type is a donut. 
    # but it's also used to create dimensions for the pie chart (as the x position)
    # it doesn't really dooooooo anything though so it doesn't matter what
    # the number is when it's a pie chart.
    # Default is 2
    # and smaller numbers will make a smaller hole.
    
    type <- match.arg(type)
    label <- match.arg(label)
    
    hsize <- ifelse(is.null(hsize), 2, hsize)
    
    toplo <- data |> 
        summarize(.by = {{variable}},
                  n = n()) |> 
        arrange({{variable}}) |> 
        mutate(percent = paste0(round(n / sum(n) * 100, 0), "%"))
    
    p <- ggplot(toplo,
                aes(x = hsize,
                    y = n,
                    fill = {{variable}},
                    group = {{variable}})) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
        geom_col(col = "black") +
        scale_fill_manual(values = cols_slr) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.text = element_text(size = rel(0.8))) +
        labs(fill = "Marshes keeping up?")
    
    
    if(label == "count"){
        p <- p +
            geom_label(aes(label = n),
                       position = position_stack(vjust = 0.5),
                       fill = "gray98",
                       col = "navy",
                       show.legend = FALSE,
                       fontface = "bold",
                       size = 4)
    }
    
    
    if(label == "percent"){
        p <- p +
            geom_label(aes(label = percent),
                       position = position_stack(vjust = 0.5),
                       fill = "gray98",
                       col = "navy",
                       show.legend = FALSE,
                       fontface = "bold",
                       size = 4)
    }
    
    if(type == "donut"){
        p <- p +
            xlim(c(0.2, hsize + 0.5))
    }
    
    p
    
}

plot_set_distn_pair <- function(distn1, distn2, title = NULL,
                                legend.position = "bottom"){
    # distn1 and distn2 should be ggplot objects,
    # output from plot_set_distn()
    
    distn1 + distn2 +
        plot_layout(guides = "collect") +
        plot_annotation(title = title) &
        theme(legend.position = legend.position)
}

plot_cumu_set <- function(data, change_column, 
                          columns = 4, pointsize = 3.5, 
                          scales = "fixed", 
                          smooth = TRUE, lty_smooth = 5){
    # data needs to be the $set piece of the output from calc_change_cumu
    ggplot(data, aes(x = date, y = {{change_column}})) +
        geom_line(col = 'lightsteelblue4') +
        {if(smooth) geom_smooth(se = FALSE, method = 'lm', 
                                col = 'steelblue4', lty = lty_smooth, linewidth = 0.8)} +
        geom_point(shape = 21, 
                   fill = 'lightsteelblue1', col = 'steelblue3', 
                   size = pointsize, alpha = 0.9) +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        {if(smooth) labs(title = 'Cumulative Change since first reading', 
                         subtitle = 'dashed line is linear regression',
                         x = 'Date',
                         y = 'Change since first reading (mm)')} +
        {if(!smooth) labs(title = 'Cumulative Change since first reading', 
                          x = 'Date',
                          y = 'Change since first reading (mm)')} +
        theme_classic()
}