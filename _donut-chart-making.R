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
    
    base_text_size <- 14
    
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
        theme_void(base_size = base_text_size) +
        theme(element_text(size = base_text_size)) +
        labs(fill = NULL)
    
    
    if(label == "count"){
        p <- p +
            geom_label(aes(label = n),
                       position = position_stack(vjust = 0.5),
                       fill = "gray98",
                       col = "navy",
                       show.legend = FALSE,
                       fontface = "bold",
                       size = rel(2))
    }
    
    
    if(label == "percent"){
        p <- p +
            geom_label(aes(label = percent),
                       position = position_stack(vjust = 0.5),
                       fill = "gray98",
                       col = "navy",
                       show.legend = FALSE,
                       fontface = "bold",
                       size = rel(2))
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


p_allSETs_longterm <- plot_set_distn(set_details, dir_slr, 
                                     type = "donut") +
    annotate("text",
             label = "long-term \nSLR",
             size = 3,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) 
p_allSETs_19yr <- plot_set_distn(set_details, dir_19yr, 
                                 type = "donut") +
    geom_label(aes(label = percent),
               position = position_stack(vjust = 0.5),
               fill = "gray98",
               col = "navy",
               show.legend = FALSE,
               fontface = "bold",
               size.unit = "pt",
               size = 10,
               label.padding = unit(0.2, "lines"),
               label.size = 0.1) +
    annotate("text",
             label = "near-term \nSLR",
             size = 4,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) 


plot_set_distn_pair(p_allSETs_longterm, p_allSETs_19yr,
                    legend.position = "none",
                    title = "Are SETs across the NERRs keeping up with ...?")

