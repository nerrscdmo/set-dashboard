toplo <- set_details |> 
    summarize(.by = dir_slr,
              n = n()) |> 
    arrange(dir_slr)

toplo2 <- set_details |> 
    summarize(.by = dir_19yr,
              n = n()) |> 
    arrange(dir_19yr)

hsize <- 2   # hole size


p_longterm <- ggplot(toplo,
       aes(x = hsize,
           y = n,
           fill = dir_slr,
           group = dir_slr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    # geom_label(aes(label = n),
    #           position = position_stack(vjust = 0.5),
    #           fill = "gray98",
    #           col = "navy",
    #           show.legend = FALSE,
    #           fontface = "bold",
    #           size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    xlim(c(0.2, hsize + 0.5)) +
    annotate("text",
             label = "long-term \nSLR",
             size = 7,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")


p_19yr <- ggplot(toplo2,
                     aes(x = hsize,
                         y = n,
                         fill = dir_19yr,
                         group = dir_19yr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    # geom_label(aes(label = n),
    #            position = position_stack(vjust = 0.5),
    #            fill = "gray98",
    #            col = "navy",
    #            show.legend = FALSE,
    #            fontface = "bold",
    #            size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    xlim(c(0.2, hsize + 0.5)) +
    annotate("text",
             label = "19-yr \nSLR",
             size = 7,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")

library(patchwork)
p_longterm + p_19yr +
    plot_layout(guides = "collect") &
    theme(legend.position = "top")






p_longterm2 <- ggplot(toplo,
                     aes(x = hsize,
                         y = n,
                         fill = dir_slr,
                         group = dir_slr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    geom_label(aes(label = n),
              position = position_stack(vjust = 0.5),
              fill = "gray98",
              col = "navy",
              show.legend = FALSE,
              fontface = "bold",
              size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    xlim(c(0.2, hsize + 0.5)) +
    annotate("text",
             label = "long-term \nSLR",
             size = 7,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")


p_19yr2 <- ggplot(toplo2,
                 aes(x = hsize,
                     y = n,
                     fill = dir_19yr,
                     group = dir_19yr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    geom_label(aes(label = n),
               position = position_stack(vjust = 0.5),
               fill = "gray98",
               col = "navy",
               show.legend = FALSE,
               fontface = "bold",
               size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    xlim(c(0.2, hsize + 0.5)) +
    annotate("text",
             label = "19-yr \nSLR",
             size = 7,
             col = "navy",
             fontface = "bold",
             x = 0.2,
             y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")

p_longterm2 + p_19yr2 +
    plot_layout(guides = "collect") &
    theme(legend.position = "top")



p_longterm3 <- ggplot(toplo,
                      aes(x = hsize,
                          y = n,
                          fill = dir_slr,
                          group = dir_slr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    # geom_label(aes(label = n),
    #            position = position_stack(vjust = 0.5),
    #            fill = "gray98",
    #            col = "navy",
    #            show.legend = FALSE,
    #            fontface = "bold",
    #            size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    # xlim(c(0.2, hsize + 0.5)) +
    # annotate("text",
    #          label = "long-term \nSLR",
    #          size = 7,
    #          col = "navy",
    #          fontface = "bold",
    #          x = 0.2,
    #          y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")


p_19yr3 <- ggplot(toplo2,
                  aes(x = hsize,
                      y = n,
                      fill = dir_19yr,
                      group = dir_19yr)) +    # group makes sure ordering is the same in each layer - without it, labels were going in the wrong direction
    geom_col(col = "black") +
    # geom_label(aes(label = n),
    #            position = position_stack(vjust = 0.5),
    #            fill = "gray98",
    #            col = "navy",
    #            show.legend = FALSE,
    #            fontface = "bold",
    #            size = 5) +
    scale_fill_manual(values = cols_slr) +
    coord_polar("y", start = 0) +
    # xlim(c(0.2, hsize + 0.5)) +
    # annotate("text",
    #          label = "19-yr \nSLR",
    #          size = 7,
    #          col = "navy",
    #          fontface = "bold",
    #          x = 0.2,
    #          y = 0) +
    theme_void() +
    labs(fill = "Keeping Up?")

p_longterm3 + p_19yr3 +
    plot_layout(guides = "collect") &
    theme(legend.position = "top")
