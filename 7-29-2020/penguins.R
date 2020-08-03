library(tidyverse)
library(palmerpenguins)

# Question: How do the average weights of Adelie penguins inhabiting each island vary by year?

ggtheme <- function(S) {
   hrbrthemes::theme_ipsum(base_family = "Arial",
                        plot_title_size = S + 1.5, plot_title_face = "bold",
                        axis_title_size = S, axis_title_face = "bold", axis_title_just = "m",
                        axis_text_size = S - 1) +
   theme(legend.title = element_text(size = S, face = "bold"),
         legend.text = element_text(size = S - 1))
}

penguins_plot <- 
   penguins %>% 
   drop_na %>% 
   filter(species == "Adelie") %>% 
   ggplot(aes(x = year, y = body_mass_g, color = island, group = interaction(island, sex))) +
      ggbeeswarm::geom_beeswarm(aes(shape = sex), size = 3, alpha = 0.6, cex = 1.5, groupOnX = T) +
      geom_smooth(method = "loess", se = F, size = 2) +
      annotate(geom = "text", x = 2007.2, y = 4250, label = "Males", fontface = "bold") +
      annotate(geom = "text", x = 2007.2, y = 3555, label = "Females", fontface = "bold") +
      labs(title = "Variation in Adelie penguin weight over time, by island",
           x = "Year", y = "Body mass (g)",
           color = "Island", shape = "Sex") +
      scale_x_continuous(breaks = c(2007, 2008, 2009), minor_breaks = NULL, expand = c(.02,.02)) +
      scale_y_continuous(breaks = seq(3000, 4400, by = 200), expand = c(.05,.05)) +
      scale_color_manual(values = c("#CC79A7", "#0072B2", "#E69F00")) +
      ggtheme(11)

penguins_plot
