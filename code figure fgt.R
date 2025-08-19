

fgt <- function(g,t,scen){
if(scen == 'constant'){
  return(2)
} else if (scen == 'lagged'){
  if(t - g >= 1){
    return(2)
  }else{
    return(0)
  }
} else if (scen == 'grptime'){
  if(t - g >= 0) {
    out <- 0 # need to initialize outcome because we also generate counterfactuals 
    # for unobserved g's which are set to 0 in this setting. 
    if(g == 3){
      out <- ifelse(t == 3, 1, ifelse(t == 4, 2, 3))
    } else if(g == 4){  # Changed to `else if` to ensure only one condition executes
      out <- ifelse(t == 4, 2, ifelse(t == 5, 3, 4))  # Fixed second condition
    } else if(g == 5){
      out <- 5
    }
    return(out)
  } else {
    return(0)
  }
}
}

library(dplyr)
library(tidyr)

g_values <- c(3, 4, 5)
t_values <- 0:5
scenarios <- c("constant", "lagged", "grptime")

# Expand grid to create all combinations
df <- expand.grid(g = g_values, t = t_values, scen = scenarios) %>%
  mutate(value = mapply(fgt, g, t, scen))

df <- df %>%
  mutate(
    scen_label = case_when(
      scen == "constant" ~ "Scenario 1 \n Constant Effect",
      scen == "lagged" ~ "Scenario 2 \n Lagged Effect",
      scen == "grptime" ~ "Scenario 3 \n Group-time Heterogeneity"
    ),
    g_label = paste("Group", g)  # Rename groups to "Group 3", "Group 4", "Group 5"
  )

plot <- ggplot(df, aes(x = t, y = value, group = g)) +
  geom_point(size = 3) +  # Points to mark values
  geom_step(direction = "hv") +  # Right-continuous step function (Vertical then Horizontal)
  facet_grid(g_label ~ scen_label, labeller = label_value) +  # Facet by scenario & group
  theme_minimal() +  # Minimal theme for clean visualization
  labs(
    x = "Time",
    y = "Evaluation of f(g,t)"
  ) +
  theme(
    axis.text = element_text(size = 34),  # Increase font size for x and y axis text
    axis.title = element_text(size = 36),  # Increase font size for axis titles
    strip.text = element_text(size = 36),  # Increase font size for facet titles
    legend.position = "none"  # Remove legend since g is in facet labels
  )

# Use cowplot for better theming
plot_grid(plot, align = "v", rel_heights = c(1))


ggsave(
  filename = "Results/Figures paper 2/fgt.pdf",
  plot = plot,
  device = "pdf",
  width = 24, 
  height = 20,
  units = "in",
  dpi = 300
)




