### A Function to create plots for meal by condition interactions in a mixed effects model
### By David A Hughes
### Date Nov 18th
my_strong_plot = function(pdata){
  ## ---- plot of the condition effect -----
  p1 = pdata |> 
    ggplot(aes(x = condition, y = energy_intake)) +
    geom_boxplot( aes(fill = condition) ) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    labs(
      title = "Simulated Energy Intake Data",
      subtitle = "Condition effect",
      y = "Energy Intake (kcal)",
      x = "Condition"
    ) 
  
  ## ---- plot of the meal effect -----
  p2 = pdata |> 
    ggplot(aes(x = meal, y = energy_intake)) +
    geom_boxplot( aes(fill = meal) ) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    labs(
      title = "Simulated Energy Intake Data",
      subtitle = "Meal effect",
      y = "Energy Intake (kcal)",
      x = "Condition"
    ) 
  
  ## ---- plot of interaction effect -----
  p3 = pdata |> 
    ggplot(aes(x = meal, y = energy_intake, fill = condition)) +
    geom_boxplot( position = position_dodge(width = 0.8) ) +
    scale_fill_brewer(palette = "Set1") +
    #scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    labs(
      title = "Simulated Energy Intake Data",
      subtitle = "Condition -x- meal effect",
      y = "Energy Intake (kcal)",
      x = "Meal"
    )
  
  ## ---- combined plot -----
  return( ( p1 + p2 ) / p3 ) 
  
}

