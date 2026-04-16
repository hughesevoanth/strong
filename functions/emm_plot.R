### A Function to plot estimated marginal means for meal by condition interactions
### By David A Hughes
### Date Nov 18th
emm_plot = function(pdata){
  shade_discrete_x <- function(df, xvar) {
    lv <- levels(df[[xvar]])
    tibble(
      x_min = seq_along(lv) - 0.5,
      x_max = seq_along(lv) + 0.5,
      ymin  = -Inf,
      ymax  = Inf,
      fill  = rep(c("white", "gray80"), length.out = length(lv))
    )
  }
  
  shades <- shade_discrete_x(pdata, "meal")
  
  ggplot(pdata, aes(x = meal, y = emmean, color = condition)) +
    # shading rectangles
    geom_rect(
      data = shades,
      aes(xmin = x_min, xmax = x_max, ymin = ymin, ymax = ymax, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.4,
      color = NA
    ) +
    # CI bars
    geom_errorbar(
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0,
      position = position_dodge(width = 0.4),
      size = 1.5
    ) +
    # points
    geom_point(position = position_dodge(width = 0.4), size = 3) +
    labs(
      y = "Estimated energy intake (kcal)",
      x = "Meal",
      color = "Condition"
    ) +
    scale_fill_identity() +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
}

