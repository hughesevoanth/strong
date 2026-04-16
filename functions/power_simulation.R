#' Run a Power Simulation for Meal-by-Condition Interaction Models
#'
#' Estimates statistical power to detect meal-specific effects of exercise on
#' energy intake by repeatedly simulating data, fitting linear mixed-effects
#' models, and summarising the proportion of significant results across a grid
#' of effect sizes.
#'
#' @param n Integer. Number of individuals (participants) per simulated dataset.
#'   Default is 120.
#' @param n_reps Integer. Number of simulation replicates per effect size.
#'   Default is 200.
#' @param effect_sizes Numeric vector. Proportional reductions in energy intake
#'   under the exercise condition to simulate (e.g., \code{seq(0, 0.80, by = 0.05)}).
#'   A value of 0.20 means a 20\% reduction relative to the REST-condition mean
#'   for the affected meal(s). Default is \code{seq(0, 0.80, by = 0.05)}.
#' @param power_group_by Character. Name of the column in the simulation output
#'   to group by when computing power summaries. Typically one of
#'   \code{"lunch_prop_effect"}, \code{"snack_prop_effect"}, or
#'   \code{"dinner_prop_effect"}. Default is \code{"lunch_prop_effect"}.
#' @param pivot_long_on Character. Name of the column to use as the x-axis
#'   variable when pivoting the power summary to long format for plotting.
#'   Usually the same as \code{power_group_by}. Default is
#'   \code{"lunch_prop_effect"}.
#' @param meals_effected Character vector. Which meals receive the exercise
#'   effect specified in \code{effect_sizes}. Any combination of
#'   \code{"lunch"}, \code{"snack"}, and \code{"dinner"}. Use \code{"none"} to
#'   simulate a null scenario (no exercise effect at any meal). Breakfast is
#'   always held at zero effect. Default is \code{"lunch"}.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{\code{my_sims}}{Data frame of all raw simulation results, one row
#'       per replicate, with columns for sample size, proportional effects,
#'       kcal effects, and p-values for the interaction and each meal contrast.}
#'     \item{\code{power_summary}}{Data frame of power estimates grouped by
#'       \code{power_group_by}, with columns for kcal effects and power for
#'       the interaction test and each meal-specific contrast.}
#'     \item{\code{power_long}}{Long-format version of \code{power_summary}
#'       used for plotting.}
#'     \item{\code{plot}}{A \code{ggplot2} power curve plot showing power
#'       (y-axis) against the grouping variable (x-axis) for each test, with
#'       a dashed reference line at power = 0.80.}
#'   }
#'
#' @examples
#' # Power to detect a lunch-only effect
#' lunch_sims <- power_simulation(
#'   n              = 12,
#'   n_reps         = 500,
#'   effect_sizes   = seq(0, 0.50, by = 0.1),
#'   meals_effected = "lunch"
#' )
#' lunch_sims$plot
#'
#' # Power when all post-exercise meals are affected equally
#' all_meal_sims <- power_simulation(
#'   n              = 12,
#'   n_reps         = 500,
#'   effect_sizes   = seq(0, 0.50, by = 0.1),
#'   meals_effected = c("lunch", "snack", "dinner")
#' )
#' all_meal_sims$plot
power_simulation = function(
    n = 120, 
    n_reps = 200, 
    effect_sizes = seq(0, 0.80, by = 0.05), 
    power_group_by = "lunch_prop_effect",
    pivot_long_on = "lunch_prop_effect",
    meals_effected = "lunch"   # any of: "none", "lunch", "snack", "dinner", c("lunch","dinner"), etc.
){
  # turn character column names into symbols
  grp_var   <- sym(power_group_by)
  pivot_var <- sym(pivot_long_on)
  
  # handle "none" explicitly
  if (identical(meals_effected, "none")) {
    meals_effected <- character(0)
  }
  
  
  ## Run the simulations
  my_sims <- map_dfr(effect_sizes, function(eff) {
    
    # decide effects for this eff
    lunch_eff  <- if ("lunch"  %in% meals_effected) eff else 0
    snack_eff  <- if ("snack"  %in% meals_effected) eff else 0
    dinner_eff <- if ("dinner" %in% meals_effected) eff else 0
    
    map_dfr(seq_len(n_reps), function(rep_id) {
      run_simulation(
        n = n,
        lunch_prop_effect  = lunch_eff,
        snack_prop_effect  = snack_eff,
        dinner_prop_effect = dinner_eff
      )
    })
  })
  
  ## estimate kcal from proportional effect
  my_sims <- my_sims |> 
    mutate(
      lunch_kcal_effect  = 616 * lunch_prop_effect, 
      snack_kcal_effect  = 209 * snack_prop_effect, 
      dinner_kcal_effect = 641 * dinner_prop_effect,
      .before = interaction
    )
  
  ## estimate power
  power_summary <- my_sims |>
    group_by(!!grp_var) |>
    summarise(
      n_sims = n(),
      ### kcal effects
      lunch_kcal_effect  = first(lunch_kcal_effect),
      snack_kcal_effect  = first(snack_kcal_effect),
      dinner_kcal_effect = first(dinner_kcal_effect),
      ### power estimates
      power_interaction = mean(interaction < 0.05),
      power_breakfast   = mean(breakfast   < 0.05),
      power_lunch       = mean(lunch       < 0.05),
      power_snack       = mean(snack       < 0.05),
      power_dinner      = mean(dinner      < 0.05),
      .groups = "drop"
    )
  
  ## pivot long
  power_long <- power_summary |>
    select(!!pivot_var, starts_with("power_")) |>
    pivot_longer(
      cols = starts_with("power_"),
      names_to = "test",
      values_to = "power"
    )
  
  ## build plot
  plot <- power_long |> 
    mutate(
      test = factor(
        test,
        levels = c(
          "power_interaction",
          "power_lunch",
          "power_snack",
          "power_dinner",
          "power_breakfast"
        )
      )
    ) |>
    ggplot(aes(x = !!pivot_var, y = power, color = test)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray50") +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = "kcal reduction at lunch",   # might want to change if pivot_long_on ≠ lunch
      y = "Power (α = 0.05)",
      color = "Test"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
  ## return to user
  list(
    my_sims       = my_sims,
    power_summary = power_summary,
    power_long    = power_long,
    plot          = plot
  )
}

