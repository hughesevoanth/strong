### A Function to test for meal by condition interactions in a mixed effects model
### By David A Hughes
### Date Nov 18th
test_meal_condition_interaction <- function(data, outcome = "total") {
  
  ## Test for a meal by condition interaction
  f0 <- as.formula(paste(outcome, "~ Condition + Meal + (1 | SubjectID)"))
  f  <- as.formula(paste(outcome, "~ Condition * Meal + (1 | SubjectID)"))
  
  fit0 <- lmerTest::lmer(f0, data = data, REML = FALSE)
  fit  <- lmerTest::lmer(f,  data = data, REML = FALSE)
  
  anova_res <- anova(fit0, fit)
  interaction_pvalue <- anova_res$`Pr(>Chisq)`[2]
  names(interaction_pvalue) <- "interaction_pvalue"
  
  ## Test for meal specific effects
  emm <- emmeans(fit, ~ Condition | Meal, level = 0.95)
  emm_pairs = pairs(emm, adjust = "none")
  
  ## Extract a data frame
  emm_df <- as.data.frame( emm )
  emm_df <- emm_df |>
    select(Meal, Condition, emmean, SE, df, lower.CL, upper.CL ) |>
    pivot_wider(
      names_from = Condition,
      values_from = c(emmean, SE, df, lower.CL, upper.CL),
      names_sep = "_"
    )
  
  ## Extract a data frame
  emm_pairs_df <- as.data.frame( emm_pairs )
  emm_pairs_df <- emm_pairs_df |>
    select(Meal, estimate, SE, df, t.ratio, p.value )
  
  emm_out = left_join(emm_df, emm_pairs_df, by = "Meal")
  emm_out = emm_out |> mutate( outcome = outcome, .after = Meal)
  
  ###
  out = list(interaction_pvalue = interaction_pvalue, emm_out = emm_out)
  return(out)
}
