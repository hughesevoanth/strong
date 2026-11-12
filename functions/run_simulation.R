### A Function to run a simulation testing for meal by condition interactions in a mixed effects model
### By David A Hughes
### Date Nov 18th
run_simulation <- function(n = 120, 
                           lunch_prop_effect = 0,
                           snack_prop_effect = 0,
                           dinner_prop_effect = 0){
  
  ## simulate
  mydata <- simulate_energy_intake(
    n_ind = n,
    beta_cond_by_meal = c(
      breakfast = 0,
      lunch     = -(616 * lunch_prop_effect),
      snack     = -(209 * snack_prop_effect),
      dinner    = -(641 * dinner_prop_effect)
    )
  )
  ## model fitting
  fit0 <- suppressWarnings( suppressMessages( lmerTest::lmer(energy_intake ~ condition + meal + (1 | id), data = mydata, REML = FALSE) ) )
  fit <- suppressWarnings( suppressMessages( lmerTest::lmer(energy_intake ~ condition * meal + (1 | id), data = mydata, REML = FALSE ) ) )
  
  ## test for interaction effect
  a = suppressMessages( anova(fit0, fit) )
  interaction_pvalue = a$`Pr(>Chisq)`[2]
  
  ## extract meal specific effects
  emm <- emmeans(fit, ~ condition | meal)
  emm_pairs = pairs(emm, adjust = "tukey")
  
  ## Extract a data frame
  emm_df <-  as.data.frame( emm_pairs )
  
  ## extract p-values for meal specific contrasts
  emm_pvalues = emm_df$p.value
  
  ## collect all pvalues
  pvalues = c(interaction_pvalue, emm_pvalues)
  names(pvalues) = c("interaction","breakfast","lunch","snack","dinner")
  
  out = c(n, lunch_prop_effect, snack_prop_effect, dinner_prop_effect, pvalues)
  names(out)[1:4] = c("n","lunch_prop_effect","snack_prop_effect","dinner_prop_effect")
  
  return(out)
  
}

