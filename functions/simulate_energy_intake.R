### A Function to simulate energy intake data with meal by condition interactions
### By David A Hughes
### Date Nov 18th
simulate_energy_intake <- function(
    n_ind = 100,
    # exercise - rest effect (in kcal) for each meal,
    # in the order: breakfast, lunch, snack, dinner
    beta_cond_by_meal = c(0, 0, 0, 0),
    # Mean and SD under REST condition, same order
    mean_meal = c(420, 616, 209, 641),
    sd_meal   = c(92, 195, 69, 153),
    sd_id = 50,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  meal_levels <- c("breakfast", "lunch", "snack", "dinner")
  
  ## ---- Check lengths and coerce to plain numeric vectors ----
  if (length(mean_meal) != 4L)  stop("mean_meal must have length 4 (breakfast, lunch, snack, dinner).")
  if (length(sd_meal)   != 4L)  stop("sd_meal must have length 4 (breakfast, lunch, snack, dinner).")
  if (length(beta_cond_by_meal) != 4L) stop("beta_cond_by_meal must have length 4 (breakfast, lunch, snack, dinner).")
  
  mean_meal         <- as.numeric(mean_meal)
  sd_meal           <- as.numeric(sd_meal)
  beta_cond_by_meal <- as.numeric(beta_cond_by_meal)
  
  ## ---- Fixed effects: intercept and meal main effects (REST) ----
  # Index: 1 = breakfast, 2 = lunch, 3 = snack, 4 = dinner
  intercept <- mean_meal[1L]  # breakfast at rest
  
  # Meal main effects at rest: difference vs breakfast
  beta_meal <- c(
    0,                          # breakfast
    mean_meal[2L] - mean_meal[1L],  # lunch  - breakfast
    mean_meal[3L] - mean_meal[1L],  # snack  - breakfast
    mean_meal[4L] - mean_meal[1L]   # dinner - breakfast
  )
  
  ## ---- Variances ----
  var_id    <- sd_id^2
  var_total <- sd_meal^2
  var_resid <- pmax(var_total - var_id, 1e-6)
  sd_resid  <- sqrt(var_resid)
  
  ## ---- Design: REST + EXERCISE days, all 4 meals ----
  # For each id:
  #   REST:     breakfast, lunch, snack, dinner
  #   EXERCISE: breakfast, lunch, snack, dinner
  n_meals  <- length(meal_levels)   # 4
  n_rows_id <- 2L * n_meals         # 8 rows per id
  
  id <- factor(rep(seq_len(n_ind), each = n_rows_id))
  
  meal <- factor(
    rep(rep(meal_levels, times = 2L), times = n_ind),
    levels = meal_levels
  )
  
  condition <- factor(
    rep(c(rep("rest",     times = n_meals),
          rep("exercise", times = n_meals)),
        times = n_ind),
    levels = c("rest", "exercise")
  )
  
  ## ---- Random intercepts ----
  b_id <- rnorm(n_ind, mean = 0, sd = sd_id)
  b_i  <- b_id[as.integer(id)]
  
  ## ---- Fixed part ----
  m_idx <- as.integer(meal)          # 1..4
  c_num <- ifelse(condition == "exercise", 1, 0)
  
  meal_eff <- beta_meal[m_idx]
  cond_eff <- c_num * beta_cond_by_meal[m_idx]
  
  mu <- intercept + meal_eff + cond_eff
  
  ## ---- Residuals ----
  eps <- rnorm(length(mu), mean = 0, sd = sd_resid[m_idx])
  
  energy_intake <- mu + b_i + eps
  
  data.frame(
    id,
    condition,
    meal,
    energy_intake
  )
}

