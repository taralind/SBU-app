library(brms)

# 1. Create dummy data with the exact structure your app uses
dummy_data <- data.frame(y = 0)

# 2. Define static, weakly informative priors 
static_priors <- c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("student_t(3, 0, 2.5)", class = "sigma"),
  set_prior("normal(0, 4)", class = "alpha") # Specific to skew_normal shape
)

# 3. Compile the C++ skeleton
# chains = 0 ensures it only compiles the math without running MCMC sampling
model_skeleton <- brm(
  formula = y ~ 1,
  data    = dummy_data,
  family  = skew_normal(),
  prior   = static_priors,
  backend = "rstan",
  chains  = 0 
)

# 4. Save the compiled model into Shiny app's directory
saveRDS(model_skeleton, "model_skeleton.rds")