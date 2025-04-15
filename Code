# A. =========================================================================== ------------ ENGINEER ------------ ===================================================================== 

# 0. SETUP ===================================================================================================================================================

library(roll)
library(xts)
library(dccmidas)
library(rumidas)
library(readxl)
library(quantmod)
library(dplyr)
library(openxlsx)
library(tseries)
library(strucchange)
library(ggplot2)
library(lubridate) 


# 0. STRUCTURE BREAK TEST FUNCTION ===========================================================================================================================================================

# Structure break test, output vector of breakpoints
bai_perron_test <- function(series, max_breaks = 5, start_date = NULL, end_date = NULL) {
  # Load necessary libraries
  library(strucchange)
  library(ggplot2)
  library(xts)
  
  # Ensure input is an xts series
  if (!inherits(series, "xts")) {
    stop("Error: Input must be an xts time series.")
  }
  
  # Subset xts series based on start_date and end_date
  if (!is.null(start_date) & !is.null(end_date)) {
    series <- series[paste(start_date, end_date, sep = "/")]
  }
  
  # Convert xts to numeric vector while preserving index
  numeric_values <- as.numeric(series)
  time_index <- index(series)  # Extract actual time index
  
  # Ensure series is numeric and not empty after subsetting
  if (any(is.na(numeric_values)) | length(numeric_values) < 2) {
    stop("Error: The series contains NAs or is too short for analysis.")
  }
  
  # Perform Bai-Perron test using breakpoints()
  bp_result <- breakpoints(numeric_values ~ 1, breaks = max_breaks)
  
  # Print results
  cat("\n🔹 Bai-Perron Structural Break Test Results:\n")
  print(bp_result)
  
  # Extract detected breakpoints (in observation index format)
  breakpoints_index <- bp_result$breakpoints  # These are positions in the numeric vector
  
  # Convert breakpoints to actual date format
  if (!is.null(breakpoints_index) && length(breakpoints_index) > 0) {
    breakpoint_dates <- time_index[breakpoints_index]
  } else {
    breakpoint_dates <- NULL
    breakpoints_index <- NULL
  }
  
  # Create data frame for plotting
  df <- data.frame(Time = time_index, Value = numeric_values)
  
  # Plot time series with breakpoints highlighted
  p <- ggplot(df, aes(x = Time, y = Value)) +
    geom_line(color = "blue", size = 1) +
    geom_vline(xintercept = as.numeric(breakpoint_dates), color = "red", linetype = "dashed", size = 1) +
    labs(title = "Bai-Perron Structural Break Test",
         subtitle = if (!is.null(breakpoint_dates)) 
           paste("Detected Breakpoints at:", paste(breakpoint_dates, collapse = ", ")) 
         else "No breakpoints detected",
         x = "Time",
         y = "Series Value") +
    theme_minimal()
  
  print(p)  # Display the plot
  
  # Return detected breakpoints
  return(list(
    breakpoints_vector = breakpoint_dates,  # Actual date format
    breakpoints_list = as.list(breakpoint_dates),  # List format (dates)
    breakpoints_index_vector = breakpoints_index,  # Numeric index format
    breakpoints_index_list = as.list(breakpoints_index)  # List format (indices)
  ))
}

# Create list of dummy variables in monthly format
create_dummy_variables <- function(breakpoints_dates, series) {
  library(xts)  # Ensure xts is loaded
  
  # Ensure input series is xts
  if (!inherits(series, "xts")) {
    stop("Error: The input series must be an xts object.")
  }
  
  # Get time index from xts series
  time_index <- index(series)
  
  # Convert date-based breakpoints to observation indices
  breakpoints <- match(breakpoints_dates, time_index)  # Find position of each date in the index
  
  # Ensure breakpoints are valid (not NA)
  if (any(is.na(breakpoints))) {
    stop("Error: Some breakpoint dates are not found in the series index.")
  }
  
  n <- length(breakpoints)  # Number of breakpoints
  dummy_vars <- list()  # List to store dummy variable series
  
  # Append start and end points
  breakpoints <- c(1, breakpoints, length(series) + 1)
  
  # Generate dummy variables
  for (i in seq_len(n)) {
    dummy <- rep(0, length(series))
    dummy[breakpoints[i+1]:(breakpoints[i + 1 + 1] - 1)] <- 1  # Assign dummy value i
    
    # Convert to xts with original index
    dummy_vars[[i]] <- xts(dummy, order.by = time_index)
  }
  
  return(dummy_vars)
}

# Convert dummy variables in daily frequency
convert_monthly_to_daily <- function(monthly_series) {
  # Ensure input is an xts object
  if (!inherits(monthly_series, "xts")) {
    stop("Error: The input series must be an xts object.")
  }
  
  # Extract time index and values
  monthly_dates <- index(monthly_series)
  values <- as.numeric(monthly_series)
  
  # Create an empty list to store daily data
  daily_data <- list()
  
  # Generate daily data for each month
  for (i in seq_along(monthly_dates)) {
    start_date <- as.Date(monthly_dates[i])  # First day of the month
    end_date <- ceiling_date(start_date, "month") - 1  # Last day of the month
    daily_dates <- seq(start_date, end_date, by = "day")  # Create daily sequence
    
    # Replicate the value for each day in the month
    daily_values <- rep(values[i], length(daily_dates))
    
    # Store in the list
    daily_data[[i]] <- xts(daily_values, order.by = daily_dates)
  }
  
  # Combine all daily xts objects into one
  daily_series <- do.call(rbind, daily_data)
  
  return(daily_series)
}

# Transform it into approporate format to be compatiable with model calculation
add_xts_matrix <- function(D_series, row, col, start_date, end_date) {
  XXX <- D_series[paste(start_date, end_date, sep = "/")]  
  output_matrix <- matrix(as.numeric(XXX), nrow = row, ncol = col, byrow = TRUE)
  return(output_matrix)
}

# 1. MEAN MODEL ESTIMATION FUNCTION ===========================================================================================================================

mean_fit <- function(returns) {
  # Ensure input is an xts object
  if (!"xts" %in% class(returns)) {
    stop("Input data must be an xts object.")
  }
  
  # Extract the return values and date index
  return_values <- coredata(returns)  # Extract numeric data
  date_index <- index(returns)  # Extract date index
  
  n <- length(return_values)  # Number of observations
  
  # Step 1: Estimate the mean (muy) using Ordinary Least Squares (OLS)
  mu_hat <- mean(return_values)
  
  # Step 2: Compute residuals
  residuals_est <- return_values - mu_hat
  
  # Step 3: Compute standard error of the mean
  std_error <- sd(return_values) / sqrt(n)
  
  # Step 4: Compute t-statistic
  t_stat <- mu_hat / std_error
  
  # Step 5: Compute p-value for the t-test (two-tailed test)
  p_value <- 2 * (1 - pt(abs(t_stat), df = n - 1))
  
  # Print results
  cat("Estimated Mean (muy):", mu_hat, "\n")
  cat("Standard Error:", std_error, "\n")
  cat("t-Statistic:", t_stat, "\n")
  cat("p-Value:", p_value, "\n")
  
  # Convert residuals back into an xts object with the original date index
  residuals_xts <- xts(residuals_est, order.by = date_index)
  colnames(residuals_xts) <- "Residuals"
  
  # Return a list containing mean, residuals (xts), and test statistics
  return(list(
    estimated_mean = mu_hat, 
    residuals = residuals_xts,  # Residuals in xts format with Date Index
    standard_error = std_error, 
    t_statistic = t_stat, 
    p_value = p_value
  ))
}


# 2. GARCH_MIDAS ESTIMATION FUNCTION ====================================================================================================================================


GM_LL <- function (param, daily_ret, mv_m, mv_m_m, K, distribution, lag_fun = "Beta", dummy = dummy)
  # param must have 7 objects
  # input the mv_m_m
  
{
  if (distribution == "norm") {
    alpha <- param[1]
    beta <- param[2]
    m <- param[3]
    theta <- param[4]
    theta_m <- param[6] # New theta
    
    w1 <- ifelse(lag_fun == "Beta", 1, 0)
    w2 <- param[5]
    w1_m <- ifelse(lag_fun == "Beta", 1, 0) # New w1 
    w2_m <- param[7] # New w2
    
    theta_m1 <- param[8] ###
    theta_m2 <- param[9] ###
    
    theta_1 <- param[10] ###
    theta_2 <- param[11] ###
    
    D1 <- dummy[[1]]
    D2 <- dummy[[2]]
    
    TT <- length(daily_ret)
    tau_d <- rep(NA, TT)
    g_it <- rep(1, TT)
    ll <- 0
    
    weight_fun <- ifelse(lag_fun == "Beta", beta_function, exp_almon)
    betas <- c(rev(weight_fun(1:(K + 1), (K + 1), w1, w2))[2:(K + 1)], 0)
    weight_fun_m <- ifelse(lag_fun == "Beta", beta_function, exp_almon) # new
    betas_m <- c(rev(weight_fun(1:(K + 1), (K + 1), w1_m, w2_m))[2:(K + 1)], 0) # new
    
    RV_component <- suppressWarnings(roll_sum(mv_m, c(K + 1), weights = betas))
    XX_component <- log(suppressWarnings(roll_sum(mv_m_m, c(K + 1), weights = betas_m)))
    
    
    tau_d <- exp(m + (theta + theta_1 * D1 + theta_2 * D2) * RV_component 
                 + (theta_m + theta_m1 * D1 + theta_m2 * D2) * XX_component)
    
    
    tau_d <- tau_d[(K + 1), ]
    
    step_1 <- (1 - alpha - beta) + (alpha) * (daily_ret)^2/tau_d
    for (i in 2:TT) {
      g_it[i] <- sum(step_1[i - 1], beta * g_it[i - 1], 
                     na.rm = T)
    }
    ll <- as.numeric(stats::dnorm(daily_ret, mean(daily_ret), 
                                  sqrt(g_it * tau_d), log = TRUE))
  }
  return(ll)
}

GM_long_run_vol <- function (param, daily_ret, mv_m, mv_m_m, K, lag_fun = "Beta", dummy = dummy)   
  # param must have 7 objects
  # input the mv_m_m
{
  alpha <- param[1]
  beta <- param[2]
  m <- param[3]
  theta <- param[4]
  theta_m <- param[6] # New theta
  
  w1 <- ifelse(lag_fun == "Beta", 1, 0)
  w2 <- param[5]
  w1_m <- ifelse(lag_fun == "Beta", 1, 0) # New w1 
  w2_m <- param[7] # New w2
  
  theta_m1 <- param[8] ###
  theta_m2 <- param[9] ###
  
  theta_1 <- param[10] ###
  theta_2 <- param[11] ###
  
  D1 <- dummy[[1]]
  D2 <- dummy[[2]]
  
  
  TT <- length(daily_ret)
  tau_d <- rep(NA, TT)
  g_it <- rep(1, TT)
  ll <- 0
  
  weight_fun <- ifelse(lag_fun == "Beta", beta_function, exp_almon)
  betas <- c(rev(weight_fun(1:(K + 1), (K + 1), w1, w2))[2:(K + 1)], 0)
  weight_fun_m <- ifelse(lag_fun == "Beta", beta_function, exp_almon) # new
  betas_m <- c(rev(weight_fun(1:(K + 1), (K + 1), w1_m, w2_m))[2:(K + 1)], 0) # new
  
  RV_component <- suppressWarnings(roll_sum(mv_m, c(K + 1), weights = betas))
  XX_component <- log(suppressWarnings(roll_sum(mv_m_m, c(K + 1), weights = betas_m)))
  
  
  tau_d <- exp(m + (theta + theta_1 * D1 + theta_2 * D2) * RV_component 
               + (theta_m + theta_m1 * D1 + theta_m2 * D2) * XX_component)
  
  
  tau_d <- tau_d[(K + 1), ]
  
  step_1 <- (1 - alpha - beta) + (alpha) * (daily_ret)^2/tau_d
  for (i in 2:TT) {
    g_it[i] <- sum(step_1[i - 1], beta * g_it[i - 1], 
                   na.rm = T)
  }
  cond_vol <- sqrt(tau_d)
  cond_vol <- as.xts(cond_vol, index(daily_ret))
}

GM_conditional_vol <- function (param, daily_ret, mv_m, mv_m_m, K, lag_fun = "Beta", dummy = dummy) 
  # param must have 7 objects
  # input the mv_m_m
{
  alpha <- param[1]
  beta <- param[2]
  m <- param[3]
  theta <- param[4]
  theta_m <- param[6] # New theta
  
  w1 <- ifelse(lag_fun == "Beta", 1, 0)
  w2 <- param[5]
  w1_m <- ifelse(lag_fun == "Beta", 1, 0) # New w1 
  w2_m <- param[7] # New w2
  
  theta_m1 <- param[8] ###
  theta_m2 <- param[9] ###
  
  theta_1 <- param[10] ###
  theta_2 <- param[11] ###
  
  D1 <- dummy[[1]]
  D2 <- dummy[[2]]
  
  TT <- length(daily_ret)
  tau_d <- rep(NA, TT)
  g_it <- rep(1, TT)
  ll <- 0
  
  weight_fun <- ifelse(lag_fun == "Beta", beta_function, exp_almon)
  betas <- c(rev(weight_fun(1:(K + 1), (K + 1), w1, w2))[2:(K + 1)], 0)
  weight_fun_m <- ifelse(lag_fun == "Beta", beta_function, exp_almon) # new
  betas_m <- c(rev(weight_fun(1:(K + 1), (K + 1), w1_m, w2_m))[2:(K + 1)], 0) # new
  
  RV_component <- suppressWarnings(roll_sum(mv_m, c(K + 1), weights = betas))
  XX_component <- log(suppressWarnings(roll_sum(mv_m_m, c(K + 1), weights = betas_m)))
  
  
  tau_d <- exp(m + (theta + theta_1 * D1 + theta_2 * D2) * RV_component 
               + (theta_m + theta_m1 * D1 + theta_m2 * D2) * XX_component)
  
  
  tau_d <- tau_d[(K + 1), ]
  
  step_1 <- (1 - alpha - beta) + (alpha) * (daily_ret)^2/tau_d
  for (i in 2:TT) {
    g_it[i] <- sum(step_1[i - 1], beta * g_it[i - 1], na.rm = T)
  }
  cond_vol <- sqrt(g_it * tau_d)
  cond_vol <- as.xts(cond_vol, index(daily_ret))
}


ugmfit_ver1 <- function (model, skew, distribution, daily_ret, mv_m, mv_m_2, K, K_2 = NULL, 
                         lag_fun = "Beta", X = NULL, out_of_sample = NULL, vol_proxy = NULL, 
                         R = 100, dummy = dummy) 
{
  if ((model != "GM") & (model != "GMX") & (model != "DAGM") & 
      (model != "DAGM2M") & (model != "DAGMX")) {
    stop(cat("#Warning:\n Valid choices for the parameter 'model' are 'GM','GMX','DAGM', 'DAGM2M' and 'DAGMX' \n"))
  }
  if ((model == "GMX" | model == "DAGMX") & (missing(X))) {
    stop(cat("#Warning:\n If the model chosen includes the 'X' term, then the 'X' variable has to be provided \n"))
  }
  if ((model == "DAGM2M") & (missing(mv_m_2) | missing(K_2))) {
    stop(cat("#Warning:\n If the model is the DAGM with two MIDAS variables (DAGM2M), then the 'mv_m_2' and 'K_2' parameters have to be provided \n"))
  }
  if ((skew != "YES") & (skew != "NO")) {
    stop(cat("#Warning:\n Valid choices for the parameter 'skew' are 'YES' and 'NO' \n"))
  }
  if ((distribution != "norm") & (distribution != "std")) {
    stop(cat("#Warning:\n Valid choices for the parameter 'distribution' are 'norm' and 'std' \n"))
  }
  if ((lag_fun != "Beta") & (lag_fun != "Almon")) {
    stop(cat("#Warning:\n Valid choices for the parameter 'lag_fun' are 'Beta' and 'Almon' \n"))
  }
  N <- length(daily_ret)
  cond_r_t <- class(daily_ret)[1]
  cond_mv_m <- class(mv_m)[1]
  if (cond_r_t != "xts") {
    stop(cat("#Warning:\n Parameter 'daily_ret' must be an xts object. Please provide it in the correct form \n"))
  }
  if (cond_mv_m != "matrix") {
    stop(cat("#Warning:\n Parameter 'mv_m' must be a matrix. Please provide it in the correct form \n"))
  }
  if (dim(mv_m)[2] != N) {
    stop(cat("#Warning:\n The columns of the matrix 'mv_m' must be equal to the length of vector 'daily_ret'. Please provide it in the correct form \n"))
  }
  if (!missing(mv_m_2)) {
    cond_mv_m_2 <- class(mv_m_2)[1]
    if (cond_mv_m_2 != "matrix") {
      stop(cat("#Warning:\n Parameter 'mv_m_2' must be a matrix. Please provide it in the correct form \n"))
    }
    #    if (dim(mv_m_2)[2] != N) {
    #     stop(cat("#Warning:\n The columns of the matrix 'mv_m_2' must be equal to the length of vector 'daily_ret'. Please provide it in the correct form \n"))
    #  }
  }
  if (!missing(vol_proxy)) {
    if (any(range(time(daily_ret)) != range(time(vol_proxy)))) {
      stop(cat("#Warning:\n The vector 'vol_proxy' has to be observed during the same time span of 'daily_ret' \n"))
    }
  }
  if (!missing(X)) {
    if (any(range(time(daily_ret)) != range(time(X)))) {
      stop(cat("#Warning:\n The vector 'X' has to be observed during the same time span of 'daily_ret' \n"))
    }
  }
  if (!missing(X)) {
    if (length(X) != length(daily_ret)) {
      stop(cat("#Warning:\n The vector 'X' must have the same length of 'daily_ret' \n"))
    }
  }
  if (missing(out_of_sample)) {
    r_t_in_s <- daily_ret
    mv_m_in_s <- mv_m
    if (!missing(mv_m_2)) {
      mv_m_2_in_s <- mv_m_2
    }
    if (missing(vol_proxy)) {
      vol_proxy_in_s <- r_t_in_s^2
    }
    else {
      vol_proxy_in_s <- vol_proxy
    }
    X_in_s <- X
  }
  else {
    r_t_in_s <- daily_ret[1:(N - out_of_sample)]
    mv_m_in_s <- mv_m[, 1:(N - out_of_sample)]
    if (!missing(mv_m_2)) {
      mv_m_2_in_s <- mv_m_2[, 1:(N - out_of_sample)]
    }
    if (missing(vol_proxy)) {
      vol_proxy_in_s <- r_t_in_s^2
    }
    else {
      vol_proxy_in_s <- vol_proxy[1:(N - out_of_sample)]
    }
    X_in_s <- X[1:(N - out_of_sample)]
  }
  
  if (model == "GM" & skew == "NO" & distribution == "norm" & 
      lag_fun == "Beta") {
    start_val <- begin_val <- ui <- ci <- NULL
    begin_val <- matrix(NA, nrow = R, ncol = 11) # Adjust fromm 7 to 11
    colnames(begin_val) <- c("alpha", "beta", "m", "theta", "w2", "theta_m", "w2_m", "theta_1", "theta_2", "theta_m1", "theta_m2") # add theta and w2_m
    begin_val[, 1] <- stats::runif(R, min = 0.001, max = 0.095)
    begin_val[, 2] <- stats::runif(R, min = 0.6, max = 0.8)
    begin_val[, 3] <- stats::runif(R, min = -1, max = 1)
    begin_val[, 4] <- stats::runif(R, min = -1, max = 1)
    begin_val[, 5] <- 1.01
    begin_val[, 6] <- stats::runif(R, min = -1, max = 1) # new 
    begin_val[, 7] <- 1.01 # new
    begin_val[, 8] <- stats::runif(R, min = -1, max = 1) # new 
    begin_val[, 9] <- stats::runif(R, min = -1, max = 1) # new 
    begin_val[, 10] <- stats::runif(R, min = -1, max = 1) # new 
    begin_val[, 11] <- stats::runif(R, min = -1, max = 1) # new 
    
    which_row <- rep(NA, R)
    for (i in 1:R) {
      which_row[i] <- sum(GM_LL(begin_val[i, ], r_t_in_s, mv_m_in_s, mv_m_2_in_s, K = K, distribution = "norm", lag_fun = "Beta", dummy = dummy)) # ad mv_m_2_in_s
    }
    start_val <- begin_val[which.max(which_row), ]
    ui <- rbind(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #
                c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), #
                c(-1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0), # 
                c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), #
                c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)) #
    ci <- c(-0.001, -0.001, 0.999, -1.001, -1.001) #
    # Mean lies within a bound
    # Omega is positive and <= realize variance
    # Alpha & beta within 0 and 1
    # Alpha + beta <= 1
    # w2 and w2_m > 1
    
    
    LOGLIK <- GM_LL
    cond_vol <- GM_conditional_vol 
    long_run_vol <- GM_long_run_vol
  }
  
  
  r_t_in_s_est <- zoo::coredata(r_t_in_s)
  if (!missing(X)) {
    X_in_s_est <- zoo::coredata(X_in_s) - mean(zoo::coredata(X_in_s))
  }
  else {
    X_in_s_est <- X_in_s
  }
  if (!missing(X)) {
    est <- suppressWarnings(maxLik(logLik = LOGLIK, start = start_val, 
                                   daily_ret = r_t_in_s_est, X = X_in_s_est, mv_m = mv_m_in_s,
                                   K = K, lag_fun = lag_fun, distribution = distribution, 
                                   constraints = list(ineqA = ui, ineqB = ci), iterlim = 1000, method = "BFGS"))
  }
  else if (model == "DAGM2M") {
    est <- suppressWarnings(maxLik(logLik = LOGLIK, start = start_val, 
                                   daily_ret = r_t_in_s_est, mv_m_1 = mv_m_in_s, mv_m_2 = mv_m_2_in_s, 
                                   K_1 = K, K_2 = K_2, lag_fun = lag_fun, distribution = distribution, 
                                   constraints = list(ineqA = ui, ineqB = ci), iterlim = 1000, 
                                   method = "BFGS"))
  }
  else {
    est <- suppressWarnings(maxLik(logLik = LOGLIK, 
                                   start = start_val, daily_ret = r_t_in_s_est, 
                                   mv_m = mv_m_in_s, mv_m_m = mv_m_2_in_s, K = K, # add mv_m_m 
                                   lag_fun = lag_fun, dummy = dummy, distribution = distribution, 
                                   constraints = list(ineqA = ui, ineqB = ci), iterlim = 1000, method = "BFGS"))
  }
  N_coef <- length(stats::coef(est))
  mat_coef <- data.frame(rep(NA, N_coef), rep(NA, N_coef), 
                         rep(NA, N_coef), rep(NA, N_coef))
  colnames(mat_coef) <- c("Estimate", "Std. Error", "t value", 
                          "Pr(>|t|)")
  rownames(mat_coef) <- names(stats::coef(est))
  mat_coef[, 1] <- round(stats::coef(est), 6)
  mat_coef[, 2] <- round(rumidas:::QMLE_sd(est), 6)
  mat_coef[, 3] <- round(stats::coef(est)/rumidas:::QMLE_sd(est), 6)
  mat_coef[, 4] <- round(apply(rbind(stats::coef(est)/rumidas:::QMLE_sd(est)), 
                               1, function(x) 2 * (1 - stats::pnorm(abs(x)))), 6)
  if (skew == "YES") {
    mat_coef2 <- mat_coef
    mat_coef2[2, ] <- mat_coef[3, ]
    rownames(mat_coef2)[3] <- "temp"
    rownames(mat_coef2)[2] <- "gamma"
    mat_coef2[3, ] <- mat_coef[2, ]
    rownames(mat_coef2)[3] <- "beta"
    mat_coef <- mat_coef2
  }
  
  est_coef <- stats::coef(est)
  if (distribution == "std") {
    est_coef <- est_coef[-N_coef]
  }
  if (!missing(X)) {
    vol_est <- (cond_vol(est_coef, r_t_in_s, X_in_s, mv_m_in_s, 
                         K = K, lag_fun = lag_fun))^2
    lr_vol <- long_run_vol(est_coef, r_t_in_s, X_in_s, mv_m_in_s, 
                           K = K, lag_fun = lag_fun)
  }
  else if (model == "DAGM2M") {
    vol_est <- (cond_vol(est_coef, r_t_in_s, mv_m_1 = mv_m_in_s, 
                         mv_m_2 = mv_m_2_in_s, K_1 = K, K_2 = K_2, lag_fun = lag_fun))^2
    lr_vol <- long_run_vol(est_coef, r_t_in_s, mv_m_1 = mv_m_in_s, 
                           mv_m_2 = mv_m_2_in_s, K_1 = K, K_2 = K_2, lag_fun = lag_fun)
  }
  else {
    vol_est <- (cond_vol(est_coef, r_t_in_s, mv_m_in_s, mv_m_2_in_s, K = K, lag_fun = lag_fun, dummy = dummy))^2 # add mv_m_2_in_s
    lr_vol <- long_run_vol(est_coef, r_t_in_s, mv_m_in_s, mv_m_2_in_s, K = K, lag_fun = lag_fun, dummy = dummy)  # add mv_m_2_in_s
  }
  if (missing(out_of_sample)) {
    N_est <- length(vol_est)
    vol_est_lf <- zoo::coredata(vol_est)
    vol_proxy_lf <- zoo::coredata(vol_proxy_in_s)
    res <- list(model = model, rob_coef_mat = mat_coef, obs = N, 
                period = range(stats::time(r_t_in_s)), loglik = as.numeric(stats::logLik(est)), 
                inf_criteria = rumidas:::Inf_criteria(est), loss_in_s = rumidas:::LF_f(vol_est_lf, vol_proxy_lf), est_vol_in_s = vol_est^0.5, est_lr_in_s = lr_vol)
  }
  else {
    r_t_oos <- daily_ret[(N - out_of_sample + 1):N]
    mv_m_oos <- mv_m[, (N - out_of_sample + 1):(N)]
    if (model == "DAGM2M") {
      mv_m_oos_2 <- mv_m_2[, (N - out_of_sample + 1):(N)]
    }
    if (missing(vol_proxy)) {
      vol_proxy_oos <- r_t_oos^2
    }
    else {
      vol_proxy_oos <- vol_proxy[(N - out_of_sample + 1):N]
    }
    N_est <- length(vol_est)
    vol_est_lf <- zoo::coredata(vol_est)
    vol_proxy_lf <- zoo::coredata(vol_proxy_in_s)
    vol_proxy_oos_lf <- zoo::coredata(vol_proxy_oos)
    if (!missing(X)) {
      X_oos <- X - mean(X)
      X_oos_f <- X_oos[(N - out_of_sample + 1):N]
      vol_est_oos <- (cond_vol(est_coef, r_t_oos, X_oos_f, 
                               mv_m_oos, K = K, lag_fun = lag_fun))^2
      LR_oos <- (long_run_vol(est_coef, r_t_oos, X_oos_f, 
                              mv_m_oos, K = K, lag_fun = lag_fun))
    }
    else if (model == "DAGM2M") {
      vol_est_oos <- (cond_vol(est_coef, r_t_oos, mv_m_1 = mv_m_oos, 
                               mv_m_2 = mv_m_oos_2, K_1 = K, K_2 = K_2, lag_fun = lag_fun))^2
      LR_oos <- (long_run_vol(est_coef, r_t_oos, mv_m_1 = mv_m_oos, 
                              mv_m_2 = mv_m_oos_2, K_1 = K, K_2 = K_2, lag_fun = lag_fun))
    }
    else {
      vol_est_oos <- (cond_vol(est_coef, r_t_oos, mv_m_oos, 
                               K = K, lag_fun = lag_fun))^2
      LR_oos <- (long_run_vol(est_coef, r_t_oos, mv_m_oos, 
                              K = K, lag_fun = lag_fun))
    }
    vol_est_oos_lf <- zoo::coredata(vol_est_oos)
    
    res <- list(model = model, rob_coef_mat = mat_coef, obs = length(r_t_in_s), 
                period = range(time(r_t_in_s)), loglik = as.numeric(stats::logLik(est)), 
                inf_criteria = rumidas:::Inf_criteria(est), loss_in_s = rumidas:::LF_f(vol_est_lf, vol_proxy_lf), est_vol_in_s = vol_est^0.5, est_lr_in_s = lr_vol, 
                loss_oos = rumidas:::LF_f(vol_est_oos_lf, vol_proxy_oos_lf), 
                est_vol_oos = vol_est_oos^0.5, est_lr_oos = LR_oos)
  }
  print("Garch_midas_done!")
  class(res) <- c("rumidas")
  return(res)
  print.rumidas(res)
}




# 3. DCC_MIDAS ESTIMATION FUNCTION ================================================================================================================================================

# set up the log likelihood function
dccmidas_ll <- function (param, res, lag_fun = "Beta", N_c, K_c, dcc_mv = NULL) # add mv_m
  
{
  a <- param[1]
  b <- param[2]
  w1 <- ifelse(lag_fun == "Beta", 1, 0)
  w2 <- param[3]
  m <- param[4] #
  theta <- param[5] #
  theta_m <- param[6] #
  w1_m <- ifelse(lag_fun == "Beta", 1, 0) #
  w2_m <- param[7] #
  
  
  Num_col <- dim(res)[1] # Number of variables (rows in res).
  TT <- dim(res)[3] # Number of time steps
  
  
  X_t <- array(0, dim = c(Num_col, Num_col, TT))
  for (tt in 1:TT) {
    X_t[, , tt] <- matrix(dcc_mv[tt], nrow = Num_col, ncol = Num_col)
  }
  C_t <- array(0, dim = c(Num_col, Num_col, TT))      # realized correlation
  V_t <- array(0, dim = c(Num_col, Num_col, TT))      # Variance matrix for standardization.
  Prod_eps_t <- array(0, dim = c(Num_col, Num_col, TT)) # Cross-products of residuals.
  
  Q_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))
  R_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))
  
  log_det_R_t <- rep(0, TT)
  R_t_solved <- array(1, dim = c(Num_col, Num_col, TT))
  Eps_t_cross_prod <- rep(0, TT)
  Eps_t_R_t_Eps_t <- rep(0, TT)
  
  
  
  for (tt in (N_c + 1):TT) {
    V_t[, , tt] <- rowSums(res[, 1, tt:(tt - N_c)] * (res[, 1, tt:(tt - N_c)])) * diag(Num_col)
    Prod_eps_t[, , tt] <- res[, , tt:(tt - N_c)] %*% t(res[, , tt:(tt - N_c)])
    V_t_0.5 <- Inv(sqrt(V_t[, , tt]))
    C_t[, , tt] <- V_t_0.5 %*% Prod_eps_t[, , tt] %*% V_t_0.5
  }
  
  
  weight_fun <- ifelse(lag_fun == "Beta", rumidas::beta_function, rumidas::exp_almon)
  betas <- c(rev(weight_fun(1:(K_c + 1), (K_c + 1), w1, w2))[2:(K_c +  1)], 0)
  weight_fun_m <- ifelse(lag_fun == "Beta", rumidas::beta_function, rumidas::exp_almon)
  betas_m <- c(rev(weight_fun_m(1:(K_c + 1), (K_c + 1), w1_m, w2_m))[2:(K_c +  1)], 0)
  
  R_t_bar <- array(1, dim = c(Num_col, Num_col, TT))
  Z_t_bar <- array(1, dim = c(Num_col, Num_col, TT)) # New
  matrix_id <- matrix(1:Num_col^2, ncol = Num_col)
  matrix_id_2 <- which(matrix_id == 1:Num_col^2, arr.ind = TRUE)
  
  
  for (i in 1:nrow(matrix_id_2)) {
    C_component <- suppressWarnings(roll::roll_sum(C_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas)) 
    X_component <- suppressWarnings(roll::roll_sum(X_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas_m))
    Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- m + theta*C_component + theta_m*log(X_component)
    R_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) - 1) / 
      (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) + 1)
  }
  
  Z_t_bar[, , 1:K_c] <- diag(Num_col)
  R_t_bar[, , 1:K_c] <- diag(Num_col)
  
  for (tt in 1:dim(Z_t_bar)[3]) {  
    diag(Z_t_bar[, , tt]) <- 1
    diag(R_t_bar[, , tt]) <- 1
  }
  
  ll <- rep(0, TT)
  
  
  for (tt in (K_c + 1):TT) {
    Q_t[, , tt] <- (1 - a - b) * R_t_bar[, , tt] + a * res[, , tt - 1] %*% t(res[, , tt - 1]) + b * Q_t[, , tt - 1]
    Q_t_star <- Inv(sqrt(diag(diag(Q_t[, , tt]))))
    R_t[, , tt] <- Q_t_star %*% Q_t[, , tt] %*% Q_t_star
    
    log_det_R_t[tt] <- log(Det(R_t[, , tt]))
    R_t_solved[, , tt] <- Inv(R_t[, , tt])
    Eps_t_R_t_Eps_t[tt] <- rbind(res[, , tt]) %*% R_t_solved[, , tt] %*% cbind(res[, , tt])
  }
  
  ll <- -(log_det_R_t + Eps_t_R_t_Eps_t)
  return(ll) 
}


dccmidas_mat_est_modified <- function (param, res, Dt, lag_fun = "Beta", N_c, K_c, dcc_mv = NULL) 
{
  a <- param[1]
  b <- param[2]
  w1 <- ifelse(lag_fun == "Beta", 1, 0)
  w2 <- param[3]
  m <- param[4] #
  theta <- param[5] #
  theta_m <- param[6] #
  w1_m <- ifelse(lag_fun == "Beta", 1, 0) #
  w2_m <- param[7] #
  
  
  Num_col <- dim(res)[1] # Number of variables (rows in res).
  TT <- dim(res)[3] # Number of time steps
  
  
  # Initialize matrices
  
  X_t <- array(0, dim = c(Num_col, Num_col, TT))
  for (tt in 1:TT) {
    X_t[, , tt] <- matrix(dcc_mv[tt], nrow = Num_col, ncol = Num_col)
  } # matrix of macroeconimic variable in similary format with C_t
  
  
  C_t <- array(0, dim = c(Num_col, Num_col, TT))  # Normalized covariance matrix
  V_t <- array(0, dim = c(Num_col, Num_col, TT))  # Rolling variance matrix
  
  Prod_eps_t <- array(0, dim = c(Num_col, Num_col, TT))  # Product of residuals
  Q_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))  # Dynamic covariance matrix
  R_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))  # Dynamic correlation matrix
  
  log_det_R_t <- rep(0, TT)  # Log determinants
  R_t_solved <- array(1, dim = c(Num_col, Num_col, TT))  # Inverted correlation matrices
  Eps_t_cross_prod <- rep(0, TT)  # Residual cross-products
  Eps_t_R_t_Eps_t <- rep(0, TT)  # Quadratic form of residuals
  S <- stats::cov(t(apply(res, 3L, c)))  # Static covariance matrix
  H_t <- array(S, dim = c(Num_col, Num_col, TT))  # Conditional covariance matrix
  
  # Compute rolling covariance and normalize
  for (tt in (N_c + 1):TT) {
    V_t[, , tt] <- rowSums(res[, 1, tt:(tt - N_c)] * (res[, 1, tt:(tt - N_c)])) * diag(Num_col)
    Prod_eps_t[, , tt] <- res[, , tt:(tt - N_c)] %*% t(res[, , tt:(tt - N_c)])
    V_t_0.5 <- Inv(sqrt(V_t[, , tt]))
    C_t[, , tt] <- V_t_0.5 %*% Prod_eps_t[, , tt] %*% V_t_0.5
  }
  
  # Determine lag weighting function
  weight_fun <- ifelse(lag_fun == "Beta", beta_function, exp_almon)
  betas <- c(rev(weight_fun(1:(K_c + 1), (K_c + 1), w1, w2))[2:(K_c + 1)], 0)
  weight_fun_m <- ifelse(lag_fun == "Beta", rumidas::beta_function, rumidas::exp_almon) # new
  betas_m <- c(rev(weight_fun_m(1:(K_c + 1), (K_c + 1), w1_m, w2_m))[2:(K_c +  1)], 0) # new
  
  # Smooth correlation matrix using weighted sums
  Z_t_bar <- array(1, dim = c(Num_col, Num_col, TT)) # New
  R_t_bar <- array(1, dim = c(Num_col, Num_col, TT))
  matrix_id <- matrix(1:Num_col^2, ncol = Num_col)
  matrix_id_2 <- which(matrix_id == 1:Num_col^2, arr.ind = TRUE)
  
  
  for (i in 1:nrow(matrix_id_2)) {
    C_component <- suppressWarnings(roll::roll_sum(C_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas)) 
    X_component <- suppressWarnings(roll::roll_sum(X_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas_m))
    Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- m + theta*C_component + theta_m*log(X_component)
    R_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) - 1) / 
      (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) + 1)
  }
  
  Z_t_bar[, , 1:K_c] <- diag(rep(1, Num_col))  # This is different with in dccmidas_ll
  R_t_bar[, , 1:K_c] <- diag(rep(1, Num_col))  # This is different with in dccmidas_ll
  
  for (tt in 1:dim(Z_t_bar)[3]) {  
    diag(Z_t_bar[, , tt]) <- 1
    diag(R_t_bar[, , tt]) <- 1
  }
  
  # Update dynamic covariance and correlation matrices
  for (tt in (K_c + 1):TT) {
    Q_t[, , tt] <- (1 - a - b) * R_t_bar[, , tt] + a * res[, , tt - 1] %*% t(res[, , tt - 1]) + b * Q_t[, , tt - 1]
    Q_t_star <- Inv(sqrt(diag(diag(Q_t[, , tt]))))
    R_t[, , tt] <- Q_t_star %*% Q_t[, , tt] %*% Q_t_star  # Normalize to get correlation matrix
    H_t[, , tt] <- Dt[, , tt] %*% R_t[, , tt] %*% Dt[, , tt]  # Compute conditional covariance
  }
  
  # Return results
  results <- list(H_t = H_t, R_t = R_t, R_t_bar = R_t_bar, C_t = C_t, Z_t = Z_t_bar, X_t = X_t, Q_t = Q_t, D_t = D_t, res = res, C_component = C_component, X_component = X_component)
  return(results)
}



DCC_cal <- function (param, res, Dt, lag_fun = "Beta", N_c, K_c, dcc_mv = NULL) 
{
  a <- param[1]
  b <- param[2]
  w1 <- ifelse(lag_fun == "Beta", 1, 0)
  w2 <- param[3]
  m <- param[4] #
  theta <- param[5] #
  theta_m <- param[6] #
  w1_m <- ifelse(lag_fun == "Beta", 1, 0) #
  w2_m <- param[7] #
  
  
  Num_col <- dim(res)[1] # Number of variables (rows in res).
  TT <- dim(res)[3] # Number of time steps
  
  
  # Initialize matrices
  
  X_t <- array(0, dim = c(Num_col, Num_col, TT))
  for (tt in 1:TT) {
    X_t[, , tt] <- matrix(dcc_mv[tt], nrow = Num_col, ncol = Num_col)
  } # matrix of macroeconimic variable in similary format with C_t
  
  
  C_t <- array(0, dim = c(Num_col, Num_col, TT))  # Normalized covariance matrix
  V_t <- array(0, dim = c(Num_col, Num_col, TT))  # Rolling variance matrix
  
  Prod_eps_t <- array(0, dim = c(Num_col, Num_col, TT))  # Product of residuals
  Q_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))  # Dynamic covariance matrix
  R_t <- array(diag(rep(1, Num_col)), dim = c(Num_col, Num_col, TT))  # Dynamic correlation matrix
  
  log_det_R_t <- rep(0, TT)  # Log determinants
  R_t_solved <- array(1, dim = c(Num_col, Num_col, TT))  # Inverted correlation matrices
  Eps_t_cross_prod <- rep(0, TT)  # Residual cross-products
  Eps_t_R_t_Eps_t <- rep(0, TT)  # Quadratic form of residuals
  S <- stats::cov(t(apply(res, 3L, c)))  # Static covariance matrix
  H_t <- array(S, dim = c(Num_col, Num_col, TT))  # Conditional covariance matrix
  
  # Compute rolling covariance and normalize
  for (tt in (N_c + 1):TT) {
    V_t[, , tt] <- rowSums(res[, 1, tt:(tt - N_c)] * (res[, 1, tt:(tt - N_c)])) * diag(Num_col)
    Prod_eps_t[, , tt] <- res[, , tt:(tt - N_c)] %*% t(res[, , tt:(tt - N_c)])
    V_t_0.5 <- Inv(sqrt(V_t[, , tt]))
    C_t[, , tt] <- V_t_0.5 %*% Prod_eps_t[, , tt] %*% V_t_0.5
  }
  
  # Determine lag weighting function
  weight_fun <- ifelse(lag_fun == "Beta", beta_function, exp_almon)
  betas <- c(rev(weight_fun(1:(K_c + 1), (K_c + 1), w1, w2))[2:(K_c + 1)], 0)
  weight_fun_m <- ifelse(lag_fun == "Beta", rumidas::beta_function, rumidas::exp_almon) # new
  betas_m <- c(rev(weight_fun_m(1:(K_c + 1), (K_c + 1), w1_m, w2_m))[2:(K_c +  1)], 0) # new
  
  # Smooth correlation matrix using weighted sums
  Z_t_bar <- array(1, dim = c(Num_col, Num_col, TT)) # New
  R_t_bar <- array(1, dim = c(Num_col, Num_col, TT))
  matrix_id <- matrix(1:Num_col^2, ncol = Num_col)
  matrix_id_2 <- which(matrix_id == 1:Num_col^2, arr.ind = TRUE)
  
  
  for (i in 1:nrow(matrix_id_2)) {
    C_component <- suppressWarnings(roll::roll_sum(C_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas)) 
    X_component <- suppressWarnings(roll::roll_sum(X_t[matrix_id_2[i, 1], matrix_id_2[i, 2], ], c(K_c + 1), weights = betas_m))
    Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- m + theta*C_component + theta_m*log(X_component)
    R_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ] <- (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) - 1) / 
      (exp(2 * Z_t_bar[matrix_id_2[i, 1], matrix_id_2[i, 2], ]) + 1)
  }
  
  Z_t_bar[, , 1:K_c] <- diag(rep(1, Num_col))  # This is different with in dccmidas_ll
  R_t_bar[, , 1:K_c] <- diag(rep(1, Num_col))  # This is different with in dccmidas_ll
  
  for (tt in 1:dim(Z_t_bar)[3]) {  
    diag(Z_t_bar[, , tt]) <- 1
    diag(R_t_bar[, , tt]) <- 1
  }
  
  # Update dynamic covariance and correlation matrices
  for (tt in (K_c + 1):TT) {
    Q_t[, , tt] <- (1 - a - b) * R_t_bar[, , tt] + a * res[, , tt - 1] %*% t(res[, , tt - 1]) + b * Q_t[, , tt - 1]
    Q_t_star <- Inv(sqrt(diag(diag(Q_t[, , tt]))))
    R_t[, , tt] <- Q_t_star %*% Q_t[, , tt] %*% Q_t_star  # Normalize to get correlation matrix
    H_t[, , tt] <- Dt[, , tt] %*% R_t[, , tt] %*% Dt[, , tt]  # Compute conditional covariance
  }
  
  # Return results
  results <- list(H_t = H_t, R_t = R_t, R_t_bar = R_t_bar, C_t = C_t, Z_t = Z_t_bar, X_t = X_t, Q_t = Q_t, D_t = Dt, res = res, C_component = C_component, X_component = X_component)
  return(results)
}


dcc_fit_modify <- function (r_t,
                            univ_model = "GM_noskew", distribution = "norm", 
                            MV = NULL, MV_M = NULL, dcc_mv = NULL, K = NULL, 
                            corr_model = "DCCMIDAS", lag_fun = "Beta", 
                            N_c = NULL, K_c = NULL, out_of_sample = NULL,
                            dummy = NULL) 
  # add MV_M which should be a list of 2 GEPU xts, MV should be a list of 2 RV of 2 assets
  # here we using MV_M for both garchmidas and dccmidas, the MV_M in DCC should have similar format to C_t, should re-check
  
{
  cond_r_t <- class(r_t)    # Check if r_t is list of assets returns series
  if (cond_r_t != "list") {
    stop(cat("#Warning:\n Parameter 'r_t' must be a list object. Please provide it in the correct form \n"))
  }
  if ((univ_model %in% c("GM_noskew", "GM_skew", "DAGM_noskew", "DAGM_skew")) & (!inherits(MV, "list") | length(MV) != length(r_t) | is.null(K))) {
    stop(cat("#Warning:\n If you want to estimate a GARCH-MIDAS model, please provide parameters MV and K in the correct form \n"))
  }
  if ((corr_model == "DCCMIDAS" | corr_model == "ADCCMIDAS") & 
      (is.null(N_c) | is.null(K_c))) {
    stop(cat("#Warning:\n If you want to estimate a DCC-MIDAS model, please provide parameters N_c and K_c \n"))
  }
  
  # ============================================================================
  Num_assets <- length(r_t)
  len <- rep(NA, Num_assets)
  
  for (i in 1:Num_assets) {
    len[i] <- length(r_t[[i]])
  }
  if (stats::var(len) != 0) {
    db <- do.call(xts::merge.xts, r_t)
    db <- db[stats::complete.cases(db), ]
  }
  else {
    db <- do.call(xts::merge.xts, r_t)
  }
  for (i in 1:Num_assets) {
    colnames(db)[i] <- colnames(r_t[[i]])
  }
  TT <- nrow(db)
  if (missing(out_of_sample)) {
    sd_m <- matrix(NA, ncol = Num_assets, nrow = TT)
    sd_m_lr <- matrix(NA, ncol = Num_assets, nrow = TT) # new
    estim_period <- range(stats::time(db))
    days_period <- stats::time(db)
    est_obs <- nrow(db)
  }
  else {
    sd_m <- matrix(NA, ncol = Num_assets, nrow = TT - out_of_sample)
    db_oos <- db[(TT - out_of_sample + 1):TT, ]
    sd_m_oos <- matrix(NA, ncol = Num_assets, nrow = out_of_sample)
    estim_period <- range(stats::time(db[1:(TT - out_of_sample), 
    ]))
    days_period <- stats::time(db[1:(TT - out_of_sample), 
    ])
    est_obs <- nrow(db[1:(TT - out_of_sample), ])
  }
  
  est_details <- list()
  likelihood_at_max <- list()
  start <- Sys.time()
  
  
  #---------------------------------------------------
  
  garch_logL <- list()
  
  if (univ_model == "GM_noskew") {
    if (missing(out_of_sample)) {
      for (i in 1:Num_assets) {
        u_est <- ugmfit_ver1(model = "GM", skew = "NO", 
                             distribution = "norm", db[, i], mv_m = MV[[i]], mv_m_2 = MV_M[[i]],
                             K = K, lag_fun = lag_fun,
                             dummy = dummy)   # add tge MV_M[[i]]
        
        est_details[[i]] <- u_est$rob_coef_mat   # The robust coefficient matrix (rob_coef_mat) for asset
        likelihood_at_max[[i]] <- u_est$loglik   # The log-likelihood value at the estimated parameters' maximum
        sd_m[, i] <- u_est$est_vol_in_s          # Save the in-sample estimated volatilities in sd_m[, i].
        sd_m_lr[, i] <- u_est$est_lr_in_s
        garch_logL[[i]] <- u_est$inf_criteria
      }
    }
    else {
      for (i in 1:Num_assets) {
        u_est <- rumidas::ugmfit(model = "GM", skew = "NO", 
                                 distribution = distribution, db[, i], MV[[i]], 
                                 K = K, lag_fun = lag_fun, out_of_sample = out_of_sample)
        est_details[[i]] <- u_est$rob_coef_mat
        likelihood_at_max[[i]] <- u_est$loglik
        sd_m[, i] <- u_est$est_vol_in_s
        sd_m_oos[, i] <- zoo::coredata(u_est$est_vol_oos)
      }
    }
  }
  
  #=============================================================================
  
  
  
  cat("First step: completed \n")
  if (missing(out_of_sample)) {
    D_t <- array(0, dim = c(Num_assets, Num_assets, TT))
    eps_t <- array(0, dim = c(Num_assets, 1, TT))
    db_a <- array(NA, dim = c(Num_assets, 1, TT))
    db_no_xts <- zoo::coredata(db)
    for (tt in 1:TT) {
      db_a[, , tt] <- t(db_no_xts[tt, ])
      diag(D_t[, , tt]) <- sd_m[tt, ]
      eps_t[, , tt] <- Inv(D_t[, , tt]) %*% db_a[, , tt]
    }
  }
  else {
    D_t <- array(0, dim = c(Num_assets, Num_assets, TT - 
                              out_of_sample))
    eps_t <- array(0, dim = c(Num_assets, 1, TT - out_of_sample))
    db_a <- array(NA, dim = c(Num_assets, 1, TT - out_of_sample))
    db_no_xts <- zoo::coredata(db[1:(TT - out_of_sample), 
    ])
    for (tt in 1:(TT - out_of_sample)) {
      db_a[, , tt] <- t(db_no_xts[tt, ])
      diag(D_t[, , tt]) <- sd_m[tt, ]
      eps_t[, , tt] <- Inv(D_t[, , tt]) %*% db_a[, , tt]
    }
    D_t_oos <- array(0, dim = c(Num_assets, Num_assets, out_of_sample))
    eps_t_oos <- array(0, dim = c(Num_assets, 1, out_of_sample))
    db_a_oos <- array(NA, dim = c(Num_assets, 1, out_of_sample))
    db_no_xts_oos <- zoo::coredata(db_oos)
    for (tt in 1:out_of_sample) {
      db_a_oos[, , tt] <- t(db_no_xts_oos[tt, ])
      diag(D_t_oos[, , tt]) <- sd_m_oos[tt, ]
      eps_t_oos[, , tt] <- Inv(D_t_oos[, , tt]) %*% db_a_oos[, 
                                                             , tt]
    }
  }
  
  #---------------------------------------------------
  
  if (corr_model == "DCCMIDAS" & lag_fun == "Beta") {
    R = 1000
    start_val <- begin_val <- ui <- ci <- NULL
    begin_val <- matrix(NA, nrow = R, ncol = 7)
    colnames(begin_val) <- c("alpha", "beta", "w2", "m", "theta", "theta_m", "w2_m") 
    begin_val[, 1] <- stats::runif(R, min = 0.001, max = 0.095)
    begin_val[, 2] <- stats::runif(R, min = 0.3, max = 0.8)
    begin_val[, 3] <- 1.01
    begin_val[, 4] <- stats::runif(R, min = -5, max = 5)
    begin_val[, 5] <- stats::runif(R, min = -1, max = 1)
    begin_val[, 6] <- stats::runif(R, min = -1, max = 1) 
    begin_val[, 7] <- 1.01 
    
    which_row <- rep(NA, R)
    for (i in 1:R) {
      which_row[i] <- sum(dccmidas_ll(begin_val[i, ], res = eps_t, lag_fun = "Beta", N_c = N_c, K_c = K_c, dcc_mv = dcc_mv))
    }
    start_val <- begin_val[which.max(which_row), ]
    
    ui <- rbind(c(1, 0, 0, 0, 0, 0, 0),
                c(0, 1, 0, 0, 0, 0, 0),
                c(-1, -1, 0, 0, 0, 0, 0),
                c(0, 0, 1, 0, 0, 0, 0),
                c(0, 0, 0, 0, 0, 0, 1)
    )
    ci <- c(-1e-04, -0.001, 0.999, -1.001, -1.001) # modify the constraint
    
    m_est <- maxLik(
      logLik = dccmidas_ll, # replace dccmidas_loglik with dccmidas_ll  
      start = start_val, res = eps_t, 
      lag_fun = lag_fun, N_c = N_c, K_c = K_c, dcc_mv = dcc_mv,  # add dcc_mv
      constraints = list(ineqA = ui, ineqB = ci), 
      iterlim = 5000, 
      method = "BFGS"
    )
  }
  
  #=============================================================================
  
  
  est_coef <- stats::coef(m_est)
  N_coef <- length(est_coef)
  mat_coef <- data.frame(rep(NA, N_coef), rep(NA, N_coef), 
                         rep(NA, N_coef), rep(NA, N_coef))
  colnames(mat_coef) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(mat_coef) <- names(est_coef)
  mat_coef[, 1] <- round(est_coef, 6)
  mat_coef[, 2] <- round(QMLE_sd(m_est), 6)
  mat_coef[, 3] <- round(est_coef/QMLE_sd(m_est), 6)
  mat_coef[, 4] <- round(apply(rbind(est_coef/QMLE_sd(m_est)), 1, function(x) 2 * (1 - stats::pnorm(abs(x)))), 6)
  
  
  
  #=============================================================================
  
  if (corr_model == "DCCMIDAS") {
    dcc_mat_est_fin <- dccmidas_mat_est_modified(est_coef, eps_t, D_t, lag_fun = lag_fun, N_c = N_c, K_c = K_c, dcc_mv = dcc_mv) # add dcc_mv
    if (!missing(out_of_sample)) {
      dcc_mat_est_fin_oos <- dccmidas_mat_est(est_coef, eps_t_oos, D_t_oos, lag_fun = lag_fun, N_c = N_c, K_c = K_c, dcc_mv = dcc_mv) # add dcc_mv
    }
    else {
      dcc_mat_est_fin_oos <- list(NA, NA, NA)
    }
  }
  
  #=============================================================================
  
  end <- Sys.time() - start
  if (corr_model == "DCCMIDAS") {
    fin_res <- list(assets = colnames(db), model = univ_model, 
                    est_univ_model = est_details, corr_coef_mat = mat_coef, garch_midas = est_details,
                    mult_model = corr_model, obs = est_obs, period = estim_period, 
                    H_t = dcc_mat_est_fin[[1]], R_t = dcc_mat_est_fin[[2]], R_t_bar = dcc_mat_est_fin[[3]], 
                    C_t = dcc_mat_est_fin[[4]], Z_t = dcc_mat_est_fin[[5]], X_t = dcc_mat_est_fin[[6]], Q_t = dcc_mat_est_fin[[7]], 
                    D_t = dcc_mat_est_fin[[8]], res = dcc_mat_est_fin[[9]], C_component = dcc_mat_est_fin[[10]], X_component = dcc_mat_est_fin[[11]],
                    garch_midas_est_vol = sd_m, garch_midas_est_lvol = sd_m_lr, resid = db_a, garch_logL = garch_logL, likelihood_at_max <- likelihood_at_max,
                    H_t_oos = dcc_mat_est_fin_oos[[1]], R_t_oos = dcc_mat_est_fin_oos[[2]], R_t_bar_oos = dcc_mat_est_fin_oos[[3]], 
                    est_time = end, Days = days_period, llk = stats::logLik(m_est))
  }
  
  class(fin_res) <- c("dccmidas")
  return(fin_res)
  print.dccmidas(fin_res)
}





# 4. FORMATING THE RESULT FUNCTION ================================================================================================================================================


to_df_multiple <- function(...) {
  # Capture argument names
  matrix_names <- as.character(match.call())[-1]  
  matrices <- list(...)
  
  df_list <- mapply(function(mat, mat_name) {
    dims <- dim(mat)  
    
    if (is.null(dims)) {
      # Handle vectors
      return(data.frame(setNames(list(mat), mat_name)))
    } 
    
    else if (length(dims) == 2) {
      # Handle 2D matrix
      temp_list <- setNames(as.data.frame(mat), paste0(mat_name, "_", seq_len(ncol(mat))))
      return(temp_list)
    } 
    
    else if (length(dims) == 3) {
      # Handle 3D matrix
      temp_list <- list()
      for (i in seq_len(dims[1])) {
        for (j in seq_len(dims[2])) {
          col_name <- paste0(mat_name, "_", i, "_", j)
          temp_list[[col_name]] <- mat[i, j, ]  
        }
      }
      return(as.data.frame(temp_list))
    } 
    
    else {
      stop("Error: Only vectors, 2D, and 3D matrices are supported.")
    }
  }, matrices, matrix_names, SIMPLIFY = FALSE)
  
  return(do.call(cbind, df_list))
}
set_date_index <- function(df, start_date=start_date, end_date=end_date) {
  # Load necessary libraries
  library(dplyr)
  library(tibble)
  
  # Convert input dates to Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Create a date sequence
  date_sequence <- seq(start_date, end_date, by = "day")
  
  # Check if the number of rows in df matches the length of the date sequence
  if (nrow(df) != length(date_sequence)) {
    stop("Error: The number of rows in the data frame does not match the length of the date sequence.")
  }
  
  # Assign date sequence to the data frame
  df$Date <- date_sequence
  
  # Convert Date column to row names
  df <- df %>% column_to_rownames(var = "Date")
  
  return(df)
}


# 5. LOAD DATA FUCNTION ================================================================================================================================================

load_data <- function(start_date, end_date) {
  # 4.1 Load Daily frequency data
  data <- read_excel("C:/Users/ASUS/Desktop/Deal 2/Data_.xlsx")
  dates <- as.Date(data$Date)
  
  log_ret_btc <- data$Ln_ret_BTC
  log_ret_brt <- data$Ln_ret_BRT
  log_ret_btc_xts <- xts(log_ret_btc, order.by = dates)
  log_ret_brt_xts <- xts(log_ret_brt, order.by = dates)
  
  db_m <- merge.xts(log_ret_btc_xts, log_ret_brt_xts)
  db_m_filtered <- window(db_m, start = start_date, end = end_date)
  
  # 4.2 Load Monthly frequency data
  data_m <- read_excel("C:/Users/ASUS/Desktop/Deal 2/Data_M_.xlsx")
  dates_m <- as.Date(data_m$Date)
  
  GEPU <- data_m$GEPU
  GEPU_xts <- xts(GEPU, order.by = dates_m)
  RV_BTC <- data_m$RV_BTC
  RV_BTC_xts <- xts(RV_BTC, order.by = dates_m)
  RV_BRT <- data_m$RV_BRT
  RV_BRT_xts <- xts(RV_BRT, order.by = dates_m)
  
  # 4.3 Transform data & input into proper format
  r_t <- list(db_m_filtered[,1], db_m_filtered[,2])
  mv_m_btc <- mv_into_mat(r_t[[1]], RV_BTC_xts, K = 12, "monthly")
  mv_m_brt <- mv_into_mat(r_t[[2]], RV_BRT_xts, K = 12, "monthly")
  mv_m_m <- mv_into_mat(r_t[[1]], GEPU_xts, K = 12, "monthly")
  
  MV <- list(mv_m_btc, mv_m_brt)
  MV_M <- list(mv_m_m, mv_m_m)
  mv_x <- mv_into_mat(r_t[[1]], GEPU_xts, K = 0, "monthly")
  
  # Return results as a list
  return(list(
    r_t = r_t,
    MV = MV,
    MV_M = MV_M,
    mv_x = mv_x,
    X = GEPU,
    X_1 = GEPU_xts
  ))
}



# 6. HEDGING EVALUATION FUNCTION =====================================================================================================================================================

hedging_performance_analysis <- function(df) {
  if (!exists("df")) {
    stop("Error: The dataframe does not exist.")
  }
  
  # Step 2: Compute hedge ratios series
  df$hedge_ratio_2 <- df$H_t_1_2 / df$H_t_1_1
  df$hedge_ratio_1 <- df$H_t_1_2 / df$H_t_2_2
  
  # Step 3: Compute hedged returns series
  df$ret_hedged_2 <- df$resid_2_1 - (df$hedge_ratio_2 * df$resid_1_1)
  df$ret_hedged_1 <- df$resid_1_1 - (df$hedge_ratio_1 * df$resid_2_1)
  
  # Step 4: Compute variance and holding period return (HPR)
  compute_stats <- function(column_name) {
    var_value <- var(df[[column_name]], na.rm = TRUE)  # Variance
    hpr_value <- sum(df[[column_name]], na.rm = TRUE)  # Holding period return
    return(c(Variance = var_value, Return = hpr_value))
  }
  
  stats_resid_1_1 <- compute_stats("resid_1_1")
  stats_ret_hedged_1 <- compute_stats("ret_hedged_1")
  stats_resid_2_1 <- compute_stats("resid_2_1")
  stats_ret_hedged_2 <- compute_stats("ret_hedged_2")
  
  # Step 5: Create a dataframe for comparison
  comparison_df <- data.frame(
    Metric = c("Variance", "Return"),
    Resid_1_1 = stats_resid_1_1,
    Ret_Hedged_1 = stats_ret_hedged_1,
    Resid_2_1 = stats_resid_2_1,
    Ret_Hedged_2 = stats_ret_hedged_2
  )
  
  # Step 6: Hedging performance evaluation
  # Extract variance values
  var_resid_1_1 <- as.numeric(comparison_df[comparison_df$Metric == "Variance", "Resid_1_1"])
  var_ret_hedged_1 <- as.numeric(comparison_df[comparison_df$Metric == "Variance", "Ret_Hedged_1"])
  var_resid_2_1 <- as.numeric(comparison_df[comparison_df$Metric == "Variance", "Resid_2_1"])
  var_ret_hedged_2 <- as.numeric(comparison_df[comparison_df$Metric == "Variance", "Ret_Hedged_2"])
  
  # Extract HPR values
  hpr_resid_1_1 <- as.numeric(comparison_df[comparison_df$Metric == "Return", "Resid_1_1"])
  hpr_ret_hedged_1 <- as.numeric(comparison_df[comparison_df$Metric == "Return", "Ret_Hedged_1"])
  hpr_resid_2_1 <- as.numeric(comparison_df[comparison_df$Metric == "Return", "Resid_2_1"])
  hpr_ret_hedged_2 <- as.numeric(comparison_df[comparison_df$Metric == "Return", "Ret_Hedged_2"])
  
  # Compute Hedging Effectiveness (HE)
  HE_1 <- (var_resid_1_1 - var_ret_hedged_1) / var_resid_1_1
  HE_2 <- (var_resid_2_1 - var_ret_hedged_2) / var_resid_2_1
  
  # Compute Sharpe Ratios
  sharpe_resid_1_1 <- hpr_resid_1_1 / sqrt(var_resid_1_1)
  sharpe_ret_hedged_1 <- hpr_ret_hedged_1 / sqrt(var_ret_hedged_1)
  sharpe_resid_2_1 <- hpr_resid_2_1 / sqrt(var_resid_2_1)
  sharpe_ret_hedged_2 <- hpr_ret_hedged_2 / sqrt(var_ret_hedged_2)
  
  # Create a dataframe for Sharpe Ratio comparison
  sharpe_comparison_df <- data.frame(
    Metric = c("Sharpe Ratio"),
    Resid_1_1 = sharpe_resid_1_1,
    Ret_Hedged_1 = sharpe_ret_hedged_1,
    Resid_2_1 = sharpe_resid_2_1,
    Ret_Hedged_2 = sharpe_ret_hedged_2
  )
  
  # Step 7: Return the results
  return(list(
    Comparison_Stats = comparison_df,
    Hedging_Effectiveness = c(HE_1 = HE_1, HE_2 = HE_2),
    Sharpe_Comparison = sharpe_comparison_df
  ))
}









# B. =========================================================================== ---- IN SAMPLE MODEL ESTIMATION --- ===================================================================== 

# 0. LOAD DATA  ==================================================================================================================================================================

start_date_is <- as.Date("2016-01-01")
end_date_is <- as.Date("2024-09-30")

load_data_is <- load_data(start_date = start_date_is, end_date = end_date_is)

r_t <- load_data_is[[1]]
MV <- load_data_is[[2]]
MV_M <- load_data_is[[3]]
mv_x  <- load_data_is[[4]]
macro <- load_data_is[[5]]
macro_xts <- load_data_is[[6]]

# 1. STRUCTURE BREAK TEST ==================================================================================================================================================================

# Structure break test to ouput breakpoints
break_test <- bai_perron_test(series = macro_xts, max_breaks = 5, start_date = as.Date("2016-01-01"), end_date = as.Date("2024-09-01")) 
breakpoints <- break_test[[1]]

dummy_list <- create_dummy_variables(breakpoints = breakpoints, series = macro_xts)

# Create list of dummy variables in format ready for calculation
dummy_input <- list()
for (i in seq_along(dummy_list)) {
  D_month <- dummy_list[[i]]  # Extract the i-th monthly data
  D_daily <- convert_monthly_to_daily(D_month)  # Convert to daily
  D_final <- add_xts_matrix(D_daily, dim(MV[[1]])[1], dim(MV[[1]])[2], start_date_is, end_date_is)  # Apply transformation
  
  dummy_input[[i]] <- D_final
}

# 2. RUN THE MODEL ================================================================================================================================================



set.seed(111)
mean_est_1 <- mean_fit(r_t[[1]])
mean_est_2 <- mean_fit(r_t[[2]])
r_t_1 <- mean_est_1$residuals
r_t_2 <- mean_est_2$residuals
r_t_ <- list(r_t_1, r_t_2)

dccmidas_est <- dcc_fit_modify(
  r_t = r_t_,
  univ_model = "GM_noskew",       # Univariate GARCH-MIDAS model
  distribution = "norm",          # Normal distribution for errors
  MV = MV,                        # MIDAS variables
  MV_M = MV_M,
  K = 12,                          # Number of MIDAS lags
  dcc_mv = mv_x,
  corr_model = "DCCMIDAS",        # DCC-MIDAS correlation model
  N_c = 36,                      # Standardized residuals lags
  K_c = 288,                       # Long-run correlation lags
  dummy = dummy_input
)

dcc_output <- dccmidas_est$corr_coef_mat
garch_output <- dccmidas_est$garch_midas

H_t <- dccmidas_est$H_t
R_t <- dccmidas_est$R_t
R_t_bar <- dccmidas_est$R_t_bar
C_t <- dccmidas_est$C_t
Z_t <- dccmidas_est$Z_t
X_t <- dccmidas_est$X_t
Q_t <- dccmidas_est$Q_t
D_t <- dccmidas_est$D_t
res <- dccmidas_est$res
C_component <- dccmidas_est$C_component
X_component <- dccmidas_est$X_component
resid <- dccmidas_est$resid

garch_midas_est_vol <- dccmidas_est$garch_midas_est_vol
garch_midas_est_lvol <- dccmidas_est$garch_midas_est_lvol 


GARCH_inf_cri <- dccmidas_est$garch_logL
GARCH_llk <- dccmidas_est$likelihood_at_max

DCC_llk <- dccmidas_est$llk
dcc_AIC <- -2*DCC_llk[[1]] + 2*dim(dcc_output)[[1]]
dcc_BIC <- -2*DCC_llk[[1]] + dim(dcc_output)[[1]] * log(length(dccmidas_est$Days))





# 3. OUTPUT RESULT ================================================================================================================================================

df_ <- to_df_multiple(H_t, R_t, R_t_bar, D_t, res, X_t, Q_t, C_t, Z_t, resid, garch_midas_est_vol, garch_midas_est_lvol, C_component, X_component)
df <- set_date_index(df_, start_date_is, end_date_is)

print(garch_output)
print(GARCH_inf_cri)
print(GARCH_llk)

print(dcc_output)
print(DCC_llk)
print(dcc_AIC)
print(dcc_BIC)



View(df)




# 4. HEDGING ANALYSIS ================================================================================================================================================

results <- hedging_performance_analysis(df_)
print(results)








# C. =========================================================================== ---- OUT SAMPLE HEDGING ANALYSIS --- ===================================================================== 

# 1. LOAD OUT-SAMPLE DATA =======================================================================================================================================

start_date_os <- as.Date("2016-01-01")
end_date_os <- as.Date("2024-09-30")

load_data_os <- load_data(start_date = start_date_os, end_date = end_date_os)

r_t_os <- load_data_os[[1]]
MV_os <- load_data_os[[2]]
MV_M_os <- load_data_os[[3]]
mv_x_os  <- load_data_os[[4]]


# 2. PREDICTION =======================================================================================================================================

# GARCH_MIDAS estimation. 

db_os <- do.call(xts::merge.xts, r_t_os)
TT_os <- nrow(db_os)
sd_m_os <- matrix(NA, ncol = 2, nrow = TT_os)

for (i in 1:2) {
  garch_coef_est_os <- garch_output[[i]][, "Estimate"]
  garch_cal_os <- GM_conditional_vol(param = garch_coef_est_os, 
                                     daily_ret = db_os[, i], 
                                     mv_m = MV_os[[i]], 
                                     mv_m_m = MV_M_os[[i]], 
                                     K = 12, 
                                     lag_fun = "Beta") 
  
  sd_m_os[, i] <- garch_cal_os         
}

D_t_os <- array(0, dim = c(2, 2, TT_os))
eps_t_os <- array(0, dim = c(2, 1, TT_os))
db_a_os <- array(NA, dim = c(2, 1, TT_os))
db_no_xts_os <- zoo::coredata(db_os)
for (tt in 1:TT_os) {
  db_a_os[, , tt] <- t(db_no_xts_os[tt, ])
  diag(D_t_os[, , tt]) <- sd_m_os[tt, ]
  eps_t_os[, , tt] <- Inv(D_t_os[, , tt]) %*% db_a_os[, , tt]
}


# DCC_MIDAS estimation.

dcc_coef_est_os <- dcc_output[, "Estimate"]

dcc_cal_os <- DCC_cal(param = dcc_coef_est_os,
                      res = eps_t_os, # take from the garch_midas estimation
                      Dt = D_t_os,    # take from the garch_midas estimation
                      lag_fun = "Beta",
                      N_c = 36,
                      K_c = 288,
                      dcc_mv = mv_x_os) 

H_t_os <- dcc_cal_os[[1]]  
R_t_os <- dcc_cal_os[[2]]  
R_t_bar_os <- dcc_cal_os[[3]]  
C_t_os <- dcc_cal_os[[4]]  
Z_t_os <- dcc_cal_os[[5]]  
X_t_os <- dcc_cal_os[[6]]  
Q_t_os <- dcc_cal_os[[7]]  
D_t_os <- dcc_cal_os[[8]]  
res_os <- dcc_cal_os[[9]]  
C_component_os <- dcc_cal_os[[10]]  
X_component_os <- dcc_cal_os[[11]]  
resid_os <- db_a_os


# 3. OUTPUT RESULT =======================================================================================================================================================================

df_os_ <- to_df_multiple(H_t_os, R_t_os, R_t_bar_os, D_t_os, res_os, X_t_os, Q_t_os, C_t_os, Z_t_os, resid_os, C_component_os, X_component_os)
df_os <- set_date_index(df_os_, start_date_os, end_date_os)

rename_columns_remove_os <- function(df) {
  # Remove "_os" from all column names
  colnames(df) <- gsub("_os", "", colnames(df))
  return(df)
}
df_os <- rename_columns_remove_os(df_os)
View(df_os)


# 4. HEDING ANALYSIS =================================================================================================================================================================

results_os <- hedging_performance_analysis(df_os)
print(results_os)







