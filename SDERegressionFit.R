####################
# SDERegressionFit.R
####################
# Description:
# * Fit stochastic differential equation parameters
# using regression on observed data.

regression_fit_gbm = function(data, risk_free)
{
    # Inputs:
    # * data: DataFrame with "prices" as column containing observed prices and row names containing
    # observation date.

    # Do first pass, assuming sigma = 0, to find mu:
    row.names(data) = strptime(row.names(data), format = '%m/%d/%Y');
    earliest_obs_date = min(row.names(data));
    log_x_0 = log(data[earliest_obs_date,]);
    log_prices = log(data[-1,, drop = FALSE]);
    time = numeric(nrow(log_prices));
    tIndex = 1;
    dts = row.names(log_prices);
    while (tIndex <= nrow(log_prices))
    {
        time[tIndex] = difftime(dts[tIndex], earliest_obs_date, units = "days") / 252;
        tIndex = tIndex + 1;
    }
    log_data = data.frame("t" = time, "log_price" = log_prices$prices);
    # Infer mu * t:
    # (Potentially use Ridge/Lasso)
    gbm_reg = lm(log_price - log_x_0 ~ time, log_data);
    mu = gbm_reg$coefficients[2];
    # Infer sigma using above mu value, using :
    sigmas = numeric(nrow(log_data));
    index = 1;
    while (index <= length(log_data$log_price))
    {
        z_t = log_data$log_price[index] - log_x_0;
        t = log_data$t[index];
        sigmas[index] = -.5 + sqrt(t - 2 * t * (z_t - mu * t)) / (2 * sqrt(t));
        index = index + 1;
    }
    sigma = max(sigmas);
    return(list("mu" = mu, "sigma" = sigma));
}

regression_fit_jump = function(data, gbm_mu, gbm_sigma)
{
    # Fit jump diffusion model to data using regression.

    
}