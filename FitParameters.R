####################
# FitParameters.R
####################
# Description:
# * Fit stochastic differential equation parameters
# using regression, mle on observed data.

library("rlang");
library("NlcOptim");

mle_gbm = function(data) {
    # *******************************
    # Fit geometric brownian motion parameters to data using 
    # Maximum Likelihood Estimation.
    # *******************************
    # Inputs:
    # * data: vector of log returns.
    # * params: Starting point for parameter estimation.
    # Output:
    # * Return list containing "loglikelihood", "params".
    gbm_pdf = function(obs, params) dnorm(obs, params[1] * 1 / 252, params[2] * sqrt(1 / 252));
    targetFunc = function(x) - log_likelihood_sum(data, x, gbm_pdf);
    results = optim(c(.02, .3), targetFunc);
    maxLL = -targetFunc(results$par);
    return(list("loglikelihood" = maxLL, "params" = results$par));
}

mle_jump_diff = function(data, gbm_params = NULL)
{
    # Inputs:
    # * data: dataframe containing "prices" and "date".
    data = data[order(as.Date(row.names(data), format = "%m/%d/%Y")),, drop = FALSE];
    log_obs = log(data$prices);
    log_obs_0 = log_obs[1];
    log_obs = log_obs[-1];
    dt = 1 / 252;
    if (is.null(gbm_params))
    {
        # x: [mu, sigma, lambda, beta, eta].
        startParams = c(.02, .3, .5, .5, .2);
        lower_bound = c(-Inf,0,-Inf,-Inf,0);
        targetFunc = function(x) {
            x = append(x, c(dt, log_obs_0), length(x));
            return(-log_likelihood_sum(log_obs, x, jump_diff_pdf));
        }
        constraintFunc = function(mu, sigma, lambda, beta, eta) {

        }
    }
    else
    {
        # x: [lambda, beta, eta].
        startParams = c(.5, .5, .2);
        lower_bound = c(-1000, -1000, .000001);
        targetFunc = function(x)
        {
            x = prepend(x, c(gbm_params[1], gbm_params[2]));
            x = append(x, c(dt, log_obs_0), length(x));
            return(-log_likelihood_sum(log_obs, x, jump_diff_pdf));
        }
        constraintFunc = function(lambda, beta, eta, gbm_sigma = gbm_params[2])
        {
            return(eta + gbm_sigma * gbm_sigma / 25);
        }
    }
    #results = optim(startParams, targetFunc, lower=lower_bound, method='L-BFGS-B');
    #results = solnl(startParams, targetFunc, lb = lower_bound);
    results = solnl(startParams, targetFunc, lb = lower_bound);
    maxLL = -targetFunc(results$par);
    return(list("loglikelihood" = maxLL, "params" = results$par));
}

jump_diff_pdf = function(log_obs, params)
{
    # Inputs:
    # * log_obs: log of current asset (excluding first price).
    # * params: vector containing
    # [mu, sigma, lambda, beta, eta, dt, log_obs_0]. 
    # * dt: return's period.
    log_obs_0 = params[7];
    pi_sqrt_2 = sqrt(2 * pi);
    dt = params[6];
    mu = params[1] * dt;
    var_ = params[2] * params[2] * dt;
    lambda = params[3] * dt;
    beta = params[4];
    eta = params[5];
    n = 0;
    prob = 0;
    while (n < 20) {
        mix_sig = sqrt(n * eta + var_);
        norm_exp = -(log_obs - log_obs_0 - mu - n * beta) ^ 2 / (2 * mix_sig * mix_sig);
        prob = prob + exp(-lambda) * (lambda ^ n) / (pi_sqrt_2 * mix_sig * factorial(n)) * exp(norm_exp);
        n = n + 1;
    }
    return(prob);
}

log_likelihood_sum = function(data, params, pdf)
{
    ll = 0;
    for (obs in data)
    {
        ll = log(pdf(obs, params));
    }
    return(ll);
}

regression_fit_gbm = function(data)
{
    # *******************************
    # Fit geometric brownian motion parameters to data using 
    # OLS regression method.
    # *******************************
    # Inputs:
    # * data: DataFrame with "prices" as column containing observed prices and row names containing
    # observation date.

    # Ensure rows are sorted in ascending order by date:
    data = data[order(row.names(data)),, drop = FALSE]
    # Use two-pass OLS regression to determine gbm parameters:
    earliest_obs_date = min(row.names(data));
    log_x_0 = log(data[earliest_obs_date,c("prices")]);
    log_prices = log(data[-1,c("prices"),drop=FALSE]);
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
    return(list("params" = c(mu, sigma)));
}

regression_fit_jump = function(data, gbm_params)
{
    # *******************************
    # Fit jump diffusion model to data using regression.
    # *******************************
    # Inputs:
    # * data: Dataframe with "prices" as column and row.names as dates for observed
    # prices.
    # * gbm_params: List containing geometric brownian motion parameters
    # "mu" and "sigma".



    
}