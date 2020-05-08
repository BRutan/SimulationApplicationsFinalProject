####################
# Main.R
####################
# Description:
# 1) Pull in SPY and WTI data, plot qq plot and bins. 
# 2) Fit jump-diffusion process parameters for SPY and WTI using regression method.
# 3) Generate large number of SPY and WTI sample paths.
# 4) Price option on SPY, using sample paths.

source("FitParameters.R")
source("InvNorm.R");
source("JumpDiffusionData.R");
source("MonteCarlo.R");
source("Plotting.R");

main = function()
{ 
    # Pull in EURCHF data:
    eurchf_prices = read.csv("EURCHF.csv");
    row.names(eurchf_prices) = strptime(eurchf_prices$Date, "%m/%d/%Y")
    eurchf_prices$Date = NULL;

    # EURCHF: Use specific jump date method (manually selected jump dates):
    gbm_obs_eurchf_prices = subset(eurchf_prices, (row.names(eurchf_prices) >= as.Date("11/3/2011", "%m/%d/%Y") & row.names(eurchf_prices) <= as.Date("1/9/2015", "%m/%d/%Y")));
    jump_obs_eurchf_prices = subset(eurchf_prices, eurchf_prices$IsJump == 1);
    names(gbm_obs_eurchf_prices)[names(gbm_obs_eurchf_prices) == "EURCHF"] = "prices";
    names(jump_obs_eurchf_prices)[names(jump_obs_eurchf_prices) == "EURCHF"] = "prices";
    eurchf_mle_data = subset(eurchf_prices, (row.names(eurchf_prices) >= as.Date("6/13/2011", "%m/%d/%Y")));
    names(eurchf_mle_data)[names(eurchf_mle_data) == "EURCHF"] = "prices";
    # Generate qq and log return plots:
    eurchf_price_plot = plot_data(eurchf_mle_data, "EURCHF Historical", y_axis_label = "EURCHF");
    eurchf_logrets_plot = log_returns_plot(eurchf_mle_data[,c("prices"),drop=FALSE], "EURCHF LogReturns", y_axis_label = "EURCHF Return", x_axis_label = "Day")
    eurchf_qq_plot = qq_plot(gbm_obs_eurchf_prices, inv_norm, "EURCHF LogReturns QQ Plot", "Norm Dist Quantile", "EURCHF LogReturn Quantile");
    ggsave("EURCHF Exchange Plot.png", eurchf_price_plot);
    ggsave("EURCHF LogReturns Plot.png", eurchf_logrets_plot);
    ggsave("EURCHF LogReturns QQ Plot.png", eurchf_qq_plot);

    # Estimate gbm parameters using regression for GBM and MLE for jump process:
    gbm_obs_eurchf_returns = log(gbm_obs_eurchf_prices$prices[-1] / gbm_obs_eurchf_prices$prices[-nrow(gbm_obs_eurchf_prices)]);
    gbm_mleResults_eurchf = regression_fit_gbm(gbm_obs_eurchf_prices[, c("prices"), drop = FALSE]);
    jump_params_eurchf = mle_jump_diff(eurchf_mle_data, gbm_mleResults_eurchf$params);
    jump_sde_params = append(gbm_mleResults_eurchf$params, jump_params_eurchf$params, length(gbm_mleResults_eurchf$params));
    jump_sde_params[5] = abs(jump_sde_params[5])
    #jump_sde_params = c(7.846252e-04,6.939031e-03,1.142403e+00,-1.266651e-01,-1.005641e-08)

    # [stock_price_0,drift,T,imp_vol,strike,lambda,beta,eta].
    path_params = c(eurchf_mle_data$prices[1], jump_sde_params[1], 1/252, jump_sde_params[2],
                    eurchf_mle_data$prices[1], jump_sde_params[3], jump_sde_params[4], jump_sde_params[5]);

    # Generate sample paths to plot:
    paths_data = monte_carlo(jump_gbm_gen, path_params, numpaths = 4, numsteps = 5 * 252, fullpath = TRUE);
    paths_plot = plot_paths(paths_data$paths, x_axis_title = "Period", y_axis_title = "EURCHF", title = "EURCHF Jump Diffusion SDE Sample Paths");
    ggsave("EURCHF Jump Diffusion SDE Sample Paths.png", paths_plot);
    # Price y1 ATM European Call option on EURCHF using Monte Carlo:
    path_params[1] = 1.06;
    path_params[5] = path_params[1];
    str_rf = -.00616;
    euro_payoff = function(params) max(params[1] - params[5], 0) * exp(-str_rf * 1);
    eurchf_option_price = price_option_mc(jump_gbm_gen, path_params, euro_payoff, numsteps = 252);
    outFile = "EURCHF_MonteCarlo_Results.csv";
    # [stock_price_0,drift,T,imp_vol,strike,lambda,beta,eta].
    cat(replicate(10, "-,"), "\n", sep = ",", file = outFile, append = TRUE);
    cat("Monte Carlo Parameters:\n", file = outFile, sep = ",", append = TRUE);
    cat(replicate(10, "-,"), "\n", sep = ",", file = outFile, append = TRUE);
    cat("S_0", path_params[1], "\n", file = outFile, sep = ",", append = TRUE);
    cat("Strike", path_params[5], "\n", file = outFile, sep = ",", append = TRUE);
    cat("T-t", path_params[3], "\n", file = outFile, sep = ",", append = TRUE);
    cat("STR_RiskFree", str_rf, "\n", file = outFile, sep = ",", append = TRUE);
    cat("Mu_GBM", path_params[2], "\n", file = outFile, sep = ",", append = TRUE);
    cat("Sigma_GBM", path_params[4], "\n", file = outFile, sep = ",", append = TRUE);
    cat("Lambda_Jump", path_params[6], "\n", file = outFile, sep = ",", append = TRUE);
    cat("Beta_Jump", path_params[7], "\n", file = outFile, sep = ",", append = TRUE);
    cat("Eta_Jump", path_params[8], "\n", file = outFile, sep = ",", append = TRUE);
    cat(replicate(10, "-,"), "\n", sep = ",", file = outFile, append = TRUE);
    cat("Monte Carlo Results:\n", file = outFile, sep = ",", append = TRUE);
    cat(replicate(10, "-,"), "\n", sep = ",", file = outFile, append = TRUE);
    cat("Option_Price:", eurchf_option_price$price, "\n", file = outFile, sep = ",", append = TRUE);
    cat("Option_StdErr:", eurchf_option_price$std_err, "\n", file = outFile, sep = ",", append = TRUE);

}

main()