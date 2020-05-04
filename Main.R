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
source("Plotting.R");

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
eurchf_logrets_plot = log_returns_plot(eurchf_mle_data[,c("prices"),drop=FALSE], "EURCHF LogReturns", y_axis_label = "EURCHF Return", x_axis_label = "Day")
eurchf_qq_plot = qq_plot(gbm_obs_eurchf_prices, inv_norm, "EURCHF LogReturns QQ Plot", "Norm Dist Quantile", "EURCHF LogReturn Quantile");
ggsave("EURCHF LogReturns Plot.png", eurchf_logrets_plot);
ggsave("EURCHF LogReturns QQ Plot.png", eurchf_qq_plot);

# Estimate gbm parameters using MLE for GBM and jump process:
gbm_obs_eurchf_returns = log(gbm_obs_eurchf_prices$prices[-1] / gbm_obs_eurchf_prices$prices[-nrow(gbm_obs_eurchf_prices)]);
gbm_mleResults_eurchf = mle_gbm(gbm_obs_eurchf_returns);
jump_params_eurchf = mle_jump_diff(eurchf_mle_data, gbm_mleResults_eurchf$params);
jump_sde_params = append(gbm_mleResults_eurchf$params, jump_params_eurchf$params, length(gbm_mleResults_eurchf$params));

# Generate five sample paths to plot:
paths = monte


if (FALSE)
{ 
# Pull in SPY, WTI data: 
data = read.csv("JumpData_Alt.csv");
row.names(data) = data$Date;
data = subset(data, select = c("SPY", "WTI"));
log_rets = as.data.frame(sapply(data, function(x) diff(log(x))))
row.names(log_rets) = row.names(data)[2:nrow(data)];
log_returns_spy = subset(log_rets, select = c("SPY"));
names(log_returns_spy)[names(log_returns_spy) == "SPY"] = "returns";
log_returns_wti = subset(log_rets, select = c("WTI"));
names(log_returns_wti)[names(log_returns_wti) == "WTI"] = "returns";


# Demonstrate that data follows jump process using qqplots and log return charts:
spy_qq = qq_plot(log_returns_spy, inv_norm, "SPY LogReturns QQ Plot", "Norm Dist Quantile", "SPY LogReturns Quantile");
wti_qq = qq_plot(log_returns_wti, inv_norm, "WTI LogReturns QQ Plot", "Norm Dist Quantile", "WTI LogReturns Quantile");
spy_log_ret_plot = log_returns_plot(log_returns_spy, title = "SPY Log-Returns", y_axis_label = "SPY Log-Return");
wti_log_ret_plot = log_returns_plot(log_returns_wti, title = "WTI Log-Returns", y_axis_label = "WTI Log-Return");

ggsave("SPY LogReturns QQ Plot.png", spy_qq);
ggsave("WTI LogReturns QQ Plot.png", wti_qq);
ggsave("SPY Log Returns Plot.png", spy_log_ret_plot);
ggsave("WTI Log Returns Plot.png", wti_log_ret_plot);

# Fit jump-diffusion process parameters to data using regression approach:
non_jump_info_spy = remove_jump_dates(log_returns_spy, inv_norm, plot_r2 = TRUE);
non_jump_info_wti = remove_jump_dates(log_returns_wti, inv_norm, plot_r2 = TRUE);
ggsave("SPY RSq Progression.png", non_jump_info_spy$plot);
ggsave("WTI RSq Progression.png", non_jump_info_wti$plot);

# Separate jump observations from non-jump observations:
gbm_obs_spy_rets = subset(log_returns_spy, (row.names(log_returns_spy) %in% non_jump_info_spy$non_jump_dates));
gbm_obs_wti_rets = subset(log_returns_spy, (row.names(log_returns_spy) %in% non_jump_info_spy$non_jump_dates));
jump_obs_spy_rets = subset(log_returns_spy, !(row.names(log_returns_spy) %in% non_jump_info_spy$non_jump_dates));
jump_obs_wti_rets = subset(log_returns_wti, !(row.names(log_returns_wti) %in% non_jump_info_wti$non_jump_dates));
gbm_obs_spy_prices = subset(data[,"SPY",drop=FALSE], (row.names(log_returns_spy) %in% non_jump_info_spy$non_jump_dates));
gbm_obs_wti_prices = subset(data[,"WTI",drop=FALSE], (row.names(log_returns_wti) %in% non_jump_info_wti$non_jump_dates));
names(gbm_obs_spy_prices)[names(gbm_obs_spy_prices) == "SPY"] = "prices";
names(gbm_obs_wti_prices)[names(gbm_obs_wti_prices) == "WTI"] = "prices";

# Use two-step regression approach to estimate GBM parameters:
gbm_params_spy = regression_fit_gbm(gbm_obs_spy_prices);
gbm_params_wti = regression_fit_gbm(gbm_obs_wti_prices);
}


