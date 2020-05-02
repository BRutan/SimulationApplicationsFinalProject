####################
# Main.R
####################
# Description:
# 1) Pull in SPY and WTI data, plot qq plot and bins. 
# 2) Fit jump-diffusion process parameters for SPY and WTI using regression method.
# 3) Generate large number of SPY and WTI sample paths.
# 4) Price option on SPY, using sample paths.

source("JumpDiffPlotting.R");
source("InvNorm.R");
source("JumpDiffusion.R");

# Calculate log returns: 
data = read.csv("JumpData.csv");
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
non_jump_dates_spy = remove_jumps(log_returns_spy, inv_norm, plot_r2 = TRUE);
non_jump_dates_wti = remove_jumps(log_returns_wti, inv_norm, plot_r2 = TRUE);

# Use regression based MLE to estimate GBM parameters:


