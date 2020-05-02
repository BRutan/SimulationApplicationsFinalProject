####################
# Main.R
####################
# Description:
# 1) Pull in SPY and WTI data, plot qq plot and bins. 
# 2) Fit jump-diffusion process parameters for SPY and WTI using regression method.
# 3) Generate large number of SPY and WTI sample paths.
# 4) Price option on SPY, using sample paths.

source("Plotting.R");
source("InvNorm.R");

data = read.csv("JumpData.csv");

# Demonstrate that data follows jump process:
spy_qq = qq_plot()

# Fit jump-diffusion process parameters to data using regression approach:
