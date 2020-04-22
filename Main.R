####################
# Main.R
####################
# Description:
# * Pull in SPY data, fit jump-diffusion process parameters
# to data using Maximum Likelihood Estimation, prices
# option on SPY using jump diffusion process with parameters.

data = read.csv("SPY.csv");

