####################
# OptionStrategies.R
####################
# 

source("BlackScholes.R");
source("Generator.R");
source("MonteCarlo.R");

gbm <- function(params, ran_norm, numsteps)
{
    # Inputs:
    # * params: array with [stock_price, risk_free, div_rate, T, imp_vol].
    # * ran_norm: Random normal variable.
    # * numsteps: Number of steps used in discretization scheme.
    dt = params[4] / numsteps;
    r = (params[2] - params[3] - params[5] ^ 2 / 2) * dt + params[5] * ran_norm * sqrt(dt);
    params[1] = params[1] * exp(r);
    return(params);
}

option_value = function(params, isCall = FALSE)
{
    # [stock_price, risk_free, div_rate, T, imp_vol, strike]
    # (s, k, q, r, t, iv, isCall)
    return(black_scholes(params[1], params[6], params[3], params[2], params[4], params[5], isCall));
}

rolling_put_strategy_pnl = function(params, roll_period, strike_strategy, strategy_period, numpaths = 1000, sde = gbm, icdf = inv_norm, paths = NULL)
{
    # Inputs:
    # * params:
    # *
    num_steps = strategy_period / roll_period;
    if (is.null(paths) == TRUE)
    {
        data = monte_carlo(sde, numsteps = num_steps, numpaths = numpaths, icdf = icdf, fullpath = TRUE);
    }
    else
    {
        data = paths;
    }
    expected_strategy_pnl = 0;
    option_tenor = params[4];
    params = append(params, 0);
    risk_free = params[2];
    ##############################
    # Determine strategy effectiveness with multiple paths:
    ##############################
    for (path in data$paths)
    {
        # Initialize strategy pnl for path:
        hedge_profit = 0;
        stock_price = params[1];
        revalue_params = params;
        revalue_params[6] = strike_strategy(params);
        option_price = option_value(revalue_params);
        borrowing = option_price * exp(params[2] * roll_period);
        # Calculate profit/loss for strategy path:
        step_num = 1;
        while (step_num <= num_steps)
        {
            # Calculate pnl of roll strategy, roll into new options:
            revalue_params[1] = path[[step_num]][1];
            revalue_params[4] = option_tenor - roll_period;
            hedge_profit = (hedge_profit + option_value(revalue_params, FALSE) - borrowing) * exp(risk_free * roll_period);
            revalue_params[4] = option_tenor;
            option_price = option_value(revalue_params, FALSE);
            # Roll into new option:
            borrowing = option_price * exp(risk_free * roll_period);
            step_num = step_num + 1;
        }
        expected_strategy_pnl = expected_strategy_pnl + ((path[[num_steps]][1] - stock_price) + hedge_profit) / numpaths;
    }
    return(expected_strategy_pnl);
}