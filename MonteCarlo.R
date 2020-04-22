####################
# MonteCarlo.R
####################
# Generic Monte Carlo simulation.

if (!exists("number_generator", mode = "function") && !exists("number_array_generator", mode = "function")) source("Generator.R");
if (!exists("inv_norm", mode = "function")) source("InvNorm.R")

get_underlying <- function(params)
{
    return(params[1]);
}

price_option_mc = function(sde, params, payoff, numsteps = 200, numpaths = 1000, icdf = inv_norm, fullpath = FALSE,
                           extr_underlying = get_underlying, numvars = 1, seed_1 = -1000, seed_2 = 2000)
{
    # Price option and get standard error using Monte Carlo simulation.
    # Inputs:
    # * sde: Discretized diffusion function, with signature sde(params, ran_num, numsteps). Must return
    # the evolved parameters.
    # * params: Array-like to serve as inputs to sde.
    # * payoff: Discounted payoff function. Must have signature payoff(params) 
    # or payoff(final_params, path) if fullpath is TRUE.
    # Optional:
    # * numsteps: Number of steps per path. Set to 200 if not specified.
    # * numpaths: Number of paths to simulate. Set to 1000 if not specified.
    # * icdf: Inverse cumulative distribution function to transform
    # generated uniform random variables. Is the inverse normal cdf
    # if not specified.
    # * rns: Pre-generated random numbers following sde's distribution.
    # * fullpath: Put TRUE if want evolved params for each step for each path
    # to be used to calculate option price. Requires that payoff has signature payoff(final_params, path).
    path_data = monte_carlo(sde, params, numsteps, numpaths, icdf, fullpath,
                            numvars = numvars, seed_1 = seed_1, seed_2 = seed_2);
    price = 0;
    price_sq = 0;
    std_err = 0;
    path = 1;
    # Compute estimated option price (average discounted payoff), standard error:
    while (path <= numpaths)
    {
        if (fullpath == TRUE)
        {
            params = path_data$paths[[path]][[numsteps]];
            all_prices = sapply(path_data$paths[[path]], get_underlying);
            new_price = payoff(params, all_prices);
        }
        else if (fullpath == FALSE)
        {
            params = path_data$paths[[path]];
            new_price = payoff(params);
        }
        price = price + new_price / numpaths;
        price_sq = price_sq + new_price ^ 2 / numpaths;
        path = path + 1;
    }
    std_err = price_sq - price ^ 2;
    std_err = sqrt(std_err / numpaths);
    return(list("price" = price, "std_err" = std_err));
}

monte_carlo = function(sde, params, numsteps = 200, numpaths = 1000, icdf = inv_norm, fullpath = FALSE,
                       numvars = 1, seed_1 = -1000, seed_2 = 2000)
{
    # Inputs:
    # * sde: Discretized diffusion function, with signature sde(params, ran_num, numsteps). Must return
    # the evolved parameters.
    # * params: Array-like to serve as inputs to sde.
    # Optional:
    # * numsteps: Number of steps per path. Set to 200 if not specified.
    # * numpaths: Number of paths to simulate. Set to 1000 if not specified.
    # * icdf: Inverse cumulative distribution function to transform
    # generated uniform random variables. Is the inverse normal cdf
    # if not specified.
    # * rns: Pre-generated random numbers following sde's distribution.
    # * fullpath: Put TRUE if want evolved params for each step for each path
    # to be included in output data. Otherwise only the final evolved params for 
    # each path will be returned.
    data = vector(mode = "list", length = numpaths);
    path = 1;
    if (fullpath == TRUE)
    {
        path_list = vector(mode = "list", length = numsteps);
    }
    if (numvars > 1)
    {
        iids = vector(length = numvars);
    }
    while (path <= numpaths)
    {
        orig_params = params;
        step = 1;
        while (step <= numsteps)
        {
            if (numvars > 1)
            {
                iid = 1;
                while (iid <= numvars)
                {
                    result = number_generator_single(seed_1, seed_2);
                    seed_1 = result$seed_1;
                    seed_2 = result$seed_2;
                    iids[iid] = icdf(result$number);
                    iid = iid + 1;
                }
                params = sde(params, iids, numsteps);
            }
            else
            {
                result = number_generator_single(seed_1, seed_2);
                seed_1 = result$seed_1;
                seed_2 = result$seed_2;
                iid = icdf(result$number);
                params = sde(params, iid, numsteps);
            }
            if (fullpath == TRUE)
            {
                path_list[[step]] = params;
            }
            step = step + 1;
        }
        if (fullpath == TRUE)
        {
            data[[path]] = path_list;
        }
        else
        {
            data[[path]] = params;
        }
        params = orig_params;
        path = path + 1;
    }
    
    return(list("paths" = data));
}