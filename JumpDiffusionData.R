####################
# JumpDiffusionData.R
####################
# Description:
# * SDE and distribution function for MLE.

library("dplyr");
library("tibble");
source("InvNorm.R");
source("Generator.R");

jump_data_env = new.env();
jump_data_env$seed_1 = as.numeric(Sys.time());
jump_data_env$seed_2 = as.numeric(Sys.time()) - 100;

remove_jump_dates = function(returns_data, icdf, plot_r2 = FALSE)
{
    # ***************************
    # Use R^2 maximization approach of observation quantiles vs norm quantiles 
    # to determine which observations are "jump" observations (i.e. # of jumps <> 0).
    # ***************************
    # Inputs:
    # * returns_data: data frame with [date, return] to fit to jump diffusion.
    # * icdf: Inverse cumulative distribution to compare quantiles.
    # ***************************
    # Outputs: 
    # * vector of dates corresponding to jump observations, and R^2 progression plot
    # if plot_r2 = TRUE.
    # Generate quantile Data:
    quantiles = gen_qq(returns_data, icdf);
    # Preserve dates:
    date_indices = data.frame("date" = row.names(quantiles));
    if (plot_r2 == TRUE)
    {
        r_2_track = numeric(nrow(quantiles) - 2);
        r_2_index = 1;
    }
    # Find maximum r_squared using iterative approach:
    max_r_2 = 0;
    prev_r_2 = 0;
    data_size = nrow(quantiles);
    front_index = 1;
    end_index = data_size;
    r_2_plot = NULL;
    non_jump_dates = NULL;
    curr_non_jump_dates = NULL;
    while (data_size > 2)
    {
        front_qq_data = slice(quantiles, front_index:end_index - 1);
        back_qq_data = slice(quantiles, front_index + 1:end_index);
        r_2_front = cor(front_qq_data$return_quantile, front_qq_data$dist_quantile) ^ 2;
        r_2_back = cor(back_qq_data$return_quantile, back_qq_data$dist_quantile) ^ 2;
        if (r_2_front > r_2_back)
        {
            # Drop back point since contributes less to R^2 than the front point:
            chosen_r_2 = r_2_front;
            curr_non_jump_dates = date_indices[row.names(front_qq_data),];
            end_index = end_index - 1;
        }
        else
        {
            # Drop front point:
            chosen_r_2 = r_2_back;
            curr_non_jump_dates = date_indices[row.names(back_qq_data),];
            front_index = front_index + 1;
        }
        if (chosen_r_2 >= max_r_2 && chosen_r_2 - prev_r_2 > 0)
        {
            max_r_2 = chosen_r_2;
            non_jump_dates = curr_non_jump_dates;
        }
        if (plot_r2 == TRUE)
        {
            r_2_track[r_2_index] = chosen_r_2;
            r_2_index = r_2_index + 1;
        }
        prev_r_2 = chosen_r_2;
        data_size = data_size - 1;
    }
    if (plot_r2 == TRUE)
    {
        df = data.frame("steps" = seq(1, length(r_2_track)), "r_2s" = r_2_track);
        r_2_plot = ggplot(data = df, aes(x = df$steps, y = df$r_2s)) + geom_bar(stat = "identity") + ggtitle("R^2 Progression") +
        xlab("Step Number") + ylab("R^2");
    }
    return(list("non_jump_dates" = non_jump_dates, "max_r_w" = max_r_2, "plot" = r_2_plot));
}

jump_gbm_gen = function(params, rand_norm, numsteps)
{
    # Generate r.v. obeying geometric brownian motion with jumps.
    # Inputs:
    # * params: vector with following values
    # [stock_price,mu,dt,imp_vol,strike,lambda,beta,eta].
    # * rand_norm: 
    # * numsteps: # of steps used in discretization.
    s_0 = params[1];
    mu = params[2];
    dt = params[3];
    iv = params[4];
    lambda = params[6];
    beta = params[7];
    eta = params[8];
    rand_unif_poiss = number_generator_single(jump_data_env$seed_1, jump_data_env$seed_2);
    jump_data_env$seed_1 = rand_unif_poiss$seed_1;
    jump_data_env$seed_2 = rand_unif_poiss$seed_2;
    seed_1 = jump_data_env$seed_1;
    seed_2 = jump_data_env$seed_2;
    # Generate random number of jumps:
    n = poiss_icdf(lambda * dt, rand_unif_poiss$number);
    i = 1;
    log_y_sum = 0;
    while (i <= n)
    {
        rand_unif_jump = number_generator_single(seed_1, seed_2);
        log_y_sum = log_y_sum + inv_norm(rand_unif_jump$number, beta, eta);
        seed_1 = rand_unif_jump$seed_1;
        seed_2 = rand_unif_jump$seed_2;
        i = i + 1;
    }
    r = (mu + .5 * iv * iv) * dt + rand_norm * iv * sqrt(dt) + (exp(log_y_sum) - 1);
    params[1] = s_0 * exp(r);
    return (params);
}

poiss_icdf = function(lambda, prob)
{
    # Inputs:
    # * lambda: hazard rate (average number of occurrences per time period).
    # Must be non-negative.
    # * prob: Probability of observing sought value or below.
    # Output:
    # * poisson(lambda) distributed random value.
    p = 0;
    n = 0;
    pois_prob = ppois(n, lambda, lower.tail = TRUE);
    while (p + pois_prob < prob)
    {
        p = p + pois_prob;
        n = n + 1;
        pois_prob = ppois(n, lambda, lower.tail = TRUE);
    }
    return(n);
}