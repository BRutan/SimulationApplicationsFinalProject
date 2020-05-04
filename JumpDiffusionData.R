####################
# JumpDiffusionData.R
####################
# Description:
# * SDE and distribution function for MLE.

library("dplyr");
library("tibble");
source("InvNorm.R");
source("Generator.R");

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

gen_qq = function(data, icdf)
{
    # Inputs:
    # * data: DataFrame with $returns as sole column.
    # * icdf: Inverse cumulative distribution function to generate quantiles of theoretical
    # dataset, for comparison purposes.
    # Outputs:
    # * Generate DataFrame with [probability, return_quantile, dist_quantile] as columns.
    sorted = data[order(data$returns),, drop = FALSE];
    # Remove NAs and infinite returns from set:
    cleaned_data = data.frame(na.omit(sorted));
    cleaned_data = cleaned_data[is.finite(cleaned_data$returns),,drop=FALSE];
    data_len = nrow(cleaned_data);
    # Discretize the target distribution:
    n_bins = data_len + 1;
    probabilities = numeric(data_len);
    dist_quantiles = numeric(data_len);
    data_quantiles = numeric(data_len);
    bin = 1;
    # Generate target distribution quantiles and title quantiles:
    while (bin < n_bins)
    {
        prob = bin / n_bins;
        probabilities[bin] = prob;
        dist_quantiles[bin] = icdf(prob);
        data_quantiles[bin] = quantile(cleaned_data$returns, prob, na.rm = TRUE);
        bin = bin + 1;
    }
    df = data.frame("probability" = probabilities, "return_quantile" = data_quantiles, "dist_quantile" = dist_quantiles);
    row.names(df) = row.names(cleaned_data);
    return(df);
}


jump_gbm_gen = function(params, rand_norm, numsteps, seed_1 = as.numeric(Sys.time()), seed_2 = as.numeric(Sys.time()) - 100)
{
    # Generate r.v. obeying geometric brownian motion with jumps.
    # Inputs:
    # * params: vector with following values
    # [stock_price,risk_free,div_rate,T,imp_vol,strike,lambda,a,b].
    # * rand_norm: 
    # * numsteps: # of steps used in discretization.
    s_0 = params[1];
    rf = params[2];
    q = params[3];
    t = params[4];
    iv = params[5];
    lambda = params[7];
    a = params[8];
    b = params[9];
    dt = params[4] / numsteps;
    rand_unif_poiss = number_generator_single(seed_1, seed_2);
    n = poiss_icdf(lambda, rand_unif_poiss$number);
    i = 0;
    log_y_sum = 0;
    while (i < n)
    {
        rand_unif_jump = number_generator_single(seed_1, seed_2);
        log_y_sum = log_y_sum + inv_norm(rand_unif_jump$number, a, b * b);
        i = i + 1;
    }
    r = (rf - .5 * iv * iv) * dt + rand_norm * iv * sqrt(dt) + log_y_sum;
    return (s_0 * exp(r));
}


poiss_icdf = function(lambda, prob)
{
    # Inputs:
    # * lambda: hazard rate (average number of occurrences per time period).
    # Must be non-negative.
    # * prob: Probability of observing sought value or below.
    # Output:
    # * poisson(lambda) distributed random value.
    p = 0
    n = 0
    while (p < prob)
    {
        p = p + ppois(n, lambda, lower.tail = TRUE);
        n = n + 1;
    }
    return(n);
}