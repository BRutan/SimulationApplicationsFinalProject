####################
# JumpDiffusion.R
####################
# Description:
# * SDE and distribution function for MLE.

source("InvNorm.R");
source("Generator.R");

jump_gbm_pdf = function(s_t, s_0, t, n, params)
{
    # Inputs:
    # * s_t: Underlying price at time t in future.
    # * s_0: Starting underlying price.
    # * t: Time in future for s(t).
    # * n: Number of jumps between [0, t].
    # * params: vector with following values
    # [lambda, a, b, mu, sig].
    lambda = params[1];
    a = params[2];
    b = params[3];
    mu = params[4];
    sigma = params[5];
    
    mu_ln = log(s_0) + (mu - .5 * sigma * sigma) * t + a * n;
    var_ln = sigma * sigma * t + b * b * n;
    x_t = log(s_0) + (mu - .5 * sigma * sigma) * t + sigma * sqrt(t);

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