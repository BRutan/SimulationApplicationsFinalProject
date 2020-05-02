####################
# MaximumLikelihoodEstimation.R
####################
# Description:
# * 




MLE_sde = function(dist, obs, interval)
{
    # Inputs:
    # * dist: Probability distribution function to maximize (non-log). Must
    # have signature dist(obs, params).
    # * obs: array-like of observations.
    # * params_0: Starting point for parameters to estimate.
    # * interval: 

    ll = function(params) log_likelihood(dist, obs, params);
    params = optimize(ll,  maximum = TRUE);

    return(params);
}

log_likelihood = function(dist, obs, params)
{
    ll = 0;
    for (ob in obs)
    {
        ll = ll + log(dist(ob, params));
    }
    return(ll);
}