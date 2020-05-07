####################
# Plotting.R
####################
# Description:
# * Useful plotting methods for project (log returns, qq plots, sde forecast plots).

source("InvNorm.R");
library("ggplot2");
library("reshape2");

plot_env = new.env();
plot_env$line_colors = c("red", "green", "blue", "orange", "purple", "aquamarine");

plot_data = function(data, title = "LogReturns Plot", y_axis_label = "Price", x_axis_label = "Day")
{
    # ***************************
    # Generate price plotusing data.
    # ***************************
    graph = ggplot() + geom_line(data = data, aes(x = seq(1,length(data$prices)), y = data$prices), color = "blue");
    graph = graph + xlab(x_axis_label) + ylab(y_axis_label);
    graph = graph + ggtitle(title);
    return(graph);
}

log_returns_plot = function(data, title = "LogReturns Plot", y_axis_label = "LogReturn", x_axis_label = "Day")
{
    # ***************************
    # Generate log returns plot using data.
    # ***************************
    # Inputs:
    # * data: Dataframe containing prices, and optionally dates as row names.
    # Optional:
    # * x_axis_labels: Column of dates mapped sequentially.
    # ***************************
    # Output: line chart with qq plot.
    log_rets = log(data[-1, c("prices"), drop = FALSE] / data[-nrow(data), c("prices"), drop = FALSE]);
    names(log_rets)[names(log_rets) == "prices"] = "returns";
    graph = ggplot(data = log_rets, aes(x = row.names(log_rets), y = log_rets$returns)) + geom_bar(stat = "identity");
    graph = graph + xlab(x_axis_label) + ylab(y_axis_label);
    graph = graph + ggtitle(title);
    return(graph);
}

plot_predictions = function(data, dt, sde, params)
{
    # ***************************
    # Plot dt-step-ahead predictionsusing stochastic differential equation.
    # ***************************
    # Inputs:
    # * data: DataFrame with "prices" and observation dates as row.names.
    # * dt: Timestep to forecast.
    # * sde: Stochastic differential equation to plot data. 
    # * params: parameters to sde.


}

qq_plot = function(data, icdf, title = "QQ Plot", x_axis_label = "Theoretical Quantile", y_axis_label = "Data Quantile")
{
    # ***************************
    # Generate QQ plot using data.
    # ***************************
    # Inputs:
    # * data: DataFrame containing "prices" to transform into continuously compounded returns.
    # * icdf: Inverse cdf function to map percentile to comparison distribution.
    # ***************************
    # Output: qq scatter plot.
    data = data[order(as.Date(row.names(data), format = "%m/%d/%Y")),, drop = FALSE];
    log_rets = log(data[-1, c("prices"), drop = FALSE] / data[-nrow(data), c("prices"), drop = FALSE]);
    names(log_rets)[names(log_rets) == "prices"] = "returns";
    quantiles = gen_qq(log_rets, icdf);
    graph = ggplot(data = quantiles, aes(x = quantiles$dist_quantile, y = quantiles$return_quantile)) + ggtitle(title);
    graph = graph + geom_point(size = 2, shape = 23);
    graph = graph + xlab(x_axis_label) + ylab(y_axis_label);

    return(graph);
}

plot_paths = function(paths, dates = NULL, x_axis_title = "Period", y_axis_title = "Value", title = "Paths", plotIndex = 1)
{
    # Inputs:
    # * paths: List of lists (each list is a path, each element of list is a point on path).
    # Optional:
    # * x_axis_title: Label title for x axis.
    # * y_axis_title: Label title for y axis.
    # * title: Title for chart.
    # * plotIndex: Index in each element of path to plot.
    getValue = function(elem) elem[plotIndex];
    x = seq(1, length(paths[[1]]));
    paths_df = data.frame("x" = x);
    num = 0;
    for (path in paths)
    {
        col_index = paste("path_", num, sep = "");
        data = unlist(lapply(path, getValue));
        paths_df[[col_index]] = data;
        num = num + 1;
    }
    paths_df = melt(paths_df, id = "x");
    graph = ggplot(paths_df, aes(x = x, y = value, color = variable)) + geom_line();
    graph = graph + xlab(x_axis_title) + ylab(y_axis_title) + ggtitle(title);
    if (!is.null(dates))
    {
        scale_x_date(date_labels = "%b %Y", breaks = ("1 month"));
    }
    return(graph);
}

gen_qq = function(data, icdf) {
    # Inputs:
    # * data: DataFrame with $returns as sole column.
    # * icdf: Inverse cumulative distribution function to generate quantiles of theoretical
    # dataset, for comparison purposes.
    # Outputs:
    # * Generate DataFrame with [probability, return_quantile, dist_quantile] as columns.
    sorted = data[order(data$returns),, drop = FALSE];
    # Remove NAs and infinite returns from set:
    cleaned_data = data.frame(na.omit(sorted));
    cleaned_data = cleaned_data[is.finite(cleaned_data$returns),, drop = FALSE];
    data_len = nrow(cleaned_data);
    # Discretize the target distribution:
    n_bins = data_len + 1;
    probabilities = numeric(data_len);
    dist_quantiles = numeric(data_len);
    data_quantiles = numeric(data_len);
    bin = 1;
    # Generate target distribution quantiles and title quantiles:
    while (bin < n_bins) {
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
