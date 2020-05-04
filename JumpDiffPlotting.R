####################
# JumpDiffPlotting.R
####################
# Description:
# * 

source("InvNorm.R");
#source("JumpDiffusion.R");
library(ggplot2);

log_returns_plot = function(log_rets, title = "LogReturns Plot", y_axis_label = "LogReturn", x_axis_label = "Day")
{
    # ***************************
    # Generate log returns plot using data.
    # ***************************
    # Inputs:
    # * data: Dataframe with $returns as column name, and optionally dates as row names.
    # Optional:
    # * x_axis_labels: Column of dates mapped sequentially.
    # ***************************
    # Output: line chart with qq plot.
    graph = ggplot(data = log_rets, aes(x = row.names(log_rets), y = log_rets$returns)) + geom_bar(stat = "identity");
    graph = graph + xlab(x_axis_label) + ylab(y_axis_label);
    graph = graph + ggtitle(title);
    return(graph);
}

qq_plot = function(data, icdf, title = "QQ Plot", x_axis_label = "Theoretical Quantile", y_axis_label = "Data Quantile")
{
    # ***************************
    # Generate QQ plot using data.
    # ***************************
    # Inputs:
    # * data: Single column of data to plot.
    # * icdf: Inverse cdf function to map percentile to comparison distribution.
    # ***************************
    # Output: qq scatter plot.
    quantiles = gen_qq(data, icdf);
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
    graph = ggplot();
    for (path in paths)
    {
        path_data = lapply(path, getValue);
        graph = graph + geom_line(data = sdata, aes(x = sdata[, 1], y = sdata[, 2]), color = "blue");
    }
    graph = graph + xlab(x_axis_title) + ylab(y_axis_title) + ggtitle(title);
    if (!is.null(dates))
    {
        scale_x_date(date_labels = "%b %Y", breaks = ("1 month"));
    }
    return(graph);
}