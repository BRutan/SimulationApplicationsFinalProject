####################
# Plotting.R
####################
# Description:
# * 

library(ggplot2);

qq_plot = function(data, icdf, title = "QQ Plot")
{
    # ***************************
    # Inputs:
    # * data: Single column of data to plot.
    # * icdf: Target inverse cumulative distribution to plot data against.
    # ***************************
    # Output: line chart with qq plot.

    # Generate data quantiles:
    sort(data);
    quantiles = quantile(data);
    data_len = length(data);
    # Discretize the target distribution:
    dist_quantiles = numeric(data_len);
    n_bins = data_len + 1;
    bin = 1;
    while (bin <= n_bins)
    {
        prob = bin / data_len;
        dist_quantiles[bin] = icdf(prob);
        bin = bin + 1;
    }
    graph = ggplot() + ggtitle(title);
    graph = graph + geom_line(data = quantiles);
    graph = graph + geom_line(data = dist_quantiles);
    graph = graph + xlab("Quantile") + ylab("Quantile");

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