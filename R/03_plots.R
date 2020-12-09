make_plot_data <- function(x) {
    select(x, percentile) %>% 
        arrange(percentile) %>%
        mutate(
            exp_percentile = row_number() / (n() + 1), 
            unifdist = row_number() / n ())

}

make_pp_plot <- function(x, lob) {
    plot_data <- make_plot_data(x)
    chart_title <- str_replace(lob, pattern = "_", replacement = " ") %>%
        str_to_title()
    
    ggplot(plot_data) +
        aes(x = exp_percentile, y = percentile) +
        geom_point() + 
        geom_abline(slope = 1, intercept = 0, color = "blue") +
        geom_abline(slope = 1, intercept = 136 / sqrt(50) * 0.01, color = "blue") +
        geom_abline(slope = 1, intercept = -136 / sqrt(50) * 0.01, color = "blue") +
        ggtitle(chart_title)
    
}