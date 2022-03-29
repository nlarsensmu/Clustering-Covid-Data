create_cluster_profile <- function(data) {
  p <- ggplot(pivot_longer(data, cols = c(deaths_per100, confirmed_cases_per1000), 
                           names_to = "feature"),
              aes(x = value, y = feature, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(cluster)) +
    ggtitle("Clutering Profile")
}

create_box_plots_per_cluster_deaths <- function(data) {
  p <- (ggplot(data, aes(x=cluster, y=deaths_per100)) + 
          geom_boxplot() + 
          ylab("Deaths per 100") + 
          ggtitle("Death Statistics per Cluster"))
  p
}
create_box_plots_per_cluster_cases <- function(data) {
  p <- (ggplot(data, aes(x=cluster, y=confirmed_cases_per1000)) + 
        geom_boxplot() + 
        ylab("Cases per 1000") + 
        ggtitle("Case Statistics per Cluster"))
  p
}