create_cluster_profile <- function(data) {
  p <- ggplot(pivot_longer(data, cols = c(deaths_per100, confirmed_cases_per1000), 
                           names_to = "feature"),
              aes(x = value, y = feature, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(cluster)) +
    ggtitle("Clutering Profile")
}