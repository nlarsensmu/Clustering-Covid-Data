
create_map_of_cluster <- function(filename_for_cluster, title, subtitle){
  data <- read_csv("for_eval.csv")
  cluster_1 <- read_csv(filename_for_cluster)
  
  #artifacts from data_prep to make them the same size
  data <- data %>% filter(county_fips_code != "06037")
  data <- data %>% filter(original_total_pop >= 1000)
  
  data$cluster <- as.factor(cluster_1$cluster)
  counties <- as_tibble(map_data("county"))
  counties <- counties %>% rename(c(county = subregion))
  
  
  data <- data %>% mutate(county = county_name %>% 
                                    str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
  
  data_clust <- counties %>% left_join(data)
  
  p <- ggplot(data_clust, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = cluster)) +
    coord_quickmap() + 
    scale_fill_viridis_d() + 
    labs(title = title, subtitle = subtitle) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank() 
    ) + ylab("") + xlab("")
  p
}
p <- create_map_of_cluster("cluster1.csv", "Kmeans All of the Features", "with 15 clusters")
p
ggsave("./charts/eval/cluster1_map.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)

p <- create_map_of_cluster("dbscan.csv", "DBSCAN Demographic and Communting Data", "with 5 clusters")
p
ggsave("./charts/eval/dbsca_map.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)

p <- create_map_of_cluster("cluster_h_1.csv", "H-Cluster Rent Burden, Median Income, and Transportation", "with 15 clusters")
p
ggsave("./charts/eval/cluster_h_map.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)

p <- create_map_of_cluster("cluster4.csv", "PAM with Population, Rent Burden, Median Income and Transportation", "with 15 clusters")
p
ggsave("./charts/eval/cluster4_map.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)



