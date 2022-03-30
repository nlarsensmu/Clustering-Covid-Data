library("cluster")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")



data <- read_csv("datatable.csv")
colnames(data)
data_final <- data %>% 
  select(
    total_pop,
    no_rent_burden, 
    rent_burden,
    severe_burden, 
    median_income,
    commuters_by_public_transportation
  ) %>% 
  scale() %>% as_tibble()

data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("no_rent_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("rent_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("severe_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("median_income"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("commuters_by_public_transportation"), ~(scale(.) %>% as.vector))


d <- dist(data_final, method = "manhattan")
p <- pam(d, k=15)

  
  
source("./clustering_helpers.R")

#we are going to look at the different clusters 
number_of_clusters <- 20
sse_scores <- rep(NA, number_of_clusters)

xValue <- 2:(number_of_clusters + 1)
d <- dist(data_final, method = "manhattan")
for (i in xValue) {
  p <- pam(d, k=i)
  score <- SSE(data_final, p$clustering)$sumWithin
  sse_scores[i - 1] <- score
}

line_data <- data.frame(xValue,sse_scores)
chosen_number_of_clusters <- 15
p <- ggplot(line_data, aes(x=xValue, y=sse_scores)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ggtitle("SSE Scores") + 
  xlab("Number of Clusters") +
  ylab("SSE Score") + 
  geom_vline(xintercept = chosen_number_of_clusters, linetype="dashed", 
             color = "red", size=1.5) + 
  geom_text(aes(x=chosen_number_of_clusters, label="15 clusters", 
                y=20000), colour="black", angle=90, vjust = 1.2)
p
ggsave("./charts/cluster4/sse_score.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


first_time <- FALSE
if (first_time) {
  km <- kmeans(data_final, centers = chosen_number_of_clusters, nstart = 10, iter.max =30)
  data_final$cluster <- km$cluster
  write.csv(data_final, file = "cluster4.csv")
} else {
  data_final <- read_csv("cluster4.csv")
}

#add the deaths and the cases data back in 
data_final$deaths_per10000 <- data$deaths_per1000 * 10
data_final$confirmed_cases_per1000 <- data$confirmed_cases_per1000
source("./clustering_helpers.R")
p <- create_cluster_profile2(data_final)
p
ggsave("./charts/cluster4/cluster_profile.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 1000,  units =  "px", dpi = 100
)

data_final$cluster <- as.factor(data_final$cluster)
p <- create_box_plots_per_cluster_deaths(data_final)
p
ggsave("./charts/cluster4/box_plot_deaths.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

data_final$cluster <- as.factor(data_final$cluster)
p <- create_box_plots_per_cluster_cases(data_final)
p
ggsave("./charts/cluster4/box_plot_cases.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

