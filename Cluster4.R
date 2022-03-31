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
  d <- dist(data_final, method = "manhattan")
  p <- pam(d, k=chosen_number_of_clusters)
  data_final$cluster <- p$clustering 
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

data_final$cluster_f <- as.factor(data_final$cluster)
p <- create_box_plots_per_cluster_deaths(cluster_f)
p
ggsave("./charts/cluster4/box_plot_deaths.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

p <- create_box_plots_per_cluster_cases(cluster_f)
p
ggsave("./charts/cluster4/box_plot_cases.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

## Measure Entropy 
selected$case_death_class <- catagorize_deaths(selected, 10)
selected$case_class <- catagorize_cases(selected, 10)
ret <- entropy(data_final$cluster, catagorize_cases(data_final, 15))
print("cases:")
ret$total
ret$indv
print("deaths:")
ret <- entropy( data_final$cluster, catagorize_deaths(data_final, 15))
ret$total
ret$indv

random_15 <- sample(1:15, nrow(data_final), replace = TRUE)
ret <- entropy( random_15, catagorize_deaths(data_final, 15))
ret$total
ret$indv
ret <- entropy( random_15, catagorize_cases(data_final, 15))
ret$total
ret$indv


#
# Sever Rent Burden
data <- read_csv("cluster4.csv")
data$cluster <- as.factor(selected$cluster)

p <- create_box_plots_per_cluster_severe_burden(data, title="Severe Rent Burden per cluster")
p
ggsave("./charts/cluster4/sevre_rent_burden_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 300,  units =  "px", dpi = 100
)
p <- create_box_plots_per_cluster_no_burden(data, title="No Rent Burden per cluster")
p
ggsave("./charts/cluster4/no_rent_burden_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 300,  units =  "px", dpi = 100
)
p <- create_box_plots_per_cluster_rent_burden (data, title="Rent Burden per cluster")
p
ggsave("./charts/cluster4/rent_burden_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 300,  units =  "px", dpi = 100
)
p <- create_box_plots_per_cluster_commuters_by_public_transportation (data, title="Normalized commuters_by_public_transportation_cluste")
p
ggsave("./charts/cluster4/commuters_by_public_transportation_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 300,  units =  "px", dpi = 100
)

p <- create_box_plots_per_cluster_median_income (data, title="Normalized median income")
p
ggsave("./charts/cluster4/commuters_by_public_transportation_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 300,  units =  "px", dpi = 100
)

