library("tidyverse")
library("ggplot2")

data <- read_csv("datatable.csv")
colnames(data)
data_final <- data %>% 
  select(
    total_pop,
    no_rent_burden, 
    rent_burden,
    severe_burden, 
    median_income,
    white_pop_per,
    black_pop_per,
    asian_pop_per,
    hispanic_pop_per,
    amerindian_pop_per,
    other_race_pop_per,
    two_or_more_races_pop_per,
    not_hispanic_pop_per,
    commuters_by_public_transportation
  ) %>% 
  scale() %>% as_tibble()

#zero mean normalize all the columns 
data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("no_rent_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("rent_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("severe_burden"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("median_income"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("white_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("black_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("asian_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("hispanic_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("amerindian_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("other_race_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("two_or_more_races_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("not_hispanic_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("commuters_by_public_transportation"), ~(scale(.) %>% as.vector))
km <- kmeans(data_final, centers = 4)
km$tot.withinss

kms <- rep(NA, 14)
sse_scores <- rep(NA, 14)

#Distance functions
euc_dist <- function(p1,p2) {
  sqrt(sum((p1 - p2) ^ 2))
}
man_dist <- function(p1,p2) {
 sum(abs(p1 - p2))
}
km <- kmeans(data_final, centers = 5, iter.max =30)

#similarity scores
cluster_data = 0
graph_coesion <- function(data, clusters, distance) {
  temp_data <- data
  temp_data$cluster <- clusters
  number_of_clusters <- max(clusters)
  similarities <- rep(0, number_of_clusters)
  for (i in 1:number_of_clusters) {
    cluster_data <- filter(temp_data, cluster == i)
    print(split(cluster_data, 1:nrow(cluster_data)))
    cluster_data <- lapply(split(cluster_data, 1:nrow(cluster_data)), as.list)
    sum = 0
    for (j in 1:dim(cluster_data)[1]) {
      for (k in (j+1):dim(cluster_data)[1]) {
        x <- cluster_data[i,1:15]
        y <- cluster_data[j,1:15]
        sum <- sum + distance(x,y)
      }
    }
    similarities[i] = sum
  }
  sum(similarities)
}
km$cluster
print(graph_coesion(data_final, km$cluster, euc_dist))
cluster_data
test <- lapply(split(data_final, 1:nrow(data_final)), as.list)
test[2][1]

xValue <- 2:30
for (i in xValue) {
  km <- kmeans(data_final, centers = i, iter.max =30)
  kms[i - 1] <- list(km)
  scores[i - 1] <- km$tot.withinss
}
line_data <- data.frame(xValue,scores)
ggplot(line_data, aes(x=xValue, y=scores)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ggtitle("SSE Scores") + xlab("Number of Clusters")


km$totss
km$tot.withinss
