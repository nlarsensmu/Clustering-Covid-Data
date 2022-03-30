library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")
library("factoextra")


data <- read_csv("datatable.csv")


selected <- data %>% select(
  median_income,
  rent_burden,
  severe_burden,
  no_rent_burden,
  commuters_by_public_transportation
)

#zero mean normalize all the columns 
selected <- selected %>% 
  mutate_at(c("median_income"), ~(scale(.) %>% as.vector))
selected <- selected %>% 
  mutate_at(c("rent_burden"), ~(scale(.) %>% as.vector))
selected <- selected %>% 
  mutate_at(c("severe_burden"), ~(scale(.) %>% as.vector))
selected <- selected %>% 
  mutate_at(c("no_rent_burden"), ~(scale(.) %>% as.vector))
selected <- selected %>% 
  mutate_at(c("commuters_by_public_transportation"), ~(scale(.) %>% as.vector))
selected <- selected %>% 
  mutate_at(c("original_total_pop"), ~(scale(.) %>% as.vector))

selected
selected <- selected %>% drop_na()

library(ggplot2)
library(ggdendro)
summary(selected)
d <- dist(selected)
hc <- hclust(d, method = "complete")
ggdendrogram(hc, rotate = TRUE, size = 1) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Dendogram H Cluster of Rent Burden, Transportation, and Population")
ggsave("./charts/hclusterdendo.jpg", width = 6.5, height = 3)
hc_10 <- cutree(hc, 10)
table(hc_10)
p <- fviz_dend(hc, k = 10, cex = 0.01, main = "Complete H-Cluster for k=10")
ggsave("./charts/Complete_H_Clusterk_10.jpg", width = 6.5, height = 3)
p

for (i in 1:20 ){
  hc_i <- cutree(hc, i)
  print(table(hc_i))
}

hc_14 <- cutree(hc, 14)
SSE(selected, hc_14)$sumWithin

# Find the elbow
number_of_clusters <- 40
sse_scores <- rep(NA, number_of_clusters)
xValue <- 2:(number_of_clusters + 1)
for (i in xValue) {
  hc_i <- cutree(hc, i)
  sse_scores[i - 1] <- SSE(selected, hc_i)$sumWithin
}
chosen_number_of_clusters <- 15
line_data <- data.frame(xValue,sse_scores)
p <- ggplot(line_data, aes(x=xValue, y=sse_scores)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ggtitle("SSE Scores  H Clustering of Rent, Transportation, and Population") + 
  xlab("Number of Clusters") +
  ylab("SSE Score") + 
  geom_vline(xintercept = chosen_number_of_clusters, linetype="dashed", 
             color = "red", size=1.5) + 
  geom_text(aes(x=chosen_number_of_clusters, label="15 clusters", 
                y=20000), colour="black", angle=90, vjust = 1.2)
p
ggsave("./charts/ElbowHcluster1.jpg", width = 6.5, height = 3)

p <- fviz_dend(hc, k = 15, cex = 0.01, main = "Complete H-Cluster for k=15")
p
ggsave("./charts/ColoredDendo.jpg", width = 6.5, height = 3)

source("./clustering_helpers.R")


# Add back the deaths
hc_15 <- cutree(hc, 15)
selected$confirmed_cases_per1000 <- data$confirmed_cases_per1000
selected$deaths_per10000 <-  data$deaths_per1000 * 10
selected$cluster <- hc_15
p <- create_cluster_profile2(selected)
p
ggsave("./charts/ColorProfileHClust.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)


print(table(hc_15))
result <- SSE(selected, hc_15)
result$sumWithin

print(table(hc_i))

## Measure Entropy 
selected$case_death_class <- catagorize_deaths(selected, 10)
selected$case_class <- catagorize_cases(selected, 10)
ret <- entropy(hc_15, catagorize_cases(selected, 10))
ret$total
ret$indv
ret <- entropy(hc_15, catagorize_deaths(selected, 10))
ret$total
ret$indv

test <- purity(hc_15, catagorize_cases(selected, 10))
test
test$total



random_10 <- sample(1:10, nrow(selected), replace = TRUE)
ret <- entropy( random_10, catagorize_deaths(selected, 15))
ret$total
ret$indv
ret <- entropy( random_10, catagorize_cases(selected, 15))
ret$total
ret$indv
