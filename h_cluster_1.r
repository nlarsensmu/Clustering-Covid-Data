library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")


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

mean(selected$median_income)
sd(selected$median_income)
mean(selected$rent_burden)
sd(selected$rent_burden)
mean(selected$commuters_by_public_transportation)
sd(selected$commuters_by_public_transportation)

max(selected$median_income)
max(selected$rent_burden)
max(selected$commuters_by_public_transportation)


min(selected$median_income)
min(selected$rent_burden)
min(selected$commuters_by_public_transportation)

sum(is.na(selected$median_income))
sum(is.na(selected$rent_burden))
sum(is.na(selected$commuters_by_public_transportation))

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

# Find the elbow
number_of_clusters <- 40
sse_scores <- rep(NA, number_of_clusters)
xValue <- 2:(number_of_clusters + 1)
for (i in xValue) {
  hc_i <- cutree(hc, i)
  sse_scores[i - 1] <- km$tot.withinss
}
chosen_number_of_clusters <- 14
p <- ggplot(line_data, aes(x=xValue, y=sse_scores)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ggtitle("SSE Scores  H Clustering of Rent Burden, Transportation, and Population") + 
  xlab("Number of Clusters") +
  ylab("SSE Score") + 
  geom_vline(xintercept = chosen_number_of_clusters, linetype="dashed", 
             color = "red", size=1.5) + 
  geom_text(aes(x=chosen_number_of_clusters, label="14 clusters", 
                y=20000), colour="black", angle=90, vjust = 1.2)
p
ggsave("./charts/ElbowHcluster1.jpg", width = 6.5, height = 3)


p <- fviz_dend(hc, k = 14, cex = 0.01, main = "Complete H-Cluster for k=14")
p
ggsave("./charts/ColoredDendo.jpg", width = 6.5, height = 3)

source("./clustering_helpers.R")


# Add back the deaths
selected$confirmed_cases_per1000 <- data$confirmed_cases_per1000
selected$deaths_per10000 <-  data$deaths_per1000 * 10
selected$cluster <- hc_14
p <- create_cluster_profile2(selected)
p
ggsave("./charts/ColorProfile.png", plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100)


print(table(hc_14))
result <- SSE(selected, hc_14)
result$sumWithin

print(table(hc_i))

test <- selected
piv <- pivot_longer(test, cols = c(deaths_per10000, confirmed_cases_per1000), 
                    names_to = "feature")
mean(filter(test, cluster == 1)$confirmed_cases_per1000)

temp_data <- tibble(.rows = 14)
temp_data$clusters <- 1:14

deaths <- vector()
cases <- vector()
for (i in 1:14) {
  deaths <- append(deaths, mean(filter(selected, cluster == i)$deaths_per10000))
  cases <- append(cases, mean(filter(selected, cluster == i)$confirmed_cases_per1000))
}
temp_data$mean_deaths_per10000 <- deaths
temp_data$mean_cases_per1000 <- cases
temp_data

clusters <- selected$cluster
uniq <- unique(clusters)
length(uniq)

ggplot(pivot_longer(temp_data, cols = c(mean_deaths_per10000, mean_cases_per1000), 
                    names_to = "feature")) +
  aes(x = value, y = feature, fill = clusters) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(clusters))

