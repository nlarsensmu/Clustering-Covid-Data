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

#we are going to look at the different clusters 
number_of_clusters <- 40
sse_scores <- rep(NA, number_of_clusters)

xValue <- 2:(number_of_clusters + 1)
for (i in xValue) {
  km <- kmeans(data_final, centers = i, nstart = 10, iter.max =30)
  sse_scores[i - 1] <- km$tot.withinss
}

length(sse_scores)
length(xValue)

chosen_number_of_clusters <- 15
line_data <- data.frame(xValue,sse_scores)
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

#here we are choosing the clustering based on the SSE graph over time
chosen_number_of_clusters <- 15
km <- kmeans(data_final, centers = chosen_number_of_clusters, nstart = 10, iter.max =30)
data_final$cluster <- km$cluster

#add the deaths and the cases data back in 
data_final$deaths_per100 <- data$deaths_per1000 * 10
data_final$confirmed_cases_per1000 <- data$confirmed_cases_per1000

p <- ggplot(pivot_longer(data_final, cols = c(confirmed_cases_per1000), names_to = "feature"),
       aes(x = value, y = feature, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
p


1