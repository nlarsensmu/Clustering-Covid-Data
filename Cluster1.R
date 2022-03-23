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
scores <- rep(NA, 14)

xValue <- 2:30
for (i in xValue) {
  km <- kmeans(data_final, centers = i, iter.max =30)
  kms[i - 1] <- km
  scores[i - 1] <- km$tot.withinss
}
line_data <- data.frame(xValue,scores)
ggplot(line_data, aes(x=xValue, y=scores)) +
  geom_line(size=1, alpha=0.9, linetype=1) +
  ggtitle("SSE Scores") + xlab("Number of Clusters")
