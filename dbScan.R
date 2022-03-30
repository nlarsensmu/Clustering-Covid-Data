library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")
library("dbscan")
library("factoextra")

data <- read_csv("datatable.csv")
data

colnames(data)
data_final <- data %>% 
  select(
    total_pop,
    white_pop_per,
    black_pop_per,
    asian_pop_per,
    hispanic_pop_per,
    amerindian_pop_per,
    commuters_by_public_transportation,
    deaths,
    #confirmed_cases I removed this because we shouln't use the ground truths on the clustering
  ) %>% 
  scale() %>% as_tibble()

#zero mean normalize all the columns 
data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("white_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("black_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("asian_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("hispanic_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("amerindian_pop_per"), ~(scale(.) %>% as.vector))
data_final <- data_final %>% mutate_at(c("commuters_by_public_transportation"), ~(scale(.) %>% as.vector))

source("./clustering_helpers.R")

#user chosen variables
minPts <- 3
k <- minPts-1

#k-nearest neighbor
kNNdistplot(data_final, k)

#user chosen epsilon from knee
eps <- 1.4
abline(h = eps, col = "red") #red line on plot at knee

db <- dbscan(data_final, eps, minPts)

#add the deaths and the cases data back in 
data_final$deaths_per10000 <- data$deaths_per1000 * 10
data_final$confirmed_cases_per1000 <- data$confirmed_cases_per1000


ggplot(data_final %>% add_column(cluster = factor(db$cluster)),
       aes(deaths, confirmed_cases, color = cluster)) + geom_point()

#plot all variables, assumes deaths and cases are at end of object
yvals <- c(length(data_final)-1,length(data_final))
for (j in yvals) {
  for (i in 1:(length(data_final)-2)) {
      p <- ggplot(data_final %>% add_column(cluster = factor(db$cluster)),
            aes(data_final[[i]], data_final[[j]], color = cluster)) + geom_point() + 
            ggtitle(paste(colnames(data_final[i])," vs ",colnames(data_final[j]))) + 
            xlab(colnames(data_final[i])) +
            ylab(colnames(data_final[j]))
      ggsave(paste("./charts/cluster_profile",i,j,".png"),  plot = p,  device = "png",  
            scale = 1,  width = 1200,  height = 1000,  units =  "px", dpi = 100)
  }
}

p = fviz_cluster(db, data_final, geom = "point")
ggsave(paste("./charts/outliers.png"),  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 1000,  units =  "px", dpi = 100)
p

#Code steven added 
data_final$cluster <- db$cluster
data_final$deaths_per10000 <- data$deaths_per1000 *10 
data_final$confirmed_cases_per1000 <- data$confirmed_cases_per1000

p <- create_cluster_profile2(data_final)
p
#End Code steven Added