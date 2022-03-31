# PCA 


# Cluster 1
data <- read_csv(file = "cluster1.csv")
selected <- data
pca <- prcomp(selected, scale. = TRUE)
pca

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

data$PC1 <- data.frame(pca$x)$PC1
data$PC2 <- data.frame(pca$x)$PC2

p <- ggplot(data, aes(x=PC1, y=PC2, color=cluster)) + geom_point(alpha = 0.5 ) +
  ggtitle("K Means Cluster 1 reduced to 2 dimensions")
p
ggsave("./charts/k_means_1_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 500,  units =  "px", dpi = 100)
###########################

# DBSCAN


data <- read_csv(file = "dbscan.csv")
selected <- data %>% select(
  total_pop,
  white_pop_per,
  black_pop_per,
  asian_pop_per,
  hispanic_pop_per,
  amerindian_pop_per,
  commuters_by_public_transportation)

pca <- prcomp(selected, scale. = TRUE)
pca

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

data$PC1 <- data.frame(pca$x)$PC1
data$PC2 <- data.frame(pca$x)$PC2

p <- ggplot(data, aes(x=PC1, y=PC2, color=cluster)) + geom_point(alpha = 0.5 ) +
  ggtitle("DBSCAN Cluster reduced to 2 dimensions")
p
ggsave("./charts/dbscan_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 500,  units =  "px", dpi = 100)


##############################


# H Cluster
data <- read_csv("cluster_h_1.csv")

selected <- data %>% select(
  median_income,
  rent_burden,
  severe_burden,
  no_rent_burden,
  commuters_by_public_transportation
)

pca <- prcomp(selected, scale. = TRUE)
pca

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)

data$PC1 <- data.frame(pca$x)$PC1
data$PC2 <- data.frame(pca$x)$PC2

p <- ggplot(data, aes(x=PC1, y=PC2, color=cluster)) + geom_point(alpha = 0.5) +
  ggtitle("H Cluster reduced to 2 dimensions")
p
ggsave("./charts/pca_h_cluster.png",  plot = p,  device = "png",  
       scale = 1,   width = 1200,  height = 500,  units =  "px", dpi = 100
)

fviz_cluster(list(data = select(data,
                                median_income,
                                rent_burden,
                                severe_burden,
                                no_rent_burden,
                                commuters_by_public_transportation),
                  cluster = data$cluster), geom = "point") + 
  ggtitle("H Cluster reduced to 2 dimensions")

ggsave("./charts/fix_pca_h_cluster.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 1000,  units =  "px", dpi = 100,
)

fviz_cluster(list(data = data, cluster = data$cluster), geom = "point")
#################################

# PAM Cluster

data <- read_csv("cluster4.csv")


selected <- data %>% select(
  total_pop,
  no_rent_burden, 
  rent_burden,
  severe_burden, 
  median_income,
  commuters_by_public_transportation
)

pca <- prcomp(selected, scale. = TRUE)
pca

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)

data$PC1 <- data.frame(pca$x)$PC1
data$PC2 <- data.frame(pca$x)$PC2

p <- ggplot(data, aes(x=PC1, y=PC2, color=cluster)) + geom_point(alpha = 0.5) +
  ggtitle("PAM Cluster reduced to 2 dimensions")
p
ggsave("./charts/pca_pam_cluster.png",  plot = p,  device = "png",  
       scale = 1,   width = 1200,  height = 500,  units =  "px", dpi = 100
)


fviz_cluster(list(data = data, cluster = data$cluster), geom = "point")
