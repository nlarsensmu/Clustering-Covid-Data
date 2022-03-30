
create_cluster_profile <- function(data) {
  p <- ggplot(pivot_longer(data, cols = c(deaths_per10000, confirmed_cases_per1000), 
                           names_to = "feature"),
              aes(x = value, y = feature, fill = cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(cluster)) +
    ggtitle("Clutering Profile")
}

create_cluster_profile2 <- function(data) {
  clusters <- data$cluster
  n_clusters <- length(unique(clusters))
  temp_data <- tibble(.rows = n_clusters)
  temp_data$clusters <- 1:n_clusters
  
  deaths <- vector()
  cases <- vector()
  for (i in 1:n_clusters) {
    deaths <- append(deaths, mean(filter(data, cluster == i)$deaths_per10000))
    cases <- append(cases, mean(filter(data, cluster == i)$confirmed_cases_per1000))
  }
  temp_data$mean_deaths_per10000 <- deaths
  temp_data$mean_cases_per1000 <- cases
  
  temp_data <- arrange(temp_data, mean_cases_per1000)
  sort()
  
  ggplot(pivot_longer(temp_data, cols = c(mean_deaths_per10000, mean_cases_per1000), 
                      names_to = "feature")) +
    aes(x = value, y = feature, fill = clusters) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(clusters))
}


create_box_plots_per_cluster_deaths <- function(data) {
  p <- (ggplot(data, aes(x=cluster, y=deaths_per10000)) + 
          geom_boxplot() + 
          ylab("Deaths per 10,000") + 
          ggtitle("Death Statistics per Cluster"))
  p
}
create_box_plots_per_cluster_cases <- function(data) {
  p <- (ggplot(data, aes(x=cluster, y=confirmed_cases_per1000)) + 
        geom_boxplot() + 
        ylab("Cases per 1000") + 
        ggtitle("Case Statistics per Cluster"))
  p
}


# From https://github.com/jhmadsen/ClustTools/blob/master/R/ClusterFunctions.R
SSE <- function(data, clusters) {
  data <- as.matrix(data)
  n <- nrow(data)
  dim <- ncol(data)
  
  se <- function(x1) {
    sum(x1 - center)^2
  }
  
  names(clusters) <- 1:n
  uniqueCluster <- unique(clusters)
  clusterWithin <- NULL
  centroidMat <- matrix(NA, nrow=length(uniqueCluster), ncol = dim)
  
  for (i in 1:length(uniqueCluster)) {
    clusterObs <- as.numeric(names(clusters[clusters==uniqueCluster[i]]))
    clusterSet <- data[clusterObs,]
    
    if(is.null(nrow(clusterSet))==TRUE){
      center <- clusterSet
      centroidMat[i,] <- clusterSet
      clusterWithin[i] <- 0
    } else {
      center <- colMeans(clusterSet)
      centroidMat[i,] <- center
      clusterWithin[i] <- sum(apply(clusterSet, 1, se))
    }
  }
  return_list <- list(centroidMat=centroidMat, clusterWithin=clusterWithin, sumWithin=sum(clusterWithin))
  return(return_list)
}

catagorize_deaths <- function(data) {
  min <- min(data$deaths_per10000)
  max <- max(data$deaths_per10000)
  med <- median(data$deaths_per10000)
  cut1 <- med - min
  cut2 <- max - med
  cut(data$deaths_per10000, 
      breaks=c(-Inf, cut1, cut2, Inf), 
      labels=c("low","middle","high"))
}
catagorize_cases <- function(data) {
  min <- min(data$confirmed_cases_per1000)
  max <- max(data$confirmed_cases_per1000)
  med <- median(data$confirmed_cases_per1000)
  cut1 <- med - min
  cut2 <- max - med
  cut(data$confirmed_cases_per1000, 
      breaks=c(-Inf, cut1, cut2, Inf), 
      labels=c("low","middle","high"))
}
