train_list <- read.csv("E:/Rkmeans/Rkmeans/total_train.csv")
train_list <- train_list[, -5]

set.seed(172)
fit =kmeans(train_list, 3)
barplot(t(fit$centers), beside =TRUE,xlab="cluster", ylab="value")

library(cluster)
clusplot(train_list, fit$cluster, color=TRUE, shade=TRUE)
par(mfrow= c(1,2))
clusplot(train_list, fit$cluster, color=TRUE, shade=TRUE)
rect(-1,1, -4,2, border = "orange", lwd=2)
clusplot(train_list, fit$cluster, color = TRUE, xlim = c(-1,1), ylim = c(-4,2))

nk <- 2:10
set.seed(30)
WSS <- sapply(nk, function(k){
  kmeans(train_list, centers = k)$tot.withinss
})
WSS

plot(nk, WSS, type = "l", xlab="number of k", ylab="within sum of squares")

monthly_list <- read.csv("E:/Rtimeline/monthly_list_final.csv")

train_list_res <- cbind(train_list_res, fit$cluster)
train_list_res <- cbind(train_list_res, colnames(monthly_list)[2:60])

#===============================================================

library(cluster)

for(i in 241:300){
  nk <- 2:10
  set.seed(i)
  WSS <- sapply(nk, function(k){
    kmeans(train_list, centers = k)$tot.withinss
  })
  WSS
  plot(nk, WSS, type = "l", xlab="number of k", ylab="within sum of squares")
  

  dif_wss_list <- NULL
  for(j in 2:10){
    dif_wss <- WSS[j] - WSS[j+1]
    dif_wss_list <- c(dif_wss_list, dif_wss)
  }
  max_wss <- max(dif_wss_list)
  
  fit =kmeans(train_list, max_wss+1)
  # barplot(t(fit$centers), beside =TRUE,xlab="cluster", ylab="value")
  
  clusplot(train_list, fit$cluster, color=TRUE, shade=TRUE)
  
  name <- paste(i,max_wss,".png")
  png(name, width = 480, height = 320)
}

