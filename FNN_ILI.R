#KNN Ananlysis of ILI rassessment on ML2 2005 & 2011
library(FNN) #find nearest neighbor
library(dplyr)
library(BEST)

View(ML2_2005)

ML2_2005 <- read.csv("E:/Documents/KNN_ILI/ML2_2005.csv",stringsAsFactors = F)
ML2_2011 <- read.csv("E:/Documents/KNN_ILI/ML2_2011.csv",stringsAsFactors = F)
ML2_2005$INT_EXT <- as.factor(ML2_2005$INT_EXT)
ML2_2011$INT_EXT <- as.factor(ML2_2011$INT_EXT)

normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

ML2_2005N <- as.data.frame(lapply(ML2_2005[c(1:3,6:7)], normalize))
ML2_2005N$Dia <- as.factor(ML2_2005$Dia)
ML2_2005N$IE <- ML2_2005$INT_EXT
ML2_2011N <- as.data.frame(lapply(ML2_2011[c(1:3,6:7)], normalize))
ML2_2011N$Dia <- as.factor(ML2_2011$Dia)
ML2_2011N$IE <- ML2_2011$INT_EXT
#test

#count interior/exterior
table(ML2_2005$INT_EXT)

#KNN of two runs minus the depth
knn_ml2_nd <- get.knnx(ML2_2005[,c(2:4,7:9)],ML2_2011[,c(2:4,7:9)],k=2)

#pull out first index and distance columns along with depth information from two DFs
knn_ml2c <- cbind(knn_ml2_nd$nn.index[,1],knn_ml2_nd$nn.dist[,1],ML2_2005[knn_ml2_nd$nn.index[,1],6],ML2_2011$depth)
colnames(knn_ml2c) <- c("index","dist","depth_05","depth_11")
knn_ml2c <- as.data.frame(knn_ml2c)

#pull out the min distance for each index to find the closest one
ml2_min <- knn_ml2c %>% group_by(index) %>% summarise(dist=min(dist))

#rejoin min distance and depths
ml2_min <- left_join(ml2_min,knn_ml2c,by=c("index","dist"))
ml2_min <- as.data.frame(ml2_min)

#compare the two data sets
ml2_best_nd <- BESTmcmc(ml2_min$depth_05,ml2_min$depth_11,numSavedSteps = 5e4)

#plot results
plotAll(ml2_best_nd)
hist(log(ml2_min$dist))

