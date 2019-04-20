#install.packages("readxl")
library(readxl)
Europe_emp <-  read_excel("C:/Users/anupr/Documents/Flex 4/Data Mining 2/case 3/Employment data.xlsx")
head(Europe_emp)
colnames(Europe_emp)


# Data Cleaning -----------------------------------------------------------

Europe_emp <- Europe_emp[-11]
head(Europe_emp)
sum(is.na(Europe_emp))
str(Europe_emp)
Europe_emp <- na.omit(Europe_emp)
dim(Europe_emp)
# K-means clustering ------------------------------------------------------
library(cluster)
library(fpc)
library(factoextra)
set.seed(12324)
index = sample(nrow(Europe_emp), nrow(Europe_emp)*0.9)
Europe_emp_sample <- Europe_emp[index,]
head(Europe_emp_sample)


fit2 <- kmeans(Europe_emp_sample[2:10], 2)
table(fit2$cluster)
fit3 <- kmeans(Europe_emp_sample[2:10], 3)
table(fit3$cluster)
fit4 <- kmeans(Europe_emp_sample[2:10], 4)
table(fit4$cluster)
fit5 <- kmeans(Europe_emp_sample[2:10], 5)
table(fit5$cluster)

fit6 <- kmeans(Europe_emp_sample[2:10], 6)
fit7 <- kmeans(Europe_emp_sample[2:10], 7)
table(fit6$cluster)
table(fit7$cluster)


p1 <- fviz_cluster(fit2, data = Europe_emp_sample[2:10])
p2 <- fviz_cluster(fit3, data = Europe_emp_sample[2:10])
p3 <- fviz_cluster(fit4, data = Europe_emp_sample[2:10])
p4 <- fviz_cluster(fit5, data = Europe_emp_sample[2:10])

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
p6 <- fviz_cluster(fit6, data = Europe_emp_sample[2:10])
p7 <- fviz_cluster(fit7, data = Europe_emp_sample[2:10])
grid.arrange(p6,p7, nrow = 2)

par(mfrow=c(2,2))

c1 <- plotcluster(Europe_emp_sample[2:10], fit2$cluster, main = "k=2")
c2 <- plotcluster(Europe_emp_sample[2:10], fit3$cluster, main = "k=3")
c3 <- plotcluster(Europe_emp_sample[2:10], fit4$cluster, main = "k=4")
c4 <- plotcluster(Europe_emp_sample[2:10], fit5$cluster, main = "k=5")

cluster1 <- Europe_emp_sample[2:10][fit4$cluster==1,]
cluster2 <- Europe_emp_sample[2:10][fit4$cluster==2,]
cluster3 <- Europe_emp_sample[2:10][fit4$cluster==3,]
cluster4 <- Europe_emp_sample[2:10][fit4$cluster==4,]
mean_emp_each_clus <- matrix(0,4,9)
industry <-  as.data.frame(c("Agriculture","Mining","Manufacturing","Power Supply","Construction","Service","Finance","Social&Personal Serbices","Transport&Communication"))

clust1_means <- as.data.frame(c(mean(cluster1$Agr),
mean(cluster1$Min),
mean(cluster1$Man),
mean(cluster1$PS),
mean(cluster1$Con),
mean(cluster1$SI),
mean(cluster1$Fin),
mean(cluster1$SPS),
mean(cluster1$TC)))
dim(clust1_means)
names(clust1_means)[1]<-"% employment"
names(industry)[1]<-"Industry"
cbind(industry,clust1_means)


clust1_means <- as.data.frame(c(mean(cluster1$Agr),
                                mean(cluster1$Min),
                                mean(cluster1$Man),
                                mean(cluster1$PS),
                                mean(cluster1$Con),
                                mean(cluster1$SI),
                                mean(cluster1$Fin),
                                mean(cluster1$SPS),
                                mean(cluster1$TC)))
dim(clust1_means)
names(clust1_means)[1]<-"% employment"
names(industry)[1]<-"Industry"
cbind(industry,clust1_means)




clust2_means <- as.data.frame(c(mean(cluster2$Agr),
                                mean(cluster2$Min),
                                mean(cluster2$Man),
                                mean(cluster2$PS),
                                mean(cluster2$Con),
                                mean(cluster2$SI),
                                mean(cluster2$Fin),
                                mean(cluster2$SPS),
                                mean(cluster2$TC)))
dim(clust2_means)
names(clust2_means)[1]<-"% employment"
names(industry)[1]<-"Industry"
cbind(industry,clust2_means)


clust3_means <- as.data.frame(c(mean(cluster3$Agr),
                                mean(cluster3$Min),
                                mean(cluster3$Man),
                                mean(cluster3$PS),
                                mean(cluster3$Con),
                                mean(cluster3$SI),
                                mean(cluster3$Fin),
                                mean(cluster3$SPS),
                                mean(cluster3$TC)))
dim(clust3_means)
names(clust3_means)[1]<-"% employment"
names(industry)[1]<-"Industry"
cbind(industry,clust3_means)


clust4_means <- as.data.frame(c(mean(cluster4$Agr),
                                mean(cluster4$Min),
                                mean(cluster4$Man),
                                mean(cluster4$PS),
                                mean(cluster4$Con),
                                mean(cluster4$SI),
                                mean(cluster4$Fin),
                                mean(cluster4$SPS),
                                mean(cluster4$TC)))
dim(clust4_means)
names(clust4_means)[1]<-"% employment"
names(industry)[1]<-"Industry"
cbind(industry,clust4_means)

#cluster 4 has no NA values for Agriculture


# Choosing the perfect k(elbow method) --------------------------------------------------

# function to compute total within-cluster sum of square ( intra-cluster variation )
set.seed(12345)

wss <- sapply(1:14, 
              function(k)
              {kmeans(Europe_emp_sample[2:10], k,iter.max = 15 )$tot.withinss})
?kmeans

wss
plot(1:14, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Prediction Strength -----------------------------------------------------

prediction.strength(Europe_emp_sample[2:10], Gmin=2, Gmax=14, M=10,cutoff=0.8)



# Hierachical clustering --------------------------------------------------

emp.dist=dist(Europe_emp_sample)
#Obtain clusters using the Wards method
emp.hclust=hclust(emp.dist, method="ward.D2")
plot(emp.hclust)
emp.3clust = cutree(emp.hclust,k=3)
emp.4clust = cutree(emp.hclust,k=2)
emp.5clust = cutree(emp.hclust,k=4)

par(mfrow=c(1,2))
plot(emp.hclust)
plotcluster(Europe_emp_sample[2:10], emp.5clust, main = "tree  cut at 4 clusters")

