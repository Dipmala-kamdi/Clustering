library(readxl)

#airlines<- read_excel("D:/excelr_DS/assignment/Clustering/EastWestAirlines.xlsx")
air<- read_excel("D:/excelr_DS/assignment/Clustering/EastWestAirlines.xlsx", sheet = 2)
View(air)

library(data.table)
ncol(air)

############    Hierarchical clustering    ###############

#normalizing data
air_data<-scale(air[,2:12])
View(air_data)

# Distance matrix
air_d<-dist(air_data,method="euclidean") 
air_d

#### cluster dendrogrm with complete linkage ####
air_fit<-hclust(air_d,method="complete")
air_fit
summary(air_fit)

# draw Dendrogram
plot(air_fit)
plot(air_fit,hang=-1)

air_groups<-cutree(air_fit,k=5)
rect.hclust(air_fit,k=5,border="red")

air_final<-data.frame(air,air_groups)
View(air_final)

aggregate(air_final,by=list(air_final$air_groups), FUN = mean)


#### cluster dendrogrm with single linkage ####
air_fit_1<-hclust(air_d,method="single")
air_fit_1
summary(air_fit_1)

# draw Dendrogram
plot(air_fit_1)
plot(air_fit_1,hang=-1)

air_groups_1<-cutree(air_fit_1,k=5)
rect.hclust(air_fit_1,k=5,border="red")

air_final_1<-data.frame(air_final,air_groups_1)
View(air_final_1)

aggregate(air_final_1,by=list(air_final_1$air_groups_1.1), FUN = mean)

#### cluster dendrogrm with average linkage ####
air_fit_2<-hclust(air_d,method="average")
air_fit_2
summary(air_fit_2)

# draw Dendrogram
plot(air_fit_2)
plot(air_fit_2,hang=-1)

air_groups_2<-cutree(air_fit_2,k=5)
rect.hclust(air_fit_2,k=5,border="red")

air_final_2<-data.frame(air_final_1,air_groups_2)
View(air_final_2)

aggregate(air_final_2,by=list(air_final_2$air_groups_2), FUN = mean)

##################

#install.packages('cluster')
library(cluster)

hc1<- agnes(air, method = 'complete')
View(hc1)

#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.1")

#install.packages('mdendro')
#library(mdendro)
#library(rJava)

#.jinit()
#.jclassPath()
#.jaddclassPath('C:\\Users\\Adika\\Documents\\R\\win-library\\3.6\\rJava\\java')
#.jaddclassPath()
#.jnew('DirectedEdge')
#.jnew('DirectedEdge',1,2,0.1)
#.jnew("DirectedEdge", 1L, 2L, 0.1) 


?linkage
#lnk_1 <- linkage(air_d, method = "arithmetic", weighted = FALSE, digits = NULL)
#.jinit(".")
#.jnew("DirectedEdge", 1L, 2L, 0.1) 

#lnk_2 <- linkage(air_data, method = "arithmetic", weighted = FALSE, digits = NULL)
#plot(lnk_2)
#arithmetic, versatile, single, complete, geometric, harmonic, ward, centroid, flexible

########################    K-mean    ##################

air_kmean <- kmeans(air_data,5)
air_kmean
str(air_kmean)

air_kmean$centers

final_kmean<- cbind(air_final,air_kmean$cluster)
View(final_kmean)

aggregate(final_kmean, by=list(final_kmean$`air_kmean$cluster`), FUN = mean)

####

clus_loop <- numeric(20)

for(i in 1:20){
  #Extract total clus_loop for i-cluster solution
  clus_loop[i] <- kmeans(air_data, centers = i)$tot.withinss  }

cbind(No.of.Cluters=1:20, clus_loop)

plot(1:20, clus_loop, type="b", xlab = "No. of clusters", ylab = "Total clus_loop", main = "Scree Plot")

