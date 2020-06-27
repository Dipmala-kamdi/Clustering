crime <- read.csv(file.choose())
View(crime)

data<- scale(crime[,2:5])
View(data)

# distance matrix
d_mat <- dist(data, method = "euclidean") 
d_mat

#### cluster dendrogrm with complete linkage ####

fit <- hclust(d_mat, method="complete")
fit
View(fit)

#dendrogram
plot(fit)
#draw clusters
rect.hclust(fit, k=3, border="red")
# cut tree into 3 clusters
groups <- cutree(fit, k=3) 
View(groups)

final_db<- data.frame(crime,groups)
View(final_db)

aggregate(crime[,-1],by=list(final_db$groups),mean)


#### cluster dendrogrm with single linkage ####

fit_1 <- hclust(d_mat, method="single")
fit_1
View(fit_1)

#dendrogram
plot(fit_1)
#draw clusters
rect.hclust(fit_1, k=3, border="red")
# cut tree into 3 clusters
groups_1 <- cutree(fit_1, k=3) 
View(groups_1)

final_db_1<- data.frame(final_db,groups_1)
View(final_db_1)

aggregate(crime[,-1],by=list(final_db_1$groups_1),mean)


#### cluster dendrogrm with average linkage ####

fit_2 <- hclust(d_mat, method="average")
fit_2
View(fit_2)

#dendrogram
plot(fit_2)
#draw clusters
rect.hclust(fit_2, k=3, border="red")
# cut tree into 3 clusters
groups_2 <- cutree(fit_2, k=3) 
View(groups_2)

final_db_2<- data.frame(final_db_1,groups_2)
View(final_db_2)

aggregate(crime[,-1],by=list(final_db_2$groups_2),mean)

####################### k mean ######################

crime_kmean <- kmeans(data,3)
crime_kmean
str(crime_kmean)

crime_kmean$centers

final_crime_kmean<- cbind(final_db,crime_kmean$cluster)
View(final_crime_kmean)

aggregate(final_crime_kmean, by=list(final_crime_kmean$`crime_kmean$cluster`), FUN = mean)

####

crime_loop <- numeric(20)

for(i in 1:20){
  #Extract total crime_loop for i-cluster solution
  crime_loop[i] <- kmeans(data, centers = i)$tot.withinss  }

cbind(No.of.Cluters=1:20, crime_loop)

plot(1:20, crime_loop, type="b", xlab = "No. of clusters", ylab = "Total crime_loop", main = "Scree Plot")



