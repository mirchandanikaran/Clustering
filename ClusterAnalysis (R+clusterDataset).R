install.packages("dplyr")
install.packages("ggplot2")
install.packages("")
library(dplyr)
library(ggplot2)

#===============================================================================
#                             CLUSTER ANALYSIS
#===============================================================================
getwd()
describe(mydata)


# Data 
#-------------
mydata <- read.csv("cluster.csv", header= TRUE)
mydata
View(mydata)
str(mydata)
names(mydata)
head(mydata)
pairs(mydata)

# Euclidean Distance
#-----------------

d=dist(mydata)
d

x = hclust(d, method="complete") #Complete hierarchichal clustering method
?hclust() # To view available clustering methods


# Plotting the number of clusters (number of clusters are specified manually)

# select plot and rect.hclust lines together

plot(x)
rect.hclust(x,2)     # 2 clusters

plot(x)
rect.hclust(x,3)     # 3 clusters

plot(x)
rect.hclust(x,4)     # 4 clusters

plot(x)
rect.hclust(x,5)     # 5 clusters


# Scatter plot 
plot(mydata$x1~ mydata$x2, data = mydata)
with(mydata,text(mydata$x1 ~ mydata$x2, labels=mydata$name,pos=3))


# Normalizing the data
#---------------------

z = mydata[,c(2:7)]
z
summary(z)
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)
nor

# Calculate distance matrix (default is Euclidean distance)
#----------------------------------------------------------

distance = dist(nor)
print(distance,digits=2)


# Hierarchical agglomerative clustering using default complete linkage
#---------------------------------------------------------------------

mydata.hclust = hclust(distance)
plot(mydata.hclust)
##If company names are available
plot(mydata.hclust,labels=mydata$name,main='Default from hclust')
plot(mydata.hclust,hang=-1)


# Hierarchical agglomerative clustering using "average" linkage
#--------------------------------------------------------------
mydata.hclust.complete <-hclust(distance,method="complete")
plot(mydata.hclust.complete ,hang=-1)

mydata.hclust.average <-hclust(distance,method="average")
plot(mydata.hclust.average,hang=-1)

# Cluster membership
#-------------------
member1 = cutree(mydata.hclust.complete,4)
table(member1)

member2 = cutree(mydata.hclust.average,4)
table(member2)

table(member1, member2)

# Characterizing clusters
#------------------------

aggregate(nor,list(member1),mean)
aggregate(mydata[,c(2:7)],list(member1),mean)

# Silhouette plot
#----------------

library(cluster)
plot(silhouette(cutree(mydata.hclust.average,3),distance))
#( We try 2, 3, 4 number of clusters and we find that 3 is the optimum number 
#of clusters)


# Scree Plot
#-----------

wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of 
     squares") 
# (Scree Plot also tells that we have 3 clusters (something similar to the 
#    Elbow Method))


# K-means clustering
#-------------------
kc<-kmeans(nor,3)
kc
kc$cluster
table(kc$cluster)
barplot(table(kc$cluster))
pie(table(kc$cluster))

kc$centers

kc$totss
kc$withinss
kc$betweenss

plot(x1~x2, mydata, col=kc$cluster)
