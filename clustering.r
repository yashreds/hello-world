#Working with raw data as text file in R
movies=read.table("movielens.txt",header=FALSE,sep="|",quote="\"")
str(movies)
colnames(movies)=c("ID","Title","Release","Video","IMDB","Unknown","Action","Adventure","Animation","Children","Comedy","Crime","Documnetary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","Scifi","Thriller","War","Western")
#If we want to remove unneeded columns from the data
movies$ID=NULL
movies$Release=NULL
movies$Video=NULL
movies$IMDB=NULL
movies=unique(movies)
 
#USING HEIRARCHICAL CLUSTERING
distances=dist(movies[2:20],method="euclidean")
clusterMovies=hclust(distances,method="ward.D")
plot(clusterMovies)
 
#Grouping the clusters
clusterGroups=cutree(clusterMovies,k=10)
tapply(movies$Action,clusterGroups,mean)
#In order to compute the mean for multiple columns in order to group them to cluster group, use
aggregate(movies[,9:10],by=list(clusterGroups),mean)
 
 
#IMAGE CLUSTERING
flower=read.csv("flower.csv",header=FALSE)
#Here data is always raw and we dont have headers for the data.
#So, we convert it into a matrix first and then convert it into a vector
#And then do the heirarchical clustering
flowerMatrix=as.matrix(flower)
flowerVector=as.vector(flowerMatrix)
str(flowerVector)
distances=dist(flowerVector,method="euclidean")
clusterIntensity=hclust(distances,method="ward.D")
plot(clusterIntensity)
clusterGroups=cutree(clusterIntensity,k=10)
tapply(flowerVector,clusterGroups,mean)
dim(clusterGroups)=c(50,50)
image(clusterGroups,axes=FALSE)