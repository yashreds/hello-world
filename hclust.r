states=read.csv("ClusterData.csv",header=TRUE)
head(states)
summary(states)
str(states)
colnames(states)

#selecting only the numerical data for analysis
st<-states[,3:27]
row.names(st)=states[,2]
colnames(st)

#Selecting only sports features
sports=st[,8:11]
colnames(sports)

#Hierarchical clustering
plot(hclust(dist(sports)),main="Sports searches")
plot(hclust(dist(st)),main="Overall search")
