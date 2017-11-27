#USING GGPLOT
 
who=read.csv("ggplot.csv")
#Normal plot is
plot(who$GNI,who$FertilityRate)
 
#USING GGPLOT
scatterplot=ggplot(who,aes(x=GNI,y=FertilityRate))
#To use a point wise representation of the plot use:
scatterplot+geom_point()
#To use a line wise representation of the plot use:
scatterplot+geom_line()
#To modify the plot by coloring the plot and changing the shape of it use:
scatterplot+geom_point(col='blue',size=7,shape=8)
 
 
#To print the plot to a file:
a=scatterplot+geom_point(col='blue',size=7,shape=8)
pdf(a)
 
 
#Advanced ggplots
ggplot(who,aes(x=GNI,y=FertilityRate,col=Region))+geom_point()
ggplot(who,aes(x=GNI,y=FertilityRate,col=LifeExpectancy))+geom_point()
 
#HEATMAP
a=ggplot(store,aes(x=Product_Base_Margin,y=Order_Priority))+geom_tile(aes(fill=Region))
 
#LINECHART
ggplot(who,aes(x=GNI,y=FertilityRate,col=LifeExpectancy))+geom_line()
 
#BAR CHART
a=ggplot(store,aes(Province,fill=Ship_Mode))+geom_bar()+scale_y_continuous(breaks=c(0,300,600,900,1200,1500,2000))
 
 
#MAPS USING GGMAPS
coimbatore=get_map(location="Coimbatore,zoom=11")
ggmap(coimbatore)