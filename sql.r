# QUERYING USING SQL(SQLDF):
#Query the data and copy the data with headers in a spreadsheet and work on the sheet
apm2<- read.csv("sample.csv")eri2<- sqldf("SELECT Mnemonic,count(Mnemonic) FROM apm2
             group by Mnemonic")
 
 
#Connecting to live database:
vDriver <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/Data/vertica-jdbc-7.1.0-0/vertica-jdbc-7.1.0-0.jar")
vertica <- dbConnect(vDriver, "jdbc:vertica://r71ap10:5433/vrtxxpgdp","appucmr","U8ll0n!1")
                    
 
library(sqldf)
library(RSQLite
   
# Fetch data from database directly by mentioning the type of the database
s<-dbGetQuery(vertica,"select * from vw_dp_ucm_data limit 10")
#Create a dataframe from the data retrieved
sqldf("select * from s")
 
 
#Remove column from dataframe
dat <- dat[,c("col1","col2",...)]
 
#Use Pie Chart using ggplot
ggplot(Store,
       +        aes(x = factor(""), fill = Province) ) +
  +     geom_bar() +
  +     coord_polar(theta = "y")
+ scale_x_discrete("")
 
#Use Stacked Barchart using ggplot(one variable/column)
ggplot(Store,  aes(x = "Segment Type", fill = Segment) ) +
  +     geom_bar()
 
#Use separate Bar Charts using ggplot(one field/column)
ggplot(Store,  aes(Segment,fill=Segment))+
  +     geom_bar()
 
#Use separate Bar charts using ggplot(two fields/column comparison)
ggplot(Store,  aes(Ship_Mode,fill=Segment))+
  +     geom_bar(position="stack")
 
ggplot(Store,  aes(Ship_Mode,fill=Segment))+
  +     geom_bar(position="dodge")