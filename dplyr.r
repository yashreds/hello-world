#When we have data that spans for about 1 million rows, we can always convert it into
#a tbl so we can vieww all of it in a single window
# Both the dplyr and hflights packages are loaded

# Convert the hflights data.frame into a hflights tbl
hflights<-tbl_df(hflights)

# Display the hflights tbl
hflights

# Create the object carriers
carriers<-hflights$UniqueCarrier

#Some useful functions in DPLYR
# select,mutate,filter,
# both hflights and dplyr are available

# Finish select call so that ex1d matches ex1r
ex1r <- hflights[c("TaxiIn", "TaxiOut", "Distance")]
ex1d <- select(hflights, contains("Taxi"), Distance)

# Finish select call so that ex2d matches ex2r
ex2r <- hflights[c("Year", "Month", "DayOfWeek", "DepTime", "ArrTime")]
ex2d <- select(hflights, Year:ArrTime, -DayofMonth)

# Finish select call so that ex3d matches ex3r
ex3r <- hflights[c("TailNum", "TaxiIn", "TaxiOut")]
ex3d <- select(hflights, starts_with("T"))

#Next we use mutate() to calculate some additional useful functions for the records of data we have
# hflights and dplyr are loaded and ready to serve you.
#MUTATE
# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.
g1<-mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)

# Add the new variable GroundTime to g1. Save the result as g2.
g2<-mutate(g1,GroundTime=TaxiIn+TaxiOut)

# Add the new variable AverageSpeed to g2. Save the result as g3.
g3<-mutate(g2,AverageSpeed=(Distance/AirTime * 60))

# Print out g3
g3

# Add a second variable loss_ratio to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay,loss_ratio=(loss/DepDelay))

# Add the three variables as described in the third instruction: m2
m2<- mutate(hflights,TotalTaxi=TaxiIn+TaxiOut,ActualGroundTime=ActualElapsedTime-AirTime,Diff=TotalTaxi-ActualGroundTime)

#FILTER
# hflights is at your disposal as a tbl, with clean carrier names

# All flights that traveled 3000 miles or more
filter(hflights, Distance >= 3000)

# All flights flown by JetBlue, Southwest, or Delta
filter(hflights, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))

# All flights where taxiing took longer than flying
filter(hflights, TaxiIn + TaxiOut > AirTime)

#Arrange
#Used to arrange obserations based on the second argument in the desc order by default
# dplyr and the hflights tbl are available

# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
arrange(dtc, DepDelay)

# Arrange dtc so that cancellation reasons are grouped
arrange(dtc, CancellationCode)

# Arrange dtc according to carrier and departure delays
arrange(dtc, UniqueCarrier, DepDelay)

#Summarise
#Creates a new data from a dataset
# Print out a summary with variables min_dist and max_dist
summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))

# Print out a summary with variable max_div
summarise(filter(hflights, Diverted == 1), max_div = max(Distance))

# hflights is available

# Remove rows that have NA ArrDelay: temp1
temp1<-filter(hflights,!is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise(temp1,earliest=min(ArrDelay),average=mean(ArrDelay),latest=max(ArrDelay),sd=sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2<-filter(hflights,!is.na(TaxiIn),!is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarise()
summarise(temp2,max_taxi_diff=max(abs(TaxiIn-TaxiOut)))

#PIPED VERSION
# hflights and dplyr are both loaded and ready to serve you

# Write the 'piped' version of the English sentences.
hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff))
# Chain together mutate(), filter() and summarise()
hflights %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
  filter(!is.na(mph), mph < 70) %>%
  summarise(n_less = n(), 
            n_dest = n_distinct(Dest), 
            min_dist = min(Distance), 
            max_dist = max(Distance))




