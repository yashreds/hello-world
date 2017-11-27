
#TEXT MINING
 
tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
 
#To check the number of tweets that are negative
 
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)
 
#To Read each of the rows that contain text in a column
#Use corpus function
 
corpus=Corpus(VectorSource(tweets$Tweet))
"corpus[[1]]" gives the first row of the column tweets$Tweet
 
#To convert the text into lower case use
corpus=tm_map(corpus,tolower)
#To remove punctuation in the row containing text
corpus=tm_map(corpus,removePunctuation)
 
#To check the stopwords in a row(line)
#This is an inbuilt function that checks the trivial words(in databse by default)
#Here it checks the words that are in english from words 1 to 10
 
stopwords("english")[1:10]
 
#To Remove the stopwords and the word apple from the line
 
corpus=tm_map(corpus,removeWords, c("apple",stopwords("english")))
corpus[[1]]
 
#To Stem the words(removing the suffixes)
corpus=tm_map(corpus,stemDocument)
 
#Now generate a matrix where we have the tweets as rows and words in the rows as columns of the matrix
frequencies<-DocumentTermMatrix(corpus)
 
#Examining the words in the matrix#gives the distribution of each word belonging to the rows and column of the document matrix
inspect(frequencies[1000:1005,505:515])#Random values
 
#Find the most occuring words in the document  matrix
findFreqTerms(frequencies, lowfreq=20)
#calculates the words that occur 20 times in the matrix
 
#Further remove the unwanted words
sparse=removeSparseTerms(frequencies, 0.995)
#here it keeps those tweets that use 0.05% of the repeating words
 
#Now convert the matrix into a dataframe so we can do predicative analysis
tweetsSparse=as.data.frame(as.matrix(sparse))
#now even after converting into a dataframe there mit be some rows that start with a number
#so convert these rows that start with numbers into all alphabets
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
 
#Now convert the initial negative matrix into Sparsenegative matrix
tweetsSparse$Negative=tweets$Negative
 
#Now Build a tree model to examine the data. use rpart
library("caTools", lib.loc="C:/Users/xbblv5s/Documents/R/R-3.1.2/library")
set.seed(123)
split=sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse=subset(tweetsSparse,split==TRUE)
testSparse=subset(tweetsSparse,split==FALSE)
tweetCart=rpart(Negative~.,data=trainSparse,method="class")
prp(tweetCart)
#Examine data from tree..
 
#To check the accuracy of the data
predictCART=predict(emailCART,newdata=test,type="class")
table(test$responsive,predictCART)
#The sum of the first diagonal elements divided by the sum of all the elements in the matrix gives the accuracy of the model
