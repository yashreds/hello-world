pacman::p_load(pacman,tm,SnowballC,dplyr)

bookJE=readLines("JaneEyre.txt")

bookWH=readLines("WutheringHeights.txt")

corpusJE=Corpus(VectorSource(bookJE)) %>%
         tm_map(removePunctuation) %>%
         tm_map(removeNumbers) %>%
         tm_map(content_transformer(tolower)) %>%
         tm_map(removeWords,stopwords("English")) %>%
         tm_map(stripWhitespace) %>%
         tm_map(stemDocument)

tdmJE=DocumentTermMatrix(corpusJE) %>%
      removeSparseTerms(1-(5/length(corpusJE)))

word.freqJE<-sort(colSums(as.matrix(tdmJE)),decreasing=TRUE)
tableJE=data.frame(word=names(word.freqJE),absolute.frequency=word.freqJE,relative.frequency=word.freqJE/length(word.freqJE))

corpusWH=Corpus(VectorSource(bookWH))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeWords,stopwords("English"))%>%
  tm_map(stemDocument)

tdmWH=DocumentTermMatrix(corpusWH)%>%
  removeSparseTerms((1-(5/length(corpusWH))))

word.freqWH<-sort(colSums(as.matrix(tdmWH)),decreasing=TRUE)
tableWH=data.frame(word=names(word.freqWH),absolute.frequency=word.freqWH,relative.frequency=word.freqWH/length(word.freqWH))

head(tableWH)
