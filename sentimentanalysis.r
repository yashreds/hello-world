#There are several different sentiment lexicons available for sentiment analysis. 
#From tidytext package these are a few

#"afinn" from Finn Årup Nielsen,
#"bing" from Bing Liu and collaborators, and
#"nrc" from Saif Mohammad and Peter Turney.

# Load dplyr and tidytext
library(dplyr)
library(tidytext)
# Choose the bing lexicon
get_sentiments("bing")
# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment
# geocoded_tweets has been pre-defined
geocoded_tweets
# Access bing lexicon: bing
bing <- get_sentiments("bing")
# Use data frame with text data
geocoded_tweets%>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)

# tweets_nrc has been pre-defined
tweets_nrc
tweets_nrc %>%
  # Filter to only choose the words associated with sadness
  filter(sentiment=="sadness") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarise(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))


# tweets_bing has been pre-defined
tweets_bing

tweets_bing %>% 
  # Group by two columns: state and sentiment
  group_by(state,sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarise(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio =  positive/negative ,
         state = reorder(state, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(state,ratio)) +
  # Make a plot with points using geom_point()
  geom_point() +
  coord_flip()

# Load tidytext
library(tidytext)

tidy_shakespeare <- shakespeare %>%
  # Group by the titles of the plays
  group_by(title) %>%
  # Define a new column linenumber
  mutate(linenumber=row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text) %>%
  ungroup()

# Pipe the tidy Shakespeare data frame to the next line
tidy_shakespeare %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)

sentiment_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing"))%>%
  # Count the number of words by title, type, and sentiment
  count(title,type,sentiment)

sentiment_counts %>%
  # Group by the titles of the plays
  group_by(title) %>%
  # Find the total number of words in each play
  mutate(total = sum(n),
         # Calculate the number of words divided by the total
         percent = n/total) %>%
  # Filter the results for only negative sentiment
  filter(sentiment=="negative") %>%
  arrange(percent)

word_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word,sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word,n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()


#TV STATION DATA
tv_sentiment %>%
  # Filter for only negative words
  filter(sentiment=="negative") %>%
  # Count by word and station
  count(word,station) %>%
  # Group by station
  group_by(station) %>%
  # Take the top 10 words for each station
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, station, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word,n,fill=station)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ station, nrow = 2, scales = "free") +
  coord_flip()


#SONGS MINING
# Load the tidytext package
library(tidytext)

# Pipe song_lyrics to the next line
tidy_lyrics <- song_lyrics %>% 
  # Transform the lyrics column to a word column
  unnest_tokens(word,lyrics)

# Print tidy_lyrics
tidy_lyrics

totals <- tidy_lyrics %>%
  # Count by song to find the word totals for each song
  count(song) %>%
  # Rename the new column
  rename(total_words = n)

# Print totals
totals

lyric_counts <- tidy_lyrics %>%
  # Combine totals with tidy_lyrics using the "song" column
  left_join(totals, by = "song")


lyric_sentiment <- lyric_counts %>%
  # Implement sentiment analysis with the "nrc" lexicon
  inner_join(get_sentiments("nrc"))

lyric_sentiment %>%
  # Find how many sentiment words each song has
  count(song, sentiment, sort = TRUE)

# What songs have the highest proportion of negative words?
lyric_sentiment %>%
  # Count using three arguments
  count(song,sentiment,total_words) %>%
  ungroup() %>%
  # Make a new percent column with mutate 
  mutate(percent=(n/total_words)) %>%
  # Filter for only negative words
  filter(sentiment=="negative") %>%
  # Arrange by descending percent
  arrange(desc(percent))

# What songs have the highest proportion of positive words?
lyric_sentiment %>%
  count(song,sentiment,total_words) %>%
  ungroup() %>%
  mutate(percent=(n/total_words)) %>%
  filter(sentiment=="positive") %>%
  arrange(desc(percent))


#MODEL NEGATIVE AND POSITIVE WORDS

negative_by_year <- lyric_sentiment %>%
  # Filter for negative words
  filter(sentiment=="negative") %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  # Define a new column: percent
  mutate(percent=(n/total_words))

# Specify the model with percent as the response and year as the predictor
model_negative <- lm(percent ~ year, data = negative_by_year)

# Use summary to see the results of the model fitting
summary(model_negative)

positive_by_year <- lyric_sentiment %>%
  # Filter for negative words
  filter(sentiment=="positive") %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  # Define a new column: percent
  mutate(percent=(n/total_words))

# Specify the model with percent as the response and year as the predictor
model_positive <- lm(percent ~ year, data = positive_by_year)

# Use summary to see the results of the model fitting
summary(model_positive)

#SO based on the models we can conclude that the positive words
#are decreasing over time