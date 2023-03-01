library(tm)
library(NLP)
library(tidyverse)
library(SnowballC)

# Exploratory Analysis
reviews <- read.csv("../input/animal-crossing/user_reviews.csv")
str(reviews)

reviews$Sentiment <- ifelse(reviews$grade > 5, 'Positive', 'Negative')
table(reviews$Sentiment)
median(reviews$grade)
ggplot(reviews %>% 
         count(grade) %>% 
         mutate(sentiment = ifelse(grade > 5, 'Positive', 'Negative')), 
       aes(grade, n, fill = sentiment)) + geom_bar(stat="identity") + theme_classic()
       
# Let's visualise the time series trend of the average daily grade: Reviews started out positively, but had a general decline afterwards.
reviews$date <- as.Date(reviews$date)
avg_grade <- aggregate(reviews$grade, by=list(reviews$date), mean)
colnames(avg_grade) <- c('Date', 'Average.Daily.Grade')
ggplot(avg_grade, aes(x=Date, y=Average.Daily.Grade)) + geom_line() + geom_smooth(method = "lm", formula = y ~x, se = FALSE)  + theme_classic()

# Text Analytics
corpus <- Corpus(VectorSource(reviews$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus , removePunctuation)
corpus <- tm_map(corpus,removeWords , c("game", "animal", "crossing", stopwords("english")))
corpus <- tm_map(corpus , stemDocument)
corpus <- DocumentTermMatrix(corpus)

inspect(corpus[1000:1005,505:515])

sparse = removeSparseTerms(corpus, 0.9)
inspect(sparse)

sparse_df = as.data.frame(as.matrix(sparse))
colnames(sparse_df) = make.names(colnames(sparse_df))
sparse_df$Sentiment = reviews$Sentiment
negative <- subset(sparse_df, Sentiment == 'Negative')
negative$Sentiment <- NULL
positive <- subset(sparse_df, Sentiment == 'Positive')
positive$Sentiment <- NULL

# What are reviewers mainly talking about?

#Top words from Negative Reviews
neg_output <- as.data.frame(t(negative))
neg_output$total_count <- rowSums(neg_output)
neg_output <- neg_output['total_count']
neg_output$word <- rownames(neg_output)
neg_output %>% 
  arrange(desc(total_count)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(word, total_count), y=total_count))+
  geom_bar(stat='identity') + coord_flip() + xlab("") + ylab("Frequency") + theme_classic() + ggtitle("Top 10 Words from Negative Reviews")
  
#Top words from Positive Reviews
pos_output <- as.data.frame(t(positive))
pos_output$total_count <- rowSums(pos_output)
pos_output <- pos_output['total_count']
pos_output$word <- rownames(pos_output)
pos_output %>% 
  arrange(desc(total_count)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(word, total_count), y=total_count))+
  geom_bar(stat='identity') + coord_flip() + xlab("") + ylab("Frequency") + theme_classic() + ggtitle("Top 10 Words from Positive Reviews")
  
#Hierarchical Clustering: Majority of the negative complains are regarding the game being 'one console per island'
hc_neg <- hclust(d = dist(t(negative), method = "euclidean"), method = "complete")
plot(hc_neg)

hc_pos <- hclust(d = dist(t(positive), method = "euclidean"), method = "complete")
plot(hc_pos)
