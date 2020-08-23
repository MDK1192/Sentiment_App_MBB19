#Functions for estimating the quality of Polarity analysis

library(sentimentr)
library(SentimentAnalysis)
library(tm)
library(dplyr)

#function for binary testing of polarity
get_polarity_evaluation_binary <- function(n = 25){
  
  
  data <- read.csv("SemiStruk_MBB19/Dataset1.csv")
  
  
  #start to clean data using a NLP-Pipeline approach
  data <- data.frame("Text"= data$review, "polarity" = data$sentiment)
  
  #Take care of non-UTF-8 Characters, replace them with ''
  iconv(data, "UTF-8", "UTF-8",sub='')
  data$Text <- as.character(data$Text)
  
  #iterate over textdata; remove twitterlike textelements ('@User123....'); transform text into UTF-8
  for(i in 1:nrow(data)){
    data$Text[i] <- gsub("@\\w+ *", "", data$Text[i])
    Encoding(data$Text[i]) <- "UTF-8"
  }
  
  #Normalize data (remove Punctuation, Numbers, Capitalization, "/()_-")
  data <- data.frame("Text" = as.character(removePunctuation(removeNumbers(tolower(data$Text)))),  "polarity" = data$polarity)
  data$Text <- as.character(data$Text)
  for(i in 1:nrow(data)){
    #remove https/http addresses from tweets
    data$Text[i] <- gsub("http\\w+ *", "", data$Text[i])
    data$Text[i] <- gsub("https\\w+ *", "", data$Text[i])
    #remove non-ASCII characters
    data$Text[i] <- gsub("[^\x01-\x7F]", "", data$Text[i])
    #remove stopwords
    # data$Text[i] <- tryCatch({rm_stopwords(data$Text[i],unlist = FALSE , separate = FALSE)},
    #                                 error=function(cond){
    #                                   Text <- "this tweet conflicts with utf8 and will be deleted"
    #                                   return(Text)})
  }
  
  data_pos_pop <- data[data$polarity == "positive",]
  data_neg_pop <- data[data$polarity == "negative",]
  sample_pos <- list()
  sample_neg <- list()
  polarity_pos <- list()
  polarity_neg <- list()
  vec_polarity_pos <- c(1:n)
  vec_polarity_neg <- c(1:n)
  for(i in 1:n){
    sample_pos[[i]] <- as.data.frame(data_pos_pop[sample(nrow(data_pos_pop), 100, replace = TRUE),])
    sample_neg[[i]] <- as.data.frame(data_neg_pop[sample(nrow(data_neg_pop), 100, replace = TRUE),])
    polarity_pos[[i]] <- analyzeSentiment(sample_pos[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    polarity_neg[[i]] <- analyzeSentiment(sample_neg[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    vec_polarity_pos[i] <- mean(polarity_pos[[i]]$SentimentGI)
    vec_polarity_neg[i] <- mean(polarity_neg[[i]]$SentimentGI)
  }
  
  print(paste0("durchschnittliche Polarität der positiven Reviews: ", mean(vec_polarity_pos)))
  print(paste0("durchschnittliche Polarität der negativen Reviews: ", mean(vec_polarity_neg)))
  
  t.test(vec_polarity_neg, vec_polarity_pos)
}

#function for multiclass testing of polarity
get_polarity_evaluation_multiscore <- function(n = 25){
  
  data <- read.csv("SemiStruk_MBB19/Dataset2.csv")

  
  #start to clean data using a NLP-Pipeline approach
  data <- data.frame("Text"= data$Review.Text, "rating" = data$Rating)
  
  #Take care of non-UTF-8 Characters, replace them with ''
  iconv(data, "UTF-8", "UTF-8",sub='')
  data$Text <- as.character(data$Text)
  
  #iterate over textdata; remove twitterlike textelements ('@User123....'); transform text into UTF-8
  for(i in 1:nrow(data)){
    data$Text[i] <- gsub("@\\w+ *", "", data$Text[i])
    Encoding(data$Text[i]) <- "UTF-8"
  }
  
  #Normalize data (remove Punctuation, Numbers, Capitalization, "/()_-")
  data <- data.frame("Text" = as.character(removePunctuation(removeNumbers(tolower(data$Text)))),  "rating" = data$rating)
  data$Text <- as.character(data$Text)
  for(i in 1:nrow(data)){
    #remove https/http addresses from tweets
    data$Text[i] <- gsub("http\\w+ *", "", data$Text[i])
    data$Text[i] <- gsub("https\\w+ *", "", data$Text[i])
    #remove non-ASCII characters
    data$Text[i] <- gsub("[^\x01-\x7F]", "", data$Text[i])
    #remove stopwords
    # data$Text[i] <- tryCatch({rm_stopwords(data$Text[i],unlist = FALSE , separate = FALSE)},
    #                                 error=function(cond){
    #                                   Text <- "this tweet conflicts with utf8 and will be deleted"
    #                                   return(Text)})
  }
  data_rating_1 <- data[data$rating == 1,]
  data_rating_2 <- data[data$rating == 2,]
  data_rating_3 <- data[data$rating == 3,]
  data_rating_4 <- data[data$rating == 4,]
  data_rating_5 <- data[data$rating == 5,]

  sample_rating_1 <- list()
  sample_rating_2 <- list()  
  sample_rating_3 <- list()
  sample_rating_4 <- list()
  sample_rating_5 <- list()
  
  polarity_rating_1 <- list()
  polarity_rating_2 <- list()  
  polarity_rating_3 <- list()
  polarity_rating_4 <- list()
  polarity_rating_5 <- list()
  
  vec_polarity_rating_1 <- c(1:n)
  vec_polarity_rating_2 <- c(1:n)
  vec_polarity_rating_3 <- c(1:n)
  vec_polarity_rating_4 <- c(1:n)
  vec_polarity_rating_5 <- c(1:n)
  
  for(i in 1:n){
    sample_rating_1[[i]] <- as.data.frame(data_rating_1[sample(nrow(data_rating_1), 100, replace = TRUE),])
    sample_rating_2[[i]] <- as.data.frame(data_rating_2[sample(nrow(data_rating_2), 100, replace = TRUE),])
    sample_rating_3[[i]] <- as.data.frame(data_rating_3[sample(nrow(data_rating_3), 100, replace = TRUE),])
    sample_rating_4[[i]] <- as.data.frame(data_rating_4[sample(nrow(data_rating_4), 100, replace = TRUE),])
    sample_rating_5[[i]] <- as.data.frame(data_rating_5[sample(nrow(data_rating_5), 100, replace = TRUE),])
    
    polarity_rating_1[[i]] <- analyzeSentiment(sample_rating_1[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    polarity_rating_2[[i]] <- analyzeSentiment(sample_rating_2[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    polarity_rating_3[[i]] <- analyzeSentiment(sample_rating_3[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    polarity_rating_4[[i]] <- analyzeSentiment(sample_rating_4[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    polarity_rating_5[[i]] <- analyzeSentiment(sample_rating_5[[i]]$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
    
    vec_polarity_rating_1[i] <- mean(polarity_rating_1[[i]]$SentimentGI, na.rm= T)
    vec_polarity_rating_2[i] <- mean(polarity_rating_2[[i]]$SentimentGI, na.rm= T)
    vec_polarity_rating_3[i] <- mean(polarity_rating_3[[i]]$SentimentGI, na.rm= T)
    vec_polarity_rating_4[i] <- mean(polarity_rating_4[[i]]$SentimentGI, na.rm= T)
    vec_polarity_rating_5[i] <- mean(polarity_rating_5[[i]]$SentimentGI, na.rm= T)
    
    }
  
  print(paste0("durchschnittliche Polarität der Reviews Stufe 5: ", mean(vec_polarity_rating_5)))
  print(paste0("durchschnittliche Polarität der Reviews Stufe 4: ", mean(vec_polarity_rating_4)))
  print(paste0("durchschnittliche Polarität der Reviews Stufe 3: ", mean(vec_polarity_rating_3)))
  print(paste0("durchschnittliche Polarität der Reviews Stufe 2: ", mean(vec_polarity_rating_2)))
  print(paste0("durchschnittliche Polarität der Reviews Stufe 1: ", mean(vec_polarity_rating_1)))
  
  df_plot <- cbind(vec_polarity_rating_1, vec_polarity_rating_2, vec_polarity_rating_3, vec_polarity_rating_4,vec_polarity_rating_5)
  boxplot(df_plot)
  df_1 <- data.frame(polarity = vec_polarity_rating_1, rating = as.factor(1))
  df_2 <- data.frame(polarity = vec_polarity_rating_2, rating = as.factor(2))
  df_3 <- data.frame(polarity = vec_polarity_rating_3, rating = as.factor(3))
  df_4 <- data.frame(polarity = vec_polarity_rating_4, rating = as.factor(4))
  df_5 <- data.frame(polarity = vec_polarity_rating_5, rating = as.factor(5))
  df_total <- rbind(df_1, df_2, df_3, df_4, df_5)
  summary(aov(df_total$polarity ~ df_total$rating))
  
}

#function for polarity based on different textlengths
get_polarity_sample_data <- function(){
  
  
  data <- readxl::read_xlsx("SemiStruk_MBB19/Dataset3.xlsx")
  
  #Take care of non-UTF-8 Characters, replace them with ''
  iconv(data, "UTF-8", "UTF-8",sub='')
  data$Text <- as.character(data$Text)
  
  #iterate over textdata; remove twitterlike textelements ('@User123....'); transform text into UTF-8
  for(i in 1:nrow(data)){
    data$Text[i] <- gsub("@\\w+ *", "", data$Text[i])
    Encoding(data$Text[i]) <- "UTF-8"
  }
  
  #Normalize data (remove Punctuation, Numbers, Capitalization, "/()_-")
  data <- data.frame("Text" = as.character(removePunctuation(removeNumbers(tolower(data$Text)))))
  data$Text <- as.character(data$Text)
  for(i in 1:nrow(data)){
    #remove https/http addresses from tweets
    data$Text[i] <- gsub("http\\w+ *", "", data$Text[i])
    data$Text[i] <- gsub("https\\w+ *", "", data$Text[i])
    #remove non-ASCII characters
    data$Text[i] <- gsub("[^\x01-\x7F]", "", data$Text[i])
    #remove stopwords
    # data$Text[i] <- tryCatch({rm_stopwords(data$Text[i],unlist = FALSE , separate = FALSE)},
    #                                 error=function(cond){
    #                                   Text <- "this tweet conflicts with utf8 and will be deleted"
    #                                   return(Text)})
  }
  
  polarity_scores <- analyzeSentiment(data$Text, language = "english", removeStopwords = TRUE, stemming = TRUE)
  data <- cbind(data, polarity_scores[,1:4])
}

#function for testing of sentiment function  
get_sentiment_sample_data <- function(){
  
  data <- read.csv("SemiStruk_MBB19/Dataset4.csv")
  
  #Take care of non-UTF-8 Characters, replace them with ''
  iconv(data, "UTF-8", "UTF-8",sub='')
  data$Text <- as.character(data$Tweets)
  
  #iterate over textdata; remove twitterlike textelements ('@User123....'); transform text into UTF-8
  for(i in 1:nrow(data)){
    data$Text[i] <- gsub("@\\w+ *", "", data$Text[i])
    Encoding(data$Text[i]) <- "UTF-8"
  }
  
  #Normalize data (remove Punctuation, Numbers, Capitalization, "/()_-")
  data <- data.frame("Text" = as.character(removePunctuation(removeNumbers(tolower(data$Text)))), "Sentiment_real"= data$Feeling)
  data$Text <- as.character(data$Text)
  for(i in 1:nrow(data)){
    #remove https/http addresses from tweets
    data$Text[i] <- gsub("http\\w+ *", "", data$Text[i])
    data$Text[i] <- gsub("https\\w+ *", "", data$Text[i])
    #remove non-ASCII characters
    data$Text[i] <- gsub("[^\x01-\x7F]", "", data$Text[i])
    #remove stopwords
    # data$Text[i] <- tryCatch({rm_stopwords(data$Text[i],unlist = FALSE , separate = FALSE)},
    #                                 error=function(cond){
    #                                   Text <- "this tweet conflicts with utf8 and will be deleted"
    #                                   return(Text)})
  }
  sentiment_text <- extract_emotion_terms(data$Text, un.as.negation = T)
  #data <- cbind(data, sentiment_text[,c(3,5,6,7,8,9)])
  #data <- na_if(data[,c(1:8)], "character(0)")
  sentiment <- arrange(attributes(sentiment_text)$elements, element_id)
  sentiment <- sentiment[sentiment$emotion_type %in% c("anger", "disgust", "fear","joy", "sadness", "surprise"),]
  emotions_result_vec <- data.frame("Sentiment_predicted" = c(1:nrow(sentiment_text)))
  emotions_result_vec$Sentiment_predicted <- NA
  for(i in 1:nrow(sentiment_text)){
    if(i %in% as.numeric(unique(sentiment$element_id))){
      tempdata <- sentiment[sentiment$element_id == i,]
      emotionscount <- table(tempdata$emotion_type)
      emotions_result_vec$Sentiment_predicted[i] <- names(which.max(emotionscount))
    }
  }
  data <- cbind(data, emotions_result_vec)
  data$Sentiment_predicted[data$Sentiment_predicted== "joy"] <- "happy"
  data$Sentiment_predicted[data$Sentiment_predicted== "sadness"] <- "sad"
  data$Sentiment_predicted[data$Sentiment_predicted== "anger"] <- "angry"

  browser()
}
