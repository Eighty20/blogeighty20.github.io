

library(tidyverse)
library(tidytext)
library(stringr)
library(rvest)
library(lubridate)


## Scrape Yeats ##

Yeats <- function() {
  
  # function to scrape for poem and it's date using poem names 
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/y/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[3] %>%
      str_extract(pattern = "[0-9]+")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  bingAnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/y/yeats_w_b.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:50] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # hardcode 2 poem names to fit in with used url name
  
  poemName[9] <- "he mourns for the change"
  poemName[24] <- "the old men admiring themselves"
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # hardcode single date with error
  
  poemDataFrame$date[40] <- "1916"
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), bingAnalysePoems)
  
  # create data frame with data and scores
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01")))) 
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- list()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame[[i]] <- nrsDataFrame
  }
  
  # rbind all nrs analysis
  
  nrsAnalysisDataFrame <- do.call(rbind, nrsAnalysisDataFrame)
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}


## Scrape Eliot ##

Eliot <- function() {
  
  # function to scrape for poem and it's date using poem names
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/e/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[4] %>%
      str_extract(pattern = "[0-9]+")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  AnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/e/eliot_t_s.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:(length(poemName) - 2)] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # hardcode 2 poem names to fit in with used url name
  
  poemName[3] <- "love song of j alfred prufrock"
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), AnalysePoems)
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01")))) 
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- list()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame[[i]] <- nrsDataFrame
  }
  
  # rbind all nrs analysis
  
  nrsAnalysisDataFrame <- do.call(rbind, nrsAnalysisDataFrame)
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}

## Scrape for Frost ##

Frost <- function() {
  
  # function to scrape for poem and it's date using poem names
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/f/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[4] %>%
      str_extract(pattern = "[0-9]+")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  AnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/f/frost_robert.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:(length(poemName) - 2)] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # hardcode poem name to fit in with used url name
  
  poemName[10] <- "the_oft-repeated_dream"
  poemName[14] <- "range_finding"
  poemName[20] <- "the_wood-pile"
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), AnalysePoems)
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01")))) 
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- list()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame[[i]] <- nrsDataFrame
  }
  
  # rbind all nrs analysis
  
  nrsAnalysisDataFrame <- do.call(rbind, nrsAnalysisDataFrame)
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}

## Scrape for Wallace ##

Wallace <- function() {
  
  # function to scrape for poem and it's date using poem names
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/s/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[3] %>%
      str_extract(pattern = "[0-9]+")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  AnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/s/stevens_wallace.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:(length(poemName) - 2)] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # hardcode poem name to fit in with used url name
  
  poemName[1] <- "the_emperor_of_ice_cream"
  poemName[2] <- "a_high_toned_old_christian_woman"
  # poemName[20] <- "the_wood-pile"
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  poemDataFrame$date[3] <- "1921"
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), AnalysePoems)
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01"))))
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- list()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame[[i]] <- nrsDataFrame
  }
  
  # rbind all nrs analysis
  
  nrsAnalysisDataFrame <- do.call(rbind, nrsAnalysisDataFrame)
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}

## Scrape for Cummings ##

Cummings <- function() {
  
  # function to scrape for poem and it's date using poem names
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/c/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[4] %>%
      str_extract(pattern = "[0-9]{4}")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  AnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/c/cummings_e_e.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:(length(poemName) - 2)] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), AnalysePoems)
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01")))) 
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- data.frame()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame <- rbind(nrsAnalysisDataFrame, nrsDataFrame)
  }
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}

## Scrape for Lawrence ##

Lawrence <- function() {
  
  # function to scrape for poem and it's date using poem names
  
  GetPoemAndDate <- function(poemName) {
    
    nameVec <- unlist(str_split(poemName, pattern = " "))
    
    url <- str_c("http://www.poetry-archive.com/l/", 
                 paste(nameVec, collapse = "_"),
                 ".html") 
    
    poem <- url %>%
      read_html() %>%
      html_nodes("dl") %>%
      html_text() %>%
      str_replace_all(pattern = "\r", replacement = "") %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[  ]{2}", replacement = "")
    
    date <- url %>%
      read_html() %>%
      html_nodes("td font") %>%
      html_text() 
    
    date <- date[4] %>%
      str_extract(pattern = "[0-9]{4}")
    
    # pause before function return
    
    Sys.sleep(runif(1,0,1))
    
    return(list(poem = poem, date = date))
  }
  
  # function to analyse poems and return scores
  
  AnalysePoems <- function(i) {
    
    poem <- data_frame(poem = poemDataFrame$poem[i])
    
    textTokenized <- poem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    bing <- sentiments %>%
      filter(lexicon == "bing") %>%
      select(-score)
    
    poemSentiment <- tidyPoem %>%
      inner_join(bing) 
    
    poemSentiment <- poemSentiment %>%
      mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
    
    finalScore <- (sum(poemSentiment$score)/length(textTokenized$word))*10
    
    return(finalScore)
  }
  
  # nrs analysis function 
  
  nrcAnalysePoems <- function(sent) {
    
    textTokenized <- nrsPoem %>%
      unnest_tokens(word, poem)
    
    data("stop_words")
    
    tidyPoem <- textTokenized %>%
      anti_join(stop_words)
    
    nrcSentiment <- sentiments %>%
      filter(lexicon == "nrc", sentiment == sent)
    
    sentimentInPoem <- tidyPoem %>%
      semi_join(nrcSentiment) %>%
      count(word, sort = TRUE)
    
    return(sum(sentimentInPoem$n))
  }
  
  # page url
  
  poemNameUrl <- "http://www.poetry-archive.com/l/lawrence_d_h.html"
  
  # scrape for poem names
  
  poemName <- poemNameUrl %>%
    read_html() %>%
    html_nodes("a font") %>%
    html_text() 
  
  poemName <- poemName[1:(length(poemName) - 2)] %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "") %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    tolower()
  
  # get poems and dates
  
  poemDataFrame <- list()
  count <- 1
  
  for (name in poemName) {
    
    poemDataFrame[[count]] <- data.frame(poem = GetPoemAndDate(name)$poem, 
                                         date = GetPoemAndDate(name)$date,
                                         stringsAsFactors = FALSE)
    
    count <- count + 1
  }
  
  # rbind all poems and dates
  
  poemDataFrame <- do.call(rbind, poemDataFrame)
  
  # hardcode single date with error 
  
  poemDataFrame$date[3] <- 1916
  
  # create data frame of names, poems and dates
  
  poemDataFrame <- cbind(poemName, poemDataFrame)
  
  # get scores
  
  scores <- sapply(1:length(poemDataFrame$poem), AnalysePoems)
  
  dateAndScore <- data.frame(scores) %>%
    mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01")))) 
  
  # do nrs analysis
  
  sentimentsVec <- c("anger", "anticipation", "disgust", "fear",
                     "joy","sadness", "surprise", "trust")
  
  nrsAnalysisDataFrame <- list()
  
  for (i in 1:length(poemDataFrame$poem)) {
    
    nrsPoem <- data_frame(poem = poemDataFrame$poem[i])
    
    nrsDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>%
      rownames_to_column() %>%
      select(sentiment = rowname, value = 2) %>%
      mutate(name = poemDataFrame$poemName[i], 
             date = poemDataFrame$date[i])
    
    nrsAnalysisDataFrame[[i]] <- nrsDataFrame
  }
  
  # rbind all nrs analysis
  
  nrsAnalysisDataFrame <- do.call(rbind, nrsAnalysisDataFrame)
  
  # return data frames 
  
  return(list(dateAndScore, nrsAnalysisDataFrame))
}