library(tm)          #text manipulation
library(gutenbergr)  #project gutenberg api
library(tidytext)    #text manipulation
library(tidyverse)   #piping, filtering, etc
library(memoise)     #caching
library(data.table)  #rbindlist

kFileLocation <- "data/book_list.csv"
kWordsPerPage <- 250

# Reads the data
ReadData <- function() {
  book.list <- read.csv(kFileLocation)
  return(book.list)
}

# Returns the df from the data corresponding to the user's level
GetUserLevel <- function(df, chosen.level) {
  if (chosen.level == "All") {
    df <- df %>%
      subset(select = -c(gutenberg.id, gutenberg.author.id, level, flesch.grade))
  } else {
    df <- df %>%
      filter(level == chosen.level) %>%
      subset(select = -c(gutenberg.id, gutenberg.author.id, level, flesch.grade))
  }
  df$flesch.value <- format(df$flesch.value, digits = 4)
  df$average.goodreads.rating <- format(df$average.goodreads.rating, digits = 3)
  return(df)
}

# Returns the book text and information about it
GetBook <- function(book.title) {
  
  books <- ReadData()
  book <- books %>%
    filter(title == book.title)
  
  # Saves the book data
  title <- book$title
  author <- book$author
  book.id <- book$gutenberg.id
  book.text <- gutenberg_download(book.id)$text
  
  # Reads and formats the text
  text <- strsplit(paste(book.text, collapse = " "), split = ' ')[[1]]
  text <- text %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    tolower()
  
  info <- data.frame(title = title, author = author, book.id = book.id)
  
  return(list(text, info))
}  

ParseBook <- memoise(function(text, author, book.id, title) {
  
  total.number.of.words <- length(text)
  total.number.of.pages <- ceiling(total.number.of.words / kWordsPerPage)
  number.of.different.words <- nrow(data.frame(table(text)))
  
  total.unique.recurring.df <- data.frame()
  #TODO: improve the speed by replacing rbind with sth faster
  for (i in 1:total.number.of.pages) {
    words <- table(text[1:(i * kWordsPerPage)])
    temporary.df <- data.frame(words)
    colnames(temporary.df)[1] <- "Words"
    colnames(temporary.df)[2] <- "Freq"
    temporary.df <- temporary.df %>%
      filter(Words != " ")  # Filters out empty spaces
    unique.temporary <- nrow(filter(temporary.df, Freq == 1)) # Filters out the words that occur only once 
    recurring.temporary <- nrow(filter(temporary.df, Freq > 1)) #Filters out the words that reoccur
    temporary.df <- data.frame(uniques = unique.temporary, 
                               recurring = recurring.temporary)
    total.unique.recurring.list <- list(total.unique.recurring.df, temporary.df)
    total.unique.recurring.df <- rbindlist(total.unique.recurring.list, use.names = TRUE)
  }
  
  stats <- data.frame(title = title,
                      author = author,
                      pages = total.number.of.pages,
                      words = total.number.of.words,
                      different.words = number.of.different.words,
                      unique.words = total.unique.recurring.df$uniques[total.number.of.pages])
  
  return(list(
    total.unique.recurring.df,
    stats
  ))
})

CalculateWords <- function(a.data.frame, a.start.page, an.end.page) {
  # Only grabs the part of df we need to save as a graph
  a.data.frame <- a.data.frame[a.start.page:an.end.page, ]
  number.of.pages <- an.end.page - a.start.page + 1
  
  # Calculates how the number of unique words changes across the book
  uniques <- vector(mode = "numeric", 
                    length = number.of.pages)
  for (i in 1:(number.of.pages - 1)) {
    uniques[i] <- (a.data.frame$uniques[i + 1] - a.data.frame$uniques[i])
  }
  
  # Calculates how the number of recurring words changes across the book
  recurrences <- vector(mode = "numeric", 
                        length = number.of.pages)
  for (i in 1:(number.of.pages - 1)) {
    recurrences[i] <- (a.data.frame$recurring[i + 1] - a.data.frame$recurring[i])
  }

  plot.df <- data.frame(Pages = a.start.page:(an.end.page),
                        Uniques = uniques,
                        Recurrences = recurrences)
  
  return(plot.df)
}

Create1Gram <- function(book.text.vector) {
  book.text.df <- data.frame(text = book.text.vector)
  book.text.df <- book.text.df %>%
    filter(text != "") # Gets rid of the extra spaces
  book.1grams <- book.text.df %>%
    count(text, sort = TRUE)
  book.1grams.clean <- book.1grams %>%
    filter(!text %in% stop_words$word)
  colnames(book.1grams.clean) <- c("nngram", "n")
  return(book.1grams.clean)
}

Create2Gram <- function(book.text.vector) {
  book.text.df <- data.frame(text = book.text.vector)
  book.text.df <- book.text.df %>%
    filter(text != "") # Gets rid of the extra spaces
  book.2grams <- book.text.df %>%
    unnest_tokens(nngram, text, token = "ngrams", n = 2) %>%
    separate(nngram, c("word1", "word2", sep = " ")) %>%
    count(word1, word2, sort = TRUE)
  book.2grams.clean <- book.2grams %>%
    filter(!word1 %in% stop_words$word) %>%  # Removes stopwords in word1
    filter(!word2 %in% stop_words$word) %>%  # Removes stopwords in word2
    unite(nngram, word1, word2, sep = " ")
  return(book.2grams.clean)
}

Create3Gram <- function(book.text.vector) {
  book.text.df <- data.frame(text = book.text.vector)
  book.text.df <- book.text.df %>%
    filter(text != "") # Gets rid of the extra spaces
  book.3grams <- book.text.df %>%
    unnest_tokens(nngram, text, token = "ngrams", n = 3) %>%
    separate(nngram, c("word1", "word2","word3", sep = " ")) %>%
    count(word1, word2,word3, sort = TRUE)
  book.3grams.clean <- book.3grams %>%
    filter(!word1 %in% stop_words$word) %>%  # Removes stopwords in word1
    filter(!word2 %in% stop_words$word) %>%  # Removes stopwords in word2
    filter(!word3 %in% stop_words$word) %>%  # Removes stopwords in word3
    unite(nngram, word1, word2, word3, sep = " ")
  return(book.3grams.clean)
}