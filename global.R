library(tm) #text manipulation
library(gutenbergr) #project gutenberg api
library(tidytext) #text manipulation
library(tidyverse) #piping, filtering, etc
library(memoise) #caching

FILE_LOCATION="data/book_list.csv"

read_file <- function() {
  book_list <- read.csv(FILE_LOCATION)
  return(book_list)
}
 
get_level <- function(df, chosen_level) {
  
  if (chosen_level == "All") {
    df <- df %>%
      subset(select = -c(gutenberg_id, gutenberg_author_id, level, flesch_grade))
  }
  else {
    df <- df %>%
      filter(level == chosen_level) %>%
      subset(select = -c(gutenberg_id, gutenberg_author_id, level, flesch_grade))
  }
  df$flesch_value <- format(df$flesch_value, digits = 4)
  df$average_goodreads_rating <- format(df$average_goodreads_rating, digits = 3)
  return(df)
}

seek_and_get_book <- function(book_title) {
  
  books <- read_file()
  
  book <- books %>%
    filter(title == book_title)
  
  #saving the book data
  title <- book$title
  author <- book$author
  book_id <- book$gutenberg_id
  book_text <- gutenberg_download(book_id)$text
  
  #reading and formatting the text
  text <- strsplit(paste(book_text, collapse = " "), ' ')[[1]]
  text <- text %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    tolower()
  
  info <- data.frame(title = title, author = author, book_id = book_id)
  
  return(list(text, info))
}  

parse_book <- memoise(function(text, author, book_id, title) {
  
  #saving the number of words
  total_number_of_words <- length(text)
  
  #saving the number of pages
  total_number_of_pages <- ceiling(total_number_of_words / 250)
  
  #save the number of different words
  number_of_different_words <- nrow(data.frame(table(text)))
  
  total_unique_recurring_df <- data.frame()
  for (i in 1:total_number_of_pages) {
    #parse an increasing number of pages and put those words into a list
    words <- table(text[1:(i * 250)])
    
    #change the table into a data frame and add column names
    temporary_df <- data.frame(words)
    colnames(temporary_df)[1] <- "Words"
    colnames(temporary_df)[2] <- "Freq"
    
    #filter out empty spaces
    temporary_df <- filter(temporary_df, Words != " ")
    
    #filter out the words that occur only once in a given range from those recurring
    unique_temporary <- nrow(filter(temporary_df, Freq == 1))
    recurring_temporary <- nrow(filter(temporary_df, Freq > 1))
    
    #create a temporary data frame with results 
    temporary_df <-
      data.frame(uniques = unique_temporary, recurring = recurring_temporary)
    
    #attach it to the main frame
    total_unique_recurring_df <-
      rbind(total_unique_recurring_df, temporary_df)
  }
  
  stats <-
    data.frame(title = title,
               author = author,
               pages = total_number_of_pages,
               words = total_number_of_words,
               different_words = number_of_different_words,
               unique_words = total_unique_recurring_df$uniques[total_number_of_pages])
  
  return(list(
    total_unique_recurring_df,
    stats
  ))
})

calculate_words <- function(a_data_frame, a_start_page, an_end_page) {
  
  #only grab the part of df we need to seve as a graph
  a_data_frame <- a_data_frame[a_start_page:an_end_page,]
  
  number_of_pages <- an_end_page - a_start_page + 1
  
  #since we've only counted the total number of unique words in a book,
  #now we calculate how the number of unique words changes across the book
  uniques <-
    vector(mode = "numeric", length = number_of_pages)
  for (i in 1:(number_of_pages - 1)) {
    uniques[i] <- a_data_frame$uniques[i + 1] - a_data_frame$uniques[i]
  }
  
  #lets do the same for the recurring words
  recurrences <-
    vector(mode = "numeric", length = number_of_pages)
  for (i in 1:(number_of_pages - 1)) {
    recurrences[i] <-
      a_data_frame$recurring[i + 1] - a_data_frame$recurring[i]
  }
  
  #lets put that data into one data frame
  plot_df <-
    data.frame(
      Pages = a_start_page:(an_end_page),
      Uniques = uniques,
      Recurrences = recurrences
    )
  
  return(plot_df)
}

##wordcloud

create_1gram_df <- function(book_text_vector) {
  book_text_df <- data.frame(text=book_text_vector)
  book_text_df <- book_text_df %>%
    filter(text!="") #getting rid of the extra spaces
  book_1grams <- book_text_df %>%
    count(text, sort = TRUE)
  book_1grams_clean <- book_1grams %>%
    filter(!text %in% stop_words$word)
  colnames(book_1grams_clean) <- c("nngram", "n")
  return(book_1grams_clean)
}

create_2gram_df <- function(book_text_vector) {
  book_text_df <- data.frame(text=book_text_vector)
  book_text_df <- book_text_df %>%
    filter(text!="") #getting rid of the extra spaces
  book_2grams <- book_text_df %>%
    unnest_tokens(nngram, text, token="ngrams", n=2) %>%
    separate(nngram, c("word1", "word2", sep=" ")) %>%
    count(word1, word2, sort=TRUE)
  book_2grams_clean <- book_2grams %>%
    filter(!word1 %in% stop_words$word) %>% #removing stopwords
    filter(!word2 %in% stop_words$word) %>% #removing stopwords
    unite(nngram, word1, word2, sep=" ")
  return(book_2grams_clean)
}

create_3gram_df <- function(book_text_vector) {
  book_text_df <- data.frame(text=book_text_vector)
  book_text_df <- book_text_df %>%
    filter(text!="") #getting rid of the extra spaces
  book_3grams <- book_text_df %>%
    unnest_tokens(nngram, text, token="ngrams", n=3) %>%
    separate(nngram, c("word1", "word2","word3", sep=" ")) %>%
    count(word1, word2,word3, sort=TRUE)
  book_3grams_clean <- book_3grams %>%
    filter(!word1 %in% stop_words$word) %>% #removing stopwords
    filter(!word2 %in% stop_words$word) %>% #removing stopwords
    filter(!word3 %in% stop_words$word) %>%
    unite(nngram, word1, word2, word3, sep=" ")
  return(book_3grams_clean)
}