# This script finds the misspelled word in a string and replaces it with the most likely correct spelling of that word.
# works with numerous misspellings now. 

library(hunspell)
library(tidyverse)
library(tidytext)

# A string with a misspelling
just_text <- c("Here's a setence wwith sevvral sillly mispellings that dno't make anny sennse.")

# get the misspelled word
# also, this gives the for loop the correct number of iterations to perorm
bad <- hunspell(just_text)

# for loop replaces misspelled words one at a time, starting with the first one it finds
for (i in 1:length(bad[[1]])) {
  
  # need to redefine the list of misspelled words, because the first one is correct now
  bad <- hunspell(just_text)
  
  # change it to a dataframe
  df_just <- tibble(line = 1, text = just_text)
  
  # separates the sentence to a dataframe of words - each word is a row
  words <- df_just %>%
    unnest_tokens(word, text) %>%
    select(word)
  
  # get the most likely correct spelling
  correct <- hunspell_suggest(bad[[1]])
  
  # change corrs object to dataframe with correct spelling
  corr_frame <- as_tibble(correct[[1]][1]) %>%
    select(word = value)
  
  # replaced misspelled word in words dataframe with most likely correct spelling
  new_words <- words %>%
    mutate(word = ifelse(word == bad[[1]][1], paste0(corr_frame$word), word))
  
  # coerce new_words dataframe back to string with most likely correct spelling in place of originial misspelling
  fixed_string <- new_words %>%
    nest(word) %>%
    mutate(text = map(data, unlist),
           text = map_chr(text, paste, collapse = " ")) %>%
    select(text)
  
  # change the name of fixed_string back to just_text and set with as.character for the loop
  just_text <- as.character(fixed_string)
  
  
}

# take a look at your corrected sentence
View(just_text)
