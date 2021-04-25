### Working with Words ####################################
# Working with stringr, regex, and tidytext
# make a wordcloud
# Created on 2021-04-19
# Created by Jamie Kerlin
###########################################################

### Load libraries ########################################
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)

### Intro to {stringr} ####################################

# Manipulation
# paste() function
paste("High temp", "Low pH") #paste two strings together
paste("High temp", "Low pH", sep = "-") #add separating dash
paste0("High temp", "Low pH") #paste with no space between
shapes <- c("Square", "Circle", "Triangle") #create vector
paste("My favorite shape is a", shapes) #use paste with vector
two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")
#Note: look up the expressions function to paste symbols
#for equations and things

# Individual characters
shapes #vector of shapes
str_length(shapes) #how many letters are in each word
seq_data <- c("ATCCCGTC") #create sequence string
str_sub(seq_data, start = 2, end = 4) #extract the 2nd to 4th AA
str_sub(seq_data, start = 3, end = 3) <- "A" #add A in 3rd position
str_sub(seq_data, times = c(2, 3)) #time is number of times to duplicate

# Whitespace
# Ex. have column and did not copy and paste treatments- have typos
# with extra white spaces
badtreatments <- c("High", " High", "High ", "Low", "Low ")
str_trim(badtreatments) #removes white space
str_trim(badtreatments, side = "left") #removes whitespace on left
str_pad(badtreatments, 5, side = "right") #add whitespace to right if less than 5 characters
str_pad(badtreatments, 5, side = "right", pad = "1") #add 1 to right if less than 5 characters

# Locale sensitive
# Can specify which language you want
# Can say if you want everything uppercase or lowercase
x <- "I Love R!"
str_to_upper(x) #make uppercase
str_to_lower(x) #make lowercase
str_to_title(x) #make title case

# Pattern matching
# Detect based on specific patterns
data <- c("AAA", "TATA", "CTAG", "GCTT")
str_view(data, pattern = "AT")
str_detect(data, pattern = "AT")
str_locate(data, pattern = "AT")

### Regex ####################################################
# Regular expressions
# Metacharacters- use \\ to escape from R seeing it as other meaning
vals <- c("a.b", "b.c", "c.d")
str_replace(vals, "\\.", " ") #searches for period, replaces with space
vals <- c("a.b.c", "b.c.d", "c.d.e")
str_replace(vals, "\\.", " ") #only replaces the first period
str_replace_all(vals, "\\.", " ") #replaces all periods

# Sequences
val2 <- c("test 123", "test 456", "test")
str_subset(val2, "\\d") #searches for strings with digits

# Character class
# list of characters enclosed by square brackets
str_count(val2, "[aeiou]") #counts number of vowels in each string
str_count(val2, "[0-9]") #counts number of digits in each string

# Quantifiers
strings <- c("550-153-7578",
         "banana",
         "435.114.7586",
         "home: 672-442-6739")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_detect(strings, phone) #which strings contain phone numbers?
test <- str_subset(strings, phone)

# Replace all the "." with "-" and extract only numbers (leave letters behind)
# Remove any extra white space. Can use a sequence of pipes"

test %>%
  str_replace_all("\\.", "-") %>%
  str_trim() %>%
  str_replace_all(pattern = "\\:",  replacement = "") %>%
  str_replace_all(pattern = "home ", "")

test %>%
  str_replace_all("\\.", "-") %>%
  str_replace_all(pattern = "[a-zA-z]\\:", replacement = "") %>%
  str_trim
  
# Tidytext ##################################################
# Practicing text mining of jane austen books

head(austen_books())

original_books <- austen_books() %>%
  group_by(book) %>% 
  mutate(line = row_number(), #find every line
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]", #start with chapter and have roman numeral
                                           ignore_case = TRUE)))) %>%
           ungroup()
head(original_books)         

#Want to clean so there is only one word per row
tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) #add column named word, with the input as the text column

head(tidy_books)

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) #dataframe without the stopwords

cleaned_books %>%
  count(word, sort = TRUE)

# Sentiment analysis
# Identify how many positive or negative words
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>%
  count(word, sentiment, sort = TRUE)
head(sent_word_counts)

# Plot to visualize positive and negative words in books
sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it gows from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

# Make word cloud
words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:50) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words

