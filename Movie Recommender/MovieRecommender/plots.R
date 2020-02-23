# load libraries
library(tm)
library(wordcloud)
library(memoise)
library(lubridate)
library(dplyr)
library(ggplot2)

# load and manipulate data
db <- read.csv("movies_metadata.csv")
db$release_date <- ymd(db$release_date)
db$year <- year(db$release_date)
db_sub <- select(db,release_date,tagline) # data for wordcloud
db_sub2 <-select(db, vote_count, vote_average, year) # data for scatterplot
Before_40s <- filter(db_sub, release_date < "1940-01-01")
Before_60s <- filter(db_sub, release_date < "1960-01-01" & release_date >= "1940-01-01" )
Before_80s <- filter(db_sub, release_date < "1980-01-01" & release_date >= "1960-01-01" )
Before_00s <- filter(db_sub, release_date < "2000-01-01" & release_date >= "1980-01-01" )
Up_to_20s <- filter(db_sub, release_date < "2020-12-31" & release_date >= "2000-01-01" )
periods <- list("1870-1940" = "Before_40s","1940-1960" = "Before_60s","1960-1980" = "Before_80s","1980-2000" = "Before_00s","2000-2020" = "Up_to_20s")
periods_dict <- list ("Before_40s" = as.character(Before_40s$tagline), 
                      "Before_60s" = as.character(Before_60s$tagline), 
                      "Before_80s" = as.character(Before_80s$tagline), 
                      "Before_00s" = as.character(Before_00s$tagline), 
                      "Up_to_20s" = as.character(Up_to_20s$tagline))

# text mining and generate word cloud
getTermMatrix <- memoise(function(period) {
  text <- periods_dict[period]

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
         c(stopwords("SMART"), "the", "and", "but"))

  myDTM = TermDocumentMatrix(myCorpus,
              control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
