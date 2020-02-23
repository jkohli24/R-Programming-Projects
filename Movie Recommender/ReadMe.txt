MSIS2506 R Programming Final Project Movie Recommender

You may find it here:
https://jocelynliang.shinyapps.io/MovieRecommender/

Idea of the App:
Visualize the whole picture of our dataset by two graphs
Recommend the best 8 movies through analyzing user’s ratings for the other 80 movies

#update rlang to install devtools
install.packages("rlang")

# install devtool to install github libraries
install.packages("devtools")

# install ShinyRatingInput
devtools::install_github("stefanwilhelm/ShinyRatingInput")

# install the following packages
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyjs")
install.packages("plotly")

# Besides the packages mentioned above, please make sure you have the following packages
library(recommenderlab)
library(data.table)
library(Matrix)
library(slam)
library(tm)
library(wordcloud)
library(memoise)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)



