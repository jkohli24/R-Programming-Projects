# ================================================================================
# This function is revised based on the source code provided by Philipp Spachtholz 
# https://github.com/pspachtholz/BookRecommender/blob/master/server.R
# ================================================================================

# read in data
movies <- read.csv('movies_metadata.csv')
ratings <- read.csv('ratings.csv')
movies$poster_path <- paste("http://image.tmdb.org/t/p/w185", movies$poster_path, sep = "", collapse = NULL)

# define functions
get_user_ratings <- function(value_list) {
  dat <- data.table(id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (id = as.numeric(id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$id, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1))
}

# reshape to movies x user matrix 
ratingmat <- sparseMatrix(ratings$id, ratings$userId, x=ratings$rating) # movie x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(id = as.character(1:163949), userId = as.character(sort(unique(ratings$userId))))
