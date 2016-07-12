
preprocess_matrix <- trainSet_matrix

aCos.sim <- function(vector1, vector2){
  ###function to calculate cosine similarity
  ###pairwise matching is applied first
  x <- vector1[!is.na(vector2)]
  x <- x[!is.na(x)]
  
  y <- vector2[!is.na(vector1)]
  y <- y[!is.na(y)]
  
  return(sum(x*y)/sqrt(sum(x^2)*sum(y^2)))
  
}

### use adjusted cosine measurement
### data pre-process for item-base
### calculate mean rating value for each user and centralize all rating values
### NA values remain
for(i in 1:nrow(preprocess_matrix)){
  preprocess_matrix[i,] <- preprocess_matrix[i,] - mean(preprocess_matrix[i,][!is.na(preprocess_matrix[i,])])
}


### pick out an item
movieId <- cNames
similarities <- NA
rate_match_times <- NA 
movie_similarities <- data.frame(movieId, similarities, rate_match_times)


#start use case for item-based 

#obtain the movie for test
#here use the first movie for test
test_movie_vector <- preprocess_matrix[,1]
test_movie_id <- colnames(preprocess_matrix)[1]


#calculate adjusted cosine measurement
for (i in 1:ncol(preprocess_matrix)) {
  #obtain the user id for this time of loop
  id <- colnames(preprocess_matrix)[i]
  #find how many users rate this two movies in common
  temp <- preprocess_matrix[,i][!is.na(test_movie_vector)]
  match_times  <- length(temp[!is.na(temp)])
  #set the match times in case of "fake correlation"
  movie_similarities$rate_match_times[movie_similarities$movieId == id] <- match_times
  #calculate similarity of two users using pearson correlation
  movie_similarities$similarities[movie_similarities$movieId == id] <- aCos.sim(test_movie_vector,preprocess_matrix[,i])
}


#sorting in decreasing manner by similarity
movie_similarities <- movie_similarities[order(movie_similarities$similarities, decreasing = T),]

#get movies with higher similarities and higher match times
mean_match_time <- mean(movie_similarities$rate_match_times)
similar_movies <- movie_similarities[which(movie_similarities$rate_match_times > mean_match_time & movie_similarities$similarities > 0.6),]

# find out the user that did not rate the test mvoie
# but the rate the similar movies
# and obtain those the ratings for the un-watched movies 
# set into a list
user_list <- list()
for (i in 2:nrow(similar_movies)){# discard the first one since it is the movie itself
  temp <- trainSet_matrix[,as.character(similar_movies[i,]$movieId)][is.na(trainSet_matrix[,test_movie_id])]
  recommand_list <- temp[!is.na(temp)]
  recommand_list <- recommand_list[recommand_list > 4]
  user_list[[i-1]] <- recommand_list
}
final_result_item_based <- tapply(unlist(user_list), names(unlist(user_list)), sum)
final_result_item_based <- sort(final_result_item_based, decreasing = T)
