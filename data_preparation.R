setwd("c:/R_workspace")
rm(list=ls())  #remove all objects
ls()

#Data Preparation

  #load movies
#movies <- read.csv(file="moviedescription.csv", header = T) #File size is huge, long duration task
                                                            #one movie record(id: 1800019080) is corrupted
                                                            #Solution: DELETE
moviesNum  <-  length(unique(movies$movieid)) #106957 movie items expected

  #Training set
trainSet <- read.csv(file="usermovieratingstrain.csv", header = T)
trainSet <- trainSet[which(trainSet$movieid != 0),] #there is corrputed data with movieid=0
                                                    #Solution: DELETE
  #discard cold user and items
  #1.obtain number of ratings by movie/user

library("dplyr")
ratings_per_movie <- trainSet %>%
  group_by(movieid) %>%
  summarize(num_ratings_permovie=n())
  max(ratings_per_movie$num_ratings_permovie)
  #sorting in decreasing manner
ratings_per_movie <- ratings_per_movie[order(ratings_per_movie$num_ratings_permovie, decreasing = T),]

ratings_per_user <- trainSet %>%
  group_by(userid) %>%
  summarize(num_ratings_peruser=n())
  max(ratings_per_user$num_ratings_peruser)
  #sorting in decreasing manner
ratings_per_user <- ratings_per_user[order(ratings_per_user$num_ratings_peruser, decreasing = T),]

  #define cold user/item by setting threshhold on ratings per user/item
  #probable reference values: median, mean
coldThresh_mean_movie <- ceiling(mean(ratings_per_movie$num_ratings_permovie))
coldThresh_mean_user <- ceiling(mean(ratings_per_user$num_ratings_peruser))
coldThresh_median_movie <- ceiling(median(ratings_per_movie$num_ratings_permovie)) #too small
coldThresh_median_user <- ceiling(median(ratings_per_user$num_ratings_peruser))

  #truncate users/items with threshhold(mean), filter out cold users/items
movies_after_filter <- ratings_per_movie[which(ratings_per_movie$num_ratings_permovie > coldThresh_mean_movie),]
users_after_filter <- ratings_per_user[which(ratings_per_user$num_ratings_peruser > coldThresh_mean_user),]

  #Obtain the size of the matrix 
rNames  <- users_after_filter$userid
cNames  <- movies_after_filter$movieid
trainSet_userNum  <-  length(rNames)
trainSet_movieNum  <- length(cNames)
  #initial the matrix (userNum * movieNum)
trainSet_matrix <- matrix(nrow = trainSet_userNum, ncol = trainSet_movieNum, dimnames  = list (rNames, cNames))

#get discarded users/items
movies_discarded <- ratings_per_movie[which(ratings_per_movie$num_ratings_permovie < coldThresh_mean_movie),]
users_discarded <- ratings_per_user[which(ratings_per_user$num_ratings_peruser < coldThresh_mean_user),]
#truncate training set rating records using discared userids/movieids 
for(i in 1:nrow(users_discarded)) {
  userId <- users_discarded[i,1]
  trainSet <- trainSet[which(trainSet$userid != as.integer(userId)),]
}

for(j in 1:nrow(movies_discarded)) {
  movieId <- movies_discarded[j,1]
  trainSet <- trainSet[which(trainSet$movieid != as.integer(movieId)),]
}

#fill matrix with data
for (i in 1:nrow(trainSet)) {
  #scan each row of training set to fill matrix with values
  #obtain user id
  userId  <-  trainSet[i,1]
  #obtain movie id
  movieId  <- trainSet[i,2]
  #set rating into corresponding position
  trainSet_matrix[rownames(trainSet_matrix) == userId, colnames(trainSet_matrix) == movieId] <- trainSet[i,4]
}
