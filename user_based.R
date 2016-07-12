setwd("d:/R_WORKSPACE")

#user-based recommandation using similarities between users
#Scenario: For a given user, obtained a list of recommanding movies for him/her 


#initiate a data frame for the similarity table
userId <- rNames
similarities <- NA
watch_match_times <- NA 
user_similarities <- data.frame(userId, similarities, watch_match_times)




#start use case for user-based 

#obtain the user for test
#here use the first user for test
test_user_vector <- trainSet_matrix[1,]
test_user_id <- rownames(trainSet_matrix)[1]


#calculate pearson correlation
for (i in 1:nrow(trainSet_matrix)) {
  #obtain the user id for this time of this loop
  id <- rownames(trainSet_matrix)[i]
  #find how many movies they watch in common
  temp <- trainSet_matrix[i,][!is.na(test_user_vector)]
  match_times  <- length(temp[!is.na(temp)])
  #set the match times in case of "fake correlation"
  user_similarities$watch_match_times[user_similarities$userId == id] <- match_times
  #calculate similarity of two users using pearson correlation
  user_similarities$similarities[user_similarities$userId == id] <- cor(test_user_vector,trainSet_matrix[i,], use = "pairwise.complete.obs")
}

#sorting in decreasing manner by match time
user_similarities <- user_similarities[order(user_similarities$watch_match_times, decreasing = T),]

#get users with higher similarities and higher match times
mean_match_time <- mean(user_similarities$watch_match_times)
similar_users <- user_similarities[which(user_similarities$watch_match_times > mean_match_time & user_similarities$similarities > 0.6),]

# find out the movies that test user did not watch
# but the similar user watched
# and obtain those the ratings for the un-watched movies 
# set into a list
movie_list <- list()
for (i in 2:nrow(similar_users)){# discard the first one since it is the user himself
  temp <- trainSet_matrix[as.character(similar_users[i,]$userId),][is.na(trainSet_matrix[test_user_id,])]
  recommand_list <- temp[!is.na(temp)]
  recommand_list <- recommand_list[recommand_list > 4]
  movie_list[[i-1]] <- recommand_list
}
final_result_user_based <- tapply(unlist(movie_list), names(unlist(movie_list)), sum)
final_result_user_based <- sort(final_result_user_based, decreasing = T)
