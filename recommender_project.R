setwd("C:/Users/Alvin/Documents/Data AR")

#install.packages("softImpute")
#install.packages("reshape2")
#install.packages("recommenderlab")

library("softImpute")
library("reshape2")
library("recommenderlab")

#function to read data
readData <- function(){
  
  ratingDF <- read.delim("u.data", header=F)
  colnames(ratingDF) <- c("userID","movieID","rating", "timestamp")
  
  ## read movie data
  moviesDF <- read.delim("movies.dat", sep="|", header=F, stringsAsFactors = FALSE)
  colnames(moviesDF)[colnames(moviesDF)=="V1"] <- "movieID"
  colnames(moviesDF)[colnames(moviesDF)=="V2"] <- "name"
  
  return(list(ratingDF=ratingDF, movieDF=moviesDF))
   
}
#function to preprocess data removing duplicate data
preProcess = function(ratingDF, moviesDF)
{  
  # remove duplicate entries for any user-movie combination
  ratingDF <- ratingDF[!duplicated(ratingDF[,1:2]),]

}

#function to split the rating data based on user with ratio
splitData <- function(data,ratio)
{
  total_user <- length(data$userID[!duplicated(data$userID)])
  ratio <-ratio
  
  user_index <- 1:total_user
  sample_user_index <- sample(1:total_user,ratio*total_user)
  
  train_user <- user_index[sample_user_index]
  train_user <- train_user[order(train_user)]
  test_user <- user_index[-sample_user_index]
  test_user <- test_user[order(test_user)]
  
  
  ratingDF_train <- c()
  ratingDF_test <- c()
  
  
  for(counter in 1 : length(train_user))
  {
    ratingDF_train <- rbind(ratingDF_train,ratingDF[ratingDF$userID==train_user[counter],])
  }
  
  for(counter in 1 : length(test_user))
  {
    ratingDF_test <- rbind(ratingDF_test,ratingDF[ratingDF$userID==test_user[counter],])
  }
  
  return(list(user_train_index=train_user,user_test_index=test_user,ratingDF_train=ratingDF_train,ratingDF_test=ratingDF_test))
}

#function to create global rating matrix
createRatingMatrix <- function(ratingDF)
{
  # converting the ratingData data frame into rating marix
  ratingDF_tmp <- dcast( ratingDF, userID ~ movieID, value.var = "rating" , index="userID")
  ratingDF <- ratingDF_tmp[,2:ncol(ratingDF_tmp)]
  
  ratingMat <- as(ratingDF, "matrix") ## cast data frame as matrix
  ## setting up the dimnames ###
  dimnames(ratingMat)[[1]] <- row.names(ratingDF)
  
  return(ratingMat)
}

#function to create incomplete rating matrix
createIncompleteRatingMatrix <- function(ratingMat)
{
  i=row(ratingMat)[!is.na(ratingMat)]
  j=col(ratingMat)[!is.na(ratingMat)]
  value=ratingMat[!is.na(ratingMat)]
  
  incompleteRatingMatrix <- Incomplete(i=i,j=j,x=value)
  
  return(incompleteRatingMatrix)
}

#function to calculate and evaluate Mean Average Error from prediction
calculateMAE<- function(model,data)
{
  
  total_diff <-0

  for(counter in 1:length(data$userID))
  {
    user<-data[counter,1]
    
    row<-match(user,non_duplicated_ratingDF_train$userID) 
    
    column<-data[counter,2]
    value<-data[counter,3]
    
    real_data <- value
    predicted_data <- impute(model,i=row,j=column,unscale = TRUE)
    
    diff <- abs(real_data-predicted_data)
    
    total_diff <- total_diff+diff
    
  }
  
  mean_avg_err <- total_diff/counter
  
  return (mean_avg_err)
}

#Function to evaluate TopN Recommendation for each user data
evaluateTopNRec<-function(user_index,good_rating,n,pred)
{
  prediction<-pred
  good_rating_param <- good_rating
  n<-n
  user_index<-user_index
  
  #Evaluate TopN 1,3,5,10,15,20 
  temp_TN<-0
  temp_FP<-0
  temp_FN<-0
  temp_TP<-0
  
  #Get the user real rating data to be checked with the prediction data
  user_withheld_rating <- ratingDF_test[ratingDF_test$userID==user_test_index[user_index],]
  
  top_n_rec<-head(prediction,n)

  #Calculate TP and FP
  #Filter out the prediction result which contain user withheld rating
  topN_eq_withheld <- c()
  for(counterk in 1:length(top_n_rec$movieID))
  {
    filter_temp <- user_withheld_rating[user_withheld_rating$movieID==top_n_rec$movieID[1],]
    topN_eq_withheld<-rbind(topN_eq_withheld,filter_temp)
  }
  
  #If the selected TopN doesn't contain withheld item
  if(length(topN_eq_withheld[!is.na(topN_eq_withheld),]$movieID)!=0)
  {
    #Filter out good and bad rating from prediction
    for(counterl in 1:length(topN_eq_withheld$movieID))
    {
      ratingx <- topN_eq_withheld$rating[counterl]
      
      if(ratingx >= good_rating_param)
      {
        temp_TP<-temp_TP+1
      }
      else
      {
        temp_FP<-temp_FP+1
      }
    }
    temp_FP<-temp_FP+(n-length(topN_eq_withheld$movieID))
   
  }
  else
  {
    temp_FP<-length(top_n_rec$movieID)
    temp_TP<-0
  }
  #Until this step, TP and FP of one test user is already acquired
  
  #Calculate TN and FN
  #Filter out prediction from top N item
  pred_out_topN <- prediction
  for(counterm in 1:length(top_n_rec$movieID))
  {
    pred_out_topN<- pred_out_topN[pred_out_topN$movieID!=top_n_rec$movieID[counterm],]
  }
  
  #Filter out prediction from 
  #Filter out the prediction result which contain user withheld rating
  pred_eq_withheld <- c()
  for(countern in 1:length(pred_out_topN$movieID))
  {
    filter_temp <- user_withheld_rating[user_withheld_rating$movieID==pred_out_topN$movieID[countern],]
    pred_eq_withheld<-rbind(pred_eq_withheld,filter_temp)
    
  }
  
  if(length(pred_eq_withheld[!is.na(pred_eq_withheld),]$movieID)!=0)
  {
    #Filter User Rating to choose good Rating to calculate FN
    for(countero in 1:length(pred_eq_withheld$movieID))
    {
      ratingz <- pred_eq_withheld$rating[countero]
      
      if(ratingz >=good_rating_param)
      {
        temp_FN<-temp_FN+1
      }
    }
  }
  else
  {
    temp_FN<-0
  }
  
  temp_TN <- length(pred_out_topN$movieID)-temp_FN

  return(list(temp_TN=temp_TN,temp_FP=temp_FP,temp_FN=temp_FN,temp_TP=temp_TP))
}

#Function to evaluate user Test data using TopN recommendation approach with n=1,3,5,10,15,20
evaluateTestUser <- function(rank,iteration,good_rating)
{
  rankx<-rank
  iterationx<-iteration
  good_rating_param<-good_rating
  
  total_result_top_N_1 <- list(TN=0,FP=0,FN=0,TP=0)
  total_result_top_N_3 <- list(TN=0,FP=0,FN=0,TP=0)
  total_result_top_N_5 <- list(TN=0,FP=0,FN=0,TP=0)
  total_result_top_N_10 <- list(TN=0,FP=0,FN=0,TP=0)
  total_result_top_N_15 <- list(TN=0,FP=0,FN=0,TP=0)
  total_result_top_N_20 <- list(TN=0,FP=0,FN=0,TP=0)

  avg_result_top_N_1 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  avg_result_top_N_3 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  avg_result_top_N_5 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  avg_result_top_N_10 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  avg_result_top_N_15 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  avg_result_top_N_20 <- list(TN=0,FP=0,FN=0,TP=0,Precision=0,Recall=0)
  
  for(user_count in 1:length(user_test_index))
  {
    test_ratingMat<-rbind(train_ratingMat,ratingMat[user_test_index[user_count],])
    print(user_test_index[user_count])
    
    #Creating incomplete matrix 
    incompleteRatingMatTest <- createIncompleteRatingMatrix(test_ratingMat)
    
    #Computing prediction using predefined parameter
    test_fits<-softImpute(incompleteRatingMatTest,trace=FALSE,type="svd",maxit=iterationx,rank.max=rankx)
    
    prediction <- c()
    for(counter in 1: length(ratingDF$movieID[!duplicated(ratingDF$movieID)]))
    {
      row <- dim(incompleteRatingMatTest)[1]
      column <- counter
      
      pred_temp <- impute(test_fits,i=row,j=column)
      prediction <- c(prediction,pred_temp)
    }
    
    movieID <- c(1:length(ratingDF$movieID[!duplicated(ratingDF$movieID)]))
    prediction <- as.data.frame(cbind(movieID,prediction))
    prediction <- prediction[order(-prediction$prediction),]
    
    temp_result_top_N_1 <- evaluateTopNRec(user_count,good_rating,1,prediction)
    temp_result_top_N_3 <- evaluateTopNRec(user_count,good_rating,3,prediction)
    temp_result_top_N_5 <- evaluateTopNRec(user_count,good_rating,5,prediction)
    temp_result_top_N_10 <- evaluateTopNRec(user_count,good_rating,10,prediction)
    temp_result_top_N_15 <- evaluateTopNRec(user_count,good_rating,15,prediction)
    temp_result_top_N_20 <- evaluateTopNRec(user_count,good_rating,20,prediction)
    
    total_result_top_N_1$TN <- total_result_top_N_1$TN+temp_result_top_N_1$temp_TN
    total_result_top_N_1$FP <- total_result_top_N_1$FP+temp_result_top_N_1$temp_FP
    total_result_top_N_1$FN <- total_result_top_N_1$FN+temp_result_top_N_1$temp_FN
    total_result_top_N_1$TP <- total_result_top_N_1$TP+temp_result_top_N_1$temp_TP

    total_result_top_N_3$TN <- total_result_top_N_3$TN+temp_result_top_N_3$temp_TN
    total_result_top_N_3$FP <- total_result_top_N_3$FP+temp_result_top_N_3$temp_FP
    total_result_top_N_3$FN <- total_result_top_N_3$FN+temp_result_top_N_3$temp_FN
    total_result_top_N_3$TP <- total_result_top_N_3$TP+temp_result_top_N_3$temp_TP
    
    total_result_top_N_5$TN <- total_result_top_N_5$TN+temp_result_top_N_5$temp_TN
    total_result_top_N_5$FP <- total_result_top_N_5$FP+temp_result_top_N_5$temp_FP
    total_result_top_N_5$FN <- total_result_top_N_5$FN+temp_result_top_N_5$temp_FN
    total_result_top_N_5$TP <- total_result_top_N_5$TP+temp_result_top_N_5$temp_TP
    
    total_result_top_N_10$TN <- total_result_top_N_10$TN+temp_result_top_N_10$temp_TN
    total_result_top_N_10$FP <- total_result_top_N_10$FP+temp_result_top_N_10$temp_FP
    total_result_top_N_10$FN <- total_result_top_N_10$FN+temp_result_top_N_10$temp_FN
    total_result_top_N_10$TP <- total_result_top_N_10$TP+temp_result_top_N_10$temp_TP
    
    total_result_top_N_15$TN <- total_result_top_N_15$TN+temp_result_top_N_15$temp_TN
    total_result_top_N_15$FP <- total_result_top_N_15$FP+temp_result_top_N_15$temp_FP
    total_result_top_N_15$FN <- total_result_top_N_15$FN+temp_result_top_N_15$temp_FN
    total_result_top_N_15$TP <- total_result_top_N_15$TP+temp_result_top_N_15$temp_TP
    
    total_result_top_N_20$TN <- total_result_top_N_20$TN+temp_result_top_N_20$temp_TN
    total_result_top_N_20$FP <- total_result_top_N_20$FP+temp_result_top_N_20$temp_FP
    total_result_top_N_20$FN <- total_result_top_N_20$FN+temp_result_top_N_20$temp_FN
    total_result_top_N_20$TP <- total_result_top_N_20$TP+temp_result_top_N_20$temp_TP
  }
  
  avg_result_top_N_1$TN <-total_result_top_N_1$TN/length(user_test_index)
  avg_result_top_N_1$FP <-total_result_top_N_1$FP/length(user_test_index)
  avg_result_top_N_1$FN <-total_result_top_N_1$FN/length(user_test_index)
  avg_result_top_N_1$TP <-total_result_top_N_1$TP/length(user_test_index)
  avg_result_top_N_1$Precision <- avg_result_top_N_1$TP/(avg_result_top_N_1$TP+avg_result_top_N_1$FP)
  avg_result_top_N_1$Recall <- avg_result_top_N_1$TP/(avg_result_top_N_1$TP+avg_result_top_N_1$FN)
  avg_result_top_N_1$TPR <- avg_result_top_N_1$TP/(avg_result_top_N_1$TP+avg_result_top_N_1$FN)
  avg_result_top_N_1$FPR <- avg_result_top_N_1$FP/(avg_result_top_N_1$FP+avg_result_top_N_1$TN)
  
  avg_result_top_N_3$TN <-total_result_top_N_3$TN/length(user_test_index)
  avg_result_top_N_3$FP <-total_result_top_N_3$FP/length(user_test_index)
  avg_result_top_N_3$FN <-total_result_top_N_3$FN/length(user_test_index)
  avg_result_top_N_3$TP <-total_result_top_N_3$TP/length(user_test_index)
  avg_result_top_N_3$Precision <- avg_result_top_N_3$TP/(avg_result_top_N_3$TP+avg_result_top_N_3$FP)
  avg_result_top_N_3$Recall <- avg_result_top_N_3$TP/(avg_result_top_N_3$TP+avg_result_top_N_3$FN)
  avg_result_top_N_3$TPR <- avg_result_top_N_3$TP/(avg_result_top_N_3$TP+avg_result_top_N_3$FN)
  avg_result_top_N_3$FPR <- avg_result_top_N_3$FP/(avg_result_top_N_3$FP+avg_result_top_N_3$TN)
  
  avg_result_top_N_5$TN <-total_result_top_N_5$TN/length(user_test_index)
  avg_result_top_N_5$FP <-total_result_top_N_5$FP/length(user_test_index)
  avg_result_top_N_5$FN <-total_result_top_N_5$FN/length(user_test_index)
  avg_result_top_N_5$TP <-total_result_top_N_5$TP/length(user_test_index)
  avg_result_top_N_5$Precision <- avg_result_top_N_5$TP/(avg_result_top_N_5$TP+avg_result_top_N_5$FP)
  avg_result_top_N_5$Recall <- avg_result_top_N_5$TP/(avg_result_top_N_5$TP+avg_result_top_N_5$FN)
  avg_result_top_N_5$TPR <- avg_result_top_N_5$TP/(avg_result_top_N_5$TP+avg_result_top_N_5$FN)
  avg_result_top_N_5$FPR <- avg_result_top_N_5$FP/(avg_result_top_N_5$FP+avg_result_top_N_5$TN)
  
  avg_result_top_N_10$TN <-total_result_top_N_10$TN/length(user_test_index)
  avg_result_top_N_10$FP <-total_result_top_N_10$FP/length(user_test_index)
  avg_result_top_N_10$FN <-total_result_top_N_10$FN/length(user_test_index)
  avg_result_top_N_10$TP <-total_result_top_N_10$TP/length(user_test_index)
  avg_result_top_N_10$Precision <- avg_result_top_N_10$TP/(avg_result_top_N_10$TP+avg_result_top_N_10$FP)
  avg_result_top_N_10$Recall <- avg_result_top_N_10$TP/(avg_result_top_N_10$TP+avg_result_top_N_10$FN)
  avg_result_top_N_10$TPR <- avg_result_top_N_10$TP/(avg_result_top_N_10$TP+avg_result_top_N_10$FN)
  avg_result_top_N_10$FPR <- avg_result_top_N_10$FP/(avg_result_top_N_10$FP+avg_result_top_N_10$TN)
  
  avg_result_top_N_15$TN <-total_result_top_N_15$TN/length(user_test_index)
  avg_result_top_N_15$FP <-total_result_top_N_15$FP/length(user_test_index)
  avg_result_top_N_15$FN <-total_result_top_N_15$FN/length(user_test_index)
  avg_result_top_N_15$TP <-total_result_top_N_15$TP/length(user_test_index)
  avg_result_top_N_15$Precision <- avg_result_top_N_15$TP/(avg_result_top_N_15$TP+avg_result_top_N_15$FP)
  avg_result_top_N_15$Recall <- avg_result_top_N_15$TP/(avg_result_top_N_15$TP+avg_result_top_N_15$FN)
  avg_result_top_N_15$TPR <- avg_result_top_N_15$TP/(avg_result_top_N_15$TP+avg_result_top_N_15$FN)
  avg_result_top_N_15$FPR <- avg_result_top_N_15$FP/(avg_result_top_N_15$FP+avg_result_top_N_15$TN)
  
  avg_result_top_N_20$TN <-total_result_top_N_20$TN/length(user_test_index)
  avg_result_top_N_20$FP <-total_result_top_N_20$FP/length(user_test_index)
  avg_result_top_N_20$FN <-total_result_top_N_20$FN/length(user_test_index)
  avg_result_top_N_20$TP <-total_result_top_N_20$TP/length(user_test_index)
  avg_result_top_N_20$Precision <- avg_result_top_N_20$TP/(avg_result_top_N_20$TP+avg_result_top_N_20$FP)
  avg_result_top_N_20$Recall <- avg_result_top_N_20$TP/(avg_result_top_N_20$TP+avg_result_top_N_20$FN)
  avg_result_top_N_20$TPR <- avg_result_top_N_20$TP/(avg_result_top_N_20$TP+avg_result_top_N_20$FN)
  avg_result_top_N_20$FPR <- avg_result_top_N_20$FP/(avg_result_top_N_20$FP+avg_result_top_N_20$TN)
  
  TopN <- c(1,3,5,10,15,20)
  TN<-c(avg_result_top_N_1$TN,avg_result_top_N_3$TN,avg_result_top_N_5$TN,avg_result_top_N_10$TN,avg_result_top_N_15$TN,avg_result_top_N_20$TN)
  FP<-c(avg_result_top_N_1$FP,avg_result_top_N_3$FP,avg_result_top_N_5$FP,avg_result_top_N_10$FP,avg_result_top_N_15$FP,avg_result_top_N_20$FP)
  FN<-c(avg_result_top_N_1$FN,avg_result_top_N_3$FN,avg_result_top_N_5$FN,avg_result_top_N_10$FN,avg_result_top_N_15$FN,avg_result_top_N_20$FN)
  TP<-c(avg_result_top_N_1$TP,avg_result_top_N_3$TP,avg_result_top_N_5$TP,avg_result_top_N_10$TP,avg_result_top_N_15$TP,avg_result_top_N_20$TP)
  Precision<-c(avg_result_top_N_1$Precision,avg_result_top_N_3$Precision,avg_result_top_N_5$Precision,avg_result_top_N_10$Precision,avg_result_top_N_15$Precision,avg_result_top_N_20$Precision)
  Recall<-c(avg_result_top_N_1$Recall,avg_result_top_N_3$Recall,avg_result_top_N_5$Recall,avg_result_top_N_10$Recall,avg_result_top_N_15$Recall,avg_result_top_N_20$Recall)
  TPR<-c(avg_result_top_N_1$TPR,avg_result_top_N_3$TPR,avg_result_top_N_5$TPR,avg_result_top_N_10$TPR,avg_result_top_N_15$TPR,avg_result_top_N_20$TPR)
  FPR<-c(avg_result_top_N_1$FPR,avg_result_top_N_3$FPR,avg_result_top_N_5$FPR,avg_result_top_N_10$FPR,avg_result_top_N_15$FPR,avg_result_top_N_20$FPR)
  
  result <- as.data.frame(cbind(TopN,TP,FN,FP,TN,Precision,Recall,TPR,FPR))
  
  return(result)
}

#Function for movie recommendation using SVD
svdRecommender <- function(userID,TopN)
{
  user<-userID
  n<-TopN
  user_rating <- ratingDF[ratingDF$userID==user,]
  movie_id <- 1:length(ratingDF[!duplicated(ratingDF$movieID),]$movieID)
  
  #Filter movie ID which haven't been rated by user
  for(counter in 1:length(user_rating$movieID))
  {
    movie_id <- movie_id[movie_id!=user_rating$movieID[counter]]
  }
  
  #Calculate prediction for each blank movie rating
  rating_pred<- c()
  for(counter in 1:length(movie_id))
  {
    temp_pred<-impute(global_fits,i=user,j=counter)
    rating_pred <- c(rating_pred,temp_pred)
  }
  
  movie_pred <- as.data.frame(cbind(movie_id,rating_pred))
  movie_pred <- movie_pred[order(-movie_pred$rating_pred),]
  
  #Choose TopN movie recommendation
  top_recommended <- head(movie_pred,n)
  
  recommended_movie_title <- c()
  for(counter in 1:length(top_recommended$movie_id))
  {
    temp_title <- movie_list[movie_list$movieID==top_recommended$movie_id[counter],]$name
    recommended_movie_title <- c(recommended_movie_title,temp_title)
  }
  
  movie_recommendation <- as.data.frame(recommended_movie_title)
  print(movie_recommendation)
  
  return(movie_recommendation)
}

################################################################################################################################################################################################
#TRAINING SET EVALUATION STEP
################################################################################################################################################################################################
##1. Split data into training and test data, evaluate the best parameter for soft-thresholded SVD
#Importing data
dataList <- readData()

#Preprocessing data
ratingDF <- preProcess(dataList$ratingDF,dataList$movieDF)
ratingDF <- ratingDF[order(ratingDF$userID),]

#Creating global rating matrix
ratingMat <-createRatingMatrix(ratingDF)

#Split data into train and test with ratio (resampling again if sampled data doesn't contain all information about the movies)
incompleteRatingMat <- as.matrix(0)
while(dim(incompleteRatingMat)[2]<dim(ratingMat)[2])
{
  split_data <- splitData(ratingDF,0.9)
  ratingDF_train <- as.data.frame(split_data$ratingDF_train)
  ratingDF_test <- as.data.frame(split_data$ratingDF_test)
  ratingDF_train <- ratingDF_train[order(ratingDF_train$userID),]
  ratingDF_test <- ratingDF_test[order(ratingDF_test$userID),]
  user_train_index <- split_data$user_train_index
  user_test_index <- split_data$user_test_index

  non_duplicated_ratingDF_train <- ratingDF_train[!duplicated(ratingDF_train$userID),]
  #Remove test data from global rating matrix
  train_ratingMat <- ratingMat[user_train_index,]

  #Creating incomplete matrix from training data
  incompleteRatingMat <- createIncompleteRatingMatrix(train_ratingMat)
} 

#Experiment to find the best parameter for soft-thresholded SVD
#Matrix completion using soft-thresholded SVD with rank.max = 1
fits1<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=1)

#Matrix completion using soft-thresholded SVD with rank.max = 5
fits5<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=5)

#Matrix completion using soft-thresholded SVD with rank.max = 10
fits10<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=10)

#Matrix completion using soft-thresholded SVD with rank.max = 25
fits25<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=25)

#Matrix completion using soft-thresholded SVD with rank.max = 50
fits50<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=50)

#Matrix completion using soft-thresholded SVD with rank.max = 100
fits100<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",maxit=200,rank.max=100)

#Calculate MAE for different rank.max
mean_avg_err1 <- calculateMAE(fits1,ratingDF_train)
mean_avg_err5 <- calculateMAE(fits5,ratingDF_train)
mean_avg_err10 <- calculateMAE(fits10,ratingDF_train)
mean_avg_err25 <- calculateMAE(fits25,ratingDF_train)
mean_avg_err50 <- calculateMAE(fits50,ratingDF_train)
mean_avg_err100 <- calculateMAE(fits100,ratingDF_train)

Mean_Avg_Err <- c(mean_avg_err1,mean_avg_err5,mean_avg_err10,mean_avg_err25,mean_avg_err50,mean_avg_err100)
rank.max <- c(1,5,10,25,50,100)

#Result of MAE from variation of rank.max
rank_variation_MAE <- as.data.frame(cbind(rank.max,Mean_Avg_Err))


#Matrix completion using soft-thresholded SVD with rank.max = 100 with variation of iteration (5,10,25,50,100)
fits100i5<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",rank.max=100,maxit=5)
fits100i10<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",rank.max=100,maxit=10)
fits100i25<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",rank.max=100,maxit=25)
fits100i50<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",rank.max=100,maxit=50)
fits100i100<-softImpute(incompleteRatingMat,trace=TRUE,type="svd",rank.max=100,maxit=100)


#Calculate MAE for different iteration for rank.max=100
mean_avg_err100i5 <- calculateMAE(fits100i5,ratingDF_train)
mean_avg_err100i10 <- calculateMAE(fits100i10,ratingDF_train)
mean_avg_err100i25 <- calculateMAE(fits100i25,ratingDF_train)
mean_avg_err100i50 <- calculateMAE(fits100i50,ratingDF_train)
mean_avg_err100i100 <- calculateMAE(fits100i100,ratingDF_train)

Mean_Avg_Err <- c(mean_avg_err100i5,mean_avg_err100i10,mean_avg_err100i25,mean_avg_err100i50,mean_avg_err100i100)
Iteration <- c(5,10,25,50,100)

#Result of MAE from variation of iteration using rank.max=100
iteration_variation_MAE <- as.data.frame(cbind(Iteration,Mean_Avg_Err))

#Conclusion : from the observation of training data, the best parameter is maxit=50  and rank.max=100

################################################################################################################################################################################################
#TEST DATA EVALUATION STEP
################################################################################################################################################################################################
#Input the test data into training model using best parameter observed in training data observation
best_results <- evaluateTestUser(rank=100,iteration=50,good_rating=3)

#Evaluate the rank.max variation (1,5,10,effect on precision,recall, TPR, and FPR
rank1_results <- evaluateTestUser(rank=1,iteration=50,good_rating=3)
rank5_results <- evaluateTestUser(rank=5,iteration=50,good_rating=3)
rank10_results <- evaluateTestUser(rank=10,iteration=50,good_rating=3)
rank25_results <- evaluateTestUser(rank=25,iteration=50,good_rating=3)
rank50_results <- evaluateTestUser(rank=50,iteration=50,good_rating=3)

#Evaluate the max iteration effect on precision,recall, TPR, and FPR
iter1_results <- evaluateTestUser(rank=100,iteration=1,good_rating=3)
iter5_results <- evaluateTestUser(rank=100,iteration=5,good_rating=3)
iter10_results <- evaluateTestUser(rank=100,iteration=10,good_rating=3)
iter25_results <- evaluateTestUser(rank=100,iteration=25,good_rating=3)
iter50_results <- evaluateTestUser(rank=100,iteration=50,good_rating=3)

################################################################################################################################################################################################
#OVERALL EVALUATION DATA RESULTS
################################################################################################################################################################################################
#Mean Average Error from variation of rank.max
print(rank_variation_MAE)
#Mean Average Error from variation of iteration
print(iteration_variation_MAE)
#Result from evaluation of TopN from user Test data with best parameter
print(best_results)
#Result from evaluation rank.max variation (1,5,10,25,50) effect on precision,recall, TPR, and FPR
print(rank1_results)
print(rank5_results)
print(rank10_results)
print(rank25_results)
print(rank50_results)

#Result from evaluation max iteration variation (1,5,10,25,50) effect on precision,recall, TPR, and FPR
print(iter1_results)
print(iter5_results)
print(iter10_results)
print(iter25_results)
print(iter50_results)

################################################################################################################################################################################################
#RECOMMENDER APPLICATION
################################################################################################################################################################################################
#Create incomplete matrix from the user data
movie_list <- dataList$movieDF
globalIncompleteMat <- createIncompleteRatingMatrix(ratingMat)

#Calculate global prediction
global_fits <- softImpute(globalIncompleteMat,trace=FALSE,type="svd",maxit=100,rank.max=100)

#SVD recommender system
recommendation <- svdRecommender(userID=743,TopN=5)
