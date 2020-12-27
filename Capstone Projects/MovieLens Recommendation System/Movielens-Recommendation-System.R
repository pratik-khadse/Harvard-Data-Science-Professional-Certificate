
## ----------------------------------------------------------------------------------------------------------

## Install Libraries

options(digits = 5)

library(tidyverse)
library(caret)
library(data.table)
library(tidyr)
library(lubridate)



#Or use the following if packages are not installed earlier

# if(!require(tidyverse)) install.packages("tidyverse", 
#                                          repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", 
#                                      repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", 
#                                           repos = "http://cran.us.r-project.org")
# if(!require(tidyr)) install.packages("tidyr", 
#                                      repos = "http://cran.us.r-project.org")
# if(!require(lubridate)) install.packages("lubridate", 
#                                          repos = "http://cran.us.r-project.org")

#Also install tinytex and kableExtra if not present
# tinytex::install_tinytex()
# install.packages("kableExtra")

## ----------------------------------------------------------------------------------------------------------


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

#Note: this takes couple of minutes. This code is provided by Edx community

# Download files
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

## Create dataframe
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))), 
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies_temp <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies_temp) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies_temp) %>%
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)

# Validation set will be 10% of Movielens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add removed rows from validation set back to edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#remove unneeded objects
rm(dl, ratings, movies, test_index, temp, movielens, removed) 



## ----------------------------------------------------------------------------------------------------------

## Sanity checks

#rows and columns
dim(edx)


## ----------------------------------------------------------------------------------------------------------

##summary statistics
summary(edx)



## ----------------------------------------------------------------------------------------------------------

##Half star vs full star ratings
edx %>%
  group_by(rating) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = rating, y = n)) +
  geom_line()


## ----------------------------------------------------------------------------------------------------------

## Obtain YearOfRating
edx <- edx %>%
  mutate(YearOfRating = year(as_datetime(timestamp))) 

validation <- validation %>%
  mutate(YearOfRating = year(as_datetime(timestamp)))


## ----------------------------------------------------------------------------------------------------------

## Obtain YearOfRelease

# for edx
edx <- edx %>%
  mutate(temp = str_extract(edx$title, "\\(\\d{4}\\)")) %>%
  mutate(YearOfRelease = as.numeric(str_extract(temp, "\\d{4}"))) %>%
  select(-temp)%>%
  select(userId, movieId, title, YearOfRelease, YearOfRating, genres, rating)

# for validation

validation <- validation %>%
  mutate(temp = str_extract(title, "\\(\\d{4}\\)")) %>%
  mutate(YearOfRelease = as.numeric(str_extract(temp, "\\d{4}"))) %>%
  select(-temp) %>%
  select(userId, movieId, title, YearOfRelease, YearOfRating, genres, rating)


## ----------------------------------------------------------------------------------------------------------

## Get genres on seperate rows for usage in EDA, stored as a seperate table

# for edx
edx1 <- edx %>%
  separate_rows(., genres, sep = "\\|")



## ----------------------------------------------------------------------------------------------------------

## Convert genres to individual columns

#Note we have to consider all factors of genres, 
#so there is no need to exclude one, for the dummy variable trap is inapplicable here
genre <- unique(edx1$genres)

# for edx
for (element in genre){
  edx <- edx %>%
    mutate(!!element := 
             ifelse(str_detect(edx$genres, element), 1, 0)) 
  ## We use Dynamic Variable here

}

edx <- edx %>%
  select(-genres)

# for validation
for (element in genre){
  validation <- validation %>%
    mutate(!!element := 
             ifelse(str_detect(validation$genres, element), 1, 0)) 
  ## We use Dynamic Variable here

}

validation <- validation %>%
  select(-genres)



## ----------------------------------------------------------------------------------------------------------
#replace -, spaces with _ and remove ()

# for edx
colnames(edx)[str_detect(names(edx), pattern = "-")] <-
  str_replace_all(colnames(edx)[str_detect(names(edx), pattern = "-")], 
                pattern = "-",
                replacement = "_")

colnames(edx)[str_detect(names(edx), pattern = "\\(")] <-
  str_replace_all(colnames(edx)[str_detect(names(edx), pattern = "\\(")], 
                pattern = "\\s",
                replacement = "_")

colnames(edx)[str_detect(names(edx), pattern = "\\(")] <-
  str_replace_all(colnames(edx)[str_detect(names(edx), pattern = "\\(")], 
                pattern = "\\(|\\)",
                replacement = "")

# for validation

colnames(validation)[str_detect(names(validation), pattern = "-")] <-
  str_replace_all(colnames(validation)[str_detect(names(validation), 
                                                  pattern = "-")], 
                pattern = "-",
                replacement = "_")

colnames(validation)[str_detect(names(validation), pattern = "\\(")] <-
  str_replace_all(colnames(validation)[str_detect(names(validation), 
                                                  pattern = "\\(")], 
                pattern = "\\s",
                replacement = "_")

colnames(validation)[str_detect(names(validation), pattern = "\\(")] <-
  str_replace_all(colnames(validation)[str_detect(names(validation), 
                                                  pattern = "\\(")], 
                pattern = "\\(|\\)",
                replacement = "")


## ----------------------------------------------------------------------------------------------------------

## view edx

head(edx) 


## ----------------------------------------------------------------------------------------------------------

## view validation

head(validation)


## ----------------------------------------------------------------------------------------------------------

## Plotting mean ratings of users: we see the distribution is left skewed

# Also considerable variation is seen, which makes it an important variable

edx %>%
  group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = mean_rating)) +
  geom_histogram(color = "black")


## ----------------------------------------------------------------------------------------------------------

## Plotting median of ratings


edx %>%
  group_by(userId) %>%
  summarize(median_ratings = median(rating)) %>%
  ggplot(aes(x = median_ratings)) +
  geom_histogram(color = "black")



## ----------------------------------------------------------------------------------------------------------

## Plot mean rating for movieId
edx %>%
  group_by(movieId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = mean_rating)) +
  geom_histogram(color = "black")



## ----------------------------------------------------------------------------------------------------------

## Plot median of ratings for movieId
edx %>%
  group_by(movieId) %>%
  summarize(median_rating = median(rating)) %>%
  ggplot(aes(x = median_rating)) +
  geom_histogram(color = "black")

# We see more variation with median ratings in movieId than in user Id,
# still we get better results using mean  


## ----------------------------------------------------------------------------------------------------------

## Barplot for genres using mean rating
edx1 %>%
  group_by(genres) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = genres, y = mean_rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## We see that not much variation is seen in genres,
# so the effect of this should'nt be very significant


## ----------------------------------------------------------------------------------------------------------

## Barplot for genres using median
edx1 %>%
  group_by(genres) %>%
  summarize(median_rating = median(rating)) %>%
  ggplot(aes(x = genres, y = median_rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
## We see lesser variation with median, similar to earlier cases


## ----------------------------------------------------------------------------------------------------------

## Plot count of no of ratings for genres. 

# We see that it follows the general trend, spikes for integer rating, 
# lower count for decimal ratings
edx1 %>%
  group_by(genres, rating) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = rating, y = n, color = genres)) +
  geom_line(size = 2)



## ----------------------------------------------------------------------------------------------------------

## Also plotting ratings received for each genre

edx1 %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = genres, y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



## ----------------------------------------------------------------------------------------------------------
## Plot mean ratings for year of rating

edx %>%
  group_by(YearOfRating) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = YearOfRating, y = mean_rating)) +
  geom_line()




## ----------------------------------------------------------------------------------------------------------
## Plot median ratings for year of rating

edx %>%
  group_by(YearOfRating) %>%
  summarize(median_rating = median(rating)) %>%
  ggplot(aes(x = YearOfRating, y = median_rating)) +
  geom_line()
## We see the median rating shows lesser variation, thus mean will better account for variations


## ----------------------------------------------------------------------------------------------------------
## Plot mean ratings for year of release

edx %>%
  group_by(YearOfRelease) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = YearOfRelease, y = mean_rating)) +
  geom_line()

# We see that mean ratings fell as we moved ahead in time. 
# This can thus be an important variable (effect) in our analysis


## ----------------------------------------------------------------------------------------------------------
## Plot median ratings for year of release
edx %>%
  group_by(YearOfRelease) %>%
  summarize(median_rating = median(rating)) %>%
  ggplot(aes(x = YearOfRelease, y = median_rating)) +
  geom_line()
# We see the median rating shows lesser variation, thus mean will better account for variations


## ----------------------------------------------------------------------------------------------------------
## Check unique values
length(unique(edx$movieId))
length(unique(edx$userId))
length(unique(edx$YearOfRating))

length(genre)
length(unique(edx$YearOfRelease))



## ----------------------------------------------------------------------------------------------------------

## Split dataset for finding parameters

set.seed(1, sample.kind = "Rounding")
test_index_cv <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)

train_set <- edx %>% slice(-test_index_cv)
temp <- edx %>% slice(test_index_cv)

# Make sure all movies and users in test set are present in train set
test_set_cv <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set_cv)
train_set <- rbind(train_set, removed)

# Define function for RMSE used in optimizing parameters
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



## ----------------------------------------------------------------------------------------------------------
#### Naive Model ####

mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set_cv$rating, mu_hat) %>%
  round(., digits = 5)


## ----------------------------------------------------------------------------------------------------------
#test on test_set_cv


results <- data.frame(method = "Only Average", RMSE = naive_rmse)

results


## ----------------------------------------------------------------------------------------------------------
## Free up space
rm(edx1, movies_temp, removed, temp, test_index_cv, element, naive_rmse)


## ----------------------------------------------------------------------------------------------------------
#### Regularized Movie Effect ####

# Set lambdas
lambdas <- seq(0,10, 0.25)

# find lambda using intermediate test set: lowest rmse
rmses_i <- sapply(lambdas, function(l){
  
  # get b_i
  b_i <- train_set %>%
    select(movieId, rating) %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
  # predict
  predicted_ratings <- test_set_cv %>%
    select(movieId) %>%
    left_join(b_i, by = "movieId") %>%
    mutate(predicted = mu_hat + b_i) %>%
    pull(predicted)
  
  return(RMSE(test_set_cv$rating, predicted_ratings))
})

#check lambda which gives lowest rmse
lambda_i <- lambdas[which.min(rmses_i)]
lambda_i


## ----------------------------------------------------------------------------------------------------------
#Confirm with plot
qplot(x = lambdas, y = rmses_i)




## ----------------------------------------------------------------------------------------------------------
# Update results dataframe

results <- rbind(results, data.frame(method = "Regularized Movie Effect", 
                                     RMSE = min(rmses_i)))

results 
                    
## We see that RMSE has reduced.  
  


## ----------------------------------------------------------------------------------------------------------
## Free space
rm(rmses_i,lambda_i)


## ----------------------------------------------------------------------------------------------------------
#### Regularized Movie+User Effect ####

# Initiate lambdas
lambdas <- seq(0,10, 0.25)


# find lambda using intermediate test set: lowest rmse

rmses_i_u <- sapply(lambdas, function(l){
  #get b_i
  b_i <- train_set %>%
    select(movieId, rating) %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n() + l))
  
  #get b_u
  b_u <- train_set %>%
    select(movieId, userId, rating) %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_i)/(n() + l))
  
  
  #predict
  predicted_ratings <-  test_set_cv %>%
    select(movieId, userId) %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(test_set_cv$rating, predicted_ratings))
}) 

#check lambda which gives lowest rmse
lambda_i_u <- lambdas[which.min(rmses_i_u)]
lambda_i_u


## ----------------------------------------------------------------------------------------------------------
#Confirm with plot
qplot(x = lambdas, y = rmses_i_u)


## ----------------------------------------------------------------------------------------------------------
#Update results
  
results <- results %>%
  rbind(data.frame(method = "Regularized Movie+User Effect", RMSE = min(rmses_i_u)))

results

# We see the RMSE reduced further


## ----------------------------------------------------------------------------------------------------------
## Free up space
rm(lambda_i_u, lambdas, rmses_i_u)



## ----------------------------------------------------------------------------------------------------------
#### Regularized Movie+User+YearOfRating Effect ####
memory.limit(size = 10000)

gc()


## ----------------------------------------------------------------------------------------------------------
# initiate lambdas
lambdas <- seq(0, 10, 0.25)

# find lambda using intermediate test set: lowest rmse

rmses_i_u_yrate <- sapply(lambdas, function(l){
  
  #get b_i
  b_i <- train_set %>%
    select(movieId, rating) %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_u
  b_u <- train_set %>%
    select(movieId, userId, rating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_i)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_yrate
  b_yrate <- train_set %>%
    select(movieId, userId, rating, YearOfRating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    group_by(YearOfRating) %>%
    summarise(b_yrate = sum(rating - mu_hat - b_i - b_u)/(n() + l)) %>%
    round(., digits = 5)
  
  #get predicted ratings
  predicted_ratings <- test_set_cv %>%
    select(movieId, userId, YearOfRating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    mutate(pred = mu_hat + b_i + b_u + b_yrate) %>%
    round(., digits = 5) %>%
    pull(pred)
  
  #return RMSE
  return(RMSE(test_set_cv$rating, predicted_ratings))
  
})


#check lambda which gives lowest rmse
lambda_i_u_yrate <- lambdas[which.min(rmses_i_u_yrate)]
lambda_i_u_yrate


## ----------------------------------------------------------------------------------------------------------
# Confirm with plot
qplot(x = lambdas, y = rmses_i_u_yrate)


## ----------------------------------------------------------------------------------------------------------
#Update results

results <- results %>%
  rbind(data.frame(method = "Regularized Movie+User+YearOfRating Effect", 
                   RMSE = min(rmses_i_u_yrate)))

results 

# We see the RMSE reduced further, although the reduction is lesser compared to earlier changes



## ----------------------------------------------------------------------------------------------------------
## Free up space
rm(lambdas, lambda_i_u_yrate, rmses_i_u_yrate)


## ----------------------------------------------------------------------------------------------------------
#### Regularized Movie+User+YearOfRating+YearOfRelease Effect ####
memory.limit(size = 12000)

gc()


## ----------------------------------------------------------------------------------------------------------
# initiate lambdas
lambdas <- seq(0, 10, 0.25)

# find lambda using intermediate test set: lowest rmse

rmses_i_u_yrate_yrel <- sapply(lambdas, function(l){
  
  #get b_i
  b_i <- train_set %>%
    select(movieId, rating) %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_u
  b_u <- train_set %>%
    select(movieId, userId, rating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_i)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_yr
  b_yrate <- train_set %>%
    select(movieId, userId, rating, YearOfRating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    group_by(YearOfRating) %>%
    summarise(b_yrate = sum(rating - mu_hat - b_i - b_u)/(n() + l)) %>%
    round(., digits = 5)
  
  b_yrel <- train_set %>%
    select(movieId, userId, rating, YearOfRating, YearOfRelease) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    group_by(YearOfRelease) %>%
    summarize(b_yrel = sum(rating - mu_hat - b_i - b_u - b_yrate)/(n() + l)) %>%
    round(., digits = 5)
  
  #get predicted ratings
  predicted_ratings <- test_set_cv %>%
    select(movieId, userId, YearOfRating, YearOfRelease) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    left_join(b_yrel, by = "YearOfRelease") %>%
    select(-YearOfRelease) %>%
    mutate(pred = mu_hat + b_i + b_u + b_yrate + b_yrel) %>%
    round(., digits = 5) %>%
    pull(pred)
  
  #return RMSE
  return(RMSE(test_set_cv$rating, predicted_ratings))
  
})


#check lambda which gives lowest rmse
lambda_i_u_yrate_yrel <- lambdas[which.min(rmses_i_u_yrate_yrel)]
lambda_i_u_yrate_yrel


## ----------------------------------------------------------------------------------------------------------
#Confirm with plot
qplot(x = lambdas, y = rmses_i_u_yrate_yrel)


## ----------------------------------------------------------------------------------------------------------
#Update results
  
results <- results %>%
  rbind(data.frame(method = "Regularized Movie+User+YearOfRating+YearOfRelease Effect", 
                   RMSE =min(rmses_i_u_yrate_yrel)))

results

# We see the RMSE reduced further significantly compared to YearOfRating effect addition


## ----------------------------------------------------------------------------------------------------------
## Free up space
rm(b_i, b_u, b_yrate, b_yrel, lambda_i_u_yrate_yrel, lambdas, mu, rmses_i_u_yrate_yrel, movie_user_YearOfRating_YearOfRelease_effect)



## ----------------------------------------------------------------------------------------------------------

#### Regularized Movie+User+YearOfRating+YearOfRelease+Genre Effect ####

# Check count of genres
genre <- colnames(edx)[7:length(colnames(edx))]
genre


b <- list()
for (i in 1:length(genre)){
  b[[i]] <- c(sum(edx[[genre[i]]]), (length(edx[[genre[i]]]) - sum(edx[[genre[i]]])))
  names(b[[i]]) <- c(1,0)
}

names(b) <- genre
b



## ----------------------------------------------------------------------------------------------------------
# Free up space
rm(b)

memory.limit(size = 13000)

gc()


## ----------------------------------------------------------------------------------------------------------
# Initiate lambdas
lambdas <- seq(1,10, 2) 
#we know that optimum lambda will be around 5, 
# so we use bigger steps to process faster. 
# Also, we check our final result with the graph to confirm

# find lambda using intermediate test set: lowest rmse

rmses_i_u_yrate_yrel_g <- sapply(lambdas, function(l){

  b_g <- list()
  b_g_x <- vector(mode = "character")
  
  
  #get b_i
  b_i <- train_set %>%
    select(movieId, rating) %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_u
  b_u <- train_set %>%
    select(movieId, userId, rating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_i)/(n() + l)) %>%
    round(., digits = 5)
  
  #get b_yr
  b_yrate <- train_set %>%
    select(movieId, userId, rating, YearOfRating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    group_by(YearOfRating) %>%
    summarise(b_yrate = sum(rating - mu_hat - b_i - b_u)/(n() + l)) %>%
    round(., digits = 5)
  
  b_yrel <- train_set %>%
    select(movieId, userId, rating, YearOfRating, YearOfRelease) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    group_by(YearOfRelease) %>%
    summarize(b_yrel = sum(rating - mu_hat - b_i - b_u - b_yrate)/(n() + l)) %>%
    round(., digits = 5)
  
  for (i in 1:length(genre)){
  b_g_x[i] <- paste("b_g", i, sep = "_")
  
  df_temp <- train_set %>%
    select(movieId, userId, YearOfRating, YearOfRelease, rating, genre[i]) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    left_join(b_yrel, by = "YearOfRelease") %>%
    select(-YearOfRelease) %>%
    mutate(b_g_effect = 0)
  
  k <- i
  
  while(k != 1){
    df_temp <- df_temp %>%
      cbind(., select(train_set, genre[k-1])) %>%
      left_join(b_g[[k-1]], by = genre[k-1]) %>%
      select(-genre[k-1]) %>%
      mutate_at(vars(b_g_x[k-1]), ~replace(., is.na(.), 0)) %>%
      mutate(b_g_effect = b_g_effect + (!!as.name(b_g_x[k-1]))) %>%
      select(-b_g_x[k-1]) %>%
      round(., digits = 5)
    k <- k-1
  }
  
  if(i != 1){
  b_g[[i]] <- df_temp %>%
    group_by_(genre[i]) %>%
    summarise(!!b_g_x[i] := sum(rating - mu_hat - b_i - b_u -
                                  b_yrate - b_yrel - b_g_effect)/(n() + l)) %>%
    filter((!!as.name(genre[i])) == 1) %>%
    round(., digits = 5)
  } else {
    b_g[[i]] <- df_temp %>%
      group_by_(genre[i]) %>%
      summarise(!!b_g_x[i] := sum(rating - mu_hat - b_i - b_u
                                  - b_yrate - b_yrel)/(n()+ l)) %>%
      filter((!!as.name(genre[i])) == 1) %>%
      round(., digits = 5)
  }
  }
  
  rm(df_temp)
  
  df_temp_test <- test_set_cv %>%
    select(movieId, userId, YearOfRating, YearOfRelease, rating) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    left_join(b_yrel, by = "YearOfRelease") %>%
    select(-YearOfRelease) %>%
    mutate(b_g = 0)
  
  for(i in 1: length(genre)){
    df_temp_test <- df_temp_test %>%
      cbind(., select(test_set_cv, genre[i])) %>%
      left_join(b_g[[i]], by = genre[i]) %>%
      select(-genre[i]) %>%
      mutate_at(vars(b_g_x[i]), ~replace(., is.na(.), 0)) %>%
      mutate(b_g = b_g + (!!as.name(b_g_x[i]))) %>%
      round(.,digits = 5) %>%
      select(-b_g_x[i])
  }
  
  predicted_ratings <- df_temp_test %>%
    mutate(pred = mu_hat + b_i + b_u + b_yrate + b_yrel + b_g) %>%
    round(.,digits = 5) %>%
    pull(pred)
  

  

return(RMSE(test_set_cv$rating, predicted_ratings))
})


#check lambda which gives lowest rmse
lambda_i_u_yrate_yrel_g <- lambdas[which.min(rmses_i_u_yrate_yrel_g)]
lambda_i_u_yrate_yrel_g


## ----------------------------------------------------------------------------------------------------------
#Confirm with plot
qplot(x = lambdas, y = rmses_i_u_yrate_yrel_g)



## ----------------------------------------------------------------------------------------------------------
#Update results


results <- results %>%
  rbind(data.frame(method = 
                     "Regularized Movie+User+YearOfRating+YearOfRelease+Genre Effect", 
                   RMSE = min(rmses_i_u_yrate_yrel_g)))

results


## ----------------------------------------------------------------------------------------------------------
#Predict for validation set
rm(rmses_i_u_yrate_yrel_g, train_set, test_set_cv)

mu <- mean(edx$rating)

b_i <- edx %>%
  select(movieId, rating) %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/
              (n() + lambda_i_u_yrate_yrel_g)) %>%
  round(., digits = 5)

b_u <- edx %>%
  select(movieId, userId, rating) %>%
  left_join(b_i, by = "movieId") %>%
  select(-movieId) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/
              (n() + lambda_i_u_yrate_yrel_g)) %>%
  round(., digits = 5)

b_yrate <- edx %>%
  select(movieId, userId, YearOfRating, rating) %>%
  left_join(b_i, by = "movieId") %>%
  select(-movieId) %>%
  left_join(b_u, by = "userId") %>%
  select(-userId) %>%
  group_by(YearOfRating) %>%
  summarize(b_yrate = sum(rating - mu - b_i - b_u)/
              (n() + lambda_i_u_yrate_yrel_g)) %>%
  round(., digits = 5)

b_yrel <- edx %>%
  select(movieId, userId, YearOfRating, YearOfRelease, rating) %>%
  left_join(b_i, by = "movieId") %>%
  select(-movieId) %>%
  left_join(b_u, by = "userId") %>%
  select(-userId) %>%
  left_join(b_yrate, by = "YearOfRating") %>%
  select(-YearOfRating) %>%
  group_by(YearOfRelease) %>%
  summarize(b_yrel = sum(rating - mu - b_i - b_u - b_yrate)/
              (n() + lambda_i_u_yrate_yrel_g)) %>%
  round(., digits = 5)

b_g <- list()
b_g_x <- vector(mode = "character")


for (i in 1:length(genre)){
  b_g_x[i] <- paste("b_g", i, sep = "_")
  
  df_temp <- edx %>%
    select(movieId, userId, YearOfRating, YearOfRelease, rating, genre[i]) %>%
    left_join(b_i, by = "movieId") %>%
    select(-movieId) %>%
    left_join(b_u, by = "userId") %>%
    select(-userId) %>%
    left_join(b_yrate, by = "YearOfRating") %>%
    select(-YearOfRating) %>%
    left_join(b_yrel, by = "YearOfRelease") %>%
    select(-YearOfRelease) %>%
    mutate(b_g_effect = 0)
  
  k <- i
  
  while(k != 1){
    df_temp <- df_temp %>%
      cbind(., select(edx, genre[k-1])) %>%
      left_join(b_g[[k-1]], by = genre[k-1]) %>%
      select(-genre[k-1]) %>%
      mutate_at(vars(b_g_x[k-1]), ~replace(., is.na(.), 0)) %>%
      mutate(b_g_effect = b_g_effect + (!!as.name(b_g_x[k-1]))) %>%
      select(-b_g_x[k-1]) %>%
      round(., digits = 5)
    k <- k-1
  }
  
  if(i != 1){
  b_g[[i]] <- df_temp %>%
    group_by_(genre[i]) %>%
    summarise(!!b_g_x[i] := 
                sum(rating - mu - b_i - b_u - 
                      b_yrate - b_yrel - b_g_effect)/
                (n() + lambda_i_u_yrate_yrel_g)) %>%
    filter((!!as.name(genre[i])) == 1) %>%
    round(., digits = 5)
  } else {
    b_g[[i]] <- df_temp %>%
      group_by_(genre[i]) %>%
      summarise(!!b_g_x[i] := 
                  sum(rating - mu - b_i - b_u - b_yrate - b_yrel)/
                  (n()+ lambda_i_u_yrate_yrel_g)) %>%
      filter((!!as.name(genre[i])) == 1) %>%
      round(., digits = 5)
  }
  }
  


 df_temp_val <- validation %>%
   select(movieId, userId, YearOfRating, YearOfRelease, rating) %>%
   left_join(b_i, by = "movieId") %>%
   select(-movieId) %>%
   left_join(b_u, by = "userId") %>%
   select(-userId) %>%
   left_join(b_yrate, by = "YearOfRating") %>%
   select(-YearOfRating) %>%
   left_join(b_yrel, by = "YearOfRelease") %>%
   select(-YearOfRelease) %>%
   mutate(b_g = 0)
  
  for(i in 1: length(genre)){
    df_temp_val <- df_temp_val %>%
      cbind(., select(validation, genre[i])) %>%
      left_join(b_g[[i]], by = genre[i]) %>%
      select(-genre[i]) %>%
      mutate_at(vars(b_g_x[i]), ~replace(., is.na(.), 0)) %>%
      mutate(b_g = b_g + (!!as.name(b_g_x[i]))) %>%
      round(.,digits  = 5) %>%
      select(-b_g_x[i])
  }
  
  movie_user_YearOfRating_YearOfRelease_genre_effect <- df_temp_val %>%
    mutate(pred = mu + b_i + b_u + b_yrate + b_yrel + b_g) %>%
    pull(pred)
  
results_final <- data.frame(method = 
                     "Regularized Movie+User+YearOfRating+YearOfRelease+Genre Effect", 
                   RMSE = RMSE(validation$rating, 
                               movie_user_YearOfRating_YearOfRelease_genre_effect))


## ----------------------------------------------------------------------------------------------------------
## the final RMSE is as follows
results_final 



## ----------------------------------------------------------------------------------------------------------
## Free up space
rm(b_i, b_u, b_g, b_g_x, b_yrate, b_yrel, df_temp, df_temp_val, i, k, lambda_i_u_yrate_yrel_g, lambdas, mu, movie_user_YearOfRating_YearOfRelease_genre_effect)



## ----------------------------------------------------------------------------------------------------------
#Results obtained through intermediate sets
results 


## ----------------------------------------------------------------------------------------------------------
#Results obtained using "edx" and "validation" sets

mu <- mean(edx$rating)

results_final <- results_final %>%
  rbind(data.frame(method = "Only average",
                   RMSE = RMSE(validation$rating, mu)))

results_final %>%
  arrange(desc(RMSE))

