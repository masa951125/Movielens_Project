#used following libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

################################################################################
#Create edx set, validation set (final hold-out test set). 
#This code is provided by the course. 

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),title = as.character(title),genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
################################################################################
#Find basic information about the dataset, edx and validation

#check the numbers of rows and columns of these datasets.
dim(edx)
#[1] 9000055       6
dim(validation)
#[1] 999999      6

#edx's columns(predictors)

#the first rows of edx
head(edx)
# userId movieId rating timestamp                         title
# 1:      1     122      5 838985046              Boomerang (1992)
# 2:      1     185      5 838983525               Net, The (1995)
# 3:      1     292      5 838983421               Outbreak (1995)
# 4:      1     316      5 838983392               Stargate (1994)
# 5:      1     329      5 838983392 Star Trek: Generations (1994)
# 6:      1     355      5 838984474       Flintstones, The (1994)

# genres
# 1:                Comedy|Romance
# 2:         Action|Crime|Thriller
# 3:  Action|Drama|Sci-Fi|Thriller
# 4:       Action|Adventure|Sci-Fi
# 5: Action|Adventure|Drama|Sci-Fi
# 6:       Children|Comedy|Fantasy

################################################################################
#check each column by plotting 

#"rating"

#find out its range
unique(edx$rating)%>% sort() 
# [1] 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0

#plot its distribution
edx %>%
  ggplot(aes(rating)) + 
  geom_bar() + 
  ggtitle("Ratings")

#"useId"

#find unique number of users
n_distinct(edx$userId)
#[1] 69878

#plot histogram of userID
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

#"movieId"

#find unique number of movies
n_distinct(edx$movieId)

#plot histogram of movieId
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")
#[1] 10677

#"timestamp"

#covert "timestamp" to POSIXct data
edx$timestamp <- as.POSIXct(edx$timestamp, origin= "1970-01-01", tz="GMT")

#and find months of ratings.It is named "date"
edx <- edx %>% mutate(date = round_date(timestamp, unit = "month"))  

#plot date and rating
edx%>%
  group_by(date) %>%
  summarize(ratings =mean(rating)) %>% 
  ggplot(aes(x = date, y=ratings)) +geom_point() + geom_smooth(method = "lm")
  ggtitle("Months of rating")
  
#"title"

#extract the release years from the title. It is named "release year"
edx <- edx %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)))

#plot release year and rating
edx %>% group_by(release_year) %>%
  summarize(ratings = mean(rating)) %>% 
  ggplot(aes(release_year, ratings)) +
  geom_point()+geom_smooth(method="lm")+
  ggtitle("Release_year")

#"genres"

#find the number of unique genres
edx$genres %>% n_distinct()

#top 10 genres in terms of rating
edx %>% group_by(genres)%>%
  summarize(ratings = mean(rating))%>%top_n(10)

#plot genres and rating. 
#using genres which have more than 20000 ratings.
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating)) %>%
  filter(n >= 20000) %>% 
  mutate(genres = reorder(genres,avg)) %>%
  ggplot(aes(x = genres, y = avg)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Genres (n>20000)")

################################################################################
#making test and training data 10% train_set, 90% test_set

set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#To make sure we don’t include users and movies in the test set 
#that do not appear in the training set, 
#we remove these entries using the semi_join function:

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
################################################################################
#1 Naive model assuming the same rating for all movies.

mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse
#[1] 1.060054

rmse_results <- tibble(method = "Rating Average", RMSE = naive_rmse)%>% as.data.frame()
rmse_results

################################################################################
#2 movie effects

mu <- mean(train_set$rating)
movie_avg <- train_set %>%
  group_by(movieId)%>%
  summarize(b_i =mean(rating -mu))

movie_effect_pred <- mu + test_set %>%
  left_join(movie_avg, by="movieId") %>%
  pull(b_i)

movie_effect_rmse <- RMSE(test_set$rating, movie_effect_pred)
movie_effect_rmse
#[1] 0.9429615

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effects Model",  
                                     RMSE =movie_effect_rmse))%>% as.data.frame()
rmse_results
################################################################################
#3 movie and user effects

user_avg <- train_set %>%
  left_join(movie_avg, by= "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating- mu -b_i))
  
movie_user_effect_pred <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(pred = mu + b_i + b_u)%>%
  pull(pred)

movie_user_effect_rmse <- RMSE(test_set$rating, movie_user_effect_pred)
movie_user_effect_rmse
#[1] 0.8646844

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie User Effects Model",  
                                 RMSE =movie_user_effect_rmse))%>% as.data.frame()
rmse_results

################################################################################
#4 genre effects

g_avg <- train_set %>%
  left_join(movie_avg, by= "movieId")%>%
  left_join(user_avg, by="userId") %>%
  group_by(genres) %>%
  summarize(g = mean(rating- mu -b_i -b_u))

movie_user_genre_effect_pred <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(g_avg, by="genres") %>%
  mutate(pred = mu + b_i + b_u + g)%>%
  pull(pred)

movie_user_genre_effect_rmse <- RMSE(test_set$rating, movie_user_genre_effect_pred)
movie_user_genre_effect_rmse
#[1] 0.8643242

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie User Genre Effects Model",  
                                 RMSE =movie_user_genre_effect_rmse))%>% as.data.frame()
rmse_results

################################################################################
#5 release year effects

y_avg <- train_set %>%
  left_join(movie_avg, by= "movieId")%>%
  left_join(user_avg, by="userId") %>%
  left_join(g_avg, by="genres") %>%
  group_by(release_year) %>%
  summarize(y = mean(rating- mu -b_i -b_u -g))

movie_user_genre_year_effect_pred <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(g_avg, by="genres") %>%
  left_join(y_avg, by="release_year")%>%
  mutate(pred = mu + b_i + b_u + g +y)%>%
  pull(pred)

movie_user_genre_year_effect_rmse <- RMSE(test_set$rating, movie_user_genre_year_effect_pred)
movie_user_genre_year_effect_rmse
#[1] 0.8641262

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie User Genre Release Year Effects Model",  
                                 RMSE =movie_user_genre_year_effect_rmse))%>% as.data.frame()
rmse_results
################################################################################
#6 regularization (movie, user) 

#introducing lambda

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  reg_movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  reg_user_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

  predicted_ratings <- 
    test_set %>% 
    left_join(reg_movie_avg, by = "movieId") %>%
    left_join(reg_user_avg ,by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas,rmses)

lambda <- lambdas[which.min(rmses)]
lambda
#[1] 5

reg_movie_user_rmse <- rmses
reg_movie_user_rmse[5]
#[1] 0.8644477

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie User Effects Model",  
                                 RMSE =reg_movie_user_rmse[5]))%>% as.data.frame()
rmse_results
###############################################################################
#7 regularization  (movie, user,genres, release_year)

#calculating lambda

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  reg_movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  reg_user_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  g_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    left_join(reg_user_avg, by="userId") %>%
    group_by(genres) %>%
    summarize(g = sum(rating - b_i -b_u - mu)/(n()+l))  
  
  y_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    left_join(reg_user_avg, by="userId") %>%
    left_join(g_avg, by="genres") %>%
    group_by(release_year) %>%
    summarize(y = sum(rating - b_i -b_u -g - mu)/(n()+l)) 
  
  predicted_ratings <-test_set %>% 
    left_join(reg_movie_avg, by = "movieId") %>%
    left_join(reg_user_avg, by = "userId") %>%
    left_join(g_avg, by="genres") %>%
    left_join(y_avg, by="release_year")%>%
    mutate(pred = mu + b_i + b_u + g + y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas,rmses)
lambda <- lambdas[which.min(rmses)]
lambda
#[1] 4.5

min(rmses)
#[1] 0.8636478

#model summary
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie User Genre Release Year Effects Model",  
                                 RMSE =rmses[4.5]))%>% as.data.frame()
rmse_results
rmse_results %>% knitr::kable()

#  |method                                          |      RMSE|
#  |:-----------------------------------------------|---------:|
#  |Rating Average                                  | 1.0600537|
#  |Movie Effects Model                             | 0.9429615|
#  |Movie User Effects Model                        | 0.8646844|
#  |Movie User Genre Effects Model                  | 0.8643242|
#  |Movie User Genre Release Year Effects Model     | 0.8641262|
#  |Reg Movie User Effects Model                    | 0.8644477|
#  |Reg Movie User Genre Release Year Effects Model | 0.8639532|

# final model "Reg Movie User Genre Release Year Effects Model" 
#is proved to extract the least RMSE Value.
################################################################################
#validation calculating lambda

# add and cut columns in the process of training
validation <- validation %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% select(-timestamp)

# define lambdas as a variable
lambdas <- seq(0, 10, 0.25)

#calculate rmse using "Reg Movie User Genre Release Year Effects Model" 
val_rmse <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
 
  reg_movie_avg <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
     
  reg_user_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
      
  g_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    left_join(reg_user_avg, by="userId") %>%
    group_by(genres) %>%
    summarize(g = sum(rating - b_i -b_u - mu)/(n()+l))  
      
  y_avg <- train_set %>% 
    left_join(reg_movie_avg, by="movieId") %>%
    left_join(reg_user_avg, by="userId") %>%
    left_join(g_avg, by="genres") %>%
    group_by(release_year) %>%
    summarize(y = sum(rating - b_i -b_u -g - mu)/(n()+l)) 
      
  val_pred <-validation %>% 
    left_join(reg_movie_avg, by = "movieId") %>%
    left_join(reg_user_avg, by = "userId") %>%
    left_join(g_avg, by="genres") %>%
    left_join(y_avg, by="release_year")%>%
    mutate(pred = mu + b_i + b_u + g + y) %>%
    pull(pred)
   
    #replace NA with mu(the average rating in the training)
    val_pred <-replace(val_pred, is.na(val_pred),mu)
    
    return(RMSE(val_pred,validation$rating))
})

#plot lambdas and rmses
qplot(lambdas,val_rmse)

#find lambda and rmse which indicate the least value
lambda <- lambdas[which.min(val_rmse)]
lambda
#[1] 5

min(val_rmse)
#[1] 0.8646978

################################################################################
