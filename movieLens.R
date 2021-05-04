#used following libraries
library(tidyverse)
library(caret)
library(data.table)

# installing tinytex to knit pdf from rmd files
install.packages('tinytex')
tinytex::install_tinytex()

################################################################################
# Create edx set, validation set (final hold-out test set)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
#Q1
dim(edx)

#Q2
sum(edx$rating==0)
sum(edx$rating==3)

#Q3
n_distinct(edx$movieId)

#Q4
n_distinct(edx$userId)

#Q5
dat <- c("Drama", "Comedy", "Thriller", "Romance")
sapply(dat, function(g){
  sum(str_detect(edx$genres,g))
})

#Q6
edx %>% group_by(movieId, title) %>%
  summarize(n=n()) %>% 
  arrange(desc(n))

#Q7, Q8
edx %>% group_by(rating) %>%
  summarize(n=n()) %>% 
  arrange(desc(n))
################################################################################
#what kinds of data?
str(edx)
summary(edx)

#unique values
edx %>% summarize(n_users =n_distinct(userId), n_movies= n_distinct(movieId),
                  n_rating =n_distinct(rating),n_title=n_distinct(title),
                  n_genres= n_distinct(genres))

#n_users n_movies n_rating n_title n_genres
#1   69878    10677       10   10676      797


#plot

#movies
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#users
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

#ratings
edx %>%
  ggplot(aes(rating)) + 
  geom_bar() + 
  ggtitle("ratings")

mean(edx$rating)

#test and training data
set.seed(2021, sample.kind = "Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
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

#1 Naive model assuming the same rating for all movies. 
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
#>[1] 1.059586

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
#[1] 0.9434034

#3 user effects

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
#[1] 0.8660241


# dealing with validation file

valid_pred_rating <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(pred = mu + b_i + b_u)%>%
  pull(pred)

#replace NA by mu
sum(is.na(valid_pred_rating))
#there is 11 NAs in the valid_pred_rating. Thus replace them with mean rating
valid_pred_rating <- replace(valid_pred_rating, which(is.na(valid_pred_rating)), mu)

RMSE(validation$rating,valid_pred_rating)
#[1] 0.8663858