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

#unique users and movies
edx %>% summarize(n_users =n_distinct(userId), n_movies= n_distinct(movieId))

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



  
