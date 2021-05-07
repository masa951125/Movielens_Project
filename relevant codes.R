# installing tinytex to knit pdf from rmd files
install.packages(c('tinytex', 'rmarkdown'))
tinytex::install_tinytex()

# other methods
#using lambda= 5
lambda
#[1] 5

mu <- mean(train_set$rating) 

reg_movie_avg <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i =n())

reg_user_avg <- train_set %>% 
  left_join(reg_user_avg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda), n_u=n())

g_avg <- train_set %>% 
  left_join(reg_movie_avg, by="movieId") %>%
  left_join(reg_user_avg, by="userId") %>%
  group_by(genres) %>%
  summarize(g = sum(rating - b_i -b_u - mu)/(n()+lambda), n_g=n())  

y_avg <- train_set %>% 
  left_join(reg_movie_avg, by="movieId") %>%
  left_join(reg_user_avg, by="userId") %>%
  left_join(g_avg, by="genres") %>%
  group_by(release_year) %>%
  summarize(y = sum(rating - b_i -b_u -g - mu)/(n()+lambda),n_y=n()) 

predicted_ratings <-test_set %>% 
  left_join(reg_movie_avg, by = "movieId") %>%
  left_join(reg_user_avg, by = "userId") %>%
  left_join(g_avg, by="genres") %>%
  left_join(y_avg, by="release_year")%>%
  mutate(pred = mu + b_i + b_u + g + y) %>%
  pull(pred)

reg_movie_user_genre_year_rmse <-RMSE(predicted_ratings, test_set$rating)
reg_movie_user_genre_year_rmse
#[1] 0.86365

validation <- validation %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% select(-timestamp)


test_val <- validation %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by= "genres") %>%
  semi_join(train_set, by="release_year")

valid_pred_rating <-validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(g, by="genres") %>%
  left_join(y, by="release_year")%>%
  mutate(pred = mu + b_i + b_u +g +y)%>%
  pull(pred)

dat <- validation[which(is.na(valid_pred_rating))]

validation_rmse <- RMSE(validation$rating, valid_pred_rating)
validation_rmse
#[1] 0.8646954


val_predicted_ratings <-validation %>% 
  left_join(reg_movie_avg, by = "movieId") %>%
  left_join(reg_user_avg, by = "userId") %>%
  left_join(g_avg, by="genres") %>%
  left_join(y_avg, by="release_year")%>%
  mutate(pred = mu + b_i + b_u + g + y) %>%
  pull(pred)
