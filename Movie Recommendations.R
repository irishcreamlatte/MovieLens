
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

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


#### Preliminary Analysis 

head(edx)
summary(edx)
str(edx)


table(edx$rating)
edx %>% summarize(n = n_distinct(movieId))
edx %>% summarize(n = n_distinct(userId))


edx %>% group_by(movieId, title) %>% 
    summarize(avg_rating = mean(rating)) %>% 
    ggplot(aes(avg_rating)) + geom_histogram() + 
    labs(title = "Average Movie Ratings", x = "Average Rating")


edx %>% group_by(movieId, title) %>% 
    summarize(n = n()) %>% 
    arrange(desc(n)) %>% 
    top_n(10) 

edx %>% group_by(movieId, title) %>% 
    summarize(n = n()) %>% 
    arrange(desc(n)) %>% 
    top_n(10)

edx %>% group_by(userId) %>% 
    summarise(avg_rating = mean(rating)) %>% 
    ggplot(aes(avg_rating)) + geom_histogram() + labs(title = "Mean User Rating", x = "Average Rating")

library(lubridate)
edx %>% mutate(year = year(as_datetime(timestamp))) %>%
    group_by(year) %>% 
    summarize(avg = mean(rating)) %>% 
    arrange(year) %>% 
    ggplot(aes(year, avg)) +
    geom_line() + 
    geom_smooth(color = "red")

edx %>% group_by(genres) %>% 
    summarize(avg = mean(rating)) %>% 
    arrange(desc(avg)) %>% 
    top_n(10)

edx %>% group_by(genres) %>% 
    summarize(avg = mean(rating)) %>% 
    arrange(avg) %>%  
    top_n(-10)

#### Clean: TIMESTAMP

edx <- edx %>% 
    mutate(year = year(as_datetime(timestamp)))
str(edx)

validation <- validation %>% 
    mutate(year = year(as_datetime(timestamp)))
str(validation)

#### RMSE Function

RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}


#### MEAN MODEL

mu <- mean(edx$rating)
mu

rmse1<- RMSE(validation$rating, mu)
rmse1

rmse_results <- tibble(Method = "Mean", RMSE = rmse1)

#### MOVIE EFFECTS

movie_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))

prediction2 <- mu + validation %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    pull(b_i)

rmse2 <- RMSE(validation$rating, prediction2)
rmse2

rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie Effects", RMSE = rmse2))

#### USER EFFECTS 

user_avgs <- edx %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = mean(rating - mu - b_i))

prediction3 <- validation %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    left_join(user_avgs, by = "userId") %>% 
    mutate(predict = mu + b_i + b_u) %>% 
    pull(predict)

rmse3 <- RMSE(validation$rating, prediction3)
rmse3

rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie and User Effects", RMSE = rmse3))

#### TIME EFFECTS 

time_effects <- edx %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    left_join(user_avgs, by = "userId") %>% 
    group_by(year) %>% 
    summarize(b_t = mean(rating - mu - b_i - b_u))

prediction4 <- validation %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    left_join(user_avgs, by = "userId") %>% 
    left_join(time_effects, by = "year") %>%  
    mutate(predict = mu + b_i + b_u + b_t) %>% 
    pull(predict)

rmse4 <- RMSE(validation$rating, prediction4)
rmse4

rmse_results <- bind_rows(rmse_results, tibble(Method = "With Time Effects", RMSE = rmse4))

#### GENRE EFFECTS 

genre_effects <- edx %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    left_join(user_avgs, by = "userId") %>% 
    left_join(time_effects, by = "year") %>%  
    group_by(genres) %>% 
    summarize(b_g = mean(rating - mu - b_i - b_u - b_t))

prediction5 <- validation %>% 
    mutate(year = year(as_datetime(timestamp))) %>% 
    left_join(movie_avgs, by = "movieId") %>% 
    left_join(user_avgs, by = "userId") %>% 
    left_join(time_effects, by = "year") %>%  
    left_join(genre_effects, by = "genres") %>% 
    mutate(predict = mu + b_i + b_u + b_t + b_g) %>% 
    pull(predict)

rmse5 <- RMSE(validation$rating, prediction5)
rmse5

rmse_results <- bind_rows(rmse_results, tibble(Method = "With Genre Effects", RMSE = rmse5))

#### REGULARIZATION

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
        group_by(movieId) %>% 
        summarize(b_i = sum(rating - mu)/(n() + l))
    
    b_u <- edx %>% 
        left_join(b_i, by = "movieId") %>% 
        group_by(userId) %>% 
        summarize(b_u = sum(rating - mu - b_i)/(n() + l))
    
    b_t <- edx %>% 
        mutate(year = year(as_datetime(timestamp))) %>% 
        left_join(b_i, by = "movieId") %>% 
        left_join(b_u, by = "userId") %>% 
        group_by(year) %>% 
        summarize(b_t = sum(rating - mu - b_i - b_u)/(n() + l))
        
    b_g <- edx %>% 
        mutate(year = year(as_datetime(timestamp))) %>% 
        left_join(b_i, by = "movieId") %>% 
        left_join(b_u, by = "userId") %>% 
        left_join(b_t, by = "year") %>% 
        group_by(genres) %>% 
        summarize(b_g = sum(rating - mu - b_i - b_u - b_t)/(n() + l))
    
    predicted_ratings <- validation %>% 
        mutate(year = year(as_datetime(timestamp))) %>% 
        left_join(b_i, by = "movieId") %>% 
        left_join(b_u, by = "userId") %>% 
        left_join(b_t, by = "year") %>% 
        left_join(b_g, by = "genres") %>% 
        mutate(prediction = mu + b_i + b_u + b_t + b_g) %>% 
        pull(prediction)
    
    return(RMSE(validation$rating, predicted_ratings))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

movie_reg <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n() + lambda))

prediction6 <- validation %>% 
    left_join(movie_reg, by = "movieId") %>% 
    mutate(prediction = mu + b_i) %>% 
    pull(prediction)

rmse6 <- RMSE(validation$rating, prediction6)
rmse6

rmse_results <- bind_rows(rmse_results, tibble(Method = "Regularized Movie Effects", RMSE = rmse6))

user_reg <- edx %>% 
    left_join(movie_reg, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))

prediction7 <- validation %>% 
    left_join(movie_reg, by = "movieId") %>% 
    left_join(user_reg, by = "userId") %>% 
    mutate(predict = mu + b_i + b_u) %>% 
    pull(predict)

rmse7 <- RMSE(validation$rating, prediction7)
rmse7

rmse_results <- bind_rows(rmse_results, tibble(Method = "With Regularized User Effects", RMSE = rmse7))

time_reg <- edx %>% 
    left_join(movie_reg, by = "movieId") %>% 
    left_join(user_reg, by = "userId") %>% 
    group_by(year) %>% 
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n() + lambda))

prediction8 <- validation %>%
    left_join(movie_reg, by = "movieId") %>% 
    left_join(user_reg, by = "userId") %>% 
    left_join(time_reg, by = "year") %>% 
    mutate(predict = mu + b_i + b_u + b_t) %>% 
    pull(predict)

rmse8 <- RMSE(validation$rating, prediction8)
rmse8

rmse_results <- bind_rows(rmse_results, tibble(Method = "With Regularized Time Effects", RMSE = rmse8))

genre_reg <- edx %>% 
    left_join(movie_reg, by = "movieId") %>% 
    left_join(user_reg, by = "userId") %>% 
    left_join(time_reg, by = "year") %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_i - b_u - b_t)/(n() + lambda))

prediction9 <- validation %>%
    left_join(movie_reg, by = "movieId") %>% 
    left_join(user_reg, by = "userId") %>% 
    left_join(time_reg, by = "year") %>% 
    left_join(genre_reg, by = "genres") %>% 
    mutate(predict = mu + b_i + b_u + b_t + b_g) %>% 
    pull(predict)

rmse9 <- RMSE(validation$rating, prediction9)
rmse9

rmse_results <- bind_rows(rmse_results, tibble(Method = "With Regularized Genre Effects", RMSE = rmse9))
