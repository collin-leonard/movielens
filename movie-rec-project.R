## Run "test-validation.R" before this code! ##
## Working directory remains the movielens project folder ##
## Load necessary libraries ##

libraries <- c("dplyr","tidyverse", "lubridate", 
               "dslabs", "caret", "recommenderlab", 
               "reshape2", "data.table", "gam", "ggplot")

xfun::pkg_attach(libraries)

## Load data from project folder

edx <- readRDS("./data/edx.rds")
validation <- readRDS("./data/validation.rds")

## Manipulate time data, stratified by week in data format

edx <- edx %>% 
       mutate(date = as_datetime(timestamp)) %>% 
       mutate(date = round_date(date, unit = "week"))
saveRDS(edx, file = "./data/edx.rds")

validation %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date = round_date(date, unit = "week")) %>% 
  saveRDS(file = "./data/validation.rds")

# Generate counts of each movie and user
counts <- edx %>% 
  group_by(movieId) %>% 
  mutate(movie_n = n()) %>% 
  ungroup() %>% 
  group_by(userId) %>% 
  mutate(user_n = n()) %>% 
  ungroup() %>% 
  select(movie_n,user_n)

# size_change function uses a filter value "n", and returns the percent of observations of edx after filtering.
# filtering criteria: movies with more ratings than n, users who have rated > n movies

size_change <- function(n){
  temp <- sum(counts$user_n > n & counts$movie_n >n)
  perc_change <- temp/length(edx$rating) * 100
  return(perc_change)
}

# mean_comp function uses the same filtering criteria as size_change, but returns the residual difference between
# the mean rating of edx and the mean rating of the new filtered data set

mean_comp <- function(n){
  temp <- edx %>% 
    group_by(movieId) %>%
    filter(n() >= n) %>% ungroup() %>% 
    group_by(userId) %>%
    filter(n() >= n) %>% ungroup() %>%
    select(rating)
  mean_dif <- mean(edx$rating)-mean(temp$rating)
  return(mean_dif)
}

# generate a data frame of percent changes and residuals based on the n values stored in sizes

sizes <- seq(0,1000,20)
dim_size <- sapply(sizes,size_change)
mean_resids <- sapply(sizes, mean_comp)
small_comp <- data.frame(sizes, dim_size, mean_resids)

# create plots to compart the results of the data frame side by side

p1 <- ggplot(small_comp, aes(x=sizes, y=dim_size)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Percent of total edx observations") +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Count filter") +
  theme_dark()

p2 <- ggplot(small_comp, aes(x=sizes, y=mean_resids)) +
  geom_line(color=rgb(0.2, 0.6, 0.9, 1),size=2) +
  ggtitle("Mean edx_small residuals") +
  scale_y_continuous(name = "Mean residuals") +
  scale_x_continuous(name = "Count filter") +
  theme_dark()

# save the plot as a png file
png(file = "./figs/filter_fig.png", width = 600, height = 400)
p1+p2
dev.off()

# We choose n > 250 to minimize mean residual while maximizing percent of edx

edx_small <- edx %>% 
  group_by(movieId) %>%
  filter(n() >= 250) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 250) %>% ungroup()

# Clear up workspace to free up memory!

rm(counts, edx, p1, p2, small_comp, mean_resids, dim_size, libraries, sizes, validation, mean_comp, size_change)

# Create a rating matrix 'y'

p1 <- ggplot(edx, aes(y= rating)) +
  geom_boxplot(fill = "#69b3a2", colour = "black", outlier.colour = "black", weight = 4) +
  ggtitle("Dist. of edx ratings") +
  scale_y_continuous(name = "Ratings") +
  theme_dark() +
  ggplot(edx_small, aes(y=rating)) +
  geom_boxplot(fill = rgb(0.2, 0.6, 0.9, 1), colour = "black", outlier.colour = "black", weight = 4) +
  ggtitle("Dist. of edx_small ratings") +
  scale_y_continuous(name = "Ratings") +
  theme_dark()

# Save plot as png file

png(file = "./figs/dist_fig.png", width = 600, height = 400)
p1
dev.off()

rm(p1)

# Create matrix pairing users to movies

y <- edx_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rm(edx_small)

# Record unique movie titles, name rows and columns of rating matrix

movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

rownames(y)<- y[,1]
y <- y[,-1]

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

rm(edx, movie_titles)

## The NA's in the rating matrix are replace with estimates from the mean user and movie rating

raw_mean <- mean(as.vector(as.matrix(y)), na.rm = TRUE )

# count number of non-NA's in each row of training set
row_valid <- rowSums(!is.na(y[,]))

# count number of non-NA's in each column of training set
col_valid <- colSums(!is.na(y[,]))

# calculate user biases
user_biases <- rowMeans(y[,] - raw_mean, na.rm = TRUE) / row_valid

# calculate item biases
item_biases <- colMeans(y[,] - raw_mean, na.rm = TRUE) / col_valid

# memory cleanup
rm(row_valid, col_valid)

# make a copy of the original matrix
ty <- y

for (i in 1:nrow(ty)) {
  for (j in 1:ncol(ty)) {
    
    # if the matrix element has an NA, fill in with baseline predictor
    if(is.na(ty[i,j])) {
      ty[i,j] <- raw_mean + user_biases[i] + item_biases[j]
      
      # ensure new values are within valid ratings bounds
      if (ty[i,j] > 5) ty[i,j] <- 5
      if (ty[i,j] < 0.5) ty[i,j] <- 0.5
    } # end if
    
  } # end for j
} # end for i

rm(i,item_biases, user_biases, raw_mean,j, y)

# Run svd on filled matrix

s <- svd(ty)

# var_exp function takes a number "i" between 1 and the length(s$d), and returns the percent variability explained by the 
# SVD using the elements 1:i
var_exp <- function(i){
  sum(s$d[1:i]^2)/sum(s$d^2) * 100
}

i <- seq(1:length(s$d))
ve <- sapply(i,var_exp)

# ive is a map of percent variability based on the number of elements used in var_exp
ive <- as.data.frame(cbind(i,ve))

# plot and store results!
p1 <- ggplot(ive, aes(x=i, y=ve)) +
  geom_line(color = rgb(0.2, 0.6, 0.9, 1),size=2) +
  ggtitle("Percent variability explained by elements of SVD") +
  scale_y_continuous(name = "Percent variability") +
  scale_x_continuous(name = "Elements of SVD accounted for") +
  theme_dark()

png(file = "./figs/pve_fig.png", width = 600, height = 400)
p1
dev.off()

# Cleon up

rm(p1, ive, i, ve, var_exp, d)

# Plot magnitudes

d_df <- data.frame(1:length(s$d),s$d)
colnames(d_df) <- c("x","d")

p1 <- ggplot(d_df, aes(x = x, y = d)) +
  geom_point(color = "#69b3a2") +
  ggtitle("Singular value magnitudes of prediction matrix") +
  scale_y_continuous(name = "Magnitudes") +
  scale_x_continuous(name = "Singular values") +
  xlim(0,1000) +
  theme_dark()

# Store plot
png(file = "./figs/sv_mag_fig.png", width = 600, height = 400)
p1
dev.off()

# my_image creates a correlation plot of a matrix
my_image <- function(x, title, ... ){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = c(-1,1), ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 2, labels = FALSE, tick = FALSE, las = 2)
  title(main = title)
}

# Calculate residuals of actual ratings - predictions for 1-3 factors

resid <-  ty - with(s,(u[,1, drop=FALSE]*d[1]) %*% t(v[,1, drop=FALSE]))
resid2 <- ty - with(s,sweep(u[,1:2], 2, d[1:2], FUN="*") %*% t(v[,1:2]))
resid3 <- ty - with(s,sweep(u[,1:3], 2, d[1:3], FUN="*") %*% t(v[,1:3]))

# Save correlation matricies

png(file = "./figs/res1_image_fig.png", width = 600, height = 400)
my_image(cor(resid[1:50,1:50]),  "Correlation between residuals (1 factor)")
dev.off()

png(file = "./figs/res2_image_fig.png", width = 600, height = 400)
my_image(cor(resid2[1:50,1:50]), "Correlation between residuals (1:2 factors)")
dev.off()

png(file = "./figs/res3_image_fig.png", width = 600, height = 400)
my_image(cor(resid3[1:50,1:50]), "Correlation between residuals (1:3 factors)")
dev.off()

rm(resid, resid2, resid3, ty, d_df, my_image)

# Seems clear that the first component takes into account the majority of the variability, so k = 1 is used 

k = 1

s_k <- Diagonal(x = s$d[1:k])
dim(s_k)

U_k <- s$u[, 1:k]
dim(U_k)

V_k <- t(s$v)[1:k, ]
dim(V_k)

# Generate a prediction table from svd decomp (first element)
predicted <- U_k %*% s_k %*% V_k

# Convert to a matrix to rename

pred_mat <- as.matrix(predicted)

colnames(pred_mat) <- colnames(ty)
rownames(pred_mat) <- rownames(ty)

# Change back into a df to utilize the cbind command, adding userId to be include in the pivot operation later
pred_df <- as.data.frame(pred_mat)
pred_df <- cbind(rownames(pred_df),pred_df)
colnames(pred_df)[1] <- "userId"

# Clean up workspace!
rm(k, U_k, V_k, s_k, s, predicted, pred_mat, y, ty)

# In a memory taxing operation, use pivot longer to generate a df to be combined with both edx and validation

svd_pred <- pivot_longer(pred_df, -userId, names_to = "title", values_to = "svd_pred")
svd_pred$userId <- as.integer(svd_pred$userId)

edx <- readRDS("./data/edx.rds")
validation <- readRDS("./data/validation.rds")
edx <- left_join(edx,svd_pred,by = c("userId","title"))
validation <- left_join(validation,svd_pred,by = c("userId","title"))

# Test the predictions that are not NA in the validation data, to see if the rmse is close to a good score
na_index_valid <- which(is.na(validation$svd_pred))
na_valid <- validation[na_index_valid,]
svd_valid <- validation[-na_index_valid,]

# RMSE function takes a vector of actual ratings and their corresponding predictions, 
# and produces the root mean square error

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

RMSE(svd_valid$rating, svd_valid$svd_pred)

# The RMSE is not even close to low enough (goal is less than .865), and will only damage a pure bias method
# so the biases method is now used fully abandoning the svd method :(

rm(na_edx, na_valid, svd_edx, svd_pred, svd_valid, na_index, na_index_edx, na_index_valid)

# Generate test and training sets for bias training. Will not touch validation data until final test

set.seed(719)
test_index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
test_set <- edx[test_index,]
train_set <- edx[-test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# rmses is a function provided by the class, that takes a lambda (for regularization), 
# and produces the resulting rmse of the data
rmses <- function(l){
  
  # mu is naive mean
  mu <- mean(train_set$rating)

  # movie bias
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user bias
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # genre bias
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # time bias
  b_t <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(date) %>%
    summarize(b_t = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  
  # join baises with test set to generate new predictions
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by = "date") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
    select(pred)
  
  return(RMSE(test_set$rating, predicted_ratings$pred))
}



# Choose the amount of decimals included in lambda
deci <- 4

# Choose what increment in first set of lambdas tested
steps <- 1

# Choose range of lambdas, with increment of steps
lambdas <- seq(1,10,steps)

# Generate empty vectors to be filled by for statement
min_rmse <- vector(mode = "numeric", length = deci)
ideal_lambda <- vector(mode = "numeric", length = deci)

# For statement cycles through the rmses function, but each time narrows down the lamdba tested
# The result is two vectors of length deci + 1, one of the lambdas used, and their corresponding rmses

for (i in 1:(1+deci)) {
  my_model <- sapply(lambdas,rmses)
  min_i <- which.min(my_model)
  min_rmse[i] <- min(my_model)
  ideal_lambda[i] <- lambdas[min_i]
  steps <- steps/5
  lambdas <- seq(lambdas[min_i]-steps,lambdas[min_i]+steps, steps)
}

# Plot the precision of lamdas to understand effiency vs. precision in lambda optimization

model_df <- as.data.frame(cbind(ideal_lambda, min_rmse))

p1 <- ggplot(model_df, aes(x = 0:4, y = min_rmse)) +
  geom_point(color = "#69b3a2") +
  ggtitle("Lambda precision vs minimum RMSE generated") +
  scale_y_continuous(name = "Minimum model RMSE") +
  scale_x_continuous(name = "Decimal precision in lambda for regularization") +
  theme_dark()

# Save plot

png(file = "./figs/lambda_prec_fig.png", width = 600, height = 400)
p1
dev.off()

# final_rmses works the same as rmses function, but uses the whole edx set as 'training'
# and the previously optimized lambda to generate predictions for the validation set

final_rmses <- function(l){
  
  # naive mean
  mu <- mean(edx$rating)
  
  # movie bias
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # user bias
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # genre bias
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  # time bias
  b_t <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(date) %>%
    summarize(b_t = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  
  # predictions joining biases with the validation set
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by = "date") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t)
  
  return(RMSE(validation$rating, predicted_ratings$pred))
}

# The rmse of this bias method is much lower that the svd method used previously!

final_rmses(ideal_lambda[3])
