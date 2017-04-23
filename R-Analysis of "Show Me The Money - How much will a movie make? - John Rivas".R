# bring in movie data and libraries
library(RcmdrMisc)
movie <- read.csv("~/movie_metadata.csv", header=TRUE)
movie2 <- movie[,c(3,4,5,6,8,12,13,14,19,23,24,25,26,28,1,16,22,9)]
movie2 <- movie2[complete.cases(movie2),]
test.num <- sample(1:3873, 100, replace=F)
movie.test <- movie2[test.num,-17]
test.gross <- movie2[test.num,c(6,18)]
test.gross$movie_title <- as.character(test.gross$movie_title)
movie.train <- movie2[-test.num,-6]
movie.train$gross <- log(movie.train$gross)

# See structure of data
str(movie)

# Run baseline model - Simple regression using all variables
movie.lm <- lm(gross~.,data = movie.train)
summary(movie.lm)
par(mfrow=c(2,2))
plot(movie.lm)

# Predict test 
pred1 <- predict(movie.lm,movie.test)
pred1 <- exp(pred1)
final1 <- cbind(round(pred1,digits = 0),test.gross)
colnames(final1) <- c("Predicted","Actual")
rownames(final1) <- seq(100)
final1 <- as.data.frame(final1)
final1$Error_sqr <- sqrt((final1$Predicted - final1$Actual)^2)
final1$Error <- final1$Predicted - final1$Actual
final1.1 <- as.data.frame(final1.1)
final1.1

# Run stepwise and build best model
stepwise(movie.lm)
movie2.lm <- lm(formula = gross ~ actor_1_facebook_likes +
                  actor_2_facebook_likes +
                  actor_3_facebook_likes +
                  cast_total_facebook_likes +
                  color +
                  content_rating +
                  duration +
                  imdb_score +
                  movie_facebook_likes +
                  num_critic_for_reviews +
                  num_voted_users +
                  title_year , 
                    data = movie.train)
summary(movie2.lm)
plot(movie2.lm)
 
# Predict test 2
pred2 <- predict(movie2.lm,movie.test)
pred2 <- exp(pred2)
final2 <- cbind(test.gross$movie_title,round(pred2,digits = 0),round(test.gross$gross,digits = 0))
colnames(final2) <- c("Movie","Predicted","Actual")
rownames(final2) <- seq(100)
final2 <- as.data.frame(final2)
final2

final2$Predicted <- format(final2$Predicted, big.mark = ",",big.interval = 3)
final2$Actual <- format(final2$Actual, big.mark = ",",big.interval = 3)
