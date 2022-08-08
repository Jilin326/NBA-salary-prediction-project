library(tidyverse)
library(leaps)
library(modelr)
library(tree)
setwd(dir = "D:/nba project")
stat_data <- read.csv(
  "Seasons_Stats.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
stat_data <- as_tibble(stat_data)
salary <- read.csv(
  "Player - Salaries per Year (1990 - 2017).csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
salary <- as_tibble(salary)
stat_data <- select(stat_data, Year, Player, Pos, Age, G, MP, WS48, PTS, TRB,
               AST, STL, BLK, eFG_percent, USG_percent 
               )
stat_data <- stat_data %>%
  filter(Year >= 2000)
salary <- select(salary, -Full_Team_Name, -Season_Start)
salary <- filter(salary, Year >= 2000)
distinct_stat <- group_by(stat_data, Year) %>%
  distinct(Player, .keep_all = TRUE)
distinct_salary <- group_by(salary, Year) %>%
  distinct(Player, .keep_all = TRUE)
joined_data <- inner_join(distinct_stat, distinct_salary,
                          by = c("Player", "Year"))
# don't know how to do time series analysis right now
# so just use 2017's data to analyze
data_2017 <- filter(joined_data, Year == 2017)
data_2017 <- mutate(data_2017, 
                    salary = Salary_in_dollar/10000,
                    MP_pg = MP/G,
                    PTS_pg = PTS/G,
                    TRB_pg = TRB/G,
                    AST_pg = AST/G,
                    STL_pg = STL/G,
                    BLK_pg = BLK/G)
#convert position to factor
data_2017$Pos <- as_factor(data_2017$Pos)
data_2017 <- mutate(data_2017,
                    position = fct_collapse(
                      Pos,
                      guard = c("SG", "PG"),
                      forward = c("SF"),
                      center = c("PF", "C", "PF-C")
                    ))
#get rid of NA
apply(is.na(data_2017), 2, which)
data_2017 <- data_2017 %>%
  filter(G > 5 & PTS > 50)
#data_2017$X3P_percent[is.na(data_2017$X3P_percent)] <- 0
#data_2017$FT_percent[is.na(data_2017$FT_percent)] <- 0.5
# exclude Lebron James
#data_2017 <- filter(data_2017, salary <= 3000)
#fit a linear model using regsubset using 19 variables
# and using 10-fold cross validation


#add a column indicating whether rookie contract
draft14 <- as_tibble(read.csv("14 Draft.csv", header = TRUE))
draft15 <- as_tibble(read.csv("15 Draft.csv", header = TRUE))
draft16 <- as_tibble(read.csv("16 Draft.csv", header = FALSE))
draft14 <- select(draft14, Player)
draft15 <- select(draft15, Player)
draft16 <- select(draft16, V4)
draft16 <- rename(draft16, Player = V4)
draft14 <- add_column(draft14, rookie=rep(1,length.out = 30))
draft15 <- add_column(draft15, rookie=rep(1,length.out = 30))
draft16 <- add_column(draft16, rookie=rep(1,length.out = 30))

data_2017 <- left_join(data_2017, draft14, by="Player")
data_2017 <- left_join(data_2017, draft15, by="Player")
data_2017 <- left_join(data_2017, draft16, by="Player")
data_2017[is.na(data_2017)] <- 0
data_2017[
  data_2017$rookie.x == 1 | data_2017$rookie.y == 1,
  "rookie"
] <- 1
data_2017 <- select(data_2017, -rookie.x, -rookie.y)

k <- 10
n <- nrow(data_2017)
var_nbr <- 12
folds <- sample(rep(1:k, length.out = n))
cv.errors <- matrix(NA, k, var_nbr,
                    dimnames = list(NULL, paste(1:var_nbr)))
predict.regsubset <- function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}
for (j in 1:k) {
  best.fit <- regsubsets(
    salary ~ Age + USG_percent +G + MP_pg + WS48 + eFG_percent 
    + PTS_pg + TRB_pg + AST_pg
    + STL_pg + BLK_pg + rookie, 
    data = data_2017[folds != j,],
    nvmax = var_nbr
    )
  for (i in 1:var_nbr) {
    pred <- predict.regsubset(best.fit, data_2017[folds == j,],
                              id = i)
    cv.errors[j,i] <- mean(
      (data_2017$salary[folds == j] - pred)^2
      )
  }
}
apply(cv.errors, 2, mean)
linear.fit <- lm(
  salary ~ Age + eFG_percent + PTS_pg + TRB_pg + rookie,
               data = data_2017)
grid <- data_grid(
  data_2017,
  Age = seq(from = range(Age)[1],to = range(Age)[2]),
  eFG_percent = 0.5,
  PTS_pg = 25,
  TRB_pg = 7,
  rookie = 0
)
grid <- grid %>%
  add_predictions(linear.fit)
data_2017 <- data_2017 %>%
  add_residuals(linear.fit) %>%
  mutate(player_no = c(1:nrow(data_2017)))
ggplot(data_2017)+
  geom_ref_line(h = 0, size = 2, colour = "red")+
  geom_point(aes(Age, resid))
data_2017 <- data_2017 %>%
  mutate(pred = predict(linear.fit, data_2017))

#tree method
library(gbm)
library(boot)
boost_error <- c(1:k)
for (i in 1:k) {
  boost <- gbm(salary ~ Age + eFG_percent + PTS_pg + TRB_pg
               +USG_percent + AST_pg +STL_pg + BLK_pg + rookie,
               data = data_2017[folds != i,],
               distribution = "gaussian",
               n.trees = 5000,
               interaction.depth = 4)
  
  boost_pred <- predict(boost,
                        newdata = data_2017[folds == i,],
                        n.trees = 5000)
  boost_error[i] <- mean(
    (data_2017$salary[folds ==i] - boost_pred)^2)
}

#tree
train <- sample(rep(1:k,length.out=n))
tree_error <- c(1:k)
for (i in 1:k) {
  tree <- tree(
    salary~Age+eFG_percent+PTS_pg+TRB_pg+USG_percent+AST_pg+
      STL_pg+BLK_pg+rookie,
    data = data_2017[train != i,]
  )
  predict_tree <- predict(
    tree, 
    newdata = data_2017[train == i,]
  )
  tree_error[i] <- 
    mean((data_2017$salary[train == i]-predict_tree)^2)
}

#random forest
library(randomForest)
random_forest_error <- c(1:k)
for (i in 1:k) {
  random_forest <- randomForest(
    salary~Age+eFG_percent+PTS_pg+TRB_pg+USG_percent+AST_pg+
      STL_pg+BLK_pg+rookie,
    data = data_2017[train != i,],
    importance=TRUE
  )
  predict_ranFOrest <- predict(
    random_forest, 
    newdata = data_2017[train == i,]
  )
  random_forest_error[i] <- 
    mean((data_2017$salary[train == i]-predict_ranFOrest)^2)
}



