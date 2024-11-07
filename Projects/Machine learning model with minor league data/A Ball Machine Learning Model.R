# Importing libraries and datasets
library(dplyr)
library(caret)
library(tidyverse)
library(dslabs)

standard <- read.csv("fangraphs-minor-league-leaders (4).csv")
advanced <- read.csv("fangraphs-minor-league-leaders (5).csv")
batted <- read.csv("fangraphs-minor-league-leaders (6).csv")
mlb <- read.csv("mlb stats - sheet2.csv")

# Cleaning and joining tables
batted <- batted %>% select(c(-4,-5,-6))
advanced <- advanced %>% select(c(-4,-5,-6))
mlb <- mlb %>% select(-1,-3)

standard %>% arrange(desc(PlayerId))

t1 <- inner_join(standard, advanced, by = c("Name", "Season", "Team"))
t2 <- inner_join(t1, batted, by = c("Name", "Season", "Team"))
table <- inner_join(t2, mlb, by = "Name")

# Restricting the dataset to plyers under the age of 21
table(table$Age)
table %>% filter(Age < 21)



# Machine learning model
ML <- function(){
  
  # Creating the test and train sets
  total_index <- createDataPartition(table$AB, 1, 0.5, list = FALSE)
  total_train <- table[total_index,]
  total_test <- table[-total_index,]
  
  # Creating a new table that contains only the values I want to test for
  test_sample <- total_test %>% select(LD., FB., GB.FB, HR.FB, BB.K)
  train_sample <- total_train %>% select(LD., FB., GB.FB, HR.FB, BB.K)
  
  # This for loop iterates through every column of test_sample, which is all the 
  # statistics I want to test for
  for(i in 1:ncol(train_sample)){
    column_data <- train_sample[i]
    test_col_data <- test_sample[i]
    
    # This for loop splits data into equal groups of 3 based off each statistic, 
    # then 4, 5, etc. until 30 equal groups have been made. 
    for(x in seq(from = 3, to = 30, by = 1)){
      
      
      col_increment <- (max(column_data) - min(column_data))/x
      sequence <- seq(from = min(column_data), to = max(column_data), 
                      by = (max(column_data) - min(column_data))/x)
      result <- c()
      low_range <- c()
      high_range <- c()
      
      # This for loop defines a success as any result that belongs to the group defined
      # by the previous for loop. It defines result as the percentage of players in a
      # group that had a wrc+ above 100 in the major leagues, essentially finding the
      # percentage of players who were above average hitters in the major leagues
      for(y in sequence){
        success <- ifelse(column_data > y & column_data < (y + col_increment), 1, 0)
        low_range <- append(low_range, y)
        high_range <- append(high_range, (y + col_increment))
        result <- append(result, mean(success == 1 & total_train$wRC..y > 100))
      }
      traindf <- data.frame(result, low_range, high_range)
      count = 1
      # This for loop is responsible for the output of the model. It finds any group
      # that has at least 20% of its players with a wrc+ over 100. It then displays the 
      # percentage of players with a wrc+ over 100 in the train model, the bounds of
      # the group, and the result in the test model as well. 
      for(result in traindf$result){
        if(result > 0.2){
          print(paste(names(column_data), x))
          print(traindf[count, ])
          success <- ifelse(test_col_data > low_range & 
                              test_col_data < high_range, 1, 0)
          test_success <- mean(success == 1 & total_test$wRC..y > 100)
          print(paste("Test Success Rate: ", test_success))
        }
        count = count + 1
      }
    }
  }
}
ML()