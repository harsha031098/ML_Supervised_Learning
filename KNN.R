library(tidyverse)
library(caTools)
library('class') 

library('caret')


setwd("C:/Users/harsh/OneDrive/Documents/Healthcare/Lab5/")
file_path <- "C:\\Users\\harsh\\OneDrive\\Documents\\Healthcare\\Lab5\\heart.csv"
data <- read_csv(file_path)

glimpse(data)

# Clean data
df_clean <- data %>%
  filter_all(all_vars(!is.na(.)))

missing_values <- is.na(df_clean)

missing_sum <- colSums(missing_values)

df_clean$output=factor(df_clean$output) # converting the 'class' feature into factor type


colnames(df_clean)


set.seed(42)
sample <- sample.split(df_clean$output,SplitRatio=0.7)
train_set <- filter(df_clean,sample == TRUE)
dim(train_set)
test_set <- filter(df_clean,sample == FALSE)
dim(test_set)
y_test=test_set$output
y_train=train_set$output
x_test=test_set[,-14]
x_train=train_set[,-14]

# Calculate errors for K values from 1 to 30
error <- vector(mode = "numeric", length = 30)
for(i in 1:30) {
  set.seed(42)
  pred_i <- knn(x_train, x_test, y_train, k = i)
  error[i] <- mean(pred_i != y_test)
}

# Plot the error rate for different K values
error_df <- tibble(K = 1:30, ErrorRate = error)
ggplot(error_df, aes(x = K, y = ErrorRate)) +
  geom_line(color = 'red', linetype = 'dashed') +
  geom_point(color = 'blue', size = 2) +
  labs(title = 'Error Rate K Value', x = 'K Value', y = 'Mean Error')

# Find the K value with the minimum error
optimal_k <- which.min(error)
minimum_error <- min(error)

cat("Minimum error:", minimum_error, "at K =", optimal_k, "\n")

pred_test=knn(train_set[,-14],test_set[,-14],train_set$output,k=4) # After training on the train data we calculating the output labels for the test data for k=2
confusion=table(pred_test,test_set$output) 
confusion
confusionMatrix(reference=test_set$output, data=pred_test,mode="everything",positive="1")

