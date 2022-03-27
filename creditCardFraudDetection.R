# CREDIT CARD FRAUD DETECTION SYSTEM

# data set loading

creditcard_data <- read.csv("D:\\Projects\\Data Science\\Credit Card fraud detection\\creditcard.csv")

head(creditcard_data)

dim(creditcard_data$Class)

#------------normalizing the amount variable as it is out of scale-------------#
creditcard_data$Amount <- scale(creditcard_data$Amount)

table(creditcard_data$target)

new_data = creditcard_data[, -c(1)]

#--------------------changing every '0' and '1' to 0 and 1---------------------#
new_data$target[new_data$target == "'0'"] = 0 
new_data$target[new_data$target == "'1'"] = 1

barplot(table(new_data$target))

head(new_data)

# -----------------splitting into training and testing set---------------------#
library(caret)

set.seed(3456)

# splitting data into training and testing data
trainIndex <- createDataPartition(new_data$target, p=0.7, list = FALSE, times = 1)
train_data <- new_data[trainIndex, ]
test_data <- new_data[-trainIndex, ]

for (i in train_data$id[train_data$Class==1]){
  ids <- append(ids, i)
}

head(train_data)

#-----------------------SMOTE : Oversampling the data--------------------------#

# finding euclidean distance between two points
euclidean_dist <- function(pt1, pt2){
  dist <- 0
  
  for (i in 1:29){
    dist = dist + (pt1[, i] - pt2[, i])^2
  }
  return (sqrt(dist))
}

#-------------------------------------------------------------------------------

distances <- function(pt1){
  dist <- c()
  
  for (i in train_data$Id[train_data$target==1]){
    dist <- append(dist, euclidean_dist(pt1, train_data[i, ]))
  }
  
  return (dist)
}

# ------------------------------------------------------------------------------

sort_fn <- function(pt1, k=5){
  ids <- c()
  
  for (i in train_data$Id[train_data$target == 1]){
    y <- train_data$Id[i] 
    ids <- append(ids, y)
  }
  
  dist <- distances(pt1)  
  n <- length(dist)
  
  for (i in c(1:(n-1))){
    for (j in c(1:(n - i))){
      if (dist[j] > dist[j+1]){
        t <- dist[j]
        dist[j] <- dist[j + 1]
        dist[j + 1] <- t
        
        t <- ids[j]
        ids[j] <- ids[j + 1]
        ids[j + 1] <- t
      }
    }
  }
  
  z <- ids[1: (k + 1)]
  return (z)
}

#-------------------------------------------------------------------------------

knn_matrix <- matrix(data = c(384, 37723, 56045, 18803, 19358, 81531), ncol = 6, byrow = TRUE)

for (i in train_data$Id[train_data$target == 1]){
  
  id <- sort_fn(train_data[i, ], 5)
  
  knn_matrix <- rbind(knn_matrix, (id[1:6]))
}

head(knn_matrix)

dim(knn_matrix)

knn_matrix[1, ]

#------------------------------------------------------------------------------
V1 <- c()
V2 <- c()
V3 <- c()
V4 <- c()
V5 <- c()
V6 <- c()
V7 <- c()
V8 <- c()
V9 <- c()
V10 <- c()
V11 <- c()
V12 <- c()
V13 <- c()
V14 <- c()
V15 <- c()
V16 <- c()
V17 <- c()
V18 <- c()
V19 <- c()
V20 <- c()
V21 <- c()
V22 <- c()
V23 <- c()
V24 <- c()
V25 <- c()
V26 <- c()
V27 <- c()
V28 <- c()
Amount <- c()
id <- c()
class1 <- c()

print(runif(1))
#-------------------------------------------------------------------------------
for (i in 1:161415){
  print(i)
  h <- sample(c(1:345), 1)
  
  k <- sample(c(2:6), 1)
  
  
  a <- train_data[train_data$Id == knn_matrix[h, 1], 1:29]
  b <- train_data[train_data$Id == knn_matrix[h, k], 1:29]
  
  diff <- a - b
  
  dp <- a + (diff * runif(1)) # new data point
  
  x <- x + 1
  
  V1 <- append(V1, dp[,1])
  V2 <- append(V2, dp[,2])
  V3 <- append(V3, dp[,3])
  V4 <- append(V4, dp[,4])
  V5 <- append(V5, dp[,5])
  V6 <- append(V6, dp[,6])
  V7 <- append(V7, dp[,7])
  V8 <- append(V8, dp[,8])
  V9 <- append(V9, dp[,9])
  V10 <- append(V10, dp[,10])
  V11 <- append(V11, dp[,10])
  V12 <- append(V12, dp[,12])
  V13 <- append(V13, dp[,13])
  V14 <- append(V14, dp[,14])
  V15 <- append(V15, dp[,15])
  V16 <- append(V16, dp[,16])
  V17 <- append(V17, dp[,17])
  V18 <- append(V18, dp[,18])
  V19 <- append(V19, dp[,19])
  V20 <- append(V20, dp[,20])
  V21 <- append(V21, dp[,21])
  V22 <- append(V22, dp[,22])
  V23 <- append(V23, dp[,23])
  V24 <- append(V24, dp[,24])
  V25 <- append(V25, dp[,25])
  V26 <- append(V26, dp[,26])
  V27 <- append(V27, dp[,27])
  V28 <- append(V28, dp[,28])
  Amount <- append(Amount, dp[,29])
  class1 <- append(class1, 1)
  id <- append(id, x)

}

head(result)

a1  <-     V1[75001:80535]
a2  <-     V2[75001:80535]
a3  <-     V3[75001:80535]
a4  <-     V4[75001:80535]
a5  <-     V5[75001:80535]
a6  <-     V6[75001:80535]
a7  <-     V7[75001:80535]
a8  <-     V8[75001:80535]
a9  <-     V9[75001:80535]
a10 <-    V10[75001:80535]
a11 <-    V11[75001:80535]
a12 <-    V12[75001:80535]
a13 <-    V13[75001:80535]
a14 <-    V14[75001:80535]
a15 <-    V15[75001:80535]
a16 <-    V16[75001:80535]
a17 <-    V17[75001:80535]
a18 <-    V18[75001:80535]
a19 <-    V19[75001:80535]
a20 <-    V20[75001:80535]
a21 <-    V21[75001:80535]
a22 <-    V22[75001:80535]
a23 <-    V23[75001:80535]
a24 <-    V24[75001:80535]
a25 <-    V25[75001:80535]
a26 <-    V26[75001:80535]
a27 <-    V27[75001:80535]
a28 <-    V28[75001:80535]
a29 <- Amount[75001:80535]
a30 <- class1[75001:80535]
a31 <-     id[75001:80535]

result <- data.frame(V1 = a1, V2 = a2, V3 = a3, V4 = a4,
                     V5 = a5, V6 = a6, V7 = a7, V8 = a8,
                     V9 = a9, V10 = a10, V11 = a11, V12 = a12,
                     V13 = a13, V14 = a14, V15 = a15, V16 = a16,
                     V17 = a17, V18 = a18, V19 = a19, V20 = a20,
                     V21 = a21, V22 = a22, V23 = a23, V24 = a24,
                     V25 = a25, V26 = a26, V27 = a27, V28 = a28,
                     Amount = a29, target = a30, Id = a31)

train_data <- rbind(train_data, result)

table(train_data$target)

barplot(table(train_data$target))
#-------------------comparing before and after oversampling-------------------#

barplot(table(new_data$target))
barplot(table(train_data$target))

#-------------------------shuffling the training data set----------------------#

set.seed(42)
rows <- sample(nrow(train_data))
train_data2 <- train_data[rows, ]

#------------------------------------------------------------------------------#
#---------------------------------data storing---------------------------------#
X <- data.matrix(train_data2[, 1:29])
Y <- data.matrix(train_data2[, 30])
X2 <- data.matrix(t(train_data2[,1:29]))
X3 <- data.matrix(test_data[,1:29])
y <- data.matrix(test_data[, 30])

#-----------------------using logistic regression algorithm--------------------#
#------------------------------basic functions---------------------------------#

dim(weights)
dim(X)

h <- function(X, weights, bias){
  dim(X)
  dim(weights)
  y_pred <- X %*% weights
  
  return (y_pred + bias)
}
# 360436 x 1

sigmoid <- function(X, weights, bias){
  
  return (1/(1 + exp(-h(X, weights, bias))))  # y predicted value
}

sigmoid2 <- function(z){
  return (1/(1 + exp(-z)))
}

cost <- function(X, weights, bias){
  z <- X %*% weights + bias
  predict_1 <- Y * (log(sigmoid2(z)))
  predict_0 <- (1 - Y) * (1 - log(sigmoid2(z)))
  
  return (-1/nrow(Y) * sum(predict_1 + predict_0))
}

head(X)


#-----------------------------gradient descent---------------------------------#

epoch <- 1000

lr <- 0.0000000002

weights <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0), byrow = FALSE, ncol = 1)

bias <- 0
cost_value <- c()

for (i in 1:epoch){
  print(i)
  y_pred <- sigmoid(X, weights, bias)  # (X : (360000, 29)), Y : (360000, 1))
  
  
  
  temp1 <- lr * (X2 %*% (Y - y_pred))    
  temp2 <- lr * sum(Y - y_pred)
  
  weights <- weights - temp1
  bias <- bias - temp2

}


#-------------------------------VALIDATION-------------------------------------#
y_pred <- X3 %*% weights
cnt <- 0

for (i in 1:85441){
  if (y_pred[i, 1] > 0.5){
    ans <- 1
  }
  
  else{
    ans <- 0
  }
  if (ans == y[i, 1]){
    cnt <- cnt + 1
  }
}

sprintf("Accuracy : %f", (cnt/85541 * 100))

#------------------------------------------------------------------------------#
print(cost(X, weights, bias))
#-------------------------------------------------------------------------------

#----------------------------------rough---------------------------------------#
table(test_data$target)

printFunction <- function(x, y){
  print(x[, 1] + y[, 1])
}

printFunction(new_data[1, ], new_data[2, ])

for (i in train_data$id[train_data$Class == 1]){
  print(i)
}

h <- train_data[1, 1:29] - train_data[2, 1:29]
print(h)
print(h[1])

table(train_data$Class)

rm(class1)

m <- c(1, 2, 3, 4)
head(m)

m <- append(m, 5)
head(m)

print(dp[, 1])

print(train_data[, 1][2])
head(train_data)


sum(h(train_data2))
print(train_data2[1, ])

g <- c(1, 2, 3)
r <- c(4, 5, 6)

train_data2$target[1] * 8
print(train_data2$target[1])

s <- train_data2$target[train_data2$Id == 279048]
print(s)
s * 8
print(s * 8)

head(train_data2)

sum((1 - train_data2$target)*log(1 - sigmoid(train_data2)))
sum(train_data2$target)

sum((1 - train_data2$target) * log(1 - sigmoid(train_data2)))

summation <- 0
for (s in 1:20){
  summation <- summation + (1 - new_training_data$target[s]) * log(1- sigmoid(new_training_data[s, ]))
  print(summation)
}

head(new_training_data)

matrix1 <- matrix(c(c(1, 2, 3), c(4, 5, 6)), byrow = TRUE, ncol = 3)
print(matrix1)

matrix2 <- matrix(c(c(1, 2), c(3, 4), c(5, 6)), byrow = TRUE, ncol = 2)
print(matrix2)

print(matrix2 %*% matrix1)

print(train_data2$V1[1])

head(sigmoid(X))

s <- matrix(c(c(1, 2, 3, 4), c(5, 6, 7, 8)), byrow = TRUE, ncol = 4)
o <- matrix(c(c(1, 2, 3, 4), c(5, 6, 7, 8)), byrow = TRUE, ncol = 4)

head(1-s)

m <- t(X)

rm(der_c)

head(a)

plot(train_data2$V1[1:40], train_data2$target[1:40], pch = 16, cex = 1.3)
plot(train_data2$V3[1:40], train_data2$target[1:40], pch = 16, cex = 1.3)
