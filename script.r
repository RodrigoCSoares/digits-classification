# Libraries import
library(pixmap)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(factoextra)

# Building dataset

  # Reading dir files 
  files_list <- list.files(path='./files')

  # Create matrix to store dataset rows
  mtx <- matrix(data = NA, nrow = length(files_list), ncol = 4096)

  # Iteration to fill the matrix
  for(i in 1:length(files_list)) {
    sample_path <- paste('./files/', files_list[i], sep = "")
    sample <- read.pnm(file = sample_path)
    sample_vector <- as.vector(sample@grey)
    mtx[i,] <- sample_vector
  }

  # Create dataframe from matrix
  dataframe <- as.data.frame(mtx)

  # Insert class column to predict methods
  dataframe$digito <- substr(files_list[1:length(files_list)], 1, 1) 

# Selecting samples to train and test objects
set.seed(4321)

sample_size <- floor(0.8 * nrow(dataframe))
samples <- sample(seq_len(nrow(dataframe)), size = sample_size)

# Building train and test objects
train_knn <- dataframe[samples, -ncol(dataframe)]
train <- dataframe[samples,]

test <- dataframe[-samples, -ncol(dataframe)]

# Getting train and test class
train_class <- dataframe[samples, ncol(dataframe)]

test_class <- as.factor(dataframe[-samples, ncol(dataframe)])

# Knn classification

knnResult1 <- knn(train_knn, test, train_class)
# knn 1 accuracy
confusionMatrix(knnResult1, test_class)
# knn 1 plot
plot(knnResult1)

knnResult3 <- knn(train_knn, test, train_class, k = 3)
# knn 3 accuracy
confusionMatrix(knnResult3, test_class)
# knn 3 plot
plot(knnResult3)

knnResult7 <- knn(train_knn, test, train_class, k = 7)
# knn 7 accuracy
confusionMatrix(knnResult7, test_class)
# knn 7 plot
plot(knnResult7)

knnResult9 <- knn(train_knn, test, train_class, k = 9)
# knn 9 accuracy
confusionMatrix(knnResult9, test_class)
# knn 9 plot
plot(knnResult9)

# SVM classification

# Building classifier model
classifier <- svm(formula = digito ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')
# Getting predict
svmPredict <- predict(classifier, newdata = test)
# SVM accuracy
confusionMatrix(svmPredict,test_class)
# SVM plot
plot(svmPredict)

# Tree decision classification

# Building tree model
model <- rpart(digito ~ ., 
                train, 
                method="class", 
                control = rpart.control(minsplit = 1))
# Getting predict
treePredict <- predict(model, newdata = test, type="class")
# Tree accuracy
confusionMatrix(treePredict, test_class)
# Tree plot
plot <- rpart.plot(model, type = 3)
# Tree predict plot
plot(treePredict)

# Cluster

# Selecting columns to cluster
cluster <- dataframe[,-ncol(dataframe)]
# kmeans with 10 values
kmeans_predict <- kmeans(cluster, 10)
# Checking accuracy with table
table(kmeans_predict$cluster)
table(dataframe[,ncol(dataframe)])

# PCA

# Create new dataframe with pca
dataframe.pca <- prcomp(dataframe[,-ncol(dataframe)], center = TRUE, scale. = FALSE)
newDataframe <- as.data.frame(predict(dataframe.pca, dataframe))
# Setting "digito" column to new dataframe
newDataframe$digito <- dataframe$digito

# Rerun predict methods

# Selecting samples to train and test objects with pca dataframe
set.seed(4321)

sample_size <- floor(0.8 * nrow(newDataframe))
samples <- sample(seq_len(nrow(newDataframe)), size = sample_size)

# Building train and test objects
train_knn <- newDataframe[samples, -ncol(newDataframe)]
train <- newDataframe[samples,]

test <- newDataframe[-samples, -ncol(newDataframe)]

# Getting train and test class
train_class <- newDataframe[samples, ncol(newDataframe)]

test_class <- as.factor(newDataframe[-samples, ncol(newDataframe)])

# Knn classification

knnResult1WithPCA <- knn(train_knn, test, train_class)
# knn 1 accuracy
confusionMatrix(knnResult1WithPCA, test_class)
# knn 1 plot
plot(knnResult1WithPCA)

knnResult3WithPCA <- knn(train_knn, test, train_class, k = 3)
# knn 3 accuracy
confusionMatrix(knnResult3WithPCA, test_class)
# knn 3 plot
plot(knnResult3WithPCA)

knnResult7WithPCA <- knn(train_knn, test, train_class, k = 7)
# knn 7 accuracy
confusionMatrix(knnResult7WithPCA, test_class)
# knn 7 plot
plot(knnResult7WithPCA)

knnResult9WithPCA <- knn(train_knn, test, train_class, k = 9)
# knn 9 accuracy
confusionMatrix(knnResult9WithPCA, test_class)
# knn 9 plot
plot(knnResult9WithPCA)

# SVM classification

# Building classifier model
classifier <- svm(formula = digito ~ .,
                  data = train,
                  type = 'C-classification',
                  kernel = 'linear')
# Getting predict
svmPredictWithPCA <- predict(classifier, newdata = test)
# SVM accuracy
confusionMatrix(svmPredictWithPCA,test_class)
# SVM plot
plot(svmPredictWithPCA)

# Tree decision classification

# Building tree model
model <- rpart(digito ~ ., 
               train, 
               method="class", 
               control = rpart.control(minsplit = 1))
# Getting predict
treePredictWithPCA <- predict(model, newdata = test, type="class")
# Tree accuracy
confusionMatrix(treePredictWithPCA, test_class)
# Tree plot
plot <- rpart.plot(model, type = 3)
# Tree predict plot
plot(treePredictWithPCA)

# Cluster

# Selecting columns to cluster
cluster <- newDataframe[,-ncol(newDataframe)]
# kmeans with 10 values
kmeans_predict_with_pca <- kmeans(cluster, 10)
# Checking accuracy with table
table(kmeans_predict_with_pca$cluster)
table(newDataframe[,ncol(newDataframe)])




