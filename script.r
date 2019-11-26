# Import das bibliotecas
library(pixmap)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)

# Leitura dos arquivos do diretório
files_list <- list.files(path='./files')

# Cria o matriz dos exemplares
mtx <- matrix(, nrow = length(files_list), ncol = 4096)

# Itera os arquivos e popula a matriz
for (i in 1:length(files_list)) {
  sample_path <- paste('./files/', files_list[i], sep = "")
  sample <- read.pnm(file = sample_path)
  sample_vector <- as.vector(sample@grey)
  mtx[i,] <- sample_vector
}

# Cria o dataframe
dataframe <- as.data.frame(mtx)

# Insere a coluna com o digito referente ao exemplar
for(i in 1:length(files_list)) {
  dataframe$digito[i] <- substr(files_list[i], 1, 1) 
}

# Classificando os dados
samples <- sample(1:nrow(dataframe), (0.8 * nrow(dataframe)))

cl <- dataframe[samples, ncol(dataframe)]
train <- dataframe[samples, -ncol(dataframe)]
test <- dataframe[-samples, -ncol(dataframe)]

# Implementando o Knn
knnResult1 <- knn(train, test, cl)
knnResult3 <- knn(train, test, cl, k = 3)
knnResult7 <- knn(train, test, cl, k = 7)
knnResult9 <- knn(train, test, cl, k = 9)
expectedResult <- as.factor(dataframe[-samples, ncol(dataframe)])

confusionMatrix(knnResult1, expectedResult)
confusionMatrix(knnResult3, expectedResult)
confusionMatrix(knnResult7, expectedResult)
confusionMatrix(knnResult9, expectedResult)

# Implementando o SVM
svmTrain <- dataframe[samples,]
svmTest <- dataframe[-samples,]
svmTestClass <- svmTest[ ,ncol(svmTest)]
svmTest <- svmTest[ ,-ncol(svmTest)]

classifier = svm(formula = digito ~ .,
                 data = svmTrain,
                 type = 'C-classification',
                 kernel = 'linear')

svmPredict = predict(classifier, newdata = svmTest)
svmAcurrancy = length(which(svmPredict == svmTestClass))/length(svmTestClass)

# Implementando o modelo da arvore de decisao
train <- dataframe[samples,]
test <-  dataframe[-samples,]

testClass <- test[,ncol(dataframe)]

test <- test[,-ncol(dataframe)]

modelo <- rpart(digito ~ ., train, method="class", control = rpart.control(minsplit = 1))
pred <- predict(modelo, newdata = test, type="class")
plot <- rpart.plot(modelo, type = 3)

confusionMatrix(pred, testClass )