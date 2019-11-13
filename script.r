library(pixmap)
library(class)
library(caret)

# Le todos os arquivos do diretorio
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

knnResult1 <- as.vector(knn(train, test, cl))
knnResult3 <- as.vector(knn(train, test, cl, k = 3))
knnResult7 <- as.vector(knn(train, test, cl, k = 7))
knnResult9 <- as.vector(knn(train, test, cl, k = 9))
expectedResult <- as.vector(dataframe[-samples, ncol(dataframe)])

resultDataset <- as.data.frame(knnResult1)
resultDataset[,2] <- as.data.frame(knnResult3)
resultDataset[,3] <- as.data.frame(knnResult7)
resultDataset[,4] <- as.data.frame(knnResult9)
resultDataset[,5] <- as.data.frame(expectedResult)

knnAccuracy <- vector()
# Calculando a acuracia do KNN
for (i in 1:ncol(resultDataset) - 1) {
  tab <- table(resultDataset[,i] == resultDataset[,5])
  freq <- tab[names(tab)==TRUE]
  knnAccuracy[i] <- freq/nrow(resultDataset)
}

knnResult1 <- knn(train, test, cl)
knnResult3 <- knn(train, test, cl, k = 3)
knnResult7 <- knn(train, test, cl, k = 7)
knnResult9 <- knn(train, test, cl, k = 9)
expectedResult <- as.factor(dataframe[-samples, ncol(dataframe)])

confusionMatrix(knnResult1, expectedResult)
confusionMatrix(knnResult3, expectedResult)
confusionMatrix(knnResult7, expectedResult)
confusionMatrix(knnResult9, expectedResult)