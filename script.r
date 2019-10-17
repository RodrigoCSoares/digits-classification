library(pixmap)
library(class)

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

knnResult1 <- knn(train, test, cl)
knnResult3 <- knn(train, test, cl, k = 3)
knnResult7 <- knn(train, test, cl, k = 7)
knnResult9 <- knn(train, test, cl, k = 9)
expectedResult <- dataframe[-samples, ncol(dataframe)]
