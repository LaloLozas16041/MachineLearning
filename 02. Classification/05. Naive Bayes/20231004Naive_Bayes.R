# Naive Bayes

# Importando el conjunto de datos
dataset <- read.csv('02. Classification/01. Logistic Regression/SNA.csv')
dataset <- dataset[3:5]

# Codificando la característica objetivo como factor
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

# Dividiendo el conjunto de datos en conjunto de entrenamiento y conjunto de prueba
# install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Escalado de características
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# Ajustando Naive Bayes al conjunto de entrenamiento
# install.packages('e1071')
library(e1071)
classifier <- naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)

# Prediciendo los resultados del conjunto de prueba
y_pred <- predict(classifier, newdata = test_set[-3])

# Creando la matriz de confusión
cm <- table(test_set[, 3], y_pred)

# Visualizando los resultados del conjunto de entrenamiento
library(ElemStatLearn)
set <- training_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Edad', 'SalarioEstimado')
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (conjunto de entrenamiento)',
     xlab = 'Edad', ylab = 'Salario Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualizando los resultados del conjunto de prueba
library(ElemStatLearn)
set <- test_set
X1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Edad', 'SalarioEstimado')
y_grid <- predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Naive Bayes (conjunto de prueba)',
     xlab = 'Edad', ylab = 'Salario Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))