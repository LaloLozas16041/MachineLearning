# Regresión Logistica

# Importando el conjunto de datos
dataset = read.csv('02. Classification/01. Logistic Regression/SNA.csv')
dataset = dataset[3:5]

# Codificar la característica de destino como factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Dividir el conjunto de datos en conjunto de entrenamiento y conjunto de prueba
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalado de factores
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Ajuste de la regresión logística al conjunto de entrenamiento
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predicción de los resultados del conjunto de pruebas
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Haciendo la Matriz de Confusión
cm = table(test_set[, 3], y_pred > 0.5)

# Visualizando los resultados del conjunto de entrenamiento
#install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Regresión Logistica (Training set)',
     xlab = 'Edad', ylab = 'Salario Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualizando los resultados del conjunto de pruebas
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Regresión Logistica (Test set)',
     xlab = 'Edad', ylab = 'Salario Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))