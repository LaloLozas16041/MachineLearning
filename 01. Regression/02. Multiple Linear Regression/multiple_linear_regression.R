# Regresión lineal múltiple

# Importando el conjunto de datos
dataset = read.csv('01. Regression/02. Multiple Linear Regression/50_Startups.csv')

# Codificación de datos categóricos
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Dividir el conjunto de datos en conjunto de entrenamiento y conjunto de prueba
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalado de características
# training_set = scale(training_set)
# test_set = scale(test_set)

# Ajustar la regresión lineal múltiple al Conjunto de Entrenamiento
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicción de los resultados del Conjunto de Test
y_pred = predict(regressor, newdata = test_set)