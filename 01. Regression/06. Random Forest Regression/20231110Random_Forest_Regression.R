# Regresión de Bosque Aleatorio

# Importando el conjunto de datos
dataset <- read.csv('01. Regression/06. Random Forest Regression/Position_Salaries.csv')
dataset <- dataset[2:3]

# Dividiendo el conjunto de datos en conjunto de entrenamiento y conjunto de prueba
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Escalado de características
# training_set = scale(training_set)
# test_set = scale(test_set)

# Ajustando la Regresión de Bosque Aleatorio al conjunto de datos
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor <- randomForest(x = dataset[-2],
                         y = dataset$Salary,
                         ntree = 500)

# Prediciendo un nuevo resultado con la Regresión de Bosque Aleatorio
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualizando los resultados de la Regresión de Bosque Aleatorio (mayor resolución)
# install.packages('ggplot2')
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Verdad o Falsedad (Regresión de Bosque Aleatorio)') +
  xlab('Nivel') +
  ylab('Salario')