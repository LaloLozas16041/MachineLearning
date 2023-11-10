# SVR (Support Vector Regression)

# Importando el conjunto de datos
dataset <- read.csv('01. Regression/04. Support Vector Regression (SVR)/Position_Salaries.csv')
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

# Ajustando SVR al conjunto de datos
#install.packages('e1071')
library(e1071)
regressor <- svm(formula = Salary ~ .,
                 data = dataset,
                 type = 'eps-regression',
                 kernel = 'radial')

# Prediciendo un nuevo resultado
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualizando los resultados de SVR
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Verdad o Falsedad (SVR)') +
  xlab('Nivel') +
  ylab('Salario')

# Visualizando los resultados de SVR (para una resolución más alta y una curva más suave)
# install.packages('ggplot2')
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Verdad o Falsedad (SVR)') +
  xlab('Nivel') +
  ylab('Salario')