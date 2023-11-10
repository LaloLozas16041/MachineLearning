# Regresión de Árbol de Decisión

# Importando el conjunto de datos
dataset <- read.csv('01. Regression/05. Decision Tree Regression/Position_Salaries.csv')
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

# Ajustando la Regresión de Árbol de Decisión al conjunto de datos
# install.packages('rpart')
library(rpart)
regressor <- rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# Prediciendo un nuevo resultado con la Regresión de Árbol de Decisión
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualizando los resultados de la Regresión de Árbol de Decisión (mayor resolución)
# install.packages('ggplot2')
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Verdad o Falsedad (Regresión de Árbol de Decisión)') +
  xlab('Nivel') +
  ylab('Salario')

# Dibujando el árbol
plot(regressor)
text(regressor)