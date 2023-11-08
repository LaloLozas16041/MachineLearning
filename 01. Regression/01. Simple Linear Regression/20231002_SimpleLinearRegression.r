# Regresión lineal simple

# Importando el conjunto de datos
dataset = read.csv('01. Regression/01. Simple Linear Regression/Salary_Data.csv')

# Dividir el conjunto de datos en conjunto de entrenamiento y conjunto de prueba
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalado de características
# training_set = scale(training_set)
# test_set = scale(test_set)

# Ajuste de regresión lineal simple al Conjunto de Entrenamiento
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# Predicción de los resultados del Conjunto de Test
y_pred = predict(regressor, newdata = test_set)

# Visualizando los resultados del Conjunto de Entrenamiento
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salario vs Años de Experiencia (Training Set)') +
  xlab('Años de Experiencia') +
  ylab('Salario')

# Visualizando los resultados del Conjunto de Test
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salario vs Años de Experiencia (Test Set)') +
  xlab('Años de Experiencia') +
  ylab('Salario')
