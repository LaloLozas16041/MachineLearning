# XGBoost para *clasificar*

# Cargar Datos
dataset = read.csv('09. Extra/02.- XGBoost/Churn_Modelling.csv')
dataset = dataset[4:14]

# Codificar las variables categoricas como factores
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Partir el conjunto en Train y Test
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Ajustar el XGBoost al Set de Train
#install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-11]), label = training_set$Exited, nrounds = 10)

# Predecir resultados en el conjunto de Test
y_pred = predict(classifier, newdata = as.matrix(test_set[-11]))
y_pred = (y_pred >= 0.5)

# Matriz de confusión
cm = table(test_set[, 11], y_pred)

# Acá va la aplicación del Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(dataset$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = xgboost(data = as.matrix(training_fold[-11]), label = training_fold$Exited, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))

