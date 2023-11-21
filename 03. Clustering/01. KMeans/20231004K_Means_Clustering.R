# K-Medias

# Importación de las librerías
dataset = read.csv('03. Clustering/01. KMeans/mall.csv')
X = dataset[4:5]

# Método del codo para hallar el número óptimo de conglomerados
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(X, i)$withinss)
plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = paste('Método del Codo'),
     xlab = 'Número de clusters',
     ylab = 'WCSS')

# Ajustar K-Medias al dataframe
set.seed(29)
kmeans = kmeans(x = X,
                centers = 5,
                iter.max = 300,
                nstart = 10)

# Visualización de los clusters
library(cluster)
clusplot(x = X,
         clus = kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters de Clientes'),
         xlab = 'Ingreso Anual',
         ylab = 'Score de Gasto')