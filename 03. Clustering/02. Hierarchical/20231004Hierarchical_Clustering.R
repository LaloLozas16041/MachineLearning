# Cluster Jerarquico

# Importar Datos
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Uso del dendrograma para obtener el número de grupos óptimos
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrograma'),
     xlab = 'Clientes',
     ylab = 'Distancia Euclidiana')

# Ajuste de modelo Jerarquico
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualización de clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters de clientes'),
         xlab = 'Ingreso Anual',
         ylab = 'Score de Gasto')
