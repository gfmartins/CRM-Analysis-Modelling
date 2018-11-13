table1 <- 



# Use: if we want to get pattern in data without a target variable
# First part: Run the model K-means
# Second part: Hiercarchical clustering and compare the clusters of the models

############################################### General template for Machine learning ################################################## 

# 1) Importar dataset
dataset_clustering<- dataset_donations %>%
  select(
    donation.date,
    donation.year,
    donor.no,
    donor.postcode,
    donor.category,
    donor.gender,
    donation.amount,
    nominal,
    source.group,
    source
  ) %>%
  group_by(donation.year, donor.no) %>%
  mutate(
    number.donation.year = n(),
    value.donations.year = mean(donation.amount)
  ) %>%
  ungroup() %>%
  group_by(donor.no) %>%
  summarize(
    number.donations = mean(number.donation.year, na.rm = TRUE),
    value.donations = mean(value.donations.year, na.rm = TRUE)
  ) %>%
  ungroup()
  

# Scale the data if neccesary (by preProcess = )
# library(caret)
## Create an object with the pre processing parameters
ObjectPreprocessParams <- preProcess(dataset_clustering, method=c("scale"))
# summarize transform parameters
print(ObjectPreprocessParams)
## Transform the dataset using the parameters
dataset_clustering <- predict(ObjectPreprocessParams, dataset_clustering)
# Summarize the transformed dataset
summary(dataset_clustering)



###### First part: K-means ###### 

# 4.1) Train the model

# Use the elbow method to find the optimal number of clusters
## If we know beforehand that there are for example two subgroups, we omit this step
## Use map_dbl to run many models with varying value of k (centers)
library(purrr)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = dataset_clustering, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
## The correct value is where the scope of the line plotted softness
table_elbow <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(table_elbow, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Number of clusters decided to use
NumberClusters<- "NUMBER OF CLUSTERS"


### Use Silhouette Analysis for determining the correct number of clusters

## Generate a k-means model using the pam() function with a k value that we want to asses its correctness
library(cluster)
ObjectPam <- pam(dataset_clustering, k = NumberClusters)

## Plot the silhouette visual for the pam_k2 model
plot(silhouette(ObjectPam))


# Plot the Silhouette index with different k values
## Use map_dbl to run many models with varying value of k (in this case k = 2:10)
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = dataset_clustering, k = k)
  model$silinfo$avg.width
})

## Generate a data frame containing both k and sil_width
table_silhouette <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

## Plot 
ggplot(table_silhouette, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)


### Applying k-means to the dataset
## Dataset, número de clusters que se ve mediante la observación del gráfico (donde se suaviza la pendiente), número de iteraciones (valor fijo 300), nstart = valor fijo en 10, iter.max = valor máximo que R buscará (es bueno ponerlo para ver cual es el mejor resultado pero no es necesario)
set.seed(29)
ObjectKM<- kmeans(dataset_clustering, NumberClusters, iter.max = 300, nstart = 10, iter.max = 50)
VectorClusters<- ObjectKM$cluster
## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_km <- mutate(dataset_clustering, cluster.assigned = VectorClusters)

# Visualize the clusters
## Option1
## lines = 0 es porque no queremos que se grafiquen lineas de distancia, shade = true (parametro fijo), color = parametro fijo, labels = 2 (para tener todo en el grafico), plotchar = parametro fijo, span = parametro fijo) 
library(cluster)
clusplot(dataset_clustering,
         VectorClusters,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of "),
         xlab = "COLUMN1",
         ylab = "COLUMN2")

## Option2
ggplot(dataset_clustered_km, aes(x = "X VALUE", y = "Y VALUE", color = factor(cluster.assigned))) +
  geom_point()




###### Second part: Hierarchical Clustering ###### 

# 4.2) Train the model

# Determine number of clusters: Dendogram
## Be aware of choosing the correct method = according to the problem
ObjectHc<- hclust(dist(dataset, method = "euclidean"), method = "ward.D")
# ObjectHc2 <- hclust(dist(dataset, method = "OTHER METHOD"), method = "OTHER METHOD")
# ObjectHc2 <- hclust(dist(dataset, method = "OTHER METHOD"), method = "OTHER METHOD")

# Plot dendogram to see the the number of clusters
## You look for the longest vertical line that is not crossed by the imaginary horizontal lines, numbers of vertical lines crossed at that hight its the number of clusters) or elbow method
## Plot one single dendogram
plot(ObjectHc)

## Compare multiple dendograms
# par(mfrow = c(1,3))
# plot(ObjectHc, main = 'METHOD')
# plot(ObjectHc2, main = 'METHOD')
# plot(ObjectHc3, main = 'METHOD')

# Plot a dendogram coloring the different clusters according to a certain distance height
library(dendextend)
## Create a dendrogram object from the hclust variable
ObjectDendogram <- as.dendrogram(ObjectHc)

## Color branches by cluster formed from the cut at a height of 20
plotColoredDend <- color_branches(ObjectDendogram, h = 20)

## Plot the dendrogram with clusters colored below height 20
plot(plotColoredDend)


# Cluster data according to number of clusters dectected
## NumberClusters = number of clusters detected on dendogram
NumberClusters<- "NUMBER OF CLUSTERS DETECTED"
## Make the clusters by assigning a number of wanted clusters
VectorClusters<- cutree(ObjectHc, NumberClusters)


# #  Make the clusters by cutting the dendogram at a certain hight (drawing an imaginary horizontal line)
# ## Normally the lower this number is, the more clusters will be created
# ## HightClusters = maximum distance permitted by cluster
# HeightClusters<- "DESIRED HEIGHT"
# ## Make the clusters by assigning a certain hight
# VectorClustersWithHeight <- cutree(hc_players, h = HeightClusters)


## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_hc <- mutate(dataset_clustering, cluster.assigned = VectorClusters)

# Explore results
## Count the cluster assignments
count(dataset_clustered_hc, cluster.assigned)

# Calculate summary statistics for each category (e.g. mean())
dataset_clustered_hc %>% 
  group_by(cluster.assigned) %>% 
  mutate(count.per.cluster = n()) %>% 
  summarise_all(funs(mean(.)))

# Plot the clusters and color them using their cluster
ggplot(dataset_clustered_hc, aes(x = "X VALUE", y = "Y VALUE", color = factor(cluster.assigned))) +
  geom_point()

