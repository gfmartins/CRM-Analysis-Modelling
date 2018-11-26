library(purrr)
library(tidyverse)
library(caret)
library(readxl)
library(cluster)
library(parallel)
library(plotly)
library(fpc)
library(factoextra)
library(dbscan)
library(magrittr)
library(ggthemes)
library(gridExtra)

# Use: if we want to get pattern in data without a target variable
# First part: Run the model K-means
# Second part: Hiercarchical clustering and compare the clusters of the models

############################################### General template for Machine learning ################################################## 

# 1) Importar dataset
dataset_pre_clustering <- dataset_donations %>%
  select(
    donation.date,
    donation.year,
    donor.no,
    donation.amount
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
  ungroup() %>% 
  filter(value.donations < 10000, number.donations < 30)

dataset_clustering<- dataset_pre_clustering %>%
  select(number.donations, value.donations) 


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
NumberClusters<- 3


## Use Silhouette Analysis for determining the correct number of clusters

## Generate a k-means model using the pam() function with a k value that we want to asses its correctness
 no_cores <- detectCores() - 1

 ## Initiate cluster
 cl <- makeCluster(no_cores, type="FORK")

 ## Create Pam object
 ObjectPam <- pam(dataset_clustering, k = NumberClusters)

 stopCluster(cl)


 ## Plot the silhouette visual for the ObjectPam model
 plot(silhouette(ObjectPam))


# Plot the Silhouette index with different k values
## Use map_dbl to run many models with varying value of k (in this case k = 2:10)

no_cores <- detectCores() - 1

## Initiate cluster
cl <- makeCluster(no_cores, type="FORK")

sil_width <- map_dbl(2:5,  function(k){
  model <- pam(x = dataset_clustering, k = k)
  model$silinfo$avg.width
})

stopCluster(cl)


## Generate a data frame containing both k and sil_width
table_silhouette <- data.frame(
  k = 2:5,
  sil_width = sil_width
)

## Plot
ggplot(table_silhouette, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:5)


### Applying k-means to the dataset
## Dataset, número de clusters que se ve mediante la observación del gráfico (donde se suaviza la pendiente), número de iteraciones (valor fijo 300), nstart = valor fijo en 10, iter.max = valor máximo que R buscará (es bueno ponerlo para ver cual es el mejor resultado pero no es necesario)
set.seed(29)
ObjectKM<- kmeans(dataset_clustering, NumberClusters, iter.max = 300, nstart = 10)
VectorClusters<- ObjectKM$cluster
## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_km <- mutate(dataset_clustering, cluster.assigned = VectorClusters)

# Visualize the clusters
## Option1
## lines = 0 es porque no queremos que se grafiquen lineas de distancia, shade = true (parametro fijo), color = parametro fijo, labels = 2 (para tener todo en el grafico), plotchar = parametro fijo, span = parametro fijo) 
clusplot(dataset_clustering,
         VectorClusters,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 5,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of "),
         xlab = "COLUMN1",
         ylab = "COLUMN2")

## Option2
plotAllClusters <- ggplot(dataset_clustered_km, aes(x = number.donations, y = value.donations, color = factor(cluster.assigned))) +
  geom_point(alpha = 0.5) +
  theme_economist() + 
  scale_color_economist() + 
  labs(subtitle = "", 
       x = "Number of Donations", 
       y = "Average Value of Donations") +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(size=20), 
    plot.subtitle = element_text(size = 12), 
    text = element_text(family = "Tahoma")
        ) + 
  guides(color=guide_legend(title= "Clusters"))




###### Third part: DBSCAN Clustering ###### 

# Compute DBSCAN using fpc package

## Determine EPS and MinPts values
## Use K-NN to find the knee on the plot and set the value of EPS
## Normally K = 5 is used, but it can vary (see sheet Statistics Concepts for clarification)
## If the graph is not plotting correctly then the data must be filtered
## Select K value
KValue<- 4
## Plot the distances
kNNdistplot(dataset_clustering, 
            k =  KValue)
## Draw a line on the knee to see eps value
abline(h = 0.12, lty = 2)
## Insert the value where the knee in the plot is formed (value h on abline())
EpsValue <- 0.12

## Create the DBSCAN object
set.seed(123)
ObjectDBSCAN <- dbscan::dbscan(dataset_clustering, 
                               EpsValue, 
                               KValue)

# Print DBSCAN and see cluster membership
## Column names are clusters, cluster 0 are outliers
print(ObjectDBSCAN)
## Cluster membership. Noise/outlier observations are coded as 0
VectorClusters<- ObjectDBSCAN$cluster
## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_dbscan <- mutate(dataset_pre_clustering, cluster.assigned = VectorClusters)

# Visualize the clusters
## Option 1
plot(ObjectDBSCAN, dataset_clustering, main = "DBSCAN", frame = FALSE)

## Option 2
## stand = FALSE if already standarized the dataset, geom = type of graph
fviz_cluster(ObjectDBSCAN, dataset_clustering, stand = FALSE, frame = FALSE, geom = "point")



###### Fourth part:Exploration ###### 


# Clusters exploration
## DBSCAN
dataset_clustered_dbscan %>% 
  group_by(cluster.assigned) %>% 
  mutate(count.per.cluster = n()) %>% 
  summarise_all(funs(mean(.))) %>% 
  # filter(cluster.assigned %in% c(0,1,2)) %>% 
  View("DBSCAN")

## K-Means
dataset_clustered_km %>% 
  group_by(cluster.assigned) %>% 
  mutate(count.per.cluster = n()) %>% 
  summarise_all(funs(mean(.))) %>% View("KM")


# Add clusters to dataset_donations 
## Include clusters in dataset_pre_clustering
dataset_post_clustering <-  dataset_pre_clustering %>% 
  mutate(cluster.assigned = dataset_clustered_km$cluster.assigned)

## Include clusters to main dataset
dataset_donations <- dataset_donations %>% 
  left_join(dataset_post_clustering, by = "donor.no")

### Exploration on main dataset with clusters

# ## Grouped by donor.type
# dataset_donations %>% 
#   mutate(total.donations = sum(donation.amount)) %>% 
#   group_by(donor.type) %>% 
#   mutate(
#     total.donors.group = n(),
#     total.donations.group = sum(donation.amount)
#     ) %>% 
#   ungroup() %>% 
#   group_by(donor.type, cluster.assigned) %>% 
#   mutate(
#     count.donors.clusters = n(),
#     perc.count.donors = count.donors.clusters / total.donors.group,
#     sum.clusters = sum(donation.amount),
#     perc.sum.donations.group = sum.clusters / total.donations.group,
#     perc.sum.donations.total = sum.clusters / total.donations
#     ) %>% 
#   distinct(donor.type, .keep_all = TRUE) %>%
#   ungroup() %>% 
#   select(
#     donor.type, 
#     cluster.assigned, 
#     count.donors.clusters, 
#     perc.count.donors, 
#     sum.clusters, 
#     perc.sum.donations.group,
#     perc.sum.donations.total
#     ) %>% 
#   arrange(desc(perc.sum.donations.total, cluster.assigned)) %>% 
#   View("count grouped source.group and cluster")


######### Data Visualisation ##########

## Grouped by donor.category
dataset_viz_clustering <- dataset_donations %>% 
  mutate(total.donations = sum(donation.amount)) %>% 
  group_by(donor.category) %>% 
  mutate(
    total.donors.group = n(),
    total.donations.group = sum(donation.amount)
  ) %>% 
  ungroup() %>% 
  group_by(donor.category, cluster.assigned) %>% 
  mutate(
    count.donors.clusters = n(),
    perc.count.donors = (count.donors.clusters / total.donors.group) * 100,
    sum.clusters = sum(donation.amount),
    perc.sum.donations.group = (sum.clusters / total.donations.group) * 100,
    perc.sum.donations.total = (sum.clusters / total.donations) * 100
  ) %>% 
  distinct(donor.category, .keep_all = TRUE) %T>% print() %>% 
  ungroup() %>% 
  select(
    donor.category, 
    cluster.assigned, 
    count.donors.clusters, 
    perc.count.donors, 
    sum.clusters, 
    perc.sum.donations.group,
    perc.sum.donations.total
  ) %>% 
  gather(index, value, -donor.category, -cluster.assigned) 


### Percentage of donors  

# Cluster 1, percentage of donors
plotPercDonorsClus1 <- dataset_viz_clustering %>% 
   filter(
     cluster.assigned == 1,
     index == "perc.count.donors"
     ) %>% 
   ggplot(aes(donor.category, value)) +
   geom_col() +
  labs(
    title = "What percentage of donations of this category are inside this cluster?", 
    subtitle = "Cluster 1", 
    x = "", 
    y = "% Donors"
       ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=13), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))


# Cluster 2, percentage of donors
plotPercDonorsClus2 <- dataset_viz_clustering %>% 
  filter(
    cluster.assigned == 2,
    index == "perc.count.donors"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  labs(
    title = "",
    subtitle = "Cluster 2", 
    x = "", 
    y = "% Donors"
    ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

# Cluster 3, percentage of donors
plotPercDonorsClus3 <- dataset_viz_clustering %>% 
  filter(
    cluster.assigned == 3,
    index == "perc.count.donors"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  labs(subtitle = "Cluster 3", x = "", y = "% Donors") +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

grid.arrange(plotPercDonorsClus1, plotPercDonorsClus2, plotPercDonorsClus3, plotAllClusters, nrow=2, ncol=2)



### Percentage of total donations  

# Cluster 1, percentage of donors
plotPercTotalDonClus1 <- dataset_viz_clustering %>% 
  filter(
    cluster.assigned == 1,
    index == "perc.sum.donations.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  labs(
    title = "What percentage of gobal donations are inside this cluster?", 
    subtitle = "Cluster 1", 
    x = "", 
    y = "% Donors"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))


# Cluster 2, percentage of donors
plotPercTotalDonClus2 <- dataset_viz_clustering %>% 
  filter(
    cluster.assigned == 2,
    index == "perc.sum.donations.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  labs(
    title = "",
    subtitle = "Cluster 2", 
    x = "", 
    y = "% Donors"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

# Cluster 3, percentage of donors
plotPercTotalDonClus3 <- dataset_viz_clustering %>% 
  filter(
    cluster.assigned == 3,
    index == "perc.sum.donations.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  labs(subtitle = "Cluster 3", x = "", y = "% Donors") +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

grid.arrange(plotPercTotalDonClus1, plotPercTotalDonClus2, plotPercTotalDonClus3, plotAllClusters, nrow=2, ncol=2)


# Insights:
# Two clusters
  # cluster 1: one time donors, donations go from low to highest values
  # cluster 2: more than one donation up to normal (median) number of donations
            #: values go from low to high, more donations tend to decrease value of donations


remove("borrar", "borrar2")
