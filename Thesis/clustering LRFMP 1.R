library(tidyverse)
library(mlbench)
library(caret)
library(readxl)
library(fpc)
library(factoextra)
library(ggthemes)
library(gridExtra)
library(plotly)
library(parallel)
library(corrplot)
library(caTools)
library(rpart.plot)
library(readr)
library(purrr)
library(cluster)
library(dbscan)



### THIS SCRIPT INCLUDES THE CLUSTERING PROCESS FROM ANALYSIS OF NUMBER OF CLUSTERS TO THE CLUSTERING ITSELF

####  Prepare the Data 

# 1) Create dataset
dataset_pre_clustering <-
  dataset_donations %>%
  # group_by(donation.year, donor.no) %>%
  # mutate(
  #   number.donation.year = n(),
  #   value.donations.year = mean(donation.amount, na.rm = TRUE),
  #   mean.number.donations = mean(number.donation.year, na.rm = TRUE),
  #   mean.value.donations = mean(value.donations.year, na.rm = TRUE)
  # ) %>%
  # ungroup() %>%
  filter(
    ## This donors seems to be outliers
    !donor.no %in% c("2640"), 
    donation.year > 2009) %>%
  select(donation.date,
         donation.year,
         donor.no,
         donation.amount) %>%
  # Calculate Length
  # Calculate Frequency
  group_by(donor.no) %>%
  arrange(donation.date) %>%
  ## Create a sequence to tag the first and last donation and calculate frequency
  mutate(
    count = sequence(n()),
    frequency = max(count),
    first.last.donation.tag = case_when(
      count == min(count) ~ "First Donation",
      count == max(count) ~ "Last Donation",
      TRUE ~ "Continue Donation"
    )
  ) %>%
  mutate(
    first.donation.date = case_when(first.last.donation.tag == "First Donation" ~ donation.date),
    last.donation.date = case_when(first.last.donation.tag == "Last Donation" ~ donation.date)
  ) %>%
  ### Complete all the rows of first and last donation date so it's easy to calculate the difference
  fill(first.donation.date) %>%
  ## Data has to be rearranged so that the formula fill can still fill it downwards
  arrange(desc(donation.date)) %>%
  fill(last.donation.date) %>%
  ## Rearrange again to ascendant
  arrange(donation.date) %>%
  mutate(
    length.pre = difftime(last.donation.date,
                          first.donation.date, units = "days"),
    length = mean(length.pre, na.rm = TRUE)
  ) %>%
  # Calculate Recency
  mutate(
    days.ndonation.to.last.donation = difftime(Sys.Date(),
                                               donation.date, units = "days"),
    recency = mean(days.ndonation.to.last.donation, na.rm = TRUE) ## pendant to limit number of recent donations
  ) %>%
  mutate(
    diff = donation.date - lag(donation.date),
    diff.days = as.numeric(diff, units = 'days'),
    peridiocity = sd(diff.days, na.rm = TRUE)
    ## Average number of days between donations
    # days.between.donations = mean.diff.days / frequency
  ) %>%
  ## Calculate Monetary
  mutate(monetary = mean(donation.amount, na.rm = TRUE)) %>%
  ungroup() %>%
  ## Exclude donors that had less than two visits
  filter(frequency > 2) %>% 
  ## Leave one row per donor
  distinct(donor.no, .keep_all = TRUE) 


dataset_clustering <- dataset_pre_clustering %>%
  select(length,
         recency,
         frequency,
         monetary,
         peridiocity) %>% 
  mutate_at(vars(length, 
                 recency), funs(as.numeric))


# Scale the data if neccesary (by preProcess = )
# library(caret)
## Create an object with the pre processing parameters
ObjectPreprocessParams <- preProcess(dataset_clustering, method=c("scale"))
# summarize transform parameters
# print(ObjectPreprocessParams)
## Transform the dataset using the parameters
dataset_clustering <- predict(ObjectPreprocessParams, dataset_clustering)
# Summarize the transformed dataset
# summary(dataset_clustering)

#####  K-means ####

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
NumberClusters<- 5


### Use Silhouette Analysis for determining the correct number of clusters

## Generate a k-means model using the pam() function with a k value that we want to asses its correctness
# Utilize parallel computing for improving speed of processing
## Calculate the number of cores
no_cores <- detectCores() - 1
## Initiate cluster
cl <- makeCluster(no_cores, type="FORK")

##Create Pam object
ObjectPam <- pam(dataset_clustering, k = NumberClusters)

# Stop parallel computing
stopCluster(cl)

## Plot the silhouette visual for the ObjectPam model
plot(silhouette(ObjectPam))


# Plot the Silhouette index with different k values
## Use map_dbl to run many models with varying value of k (in this case k = 2:10)
cl <- makeCluster(no_cores, type="FORK")

sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = dataset_clustering, k = k)
  model$silinfo$avg.width
})

stopCluster(cl)

## Generate a data frame containing both k and sil_width
## The highest sil_width is the best, value k (x axis) is number of clusters
table_silhouette <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

## Plot 
ggplot(table_silhouette, aes(x = k, y = sil_width)) +
  geom_col(fill = "deepskyblue4", alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  labs(title = "Silhouette Index per Different K Values",
       subtitle = "K = 5 has the highest Silhouette width",
       x = "K value",
       y = "Silhouette Index") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm")
        ) +
  scale_x_continuous(breaks = 2:10) +
  annotate("rect", xmin = 4.55, xmax = 5.45, ymin = 0, ymax = 0.423, fill = "darkorange1", alpha = .6, color = NA) 




### Applying k-means to the dataset
## Dataset, número de clusters que se ve mediante la observación del gráfico (donde se suaviza la pendiente), número de iteraciones (valor fijo 300), nstart = valor fijo en 10, iter.max = valor máximo que R buscará (es bueno ponerlo para ver cual es el mejor resultado pero no es necesario)
set.seed(29)
ObjectKM<- kmeans(dataset_clustering, 
                  NumberClusters, 
                  iter.max = 300, 
                  nstart = 10) 

## Create vector with cluster assignements 
VectorClusters<- ObjectKM$cluster
## Create a new column with the clients and the cluster in which it was assigned
## If dataset_pre_clustering was created, use it here
dataset_clustered_km <- mutate(dataset_pre_clustering, cluster.assigned = VectorClusters) 

# Visualize the clusters
## Option1
## lines = 0 es porque no queremos que se grafiquen lineas de distancia, shade = true (parametro fijo), color = parametro fijo, labels = 5 (para tener todo en el grafico es 2), plotchar = parametro fijo, span = parametro fijo) 
# library(cluster)
# clusplot(dataset_clustering,
#          VectorClusters,
#          lines = 0,
#          shade = TRUE,
#          color = TRUE,
#          labels = 5,
#          plotchar = FALSE,
#          span = TRUE,
#          main = paste("Clusters of "),
#          xlab = "COLUMN1",
#          ylab = "COLUMN2")

fviz_cluster(ObjectKM, 
             dataset_clustering,
             palette = c("#a6dbed","#0099CC", "#99E6FF", "#99c2d6", "#6e6e9e"),
             alpha = 0.8,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
             ) +
  labs(title = "Clusters of LRFMP Variables in a Two-Dimensional Space",
       subtitle = "Principal Components were used to reduce dimensions")

# Explore the clusters without main dataset data
## Calculate summary statistics for each category (e.g. mean())
## data_set_clustered_ depends on what type of cluster is being analized (in this case KM) 
dataset_clustered_km %>% 
  select(
    cluster.assigned,
    length,
    recency,
    frequency,
    monetary,
    peridiocity
    ) %>% 
  group_by(cluster.assigned) %>% 
  mutate(count.per.cluster = n()) %>% 
  summarise_all(funs(round(mean(., na.rm = TRUE))),2) %>% 
  View("bien")


## Include clusters to main dataset with all the data
dataset_donations_clustered <- dataset_donations %>%
  filter(!donor.no %in% c("2640"), 
         donation.year > 2009
  ) %>% 
  left_join(dataset_clustered_km, by = "donor.no") %>% 
  filter(frequency > 2) %>% 
  # In case some clients were excluded from the clustering, they are assigned as Outliers
  replace_na(list(cluster.assigned = "Outlier")) 
  

###### Third part: DBSCAN Clustering ###### 

# Compute DBSCAN using fpc package

## Determine EPS and MinPts values
## Use K-NN to find the knee on the plot and set the value of EPS
## Normally K = 5 is used, but it can vary (see sheet Statistics Concepts for clarification)
## If the graph is not plotting correctly then the data must be filtered
## Select K value
KValue<- 5
## Plot the distances
# library(dbscan)
kNNdistplot(dataset_clustering, 
            k =  KValue)
## Draw a line on the knee to see eps value
abline(h = 0.8, lty = 2)
## Insert the value where the knee in the plot is formed (value h on abline())
EpsValue <- 0.8

## Create the DBSCAN object
# library(fpc)
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
## If dataset_pre_clustering was created, use it here
dataset_clustered_dbscan <- mutate(dataset, cluster.assigned = VectorClusters)


# Visualize the clusters
## Option 1
plot(ObjectDBSCAN, dataset_clustering, main = "DBSCAN", frame = FALSE)

## Option 2
library("factoextra")
## stand = FALSE if already standarized the dataset, geom = type of graph
fviz_cluster(ObjectDBSCAN, dataset_clustering, stand = FALSE, frame = FALSE, geom = "point")

