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


#####  Prepare the Data ####

# 1) Create dataset

dataset_pre_clustering <-
  dataset_donations %>%
  group_by(donation.year, donor.no) %>%
  mutate(
    number.donation.year = n(),
    value.donations.year = mean(donation.amount, na.rm = TRUE),
    mean.number.donations = mean(number.donation.year, na.rm = TRUE),
    mean.value.donations = mean(value.donations.year, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(mean.value.donations < 20000,
         mean.number.donations < 30,
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
NumberClusters<- 5


### Use Silhouette Analysis for determining the correct number of clusters

## Generate a k-means model using the pam() function with a k value that we want to asses its correctness
library(cluster)
# Utilize parallel computing for improving speed of processing
## Calculate the number of cores
library(parallel)
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
  geom_line() +
  scale_x_continuous(breaks = 2:10)


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
library(cluster)
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

# Explore the clusters without main dataset data
## Calculate summary statistics for each category (e.g. mean())
## data_set_clustered_ depends on what type of cluster is being analized (in this case KM) 
dataset_clustered_km %>% 
  group_by(cluster.assigned) %>% 
  mutate(count.per.cluster = n()) %>% 
  summarise_all(funs(mean(.))) %>% 
  select(cluster.assigned, 
         count.per.cluster) %>% 
  View()


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








############################################### Data Preparation for Supervised Learning ################################################## 


### Prepare data

## ADD THREE DIGITS OF POSTCODE

## Create main ML dataset
dataset_ml <- dataset_donations %>%
  arrange(donation.date) %>%
  select(
    donor.no,
    cluster.assigned,
    donation.date,
    donation.amount,
    development.income,
    group.name,
    application,
    payment.type,
    donor.type,
    source,
    donor.category,
    closest.retail.store,
    donor.gender,
    date.of.first.donation,
    number.of.donations,
    # income.stream,
    donation.month,
    donation.year,
    acquisition.source
  ) %>%
  # Perform feature creation
  mutate(days.from.first.donation = difftime(Sys.time(), date.of.first.donation, units = "days")) %>% 
  mutate_at(vars(days.from.first.donation), funs(as.numeric)) %>% 
  ## yearly and value and number of donations
  group_by(donation.year, donor.no) %>%
  mutate(
    number.donation.year = n(),
    value.donations.year = mean(donation.amount)
  ) %>%
  ungroup() %>%
  ## lifetime number, value donations, average days between donations
  group_by(donor.no) %>%
  mutate(
    mean.number.donations.year = mean(number.donation.year, na.rm = TRUE),
    mean.value.donations.year = mean(value.donations.year, na.rm = TRUE),
    diff = donation.date - lag(donation.date),
    diff.days = as.numeric(diff, units = 'days'),
    mean.diff.days = mean(diff.days, na.rm = TRUE),
    ## Average number of days between donations
    days.between.donations = mean.diff.days / mean.number.donations.year, #
    counter.donation = sequence(n()),
    value.previous.donation = case_when(lag(donation.amount) >= 30 ~ "High Value Donation",
                                        lag(donation.amount) <= 30 ~ "Low Value Donation"),
    ## Threshold of what is a high or low donation
    binari.high.value.actual.donation = case_when(donation.amount >= 30 ~ "yes",
                                                  donation.amount <= 30 ~ "no"),
    class.prev.development.income = lag(development.income),
    class.prev.group.name = lag(group.name),
    class.prev.payment.type = lag(payment.type)
  ) %>%
  mutate_at(vars(class.prev.development.income:class.prev.payment.type), funs(as.character)) %>% 
  ungroup() %>%
  mutate(activity.clusters = case_when(source %in% c("STRTIC",
                                                     "STRREG",
                                                     "STRSPO",
                                                     "DOGREG",
                                                     "DOGSPO",
                                                     "LLUTIC",
                                                     "GNTTIC",
                                                     "CLRREG",
                                                     "CLRSPO",
                                                     "GOLDON",
                                                     "GOLREG",
                                                     "MIDREG",
                                                     "MIDSPO",
                                                     "FIRTIC",
                                                     "CINTIC",
                                                     "SILTIC") ~ "Cluster Events",
                                       source %in% c("SEVONE",
                                                     "SEVCOF",
                                                     "SEVCEL", 
                                                     "SEVGNR",
                                                     "INDFUN",
                                                     "GNRREG",
                                                     "CELDON",
                                                     "CELBDG",
                                                     "SGPCCT",
                                                     "SGPWCT",
                                                     "SKYREG",
                                                     "SKYSPO") ~ "Cluster Supporter Led Giving",
                                       source %in% c("COYOTH",
                                                     "COYDON",
                                                     "COYSPO",
                                                     "COYCOL",
                                                     "COYGLO",
                                                     "88CMEM",
                                                     "COLBOX",
                                                     "STRCOR",
                                                     "DOGCOR",
                                                     "LLUCOR",
                                                     "GNTCOR",
                                                     "CLRCOR",
                                                     "GOLCOR",
                                                     "MIDCOR",
                                                     "FIRCOR",
                                                     "CINCOR",
                                                     "SILCOR") ~ "Cluster Corporate Giving",
                                       source %in% c("REGGIV",
                                                     "FRIEND",
                                                     "CAMPOC",
                                                     "GIVAYE") ~ " Cluster Regular Giving",
                                       source %in% c("INMEMO", 
                                                     "INMCOL") ~ "Cluster In Memory Donations/Collections",
                                       source %in% c("INMTRE", 
                                                     "SUNDON", 
                                                     "LIGDON") ~ "Cluster In Memory Campaign/Product",
                                       source %in% c("PFDDON",
                                                     "GENDON",
                                                     "RAFTIC",
                                                     "RAFDON",
                                                     "DONINS",
                                                     "30ADON") ~ "Cluster Individual Giving",
                                       source %in% c("LEGDON") ~ "Cluster Legacy Donors",
                                       source %in% c("TRUSTS",
                                                     "TRUSUN") ~ "Cluster Trusts")
  ) %>% 
  # droplevels() %>% 
  replace_na(list(days.between.donations = 0, 
                  diff.days = 0,
                  value.previous.donation = "No Previous Donation", 
                  class.prev.development.income = "No Previous Donation",
                  class.prev.group.name = "No Previous Donation",
                  activity.clusters = "Not Clustered",
                  class.prev.payment.type = "No Previous Donation")) %>%
  mutate(binari.cluster = if_else(cluster.assigned == "2", "Low Value Cust", "High Value Cust")) %>% 
  mutate_at(vars(binari.cluster), funs(as.factor)) %>% 
  mutate_if(is.character, as.factor) 
# %>% 
# mutate_at(vars(diff), funs(as.numeric(., units = 'days'))) %>% 
# filter(mean.value.donations.year < 20000, mean.number.donations.year < 30) 



## Create dataset for the business problem:
## If a donors ‘acquisition source’ is INMEMO or INMCOL  
## how many donors go on to make a second donation? 
##  (probability of a donor with ‘acquisition source’ is INMEMO or INMCOL, to make a second donation. 
##   If they do make a second donation what activity do they do (which cluster?) 


## This dataset is useful for predicting a donor making a second donation
dataset_ml_3 <- dataset_ml %>% 
  droplevels() %>% 
  select(-c(donation.date,
            # donation.amount,
            donation.month,
            value.previous.donation,
            application,
            # source, # will be deleted later
            # payment.type,
            class.prev.payment.type,
            date.of.first.donation,
            number.of.donations,
            # income.stream,
            donation.year,
            number.donation.year,
            value.donations.year,
            mean.number.donations.year,
            mean.value.donations.year,
            diff,
            diff.days,
            mean.diff.days,
            # binari.high.value.actual.donation,
            days.between.donations, 
            days.from.first.donation,
            class.prev.development.income,
            class.prev.group.name
            # payment.type
            # group.name
            # closest.retail.store) # Include this variable when the grouping in clusters is done
  )
  ) %>% 
  filter(counter.donation %in% c(1,2)) %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.regular.giver = ifelse(source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE"), "Yes", "No")) %>%
  # filter(counter.donation == 1) %>% 
  mutate(binari.second.donation = case_when(max.number.donations == 2 ~ "Yes",
                                            max.number.donations == 1 ~ "No")
  ) %>% 
  mutate_at(vars(binari.second.donation), funs(as.factor)) %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  na.omit() %>% 
  select(-c(donor.no, max.number.donations, counter.donation, source, group.name))



## This dataset is useful for predicting a donor making more than two donations
dataset_ml_2 <- dataset_ml %>% 
  droplevels() %>% 
  select(-c(donation.date,
            # donation.amount,
            donation.month,
            value.previous.donation,
            application,
            # source, # will be deleted later
            # payment.type,
            class.prev.payment.type,
            date.of.first.donation,
            number.of.donations,
            # income.stream,
            donation.year,
            number.donation.year,
            value.donations.year,
            mean.number.donations.year,
            mean.value.donations.year,
            diff,
            diff.days,
            mean.diff.days,
            # binari.high.value.actual.donation,
            days.between.donations, 
            days.from.first.donation,
            class.prev.development.income,
            class.prev.group.name
            # payment.type
            # group.name
            # closest.retail.store) # Include this variable when the grouping in clusters is done
  )
  ) %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.regular.giver = ifelse(source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE"), "Yes", "No")) %>%
  filter(!binari.regular.giver == "Yes") %>% #new 
  mutate(binari.more.two.donations = case_when(max.number.donations > 2 ~ "Yes",
                                               max.number.donations <= 2 ~ "No")
  ) %>% 
  mutate_at(vars(binari.more.two.donations), funs(as.factor)) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>% #new
  na.omit() %>% 
  select(-c(donor.no, 
            max.number.donations, 
            counter.donation, 
            source, group.name,
            binari.regular.giver))

