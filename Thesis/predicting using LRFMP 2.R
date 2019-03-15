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
library(magrittr)
library(corrplot)



### THIS SCRIPT INCLUDES THE JUST THE CLUSTERING PROCESS ITSELF WITHOUT ANALYSIS OF NUMBER OF CLUSTERS 


########  Unsupervised Learning ########

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
    frequency = max(count)
  ) %>% 
  ## Exclude donors that had less than two visits
  filter(frequency > 2) %>%
  mutate(
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
  ## Measure the difference between donations and "today"
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
  ## Convert to numeric this variables, as they are in days
  mutate_at(vars(length, 
                 recency), funs(as.numeric)) %>% 
  ## Leave one row per donor
  distinct(donor.no, .keep_all = TRUE) 



dataset_clustering <- dataset_pre_clustering %>%
  select(length,
         recency,
         frequency,
         monetary,
         peridiocity)


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


#### K-means 

# 4.1) Train the model

# Number of clusters decided to use
NumberClusters<- 5


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
dataset_clustered_km <- mutate(dataset_pre_clustering, cluster.assigned = VectorClusters) %>% 
  ## Just select variables that are not in main dataset, to avoid columns .x, .y
  select(donor.no,
         frequency,                      
         length,
         recency,
         peridiocity,                    
         monetary,
         cluster.assigned) 


########  Supervised Learning ########

## Make a dataset of datasetdonations that has the labels for ML that will be joined with clusters
dataset_donations_to_join_clus <- dataset_donations %>% 
  arrange(donation.date) %>%
    select(
      donor.no,
      # frequency,                      
      # length,
      # recency,
      # peridiocity,                    
      # monetary,
      # cluster.assigned,
      donation.date,
      donation.amount,
      # development.income,
     # group.name,
     application,
     payment.type,
     donor.type,
     nominal,
     source,
     source.group,
     donor.category,
     closest.retail.store,
     donor.gender,
     donation.month,
     donation.year,
     acquisition.source
   ) %>% 
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
 mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
 ## Threshold of what is a high or low donation
 group_by(donor.no) %>% 
 arrange(donation.date) %>% 
 mutate(
   binari.high.value.actual.donation = case_when(donation.amount >= 30 ~ "yes",
                                                 donation.amount <= 30 ~ "no"),
   class.prev.nominal = lag(nominal),
   class.prev.source.group = lag(source.group),
   class.prev.payment.type = lag(payment.type),
   class.prev.high.value.donation = lag(binari.high.value.actual.donation),
   class.prev.activity.clusters = lag(activity.clusters)
 ) %>%
 mutate_at(vars(class.prev.nominal:class.prev.payment.type), funs(as.character)) %>% 
 ungroup() %>%
 # droplevels() %>% 
 replace_na(list(
                 value.previous.donation = "No Previous Donation", 
                 class.prev.nominal = "No Previous Donation",
                 class.prev.source.group = "No Previous Donation",
                 activity.clusters = "Not Clustered",
                 class.prev.payment.type = "No Previous Donation",
                 class.prev.high.value.donation = "No Previous Donation",
                 class.prev.activity.clusters = "No Previous Donation")) %>%
 mutate_if(is.character, as.factor) 
  


## Include clusters to main dataset with all the data and ML variables
dataset_ml <- dataset_donations_to_join_clus %>%
  filter(!donor.no %in% c("2640"), 
         donation.year > 2009
         ) %>% 
  left_join(dataset_clustered_km, by = "donor.no") %>% 
  filter(frequency > 2) %>% 
  # In case some clients were excluded from the clustering, they are assigned as Outliers
  replace_na(list(cluster.assigned = "Outlier")) %>%
  mutate_if(is.character, as.factor) 



# Asses multicolinearity with the corrplot
## The variable frequency it's the one with most linear correlation (with length and peridiocity)
## This leads to exclude frequency and length in the model
## Frequency is obvious as is the response variable, and lenght because of corr with frequency
dataset_corr<- dataset_ml %>% 
  ## Make the corrplot to every cluster to demostrate that is better to tale out peridiocity
  filter(cluster.assigned == 1) %>%
  select_if(is.numeric) %>% 
  select(-donation.month,
         -donation.year,
         -donation.amount)

## Compute the correlation matrix for these variables
corrMat<- cor(dataset_corr)

## Plot the corrplot to see what value will be the cutoff
corrplot(corrMat, method = "ellipse")

# Te output will be the names of columns to eliminate
findCorrelation(corrMat, cutoff = 0.4, verbose = TRUE, names = TRUE,
                exact = TRUE)


## This dataset is useful for predicting a donor making more than two donations
dataset_ml_2 <- dataset_ml %>% 
  # filter(cluster.assigned == "5") %>%
  ## Demostrate that 5 donations is a good value
  # filter(!cluster.assigned == "5") %>%
  # hist(dataset_ml_2$frequency, breaks = 66)
  # summary(dataset_ml_2) ## see first quartile is frequency = 7
  droplevels() %>% 
  select(donor.no,
         frequency,
         length,
         recency,
         peridiocity,
         monetary,
         # cluster.assigned,
         # donation.date,                    
         # donation.amount,
         # development.income, 
         acquisition.group,
         # group.name,
         # application,                      
         # payment.type,
         # donor.type,                       
         source, # It's going to be deleted later
         source.group,
         # donor.category,                   
         closest.retail.store,
         donor.gender,                     
         # donation.month,
         # donation.year,                    
         # acquisition.source,
         # binari.high.value.actual.donation,
         # class.prev.development.income,
         class.prev.source.group,            
         class.prev.payment.type,
         class.prev.high.value.donation,
         class.prev.activity.clusters) %>% 
  mutate(binari.regular.giver = ifelse(source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE"), "Yes", "No")
  ) %>% 
  group_by(donor.no) %>% 
  mutate(
    count = sequence(n()),
    max.num.donations = max(count)
    ) %>%
  ungroup() %>% 
  # filter(!binari.regular.giver == "Yes") %>% #new
  mutate(binari.more.five.donations = case_when(max.num.donations > 5 ~ "yes",
                                               max.num.donations <= 5 ~ "no")
  ) %>% 
  mutate_at(vars(binari.more.five.donations), funs(as.factor)) %>% 
  # mutate_at(vars(length, 
  #                recency), funs(as.numeric)) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>% #new
  # na.omit() %>%
  select(-c(
    source, 
    binari.regular.giver, ## add eventually to the model
    donor.no,
    count,
    max.num.donations,
    frequency, ## too obvious predictor
    length, ## too much predictor, this might lead to reduce dataset to just 5 years
    peridiocity, ## good predictor
    monetary, ## regular good predictor
    recency
    
    )
  )



########## Train the Random Forest model ########## 

### Prepare training sets
# Change dataset_ml_n according to question


# Randomize dataset
set.seed(234)
# random<- sample(nrow(dataset_ml_2), 2000)
random<- sample(nrow(dataset_ml_2))
dataset_ml_2<- dataset_ml_2[random, ]

# Separar el training del test set
set.seed(123)
## El sample.split se lo efectua con la variable dependiente
split<- sample.split(dataset_ml_2$binari.more.five.donations, SplitRatio = 0.8)

# Crear los sets de training y test
training_set<- subset(dataset_ml_2, split == TRUE)
test_set<- subset(dataset_ml_2, split == FALSE)

# summary(training_set)
# summary(test_set)


# Create custom indices: myFolds
## This is for being able to run different models with the same folds and bein able to compare them (apples with apples)
myFolds <- createFolds(training_set$binari.more.five.donations, k = 5)


# If we want to have the ROC index of a binary classification algoritm instead of Accuracy
my_control <- trainControl(
  method = "cv", # Crossvalidation
  number = 10, # Number of folds
  summaryFunction = twoClassSummary, # The way we want to print the summary statistics
  classProbs = TRUE, # Calculate probabilities
  verboseIter = TRUE, # Print training log
  savePredictions = TRUE, # If we are not comparing models after, dont use this
  index = myFolds
)


# Utilize parallel computing for improving speed of processing
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores, type="FORK")


# Random Forest
## If running multiple models and later comparing them (resamples()), remember to differenciate names of the classifiers

## For dataset_ml3
tgrid <- expand.grid(
  .mtry = 112,
  .splitrule = "gini",
  .min.node.size = c(1)
)

## For dataset_ml2
# tgrid <- expand.grid(
#   .mtry = 112,
#   .splitrule = "gini",
#   .min.node.size = c(1)
# )


### Make the classifier

## BINARY OUTPUT
## Question: Is a donor making a second donation?
# classifier <- train(binari.second.donation ~.,
#                     data = training_set,
#                     metric ="ROC", #THIS HAS TO BE SPECIFIED IF THE MODEL IS NOT "glm"
#                     method = "ranger",
#                     trControl = my_control,
#                     num.trees = 500,
#                     # tuneGrid = tgrid,
#                     importance = "permutation"
# )

## Question: Is a donor making more than two donations?
classifier <- train(binari.more.five.donations ~.,
                    data = training_set,
                    metric ="ROC", #THIS HAS TO BE SPECIFIED IF THE MODEL IS NOT "glm"
                    method = "ranger",
                    trControl = my_control,
                    num.trees = 100,
                    # tuneGrid = tgrid,
                    importance = "permutation"
)


# Stop parallel computing
stopCluster(cl)

# Print or plot the model to see the best hyperparamethers (optional)
## General information
print(classifier)
## The graph will show the tunning paramethers in X axis, and the indicator of its accuracy 
## The best tunning paramether will be the one with the highest point on Y axis
plot(classifier)
## Print a summary of the final model
classifier$finalModel
classifier$resample
## After running the model see what variables were the most important
varImp(object = classifier) 


# 5) Model accuracy estimation
# Check the metrics of the model
classifier$resample

# Make a confusion matrix
## Type must be "raw"
y_pred <- predict.train(classifier, test_set, type = "raw")
confusionMatrix(y_pred, test_set$binari.more.five.donations)


############## Model Prediction/Simulation of Random Forest Model ############# 

# ## This table will contain the main values to replicate in dataset used to predict, and number of observations
# ## E.g. if filtering certain week, it will pull related variables like month
# ## Include in the filtering those variables that either won't be used for simulating or have a lot of related vars
# table_base_values_pred<- dataset_ml %>% 
#   filter("VARIABLE TO FILTER" == "VALUE TO FILTER") %>% 
#   head(n = 10)
# 
# ## See names of columns to create de predict data frame
# colnames(dataset_ml)
# 
# ## Assign individual values
# cluster.assigned <- 1
# donation.amount <- 25
# development.income <- 1
# group.name <- 1
# payment.type <- 1
# donor.type <- 1
# donor.category <- 1
# donor.gender <- 1
# donation.month <- 1
# donation.month <- 1
# donation.month <- 1
# donation.month <- 1
# 
# 
# 
# ## Extract the values from table_base_values_pred
# "VARIABLE" <- table_base_values_pred$"VARIABLE TO EXTRACT"
# ## Variables that make no differences in the prediction can be assigned directly (randomly from table_base_values_pred)
# "VARIABLE" <- table_base_values_pred$"VARIABLE TO EXTRACT"
# 
# # Data frame with simulation data
# predict_dataframe<- data.frame(
#   "VARIABLE",
#   "VARIABLE",
#   "VARIABLE"
# ) 
# 
# # Predict the results
# 
# ####################### CLASSIFICATION
# 
# #### BINARY OUTPUT 
# # Predict 
# ## When raw, the predictions will appear in format "yes", "no"
# ## When prob, the predictions will appear in format 0.8 (probability of bein "yes" or "no")
# y_pred <- predict.train(classifier, predict_dataframe, type = "raw")
# y_pred <- predict.train(classifier, predict_dataframe, type = "prob") 
# 
# ## In this example its assumed that type = "raw"
# dataset_post_simulation <- predict_dataframe %>% 
#   mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)
# 
# 
# #### NOT BINARY OUTPUT
# # Predict 
# y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])
# 
# dataset_post_simulation <- predict_dataframe %>% 
#   mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)
# 
# 
# ####################### REGRESSION
# 
# # Predict
# y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])
# 
# dataset_post_simulation <- predict_dataframe %>% 
#   mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)
# 


########## Visualise Decision Tree ########## 

# Question: What makes a donor make a second donation?
# Train model using Caret
# classifier <- train(
#   binari.second.donation ~ .,
#   data = training_set,
#   method = "rpart",
#   trControl = my_control,
#   metric = "ROC",
#   tuneLength = 10,
#   parms = list(split = 'gini')
# )

# Question: What makes a donor make more than two donations?
classifier <- train(
  binari.more.five.donations ~ .,
  data = training_set,
  method = "rpart",
  trControl = my_control,
  metric = "ROC",
  tuneLength = 10,
  # tuneGrid = tgrid,
  parms = list(split = 'gini')
)


# Make a confusion matrix
## Check if it's worth take the rules from the model applying rpart instead of ranger
y_pred <- predict.train(classifier, test_set, type = "raw")
confusionMatrix(y_pred, test_set$binari.more.five.donations)

# Plot decision tree
rpart.plot(classifier$finalModel)
rules <- rpart.rules(classifier$finalModel, cover = TRUE) 
View(rules)
# Write a .csv file to see the rules in excel and manipulate (give colors, share, etc.)
write_csv(rules, "rules.csv")

