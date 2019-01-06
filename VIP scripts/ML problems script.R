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

########################  Cluster data (Unsupervised Learning) ########################  

### Prepare data

# 1) Create dataset
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
  filter(value.donations < 20000, number.donations < 30)

dataset_clustering<- dataset_pre_clustering %>%
  select(number.donations, value.donations) 


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


### Fit model

# Applying k-means to the dataset
## Value gathered through clustering analysis
NumberClusters <- 3

set.seed(29)
ObjectKM<- kmeans(dataset_clustering, NumberClusters, iter.max = 300, nstart = 10)
VectorClusters<- ObjectKM$cluster
## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_km <- mutate(dataset_clustering, cluster.assigned = VectorClusters)


# Add clusters to dataset_donations 
## Include clusters in dataset_pre_clustering
dataset_post_clustering <-  dataset_pre_clustering %>% 
  mutate(cluster.assigned = dataset_clustered_km$cluster.assigned)

## Include clusters to main dataset
dataset_donations <- dataset_donations %>% 
  left_join(dataset_post_clustering, by = "donor.no") %>%  replace_na(list(cluster.assigned = "Outlier")) %>% 
  mutate_at(vars(cluster.assigned), funs(as.factor))


############################################### Predict (Supervised Learning) ################################################## 


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
dataset_ml_3 <- dataset_ml %>% 
  droplevels() %>% 
  select(-c(donation.date,
            # donation.amount,
            donation.month,
            value.previous.donation,
            application,
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
            # group.name)
            # closest.retail.store) # Include this variable when the grouping in clusters is done
  )
         ) %>% 
  filter(counter.donation %in% c(1,2)) %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.regular.giver = ifelse(acquisition.source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE"), "Yes", "No")) %>%
  # filter(counter.donation == 1) %>% 
  mutate(binari.second.donation = case_when(max.number.donations == 2 ~ "Yes",
                                            max.number.donations == 1 ~ "No")) %>% 
  mutate_at(vars(binari.second.donation), funs(as.factor)) %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  na.omit() %>% 
  # filter(binari.regular.giver == "No",
  #        !payment.type %in% "19") %>% 
  # mutate_if(is.character, as.factor) %>% 
  select(-c(donor.no, max.number.donations, counter.donation, binari.regular.giver))
  


########## 4) Train the model ########## 

### Hipothesis: Given a first donation and its characteristics, what's going to be the next donation activity?
# To do: Group events to maximum three categories (too many at me moment makes it difficult to group.name)

# Randomize dataset
set.seed(234)
# random<- sample(nrow(dataset_ml_3), 10000)
random<- sample(nrow(dataset_ml_3))
dataset_ml_3<- dataset_ml_3[random, ]

# Separar el training del test set
set.seed(123)
## El sample.split se lo efectua con la variable dependiente
split<- sample.split(dataset_ml_3$binari.second.donation, SplitRatio = 0.8)

# Crear los sets de training y test
training_set<- subset(dataset_ml_3, split == TRUE)
test_set<- subset(dataset_ml_3, split == FALSE)

# Create custom indices: myFolds
## This is for being able to run different models with the same folds and bein able to compare them (apples with apples)
myFolds <- createFolds(training_set$binari.second.donation, k = 5)

############# NOT BINARY OUTPUT

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


############### NOT BINARY OUTPUT
## If running multiple models and later comparing them (resamples()), remember to differenciate names of the classifiers

tgrid <- expand.grid(
  .mtry = 239,
  .splitrule = "gini",
  .min.node.size = c(1)
)


# Make the classifier
### BINARY OUTPUT
classifier <- train(binari.second.donation ~., 
                    data = training_set,
                    metric ="ROC", #THIS HAS TO BE SPECIFIED IF THE MODEL IS NOT "glm"
                    method = "ranger",
                    trControl = my_control,
                    num.trees = 500,
                    tuneGrid = tgrid,
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

####################### CLASSIFICATION

#### NOT BINARY OUTPUT 
# Check the metrics of the model
classifier$resample


#### BINARY OUTPUT 
# Make a confusion matrix
## Type must be "raw"
y_pred <- predict.train(classifier, test_set, type = "raw")
confusionMatrix(y_pred, test_set$binari.second.donation)


############# Model Prediction/Simulation ############# 

## This table will contain the main values to replicate in dataset used to predict, and number of observations
## E.g. if filtering certain week, it will pull related variables like month
## Include in the filtering those variables that either won't be used for simulating or have a lot of related vars
table_base_values_pred<- dataset_ml %>% 
  filter("VARIABLE TO FILTER" == "VALUE TO FILTER") %>% 
  head(n = 10)

## See names of columns to create de predict data frame
colnames(dataset_ml)

## Assign individual values
cluster.assigned <- 1
donation.amount <- 25
development.income <- 1
group.name <- 1
payment.type <- 1
donor.type <- 1
donor.category <- 1
donor.gender <- 1
donation.month <- 1
donation.month <- 1
donation.month <- 1
donation.month <- 1



## Extract the values from table_base_values_pred
"VARIABLE" <- table_base_values_pred$"VARIABLE TO EXTRACT"
## Variables that make no differences in the prediction can be assigned directly (randomly from table_base_values_pred)
"VARIABLE" <- table_base_values_pred$"VARIABLE TO EXTRACT"

# Data frame with simulation data
predict_dataframe<- data.frame(
  "VARIABLE",
  "VARIABLE",
  "VARIABLE"
) 

# Predict the results

####################### CLASSIFICATION

#### BINARY OUTPUT 
# Predict 
## When raw, the predictions will appear in format "yes", "no"
## When prob, the predictions will appear in format 0.8 (probability of bein "yes" or "no")
y_pred <- predict.train(classifier, predict_dataframe, type = "raw")
y_pred <- predict.train(classifier, predict_dataframe, type = "prob") 

## In this example its assumed that type = "raw"
dataset_post_simulation <- predict_dataframe %>% 
  mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)


#### NOT BINARY OUTPUT
# Predict 
y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])

dataset_post_simulation <- predict_dataframe %>% 
  mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)


####################### REGRESSION

# Predict
y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])

dataset_post_simulation <- predict_dataframe %>% 
  mutate("predicts.WHAT.IS.BEING.PREDICTED" = y_pred)



