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
    donor.category,
    closest.retail.store,
    donor.gender,
    date.of.first.donation,
    number.of.donations,
    income.stream,
    donation.month,
    donation.year
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
    mean.number.donations = mean(number.donation.year, na.rm = TRUE),
    mean.value.donations = mean(value.donations.year, na.rm = TRUE),
    diff = donation.date - lag(donation.date),
    diff.days = as.numeric(diff, units = 'days'),
    mean.diff.days = mean(diff.days, na.rm = TRUE),
    ## Average number of days between donations
    days.between.donations = mean.diff.days / mean.number.donations,
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
  # droplevels() %>% 
  replace_na(list(days.between.donations = 0, 
                  value.previous.donation = "No Previous Donation", 
                  class.prev.development.income = "No Previous Donation",
                  class.prev.group.name = "No Previous Donation",
                  class.prev.payment.type = "No Previous Donation")) %>%
  mutate(binari.cluster = if_else(cluster.assigned == "2", "Low Value Cust", "High Value Cust")) %>% 
  mutate_at(vars(binari.cluster), funs(as.factor)) %>% 
  mutate_if(is.character, as.factor) %>% 
  # mutate_at(vars(diff), funs(as.numeric(., units = 'days'))) %>% 
  filter(mean.value.donations < 20000, mean.number.donations < 30) 
  


# Dataset for: What drives clients to be on a particular cluster?
## BE AWARE THAT THIS DATASET NEEDS REVISION, AS ITS JUST SHOWING FIRST DONATION
dataset_ml_2<- dataset_ml %>% 
  droplevels() %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  select(-c(
    donation.amount,
    donation.date,
    date.of.first.donation,
    donation.month,
    donation.year,
    number.donation.year,
    value.donations.year,
    diff,
    diff.days,
    mean.diff.days,
    cluster.assigned,
    mean.number.donations,
    mean.value.donations,
    group.name,
    application,
    payment.type,
    income.stream,
    class.prev.development.income,
    class.prev.group.name,
    donor.no)
  ) 
# %>% 
  # na.omit()


# Dataset for: Given a first donation and its characteristics, what's going to be the next donation activity?
dataset_ml_3 <- dataset_ml %>% 
  droplevels() %>% 
  select(-c(donor.no,
            donation.date,
            donation.amount,
            application,
            payment.type,
            date.of.first.donation,
            number.of.donations,
            income.stream,
            donation.year,
            number.donation.year,
            value.donations.year,
            mean.number.donations,
            mean.value.donations,
            diff,
            diff.days,
            mean.diff.days,
            binari.high.value.actual.donation,
            days.between.donations
            )) %>% 
  select(everything(), -group.name, group.name) %>% 
  na.omit()


# Dataset for: Given a first donation and its characteristics, the next donation will be high or low value?
dataset_ml_4 <- dataset_ml %>% 
  droplevels() %>% 
  select(-c(donor.no,
            donation.date,
            donation.amount,
            application,
            date.of.first.donation,
            number.of.donations,
            income.stream,
            development.income, # replaced by class.prev.development.income
            group.name, # replaced by class.prev.group.name
            payment.type, # replaced by class.prev.payment.type
            donation.year,
            number.donation.year,
            value.donations.year,
            mean.number.donations,
            mean.value.donations,
            diff,
            diff.days,
            mean.diff.days,
            days.between.donations
  )) %>% 
  select(everything(), -binari.high.value.actual.donation, binari.high.value.actual.donation) %>% 
  na.omit()
  
########## Feature selection ########## 
# # 2) Apply feature selection with Recursive Feature Elimination and corrplot
# 
# # Asses multicolinearity with the corrplot
# ## Extract the numerical variables 
# dataset_corr<- dataset_ml_3 %>% select_if(is.numeric)
# 
# ## Compute the correlation matrix for these variables
# corrMat<- cor(dataset_corr)
# 
# ## Plot the corrplot to see what value will be the cutoff
# corrplot(corrMat, method = "ellipse")
# 
# # Te output will be the names of columns to eliminate
# findCorrelation(corrMat, cutoff = 0.8, verbose = TRUE, names = TRUE,
#                exact = TRUE)
# 
# 
# 
# ## See the independent variables that explain the most the predicted one with Recursive Feature
# ## When construncting the input dataset put the dependant variable at the end  
# library(mlbench)
# library(caret)
# library(randomForest)
# 
# set.seed(7)
# ## Define the control using a Random Forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# 
# ## Utilize parallel computing for improving speed of processing
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores, type="FORK")
# 
# ## Run the RFE algorithm
# results <- rfe(dataset_ml_2[,1:8], dataset_ml_2$binari.cluster, sizes=c(1:8), rfeControl=control)
# 
# # Stop parallel computing
# stopCluster(cl)
# 
# 
# ## Summarize the results
# print(results)
# ## Print a list of the most relevant features/variables
# predictors(results)
# ## Print the variables from least to most important (the output will be the column number)
# order(results$results$Accuracy, decreasing = TRUE) #normally is the first column of the table created with print(results)
# ## Plot the results
# plot(results, type=c("g", "o"))



########## 4) Train the model ########## 

########## Hipothesis: Given a first donation and its characteristics, what's going to be the next donation activity? ########## 
# To do: Group events to maximum three categories (too many at me moment makes it difficult to group.name)

# Randomize dataset
random<- sample(nrow(dataset_ml_3))
dataset_ml_3<- dataset_ml_3[random, ]

# Separar el training del test set
set.seed(123)
## El sample.split se lo efectua con la variable dependiente
split<- sample.split(dataset_ml_3$group.name, SplitRatio = 0.8)

# Crear los sets de training y test
training_set<- subset(dataset_ml_3, split == TRUE)
test_set<- subset(dataset_ml_3, split == FALSE)

# Create custom indices: myFolds
## This is for being able to run different models with the same folds and bein able to compare them (apples with apples)
myFolds <- createFolds(training_set$group.name, k = 5)

############# NOT BINARY OUTPUT

## If we want accuracy of a not binary prediction
my_control<- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE, 
  savePredictions = TRUE, 
  index = myFolds #if we are not comparing models after, dont use this
)

# Utilize parallel computing for improving speed of processing
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores, type="FORK")


############### NOT BINARY OUTPUT
## If running multiple models and later comparing them (resamples()), remember to differenciate names of the classifiers

classifier1 <- train(group.name ~ ., 
                     data = training_set,
                     method = 'ranger',
                     tuneLength = 5, 
                     trControl = my_control,
                     num.trees = 300,
                     importance = "permutation"
)

# Stop parallel computing
stopCluster(cl)

# Print or plot the model to see the best hyperparamethers (optional)
## General information
print(classifier1)
## The graph will show the tunning paramethers in X axis, and the indicator of its accuracy 
## The best tunning paramether will be the one with the highest point on Y axis
plot(classifier1)
## Print a summary of the final model
classifier1$finalModel
classifier1$resample
## After running the model see what variables were the most important
varImp(object = classifier1) 


# 5) Model accuracy estimation


#### NOT BINARY OUTPUT
# Predict 
y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])

#### BINARY OUTPUT 
# Predict 
## When raw, the predictions will appear in format "yes", "no"
## When prob, the predictions will appear in format 0.8 (probability of bein "yes" or "no")
y_pred <- predict.train(classifier, test_set, type = "raw")
y_pred <- predict.train(classifier, test_set, type = "prob")

# Add a threshold (example in logistic regresion with probability as an output)
# Predict 
y_pred<- ifelse(prob_pred > 0.5, 1, 0)

# Make a confusion matrix
## Type must be "raw"
y_pred <- predict.train(classifier, test_set, type = "raw")
confusionMatrix(y_pred, test_set$dependentV)



########## Hipothesis: Given a first donation and its characteristics, te next donation is going to be high value? ########## 

# Randomize dataset
random<- sample(nrow(dataset_ml_4))
dataset_ml_4<- dataset_ml_4[random, ]

# Separar el training del test set
set.seed(123)
## El sample.split se lo efectua con la variable dependiente
split<- sample.split(dataset_ml_4$binari.high.value.actual.donation, SplitRatio = 0.8)

# Crear los sets de training y test
training_set<- subset(dataset_ml_4, split == TRUE)
test_set<- subset(dataset_ml_4, split == FALSE)

# Create custom indices: myFolds
## This is for being able to run different models with the same folds and bein able to compare them (apples with apples)
myFolds <- createFolds(training_set$binari.high.value.actual.donation, k = 5)

############# BINARY OUTPUT

# If we want to have the ROC index of a binary classification algoritm instead of Accuracy
 my_control <- trainControl(
   method = "cv", # Crossvalidation
   number = 5, # Number of folds
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
 
 
############### BINARY OUTPUT
# Model without imbalance handling
 classifier <- train(binari.high.value.actual.donation ~., 
                    data = training_set,
                    metric ="ROC", #THIS HAS TO BE SPECIFIED IF THE MODEL IS NOT "glm"
                    method = "ranger",
                    trControl = my_control,
                    num.trees = 200,
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

# Make a confusion matrix
## Not balanced model
y_pred <- predict.train(classifier, test_set, type = "raw")
confusionMatrix(y_pred, test_set$binari.high.value.actual.donation)


############# Model Prediction/Simulation ############# 

## This table will contain the main values to replicate in dataset used to predict, and number of observations
## E.g. if filtering certain week, it will pull related variables like month
## Include in the filtering those variables that either won't be used for simulating or have a lot of related vars

table_base_values_pred<- dataset_ml_4 %>% 
  filter(donor.type == "Individual") %>% 
  head(n = 5)

## See names of columns to create de predict data frame
colnames(dataset_ml_4)

## Assign individual values
cluster.assigned <- 3
## Extract the values from table_base_values_pred
donor.type <- table_base_values_pred$donor.type
donor.category <- table_base_values_pred$donor.category
## Variables that make no differences in the prediction can be assigned directly (randomly from table_base_values_pred)
closest.retail.store <- table_base_values_pred$closest.retail.store
# table(dataset_ml_4$donor.gender)
donor.gender <- rep(c("Male", "Female"), length.out = 5)
donation.month <- table_base_values_pred$donation.month
# hist(dataset_ml_4$days.from.first.donation)
days.from.first.donation<- rep(c(80, 500, 5000), length.out = 5)
counter.donation <- 2
value.previous.donation<- rep(c("Low Value Donation", "High Value Donation"), length.out = 5)
# table(dataset_ml_4$class.prev.development.income)
class.prev.development.income <- "Events"
# table(dataset_ml_4$class.prev.group.name)
class.prev.group.name <- "Collection Box"
# table(dataset_ml_4$class.prev.payment.type)
class.prev.payment.type <- 4
binari.cluster <- rep(c("Low Value Cust", "High Value Cust"), length.out = 5)

# Data frame with simulation data
predict_dataframe<- data.frame(cluster.assigned, 
                               donor.type,
                               donor.category,
                               closest.retail.store,
                               donor.gender,
                               donation.month,
                               days.from.first.donation,
                               counter.donation,
                               value.previous.donation,
                               class.prev.development.income,
                               class.prev.group.name,
                               class.prev.payment.type,
                               binari.cluster
) %>% 
  mutate_at(vars(cluster.assigned, class.prev.payment.type), funs(as.factor))

# Predict the results

#### BINARY OUTPUT 
# Predict 
## When raw, the predictions will appear in format "yes", "no"
## When prob, the predictions will appear in format 0.8 (probability of bein "yes" or "no")
y_pred <- predict.train(classifier, predict_dataframe, type = "raw")

dataset_post_simulation <- predict_dataframe %>% 
  mutate(predicts.high.actual.donation = y_pred) %>% 
  select(cluster.assigned,
         predicts.high.actual.donation,
         binari.cluster,
         days.from.first.donation,
         value.previous.donation,
         class.prev.development.income,
         class.prev.group.name
  ) %>% View()


# # 6) Compare the models
# # Create model_list
# model_list <- list(item1 = classifier, item2 = classifier_smote)
# 
# # Pass model_list to resamples(): resamples
# resamples<- resamples(model_list)
# 
# # Summarize the results
# summary(resamples)
