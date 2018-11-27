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
    days.between.donations = mean.diff.days / mean.number.donations,
    counter.donation = sequence(n()),
    value.previous.donation = case_when(lag(donation.amount) >= 30 ~ "High Value Donation",
                                        lag(donation.amount) <= 30 ~ "Low Value Donation")
  ) %>%
  ungroup() %>%
  replace_na(list(days.between.donations = 0, 
                  value.previous.donation = "No Previous Donation")) %>% 
  mutate(binari.cluster = if_else(cluster.assigned == "2", "Low Value Cust", "High Value Cust")) %>% 
  mutate_at(vars(binari.cluster), funs(as.factor)) %>% 
  # mutate_at(vars(diff), funs(as.numeric(., units = 'days'))) %>% 
  filter(mean.value.donations < 20000, mean.number.donations < 30) 
  
  

# %>% 
#   select(development.income:binari.cluster,
#          cluster.assigned)
        
  
# 2) Apply feature selection with Recursive Feature Elimination and corrplot

# # Asses multicolinearity with the corrplot
# ## Extract the numerical variables 
# dataset_corr<- dataset_ml %>% select_if(is.numeric)
# 
# ## Compute the correlation matrix for these variables
# corrMat<- cor(dataset_corr)
# 
# ## Plot the corrplot to see what value will be the cutoff
# corrplot(corrMat, method = "ellipse")
# 
# # Te output will be the names of columns to eliminate
# findCorrelation(corrMat, cutoff = 0.8, verbose = TRUE, names = TRUE,
#                 exact = TRUE)


## See the independent variables that explain the most the predicted one with Recursive Feature
## When construncting the input dataset put the dependant variable at the end  
library(mlbench)
library(caret)
library(randomForest)

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
    donor.no)
  ) 
# %>% 
  # na.omit()


# Dataset for: What's the next donation activity after a particular donation activity?
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
            days.between.donations,
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
  na.omit()
  

set.seed(7)
## Define the control using a Random Forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

## Utilize parallel computing for improving speed of processing
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")

## Run the RFE algorithm
results <- rfe(dataset_ml_2[,1:8], dataset_ml_2$binari.cluster, sizes=c(1:8), rfeControl=control)

# Stop parallel computing
stopCluster(cl)


## Summarize the results
print(results)
## Print a list of the most relevant features/variables
predictors(results)
## Print the variables from least to most important (the output will be the column number)
order(results$results$Accuracy, decreasing = TRUE) #normally is the first column of the table created with print(results)
## Plot the results
plot(results, type=c("g", "o"))




    

## Conclutions: Im goint to include in the model the following variables

# Select the columns that are relevant
dataset_ml<- dataset %>% select()




# Randomize dataset
random<- sample(nrow(dataset_ml))
dataset_ml<- dataset_ml[random, ]

# See if its neccesary to scale
## If mean and SD vary among variables is better to scale
## Mean of each variable
colMeans(dataset_ml)
## Standard deviation of each variable
apply(dataset_ml, 2, sd)

# Scale the data if neccesary (by preProcess = )
library(caret)
## Create an object with the pre processing parameters
## Select the desired method, in this case is "scale"
ObjectPreprocessParams <- preProcess(dataset_ml[,"COLUMNAS A MODIFICAR"], method=c("scale"))
# summarize transform parameters
print(ObjectPreprocessParams)
## Transform the dataset using the parameters
dataset_ml <- predict(ObjectPreprocessParams, dataset_ml[,"COLUMNAS A MODIFICAR"])
# Summarize the transformed dataset
summary(dataset_ml)

## If neccesary apply dimensionality reduction (either manually {see NLPCA or PCA script} or by preProcess = )

# Separar el training del test set
library(caTools)
set.seed(123)
## El sample.split se lo efectua con la variable dependiente
split<- sample.split(dataset_ml$"dependentV", SplitRatio = "PORCENTAJE DE SEPARACION")

# Crear los sets de training y test
training_set<- subset(dataset_ml, split == TRUE)
test_set<- subset(dataset_ml, split == FALSE)


# 4) Train the model

# Create custom indices: myFolds
## This is for being able to run different models with the same folds and bein able to compare them (apples with apples)
myFolds <- createFolds(training_set$"dependentV", k = 5)

############# BINARY OUTPUT

## If we want to have the ROC index of a binary classification algoritm instead of Accuracy
# my_control <- trainControl(
#   method = "cv",
#   number = 10,
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE,
#   verboseIter = TRUE,
#   savePredictions = TRUE,
#   index = myFolds
# )


############# NOT BINARY OUTPUT

## If we want accuracy of a not binary prediction
my_control<- trainControl(
  method = "cv", 
  number = 10,
  verboseIter = TRUE,
  savePredictions = TRUE, #if we are not comparing models after, dont use this
  index = myFolds #if we are not comparing models after, dont use this
)

# Utilize parallel computing for improving speed of processing
# Calculate the number of cores
library(parallel)
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores, type="FORK")


################ BINARY OUTPUT
#classifier <- train("dependentV" ~., 
#                     data = training_set,
#                     metric ="ROC", #THIS HAS TO BE SPECIFIED IF THE MODEL IS NOT "glm"
#                     method = "NOMBRE del METODO",
#                     tuneLenght = 10, #10 is a normal used value
#                     trControl = my_control,
#                     preProcess = c("OPTION1", "OPTIOPN2", "OPTION3"))

## if random forest
# classifier <- train("dependentV" ~.,
#                     data = training_set,
#                     metric ="ROC", 
#                     method = "rf",
#                     tuneLenght = 10, 
#                     ntree = 500,
#                     trControl = my_control
# )


############### NOT BINARY OUTPUT
## If running multiple models and later comparing them (resamples()), remember to differenciate names of the classifiers

classifier1 <- train("dependentV" ~.,
                     data = training_set,
                     method = "NOMBRE DEL METODO",
                     tuneLenght = 10, #10 is a normal used value
                     trControl = my_control,
                     preProcess = c("OPTION1", "OPTIOPN2", "OPTION3") #if any (random forest doesn't need much for example)
)


classifier2 <- train("dependentV" ~.,
                     data = training_set,
                     method = "NOMBRE DEL METODO",
                     tuneLenght = 10, #10 is a normal used value
                     trControl = my_control,
                     preProcess = c("OPTION1", "OPTIOPN2", "OPTION3") #if any (random forest doesn't need much for example)
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

####################### CLASSIFICATION #####################

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


####################### REGRESSION #####################

# Predict
y_pred<- predict(classifier, test_set[,"COLUMNS WITH ALL independentVs"])

## Make a data frame with predicted values and real ones
submit <- data.frame(Real = test_set$Occ, Predicted = y_pred)
library(dplyr)
submit %>% mutate(Variance = (1-(Predicted / Real))) %>% summarise(Percentage_Difference = mean(Variance)*100)


# 6) Compare the models
# Create model_list
model_list <- list(item1 = classifier1, item2 = classifier2)

# Pass model_list to resamples(): resamples
resamples<- resamples(model_list)

# Summarize the results
summary(resamples)


