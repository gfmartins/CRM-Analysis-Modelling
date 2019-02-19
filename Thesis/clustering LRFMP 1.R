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



########################  Cluster data (Unsupervised Learning) ########################  

### Prepare data

# 1) Create dataset
dataset_pre_clustering <- 
  
  dataset_donations_sample<- sample_n(dataset_donations, 15000)
  


dataset_donations_sample %>%
  select(
    donation.date,
    donation.year,
    donor.no,
    donation.amount
  ) %>%
  ## Calculate length
  group_by(donor.no) %>% 
  arrange(donation.date) %>% 
  ## Create a sequence to tag the first and last donation
  mutate(count = sequence(n())) %>% 
  mutate(first.last.donation.tag = case_when(count == min(count) ~ "First Donation",
                                         count == max(count) ~ "Last Donation",
                                        TRUE ~ "Continue Donation")) %>%
  mutate(first.donation.date = case_when(first.last.donation.tag == "First Donation" ~ donation.date),
        last.donation.date = case_when(first.last.donation.tag == "Last Donation" ~ donation.date)
         ) %>% 
  ### Complete all the rows of first and last donation date so it's easy to calculate the difference
  fill(first.donation.date) %>% 
  ## Data has to be rearranged so that the formula fill can still fill it downwards
  arrange(desc(donation.date)) %>% 
  fill(last.donation.date) %>% 
  ungroup() %>% 
  mutate(days.first.to.last.donation = difftime(first.donation.date, 
                                                last.donation.date, units = "days")) %>% View()
  ## Calculate average and number of donations per donor, per year
  group_by(donation.year, donor.no) %>%
  mutate(
    number.donation.year = n(),
    value.donations.year = mean(donation.amount, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(donor.no) %>%
  ## Calculate average and number of donations per donor, global
  summarize(
    number.donations = mean(number.donation.year, na.rm = TRUE),
    value.donations = mean(value.donations.year, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  ## Filter to stay just with donors that have a reasonable number and average of donations
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

