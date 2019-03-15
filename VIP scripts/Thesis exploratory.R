# 1) Create dataset

dataset_donations_exploratory <-  dataset_donations %>%
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
  ## Convert to numeric this variables, as they are in days
  mutate_at(vars(length, 
                 recency), funs(as.numeric)) %>% 
  ## Leave one row per donor
  distinct(donor.no, .keep_all = TRUE) 

## See percentage of donors according to frequency
dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) 


## Sample of the analysis
dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) %>% 
  filter(frequency > 1) %>% 
  summarise(sample = sum(donors.per.frequency / total.n))

# See % of donors of more than n% donations

dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  filter(frequency > 5) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) %>% View()


# Explore the clusters without main dataset data
## Calculate summary statistics for each category (e.g. mean())
## data_set_clustered_ depends on what type of cluster is being analized (in this case KM) 
ObjectMeanClusters <- dataset_clustered_km %>% 
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
  gather(index, mean.value.cluster, -cluster.assigned) 


# table

## Calculate main statistics of LRFMP variables
ObjectMainStatis <- dataset_pre_clustering %>% 
select(length,
       recency,
       frequency,
       monetary,
       peridiocity) %>% 
mutate_at(vars(length, 
               recency), funs(as.numeric)) %>% 
summarise_all(funs(max, 
                   min,
                   mean,
                   sd)) %>% 
  gather(var, mean.value.general) %>% 
  separate(var, c("index", "stat"))


# Join mean of all clusters with mean of each cluster
table_mean_comparison <-  ObjectMainStatis %>% 
  filter(stat == "mean") %>% 
  left_join(ObjectMeanClusters, by = "index") %>% 
  select(index,
         cluster.assigned,
         mean.value.cluster,
         mean.value.general,
         -stat)


table_mean_comparison %>% 
  gather(mean.value, value, -index, -cluster.assigned) %>% 
  ggplot(aes(index,
             log(value), fill = mean.value)) +
  geom_col(width = 0.7, position = "dodge") + 
  facet_grid(. ~ cluster.assigned) +
  theme(axis.text.x=element_text(angle=-45,hjust=0,vjust=1, size=10))

# See how many 

dataset_ml %>% 
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
  distinct(donor.no, .keep_all = TRUE) %>% 
  group_by(cluster.assigned, binari.more.five.donations) %>% 
  summarise(total = n()) %>%
  group_by(cluster.assigned) %>% 
  mutate(total.per.cluster = sum(total)) %>%
  mutate(perc = round((total/total.per.cluster) * 100, 2)) %>% 
  View("grouped clus more five")
