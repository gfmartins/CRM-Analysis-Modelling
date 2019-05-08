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
    recency = mean(days.ndonation.to.last.donation, na.rm = TRUE) 
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


# See percentage of donors according to frequency
dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) %>% 
  ## Filter donors that have done less than 15 donations 
  filter(frequency < 15) %>% 
  ggplot(aes(frequency, perc)) +
  geom_col(alpha = 0.8, fill =  "deepskyblue4") +
  theme_minimal() +
  scale_fill_economist() +
  labs(title = "Percentage of Donors by Number of Donations",
       subtitle = "Majority of donors have done less than three donations",
       x = "Number of Donations",
       y = "Percentage (%)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"), 
        panel.background = element_rect(fill = "azure1"),
        plot.background = element_rect(fill = "azure1"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm")
  )
        

# See how many donors with more than 5 donations there are by cluster
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
  ggplot(aes(binari.more.five.donations, perc, fill = binari.more.five.donations)) + 
  geom_col(alpha = 0.8) +
  facet_grid(. ~ cluster.assigned) +
  theme_minimal() +
  scale_fill_economist(labels = c(" < 5 Donations ", " > 5 Donations")) +
  labs(title = "Percentage by Cluster of Donors with More than 5 Donations",
       subtitle = "Clusters have donors with different engagement levels",
       y = "Percentage (%)") +
  theme(legend.title = element_blank(),
        legend.position="bottom",
        axis.title.x = element_blank(),
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"), 
        panel.background = element_rect(fill = "azure1"),
        plot.background = element_rect(fill = "azure1"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm")
  ) 



# See how many donors per clusters are regular givers
dataset_ml %>% 
  # filter(cluster.assigned == "1") %>%
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
         class.prev.activity.clusters, 
         cluster.assigned) %>% 
  mutate(binari.regular.giver = ifelse(source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE"), "Yes", "No")
  ) %>% 
  group_by(cluster.assigned) %>% 
  mutate(total.n = n()) %>% 
  mutate(number.regular.donors.cluster = sum(binari.regular.giver == "Yes")) %>% 
  ungroup() %>% 
  mutate(per = round((number.regular.donors.cluster/total.n) * 100, 2)) %>% 
  distinct(cluster.assigned, .keep_all = TRUE) %>% 
  select(
    total.n,
    number.regular.donors.cluster,
    per,
    cluster.assigned
  ) %>% 
  ggplot(aes(cluster.assigned, 
             per, fill = cluster.assigned)) +
  geom_col(alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  labs(title = "Percentage of Regular Giver by Cluster",
       subtitle = "All clusters have at least a small number of Regular Givers",
       y = "Percentage (%)",
       x = "Cluster") +
  theme(legend.title = element_blank(),
        legend.position="none",
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"), 
        panel.background = element_rect(fill = "azure1"),
        plot.background = element_rect(fill = "azure1"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm")
  ) 




## Sample used for the analysis
# Data filtered by patients that had more than two donations
ObjectPerSample <- dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) %>% 
  filter(frequency > 2) %>% 
  summarise(sample = sum(donors.per.frequency / total.n) *100) %$% 
  return(sample)


ObjectNumSample <- dataset_donations_exploratory %>% 
  select(donor.no, frequency) %>% 
  group_by(frequency) %>% 
  summarise(donors.per.frequency = n()) %>% 
  ungroup() %>% 
  mutate(total.n = sum(donors.per.frequency),
         perc = round((donors.per.frequency/total.n) * 100, 2)) %>% 
  filter(frequency > 2) %>% 
  summarise(sample = sum(donors.per.frequency)) %$% 
  return(sample)


# See % of donors of more than n% donations (5 donations)
# dataset_donations_exploratory %>% 
#   select(donor.no, frequency) %>% 
#   filter(frequency > 5) %>% 
#   group_by(frequency) %>% 
#   summarise(donors.per.frequency = n()) %>% 
#   ungroup() %>% 
#   mutate(total.n = sum(donors.per.frequency),
#          perc = round((donors.per.frequency/total.n) * 100, 2)) %>% View()



# See the percentag of donors per cluster and acquisition group (top 3)
dataset_ml %>% 
  # filter(cluster.assigned == "1") %>%
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
         class.prev.activity.clusters, 
         cluster.assigned) %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  group_by(cluster.assigned) %>% 
  mutate(total.n = n()) %>%
  ungroup() %>% 
  group_by(cluster.assigned, acquisition.group) %>% 
  mutate(number.per.acq.group = n(),
         per = round((number.per.acq.group/total.n) * 100, 2)) %>% 
  ungroup() %>% 
  group_by(cluster.assigned) %>% 
  distinct(acquisition.group, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(acquisition.group,
         total.n,
         number.per.acq.group,
         per,
         cluster.assigned
  ) %>% 
  group_by(cluster.assigned) %>% 
  arrange(desc(per)) %>% 
  mutate(count = sequence(n())) %>% 
  ungroup() %>% 
  filter(count <= 3) %>%
  # filter(cluster.assigned == 3) %>%
  droplevels() %>% 
  arrange(desc(cluster.assigned, per)) %>% 
  ggplot(aes(acquisition.group,
             per, fill = acquisition.group)) +
  geom_col(alpha = 0.8) +
  facet_grid(. ~ cluster.assigned) +
  theme_minimal() +
  scale_fill_economist() +
  labs(title = "Top Three of Donor's Acquisition Group by Cluster",
       subtitle = "Some acquisition groups are present on every cluster",
       y = "Percentage (%)",
       x = "Cluster") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none",
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=-45,hjust=0,vjust=1, size=6),
        panel.background = element_rect(fill = "azure1"),
        plot.background = element_rect(fill = "azure1"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm")
  ) 



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


## Join mean of all clusters with mean of each cluster
table_mean_comparison <-  ObjectMainStatis %>% 
  filter(stat == "mean") %>% 
  left_join(ObjectMeanClusters, by = "index") %>% 
  select(index,
         cluster.assigned,
         mean.value.cluster,
         mean.value.general,
         -stat)

## Plot of mean value of each cluster vs mean of general data
table_mean_comparison %>% 
  gather(mean.value, value, -index, -cluster.assigned) %>% 
  ggplot(aes(index,
             log(value), fill = mean.value)) +
  geom_col(width = 0.7, position = "dodge", alpha = 0.8) + 
  facet_grid(. ~ cluster.assigned) +
  theme_minimal() +
  scale_fill_economist(labels = c("Cluster", "General")) +
  labs(title = "Comparison of Clusters' Average LRFM Values vs All Donors",
       subtitle = "Donors on each cluster have different behaviors",
       y = "Average Values (log)") +
  theme(legend.title = element_blank(),
        legend.position="bottom",
        axis.title.x = element_blank(),
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"), 
        panel.background = element_rect(fill = "azure1"),
        plot.background = element_rect(fill = "azure1"), 
        plot.margin=unit(c(1,1.5,0.5,1.5),"cm"),
        axis.text.x=element_text(angle=-45,hjust=0,vjust=1, size=10)
        ) 




  
  
  
  
 