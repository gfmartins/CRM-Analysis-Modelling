library(plotly)

# # See meaning grouping by group
# dataset_donations %>% 
#   # filter(source == "INMCOL") %>%
#   # filter(group.name == "In Memoriam") %>% 
#   filter(source.group == "EVE") %>% 
#   select(source.desc:group.name) %>% 
#   View("description source")
# 
# # See levels of a group
#  dataset_donations %>% 
#    # filter(source == "INMCOL") %>%
#    # filter(group.name == "In Memoriam") %>% 
#    filter(source.group == "CLR") %>%
#    droplevels() %$% 
#    table(source.desc) %>% 
#    View("2")
 
# See ranking acquisource by donor and source
# dataset_ml %>% 
#   distinct(donor.no, .keep_all = TRUE) %>% 
#   group_by(acquisition.source) %>% 
#   mutate(number.times = n()) %>% 
#   ungroup() %>% 
#   arrange(desc(number.times)) %>% 
#   select(acquisition.source, number.times) %>% 
#   distinct(acquisition.source, .keep_all = TRUE) %>% 
#   View("ranking acquisition (per donor)")
  
# # See ranking acquisource by total and source
# dataset_ml %>% 
#   # distinct(donor.no, .keep_all = TRUE) %>% 
#   group_by(acquisition.source) %>% 
#   mutate(number.times = n()) %>% 
#   ungroup() %>% 
#   arrange(desc(number.times)) %>% 
#   select(acquisition.source, number.times) %>% 
#   distinct(acquisition.source, .keep_all = TRUE) %>% 
#   View("ranking acquisition (per all)")


# See ranking acquisource by donor and group 
dataset_ml %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  filter(donation.year > 2008) %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  group_by(acquisition.group) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(acquisition.group, number.times) %>% 
  distinct(acquisition.group, .keep_all = TRUE) %>% 
  head(n = 10) %>% 
  ggplot(aes(acquisition.group, number.times)) +
  geom_col()


###### Donations by sequencial donation ##########

# See what is the most common reason of second donations given In Memory as the first one, per donor

table.2 <- dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 2) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "2") %>%
  distinct(activity.clusters, .keep_all = TRUE) 


# See what is the most common reason of third donations given In Memory as the first one, per donor
table.3 <- dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 3) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>%
  mutate(donation.count = "3") %>%
  distinct(activity.clusters, .keep_all = TRUE)


# See what is the most common reason of 4th on donations given In Memory as the first one, per donor
table.4 <- dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 4) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "4") %>%
  distinct(activity.clusters, .keep_all = TRUE)


# See what is the most common reason of 5th on donations given In Memory as the first one, per donor
table.5 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 5) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>%
  mutate(donation.count = "5") %>%
  distinct(activity.clusters, .keep_all = TRUE) 


# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.6 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 6) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "6") %>%
  distinct(activity.clusters, .keep_all = TRUE) 


# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.7 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 7) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "7") %>%
  distinct(activity.clusters, .keep_all = TRUE) 

# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.8 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 8) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "8") %>%
  distinct(activity.clusters, .keep_all = TRUE) 


# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.9 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 9) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>% 
  mutate(donation.count = "9") %>%
  distinct(activity.clusters, .keep_all = TRUE) 

# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.10 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 10) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>%
  mutate(donation.count = "10") %>%
  distinct(activity.clusters, .keep_all = TRUE) 


# See what is the most common reason of 6th on donations given In Memory as the first one, per donor
table.11 <-dataset_ml %>% 
  mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  filter(acquisition.group == "INM",
         donation.year > 2008,
         counter.donation == 11) %>% 
  # distinct(donor.no, .keep_all = TRUE) %>%
  group_by(activity.clusters) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(activity.clusters, number.times) %>%
  mutate(donation.count = "11") %>%
  distinct(activity.clusters, .keep_all = TRUE) 

plot.total <- table.2 %>% 
  bind_rows(table.3, 
            table.4, 
            table.5, 
            table.6, 
            table.7, 
            table.8, 
            table.9, 
            table.10, 
            table.11) %>% 
  mutate_at(vars(donation.count), funs(as.numeric)) %>% 
  ggplot(aes(donation.count, number.times, colour = activity.clusters)) + 
  geom_line()


ggplotly(plot.total)
