library(plotly)
library(gridExtra)


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

# See what is the most common reason of second, third... 10th donations given In Memory as the first one, per donor

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


###### Identifying donors that are likely to engage ##########

# See if payment type can be a predictor 

### Prepare data

Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl <-   tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}

## What payments methods use more engaged donors?
## Looks like Cash and Checks and Just Giving
dataset_ml %>% 
  droplevels() %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation),
         most.used.paymentchannel = Mode(payment.type)) %>% 
  ungroup() %>% 
  filter(max.number.donations < 200,
         payment.type %in% c(1,2,11, 14:16),
         !source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE")
         ) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  select(donor.no, payment.type, most.used.paymentchannel, max.number.donations) %>% 
  na.omit() %>% 
  ggplot(aes(max.number.donations)) +
  geom_histogram(bins = 200) +
  facet_grid(payment.type ~ ., scales = "free_y")



# Are donors that live closer to the hospice more engaged?
## Yes, but the difference is not that relevant
plot1 <- dataset_ml %>% 
  droplevels() %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.outside.area = ifelse(closest.retail.store == "Outside of Catchment Area", "Yes", "No")) %>% 
  filter(max.number.donations < 20,
         # payment.type %in% 14,
         !source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE")
  ) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  select(donor.no, payment.type, max.number.donations, binari.outside.area) %>% 
  na.omit() %>% 
  ggplot(aes(max.number.donations)) +
  geom_histogram(bins = 20) +
  facet_grid(binari.outside.area ~ ., scales = "free_y", labeller = label_both) +
  labs(title = "Just Giving")

plot2 <- dataset_ml %>% 
  droplevels() %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.outside.area = ifelse(closest.retail.store == "Outside of Catchment Area", "Yes", "No")) %>% 
  filter(max.number.donations < 20,
         # payment.type %in% 15,
         !source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE")
  ) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  select(donor.no, payment.type, max.number.donations, binari.outside.area) %>% 
  na.omit() %>% 
  ggplot(aes(max.number.donations)) +
  geom_histogram(bins = 20) +
  facet_grid(binari.outside.area ~ ., scales = "free_y", labeller = label_both) +
  labs(title = "PayPal")

grid.arrange(plot1, plot2, nrow=2, ncol=1)


# Is there any difference between Just Giving and PayPal users?
## Yes, a relevant

plot1 <- dataset_ml %>% 
  droplevels() %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.outside.area = ifelse(closest.retail.store == "Outside of Catchment Area", "Yes", "No")) %>% 
  filter(max.number.donations < 100,
         payment.type %in% 14,
         !source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE")
  ) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  select(donor.no, payment.type, max.number.donations, binari.outside.area) %>% 
  na.omit() %>% 
  ggplot(aes(max.number.donations)) +
  geom_histogram(bins = 100) +
  facet_grid(binari.outside.area ~ ., scales = "free_y", labeller = label_both) +
  labs(title = "Just Giving")

plot2 <- dataset_ml %>% 
  droplevels() %>% 
  group_by(donor.no) %>% 
  mutate(max.number.donations = max(counter.donation)) %>% 
  ungroup() %>% 
  mutate(binari.outside.area = ifelse(closest.retail.store == "Outside of Catchment Area", "Yes", "No")) %>% 
  filter(max.number.donations < 100,
         payment.type %in% 15,
         !source %in% c("CAMPOC", "REGGIV","FRIEND", "GIVAYE")
  ) %>% 
  distinct(donor.no, .keep_all = TRUE) %>%
  select(donor.no, payment.type, max.number.donations, binari.outside.area) %>% 
  na.omit() %>% 
  ggplot(aes(max.number.donations)) +
  geom_histogram(bins = 100) +
  facet_grid(binari.outside.area ~ ., scales = "free_y", labeller = label_both) +
  labs(title = "PayPal")

grid.arrange(plot1, plot2, nrow=2, ncol=1)


# Types of activities paid with Just Giving
## Do donations made by Just Giving reveal a certain activity that causes engagement? 

dataset_donations %>% 
  # distinct(donor.no, .keep_all = TRUE) %>% 
  filter(donation.year > 2008,
         payment.type == 14) %>% 
  # mutate(acquisition.group = str_sub(.$acquisition.source, 1,3)) %>% 
  group_by(group.name) %>% 
  mutate(number.times = n()) %>%
  ungroup() %>% 
  arrange(desc(number.times)) %>% 
  select(group.name, number.times) %>% 
  distinct(group.name, .keep_all = TRUE) %>% 
  head(n = 10) %>% 
  ggplot(aes(group.name, number.times)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=6))


# Development income total by year
plotly1 <- dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(between(donation.year, 2000, 2018)) %>%
  # group_by(donation.year) %>%
  # mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  group_by(donation.year, development.income) %>% 
  mutate(sum.donations.year.dev.income = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  # mutate(perc.dev.incomme.total.don = round((sum.donations.year.dev.income / sum.donations.year) * 100, 2)) %>% 
  filter(development.income != "NEHL") %>% 
  ggplot(aes(donation.year, sum.donations.year.dev.income, colour = development.income)) +
  geom_line()

ggplotly(plotly1)


# Plot of growth of number donations by developement income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(between(donation.year, 2000, 2018)) %>%
  filter(
    development.income %in% c(
      "Events",
      "Commmunity",
      "Individuals/ Donor development",
      "Legacies",
      "Restricted Trust Income",
      "Unrestricted Trust Income"
    )
  ) %>%
  group_by(donation.year, development.income) %>%
  mutate(number.donations.year = n()) %>%
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(development.income, donation.year) %>% 
  mutate(perc.prev.year = round((
    number.donations.year / lag(number.donations.year)
  ) * 100, 2)) %>%
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  labs(title = "Growth of Number of Donations") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")


# Plot of growth of new donors by year
dataset_donations %>%
  filter(between(donation.year, 2000, 2018)) %>%
  filter(
    development.income %in% c(
      "Events",
      "Commmunity",
      "Individuals/ Donor development",
      "Legacies",
      "Restricted Trust Income",
      "Unrestricted Trust Income"
    )
  ) %>%
  group_by(donor.no, development.income) %>% 
  mutate(donations.per.donor = n()) %>%
  # gather(index, value, -c(journal.no:donation.year, MY)) %>%
  ungroup() %>% 
  filter(donations.per.donor == 1) %>%
  group_by(donation.year, development.income) %>%
  mutate(sum.new.donors = n()) %>%
  distinct(donation.year, .keep_all = TRUE) %>% 
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    sum.new.donors / lag(sum.new.donors)
  ) * 100, 2)) %>%  
  # mutate_if(is.character, as.factor) %>% 
  ggplot(aes(
    donation.year,
    perc.prev.year
  )) + 
  labs(title = "Growth of New Donors") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")



# Plot of growth of average donations by developement income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(between(donation.year, 2000, 2018)) %>%
  filter(
    development.income %in% c(
      "Events",
      "Commmunity",
      "Individuals/ Donor development",
      "Legacies",
      "Restricted Trust Income",
      "Unrestricted Trust Income"
    )
  ) %>%
  group_by(donation.year, development.income) %>%
  mutate(avg.donations.year = mean(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>% 
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    avg.donations.year / lag(avg.donations.year)
  ) * 100, 2)) %>% 
  filter(perc.prev.year < 3000) %>% 
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  labs(title = "Growth of Mean Donations") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")
