library(tidyverse)
library(gridExtra)
library(plotly)



# Plot of total donations $$ 
plot1 <- dataset_donations %>%
  select(donation.amount,
         donation.year) %>% 
  filter(donation.year > 2000) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  ggplot(aes(donation.year, sum.donations.year)) +
  geom_hline(yintercept = 928000, colour = "orange1") +
  geom_hline(yintercept = 967000, colour = "orange3") +
  labs(title = "Sum of Total $$ Donations") +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm")

# Plot of growth of total donations $$ 
plot2 <- dataset_donations %>%
  select(donation.amount,
         donation.year) %>% 
  filter(donation.year > 2000) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(donation.year) %>%
  mutate(perc.prev.year = round((
    sum.donations.year / lag(sum.donations.year)
  ) * 100, 2)) %>%
  ggplot(aes(donation.year, perc.prev.year)) +
  labs(title = "Growth of total $$ Donations") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "lm")

grid.arrange(plot1, plot2, nrow=2, ncol=1)


# Development income total by year
plotly1 <- dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(donation.year > 2000) %>%
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


# Plot of growth of donations $$ by developement income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(
    donation.year > 2008,
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
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    sum.donations.year / lag(sum.donations.year)
  ) * 100, 2)) %>%
  filter(perc.prev.year < 1000) %>% 
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  labs(title = "Growth of $$ Donations") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")


# Percentage of donation by Development Income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(donation.year > 2000) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(donation.year, development.income) %>% 
  mutate(sum.donations.year.dev.income = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  mutate(perc.dev.incomme.total.don = round((sum.donations.year.dev.income / sum.donations.year) * 100, 2)) %>% 
  filter(development.income != "NEHL") %>% 
  ggplot(aes(donation.year, perc.dev.incomme.total.don)) +
  labs(title = "Percentage Income by Development Income") +
  geom_point() +
  geom_line() +
  facet_grid(development.income ~ ., scales = "free_y")


# Total donations with colour by log of sum donations of every development income
## This graph shows in what period donations of a particular dev.income was high or low
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(donation.year > 2000) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(donation.year, development.income) %>% 
  mutate(sum.donations.year.dev.income = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(development.income != "NEHL") %>% 
  arrange(donation.year) %>%
  ggplot(aes(donation.year, sum.donations.year, colour = log(sum.donations.year.dev.income))) +
  labs(title = "Total Donations by Sum of Every Development Income") +
  geom_point() +
  geom_line() +
  facet_grid(development.income ~ .)


# Plot of growth of number donations by developement income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(
    donation.year > 2008,
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


# Plot of growth of average donations by developement income
dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(
    donation.year > 2008,
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


# Plot of growth of new donors by year
dataset_donations %>%
  filter(
    donation.year > 2000,
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



## Correlation new donors income

VectorPercTotalDon<- dataset_donations %>%
  filter(donation.year > 2012) %>% 
  arrange(donation.year) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>%
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(donation.year) %>%
  mutate(perc.prev.year = round((
    sum.donations.year / lag(sum.donations.year)
  ) * 100, 2)) %>%
  pull(perc.prev.year)


VectorPercNewDon <- dataset_donations %>%
  filter(donation.year > 2012) %>% 
  select(
    # donation.date,
    donation.year,
    donor.no
  ) %>%
  # mutate(Y = strftime(.$donation.date, format = "%Y")) %>%
  # mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  # unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>%
  # ungroup() %>%
  group_by(donor.no) %>% 
  mutate(donations.per.donor = n()) %>%
  ungroup() %>%
  # gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(donations.per.donor == 1) %>%
  group_by(donation.year) %>%
  mutate(sum.new.donors = n()) %>%
  ungroup() %>%
  arrange(donation.year) %>% 
  distinct(donation.year, .keep_all = TRUE) %>% 
  mutate(perc.prev.year = round((
    sum.new.donors / lag(sum.new.donors)
  ) * 100, 2)) %>% 
  pull(perc.prev.year)


cor(VectorPercNewDon, VectorPercTotalDon, use = "pairwise.complete.obs")

## December used to be a low donation date
p1 <- dataset_donations %>% 
  filter(donation.year > 2008) %>% 
  mutate(year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month, year) %>% 
  group_by(MY) %>% 
  mutate(sum.donations = sum(donation.amount)) %>% 
  ungroup() %>% 
  ggplot(aes(donation.month, sum.donations, colour = as.factor(donation.year))) +
  geom_line(aes(group = donation.year)) 

ggplotly(p1)

