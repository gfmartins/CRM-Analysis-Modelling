library(tidyverse)
library(readxl)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggthemes)
library(extrafont)
library(astsa)
library(forecast)
library(xts)

### Cleaning

## See histogram before any cleaning and then compare after
# hist_before<- hist(dataset_donations$donor.no)

## See empty or cero rows
addmargins(apply(dataset_donations, 2, is.na)) %>% tail(n=1) %>% View()

NAs<- which(is.na(dataset_donations$nominal))
dataset_donations[NAs,] %>% droplevels() %>% View()
levels(borrar$source)

borrar %>% View()

# See outliers
boxplot(donation.amount ~ donor.type, data = dataset_donations)
dataset_donations %>% filter(donation.amount > 20000) %>% View()

## See in depth birthday
# # Conclution: Birthday is not useful
# is.na(dataset_donations$date.of.birth) %>% 
# dataset_donations %>% filter(number.of.donations == 1, date.of.last.donation == date.of.first.donation) %>% View()

# Se number of unique donators
# Insight: There have been 6345 unique donators in the past two years
#        : 38.4% has made just one donation in the past two years
dataset_donations %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  mutate(N = n()) %>% filter(number.of.donations == 1) %>% 
  summarise(PerUniqueDon = (n() / mean(N)))
                                   
# Time series analysis
# dataset_donations %>%
#   select(donation.date, donation.amount, nominal.code.description, year, week) %>%  
#   group_by(year, week, nominal.code.description) %>% 
#   mutate(Donation = mean(donation.amount)) %>% 
#   ggplot(aes(x = donation.date, y = log(Donation))) + geom_line(aes(colour = nominal.code.description)) 

# Time series of every source of income
dataset_donations %>%
  select(donation.date, development.income, year, week, donation.amount) %>%  
  group_by(year, week, development.income) %>% 
  mutate(Donation = mean(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + 
  geom_line() +
  geom_smooth(method = "lm") + 
  facet_grid(.~ development.income, scales = "free_y") + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Donations by Source", x = "Date", y = "Log Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

# Time series of income stream 
dataset_donations %>%
  select(donation.date, income.stream, year, week, donation.amount) %>%  
  group_by(year, week, income.stream) %>% 
  mutate(Donation = mean(donation.amount)) %>% 
  filter(income.stream == "EV") %>%
  ggplot(aes(x = donation.date, y = (Donation))) + 
  geom_line() +
  geom_smooth() + 
  facet_grid(.~ income.stream, scales = "free_y") + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Donations by Income Stream", x = "Date", y = "Log Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

View(dataset_donations %>%
       group_by(year, week, income.stream) %>% 
       mutate(Donation = mean(donation.amount)) %>% 
       filter(income.stream == "EV"))


# Time series of every stream of income
dataset_donations %>%
  select(donation.date, development.income, year, week, donation.amount) %>%  
  group_by(year, week, development.income) %>% 
  mutate(Donation = mean(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + 
  geom_line() +
  geom_smooth(method = "lm") + 
  facet_grid(.~ development.income, scales = "free_y") + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Donations by Source", x = "Date", y = "Log Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

# Time series of monthly income
dataset_donations %>%
  select(donation.date, donation.amount, month, year, week) %>%  
  group_by(year, month, week) %>% 
  mutate(Donation = sum(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + 
  geom_line() +
  geom_smooth(method = "lm")

# Time series with filtering
plot1<- dataset_donations %>% 
  filter(income.stream %in% c("RG")) %>% 
  select(donation.date, donation.amount) %>%  
  group_by(donation.date) %>% 
  summarise(Donation = sum(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + geom_line() + 
  geom_smooth(method = "lm")

plot2<- dataset_donations %>%
  select(donation.date, donation.amount) %>%  
  group_by(donation.date) %>% 
  summarise(Donation = sum(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + geom_line() + 
  geom_smooth(method = "lm")


grid.arrange(plot1, plot2, nrow=2, ncol=1)


