library(magrittr)
library(gridExtra)
library(plotly)

# Year to year comparison

# Plot of growth of donations $$ by developement income
dataset_donations %>%
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
  arrange(donation.year) %>%
  group_by(donation.year, development.income) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>%
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    sum.donations.year / lag(sum.donations.year)
  ) * 100, 2)) %>%
  filter(perc.prev.year < 1000) %>% 
  select(donation.year,
         sum.donations.year,
         perc.prev.year,
         development.income) %>%
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")

# Plot of growth of number donations by developement income
dataset_donations %>%
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
  arrange(donation.year) %>%
  group_by(donation.year, development.income) %>%
  mutate(number.donations.year = n()) %>%
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    number.donations.year / lag(number.donations.year)
  ) * 100, 2)) %>%
  select(donation.year,
         number.donations.year,
         perc.prev.year,
         development.income) %>%
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")

# # Plot of sum of donations by developement income
# dataset_donations %>%
#   filter(
#     donation.year > 2000,
#     development.income %in% c(
#       "Events",
#       "Commmunity",
#       "Individuals/ Donor development",
#       "Legacies",
#       "Restricted Trust Income",
#       "Unrestricted Trust Income"
#     )
#   ) %>%
#   arrange(donation.year) %>%
#   group_by(donation.year, development.income) %>%
#   mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>%
#   distinct(donation.year, .keep_all = TRUE) %>%
#   ungroup() %>%
#   select(donation.year,
#          sum.donations.year,
#          development.income) %>%
#   ggplot(aes(donation.year, sum.donations.year), colour = development.income) +
#   geom_line() +
#   geom_smooth(se = FALSE, method = "loess") +
#   facet_grid(development.income ~ ., scales = "free_y")


# Plot of growth of events donations
dataset_donations %>%
  filter(
    donation.year > 2000,
    development.income ==
      "Events"
  ) %>%
  arrange(donation.year) %>%
  group_by(donation.year, development.income) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>%
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(development.income, donation.year) %>%
  mutate(perc.prev.year = round((
    sum.donations.year / lag(sum.donations.year)
  ) * 100, 2)) %>%
  select(donation.year,
         sum.donations.year,
         perc.prev.year,
         development.income) %>%
  ggplot(aes(donation.year, perc.prev.year), colour = development.income) +
  labs(title = "Growth of events donations") +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(development.income ~ ., scales = "free_y")

# # Plot of growth of donations total
# dataset_donations %>%
#   filter(donation.year > 2000) %>% 
#   arrange(donation.year) %>%
#   group_by(donation.year) %>%
#   mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>%
#   distinct(donation.year, .keep_all = TRUE) %>%
#   ungroup() %>%
#   arrange(donation.year) %>%
#   mutate(perc.prev.year = round((
#     sum.donations.year / lag(sum.donations.year)
#   ) * 100, 2)) %>%
#   select(donation.year,
#          sum.donations.year,
#          perc.prev.year) %>%
#   ggplot(aes(donation.year, perc.prev.year)) +
#   labs(title = "Growth of Donations") +
#   geom_line() +
#   geom_smooth(se = FALSE, method = "lm")


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

# Plot of growth of new donors by year
dataset_donations %>%
  filter(donation.year > 2000) %>% 
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
  # mutate_if(is.character, as.factor) %>% 
  ggplot(aes(
    donation.year,
    perc.prev.year
  )) + 
  labs(title = "Growth of New Donors") +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm")


# Plot of growth of new donors by year of events
plot4 <- dataset_donations %>%
  filter(donation.year > 2000,
         development.income == "Events") %>% 
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
  # mutate_if(is.character, as.factor) %>% 
  ggplot(aes(
    donation.year,
    perc.prev.year
  )) + 
  labs(title = "Growth of Events' New Donors") +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm")


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

grid.arrange(plot1, plot2, plot3, plot4, nrow=4, ncol=1)


cor(VectorPercNewDon, VectorPercTotalDon, use = "pairwise.complete.obs")


########################################## General template for ARIMA/Forecasting Modelling ################################################## 
library(forecast)
library(astsa)
library(xts)

# Select the variable to predict
# vectorT<- dataset %>% select("COLUMNA A SELECCIONAR") %>% as.ts()
dataset_forecast <- dataset_donations %>%
  filter(donation.year > 2000) %>% 
  mutate(month = strftime(.$donation.date, format = "%m")) %>%
  mutate(year = strftime(.$donation.date, format = "%Y")) %>% 
  unite(MY, month, year) %>%
  group_by(MY) %>% 
  group_by(MY) %>%
  mutate(sum.donations.month = sum(donation.amount, na.rm = TRUE)) %>%
  distinct(MY, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(MY) %>% 
  mutate(perc.prev.year = round((
    sum.donations.month / lag(sum.donations.month)
  ) * 100, 2)) 



## Create filtered xts object
vectorXTS<- xts(dataset_forecast$perc.prev.year, order.by = (dataset_forecast$donation.date))

# Create xts timeseries to ts
vectorT<- vectorXTS %>% 
  coredata() %>% 
  as.ts()

# Eliminate outliers
vectorT <- ts(as.vector(vectorT)) %>% tsclean()


# Plot time series
vectorT %>% ts.plot()

# 1) Exploratory analysis

# Plot time series
vectorT %>% ts.plot()

# Plot acf to see if there is correlation between lags (spot seasonality or trend)
## N = depends on the lenght of time series, how many observations behind we want to analyze
vectorT %>% acf(lag.max = 150)

# Apply Box test to see if time series is White Noise (if p- value less than 0.05 it's not white noise)
vectorT %>% Box.test()


######## 2.2) Predict two different models on test and train set and keep the best one

# Make the test set
test_window<- round(length(vectorT)*0.2)
forecast_window<- 12
train <- subset(vectorT, end = length(vectorT) - test_window) ## 20 es la longitud del test set en este ejemplo

# Fit the desired model to the training data (in this cas an ARIMA and an ETS model)
lambda<- vectorT %>% BoxCox.lambda()
classifierT1 <- auto.arima(train, lambda = lambda, stepwise = FALSE, seasonal = FALSE)

# Check that both models have white noise residuals
checkresiduals(classifierT1)

## Use accuracy() to compare the paramethers of auto.arima vs the manual fit
for_classifierT1 <- forecast(classifierT1, h = forecast_window)
accuracy(for_classifierT1, vectorT)

# Produce forecasts for each model
## Train full time series
classifierFull1 <- auto.arima(vectorT, lambda = lambda, stepwise = FALSE, seasonal = FALSE)

## Forecast full time series
for_classifierFull1 <- forecast(classifierFull1, h = forecast_window)

## Plot the train data vs the predicted values, also plot the forecast of the train data comparing it to the real values 
autoplot(for_classifierFull1) + autolayer(vectorT) + autolayer(fitted(for_classifierFull1))

# Print the mean of the predicted values to have just one value predicted and not a 80 to 95% confidence range
for_classifierFull1$mean


############## General income analysis ############

# Plot of total donations $$ 
dataset_donations %>%
  select(donation.amount,
         donation.year) %>% 
  filter(donation.year > 2000) %>%
  group_by(donation.year) %>%
  mutate(sum.donations.year = sum(donation.amount, na.rm = TRUE)) %>% 
  distinct(donation.year, .keep_all = TRUE) %>%
  ungroup() %>%
  ggplot(aes(donation.year, sum.donations.year)) +
  labs(title = "Growth of total $$ Donations") +
  geom_line() +
  geom_hline(yintercept = 100, colour = "orange1") +
  geom_smooth(se = FALSE, method = "loess")

# Plot of growth of total donations $$ 
dataset_donations %>%
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



# Plot of sum of total donations $$ with colour by sum of development.income
levels(dataset_donations$development.income)

DevIncome <- "Commmunity"

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
  filter(development.income == DevIncome) %>%
  arrange(donation.year) %>%
  ggplot(aes(donation.year, sum.donations.year, colour = log(sum.donations.year.dev.income))) +
  labs(title = "Growth of total $$ Donations") +
  geom_point() +
  geom_line() 


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
  # filter(development.income == DevIncome) %>% 
  arrange(donation.year) %>%
  ggplot(aes(donation.year, sum.donations.year, colour = log(sum.donations.year.dev.income))) +
  labs(title = "Total Donations by Sum of Every Development Income") +
  geom_point() +
  geom_line() +
  facet_grid(development.income ~ .)


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


# Development income total by year
plotly1 <- dataset_donations %>%
  select(donation.amount,
         donation.year,
         development.income) %>% 
  filter(donation.year > 2008) %>%
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
         