library(tidyverse)
library(readxl)
library(readxl)
library(gridExtra)
library(ggthemes)
library(astsa)
library(forecast)
library(xts) 
library(ggmap)


########################  Import data ########################  

# Donations dataset

### Group codes
## Import table with group codes
table_group_codes<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/nominal ledgers and source groups.xlsx") %>%
  select(-Recnum) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .)))

### Source codes, group codes
## Import table with sources codes and join with group codes
table_source_codes<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/all source codes.xlsx",
                                skip = 1) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(-c(not.in.use)) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  left_join(table_group_codes, by = "source.group") %>%
  mutate_if(is.character, funs(as.factor))

### Source codes, group codes, nominal descriptions
## Import table with nominal descriptions and join with source/group codes
table_nominal_descriptions <- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/nominal descriptions.xlsx") %>%
  rename_all(funs(tolower(make.names(.)))) %>% 
  mutate_all(funs(as.factor)) %>%
  left_join(table_source_codes, by = c("nominal.codes" = "nominal.ledger")) 

### Income stream codes
## Import Income Stream table
table_income_stream <- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/income stream.xlsx", 
                                  skip = 1) %>%
  rename_all(funs(tolower(make.names(.))))

### Regular donors details
## Import Regular Givers details
table_RG_details<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Regular givers detail.xlsx") %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  select(-c(start.date, terminated.on)) %>%
  mutate_at(vars(frequency), funs(as.factor)) 

### Donations
## Import main donations dataset
dataset_donations <- as.tibble(read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donations_6Years (nopass).xlsx",
                                          sheet = "Fernando_6year_3.7.18_1")) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  select(-c(address1))


### Source codes, group codes, income stream, regular givers details, main dataset
## Join main donation dataset with Income Stream and RG
dataset_donations<- dataset_donations %>% 
  inner_join(table_income_stream, by ="journal.no") %>%
  left_join(table_RG_details, by = c("donor.no", c("donation.amount" = "instalment.amount"))) 


### Source codes, group codes, nominal descriptions, income stream, regular givers details, main dataset
## Join main donations dataset with Income stream and RG with source codes, group codes, nominal descriptions 
dataset_donations <- dataset_donations %>%
  mutate(day = strftime(dataset_donations$donation.date, format = "%d")) %>%
  mutate(week = strftime(dataset_donations$donation.date, format = "%V")) %>%
  mutate(month = strftime(dataset_donations$donation.date, format = "%m")) %>%
  mutate(year = strftime(dataset_donations$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  left_join(table_nominal_descriptions, by = "source") %>%
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_at(vars(donor.no), funs(as.factor)) %>%
  # select(- c(source.group.x, nominal, acquisition.source, nominal.codes, source.group.y)) %>%
  distinct(journal.no, .keep_all = TRUE)


# Geo dataset

# Import UK postcodes table (open data)
table_postcodes<- read_csv("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/ukpostcodes.csv") %>% 
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(postcode, latitude, longitude)

# Import events Postcode and join it with UK postcodes to get the longitud and latitude
table_events_postcodes<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Events Postcodes.xlsx",
                                    skip = 1) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  inner_join(table_postcodes, by = "postcode") %>%
  select(source, postcode.event = postcode, latitude.event = latitude, longitude.event = longitude)


# Import retail Postcode and join it with UK postcodes to get the longitud and latitude
table_retail_postcodes<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Retail Postcodes.xlsx",
                                    skip = 1) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  inner_join(table_postcodes, by = "postcode") %>%
  select(type, postcode, opened, latitude, longitude)


# Include postcodes of donations and events into main dataset
dataset_donations<- dataset_donations %>%
  left_join(table_postcodes, by = "postcode") %>%
  left_join(table_events_postcodes, by = "source")

# Save the coordinates of a city in a vector (search in google )
### In this case Durham 
Coord.City <- c(lon = -1.581517, lat = 54.77952)

## coordinates, zoom = how zoomed will the map be showed, scale = resolution of the map (1 worst ), map type = tipo de mapa (?getmap para ver todos los tipos), source = de donde se lo descarga (si no se especifica es “google”
map <- get_map(Coord.City, zoom = 11, scale = 1)

map2<- get_map(Coord.City, zoom = 10, scale = 1)



################################################################################################################

# Time series of every source of income, sum of donations weekly
dataset_donations %>%
  select(donation.date, development.income, year, week, donation.amount) %>%  
  group_by(year, week, development.income) %>% 
  mutate(Donation = sum(donation.amount)) %>% 
  ggplot(aes(x = donation.date, y = log(Donation))) + 
  geom_line() +
  geom_smooth(method = "lm") + 
  facet_grid(.~ development.income, scales = "free_y") + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Donations by Source", x = "Date", y = "Log Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 


# Time series of source of income
income<- "Events"

dataset_donations %>%
  select(donation.date, development.income, year, week, donation.amount) %>%  
  group_by(year, week, development.income) %>% 
  mutate(Donation = sum(donation.amount)) %>% 
  filter(development.income == income) %>%
  ggplot(aes(x = donation.date, y = (Donation))) + 
  geom_line() +
  geom_smooth(method = "loess") + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Donations by Source", x = "Date", y = "Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 


### ### ### ### ### ### Comparison of RG donations vs others, with projection ### ### ### ### ### ### ### ### 

# Construct data of regular donors

## Filter data of monthly regular donors
dataset_filtered<- dataset_donations %>%
  filter(income.stream == "RG", frequency == "monthly") 

lengthT<- length(dataset_filtered$donor.no)

## Create table with regular donors that are not monthly
mensualized_RG_notM<- dataset_donations %>%
  select(journal.no, donation.amount, income.stream, frequency) %>%
  filter(income.stream %in% c("RG","PG"), frequency %in% c(NA,"quarterly", "annually", "biannual", "weekly")) %>%
  summarise(mensualized = (sum(donation.amount))) 

## Vector with donations that are not monthly
mensualized_total<- mensualized_RG_notM$mensualized

## Add the non monthly values equally to every month
dataset_filtered<- dataset_filtered %>% 
  mutate(donation.amount.total = ((mensualized_total / lengthT) +  donation.amount)) %>%
  unite(MY, month, year) %>%
  group_by(MY) %>% 
  mutate(donation = (sum(donation.amount.total))) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE)

# income.to.compare<- "Commmunity"

# dataset_filtered2<- dataset_donations %>%
#   filter(income.stream != "RG", income.stream != "PG", development.income == income.to.compare) %>%
#   unite(MY, month, year) %>%
#   group_by(MY) %>% 
#   mutate(donation = (sum(donation.amount))) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE)

## Contruct data of non regular donors
dataset_filtered2<- dataset_donations %>%
  filter(income.stream != "RG", income.stream != "PG", development.income != c("Room hire", "Legacies", "Grant Income", "Restricted Trust Income", "Unrestricted Trust Income")) %>%
  unite(MY, month, year) %>%
  group_by(MY) %>%
  mutate(donation = (sum(donation.amount))) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE)

# dataset_filtered2<- dataset_donations %>%
#   filter(income.stream != "RG", income.stream != "PG") %>%
#   unite(MY, month, year) %>%
#   group_by(MY) %>% 
#   mutate(donation = (sum(donation.amount))) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE)


## Create filtered xts object
vectorXTS_filtered<- xts(dataset_filtered$donation, order.by = (dataset_filtered$donation.date))

vectorXTS_filtered2<- xts(dataset_filtered2$donation, order.by = (dataset_filtered2$donation.date))

# Create xts timeseries to ts
vectorT<- vectorXTS_filtered %>% 
  coredata() %>% 
  as.ts()

vectorT2<- vectorXTS_filtered2 %>% 
  coredata() %>% 
  as.ts()

# Create a cleaned timeseries object
vectorT <- ts(as.vector(vectorT)) %>% tsclean()

vectorT2 <- ts(as.vector(vectorT2)) %>% tsclean()

# # Plot time series
# vectorT %>% ts.plot()
# 
# vectorT2 %>% ts.plot()


# Apply Box test to see if time series is White Noise (if p- value less than 0.05 it's not white noise)
vectorT %>% Box.test()

# Make the test set
test_window<- round(length(vectorT)*0.2)
forecast_window<- 12 #3 month
train <- subset(vectorT, end = length(vectorT) - test_window) 

# Fit the desired model to the training data (in this cas an ARIMA and an ETS model)
lambda<- vectorT %>% BoxCox.lambda()
classifierT1 <- auto.arima(train, lambda = lambda, stepwise = FALSE, seasonal = TRUE)

# Check that both models have white noise residuals
# checkresiduals(classifierT1)

## Use accuracy() to compare the paramethers of auto.arima vs the manual fit
for_classifierT1 <- forecast(classifierT1, h = forecast_window)
# for_classifierT11 <- forecast(classifierT11, h = forecast_window)
accuracy(for_classifierT1, vectorT)

# Produce forecasts for each model
## Train full time series
classifierFull1 <- auto.arima(vectorT, lambda = lambda, stepwise = FALSE, seasonal = TRUE)

## Forecast full time series
for_classifierFull1 <- forecast(classifierFull1, h = forecast_window)

## Plot the train data vs the predicted values, also plot the forecast of the train data comparing it to the real values 
autoplot(for_classifierFull1) + 
  autolayer(vectorT) + 
  autolayer(fitted(for_classifierFull1)) + 
  autolayer(vectorT2) + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Amount of Donations of Regular vs Non Regular Givers", x = "Time (Months)", y = "Donations (GBP)", fill = "% of Confidence") +
  theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 


autoplot(vectorT) + 
  autolayer(for_classifierFull1) + 
  theme_economist() +
  scale_fill_economist() + 
  labs(title = "St. Cuthbert's Hospice", subtitle = "Evolution of donations of Regular Givers", x = "Time (Months)", y = "Donations (GBP)", fill = "% of Confidence") +
  theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

# Print the mean of the predicted values to have just one value predicted and not a 80 to 95% confidence range
for_classifierFull1$mean


### ### ### ### ### ### Comparison of number of donators RG vs others, with projection ### ### ### ### ### 

## Create dataset of regular donors
dataset_filtered<- dataset_donations %>%  
  filter(income.stream %in% c("PG", "RG")) %>%
  select(donation.date, development.income, year, month, donor.no) %>%
  group_by(year, month, development.income, donor.no) %>%
  distinct(donor.no, .keep_all = TRUE) %>%
  mutate(donors = length(donor.no)) %>%
  unite(MY, month, year) %>%
  group_by(MY) %>%
  mutate(donors.total = sum(donors)) %>%
  distinct(MY, .keep_all = TRUE)

## Create dataset of non regular donors
dataset_filtered2<- dataset_donations %>%  
  filter(income.stream != "PG", income.stream != "RG") %>%
  select(donation.date, development.income, year, month, donor.no) %>%
  group_by(year, month, development.income, donor.no) %>%
  distinct(donor.no, .keep_all = TRUE) %>%
  mutate(donors = length(donor.no)) %>%
  unite(MY, month, year) %>%
  group_by(MY) %>%
  mutate(donors.total = sum(donors)) %>%
  distinct(MY, .keep_all = TRUE)

## Create filtered xts object
vectorXTS_filtered<- xts(dataset_filtered$donors.total, order.by = (dataset_filtered$donation.date))

vectorXTS_filtered2<- xts(dataset_filtered2$donors.total, order.by = (dataset_filtered2$donation.date))

# Create xts timeseries to ts
vectorT<- vectorXTS_filtered %>% 
  coredata() %>% 
  as.ts()

vectorT2<- vectorXTS_filtered2 %>% 
  coredata() %>%
  as.ts()

vectorT <- ts(as.vector(vectorT)) %>% tsclean()

vectorT2 <- ts(as.vector(vectorT2)) %>% tsclean()

# # Plot time series
# vectorT %>% ts.plot()
# 
# vectorT2 %>% ts.plot()


# Apply Box test to see if time series is White Noise (if p- value less than 0.05 it's not white noise)
vectorT %>% Box.test()

# Make the test set
test_window<- round(length(vectorT)*0.2)
forecast_window<- 12 #3 month
train <- subset(vectorT, end = length(vectorT) - test_window) 

# Fit the desired model to the training data (in this cas an ARIMA and an ETS model)
lambda<- vectorT %>% BoxCox.lambda()
classifierT1 <- auto.arima(train, lambda = lambda, stepwise = FALSE, seasonal = TRUE)
# classifierT2 <- ets(train)

# Check that both models have white noise residuals
# checkresiduals(classifierT1)
# checkresiduals(classifierT2)

## Use accuracy() to compare the paramethers of auto.arima vs the manual fit
for_classifierT1 <- forecast(classifierT1, h = forecast_window)
# for_classifierT11 <- forecast(classifierT11, h = forecast_window)
accuracy(for_classifierT1, vectorT)
# accuracy(for_classifierT11, vectorT)

# Produce forecasts for each model
## Train full time series
classifierFull1 <- auto.arima(vectorT, lambda = lambda, stepwise = FALSE, seasonal = TRUE)
# classifierFull11 <- vectorT %>% Arima(order = c(0,1,2), lambda = lambda, seasonal = FALSE)

## Forecast full time series
for_classifierFull1 <- forecast(classifierFull1, h = forecast_window)
# for_classifierFull11 <- forecast(classifierFull11, h = forecast_window)

## Plot the train data vs the predicted values, also plot the forecast of the train data comparing it to the real values 
autoplot(for_classifierFull1) + 
  autolayer(vectorT) + 
  autolayer(fitted(for_classifierFull1)) + 
  autolayer(vectorT2) + 
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Number of Regular vs Non Regular donors", x = "Time (Months)", y = "Number of donors", fill = "% of Confidence", colour = "Time Series") +
  theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

autoplot(vectorT) + 
  autolayer(for_classifierFull1) + 
  theme_economist() +
  scale_fill_economist() + 
  labs(title = "St. Cuthbert's Hospice", subtitle = "Evolution of Regular donors", x = "Time (Months)", y = "Donors") +
  theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 


# autoplot(for_classifierFull11) + autolayer(vectorT) + autolayer(fitted(for_classifierFull11))

# Print the mean of the predicted values to have just one value predicted and not a 80 to 95% confidence range
for_classifierFull1$mean
# for_classifierFull11$mean



### ### ### ### ### ### ### ### ### Donations by payment type ### ### ### ### ### ### ### ### ### ###

# Timeseries of payments by payment type

# ## Number of donations
# plot1<- dataset_donations %>% 
#   filter(payment.type != 5, payment.type != 10, payment.type != 20, payment.type != 13)  %>%
#   arrange(donation.date) %>%
#   unite(MY, month, year) %>%
#   group_by(MY, payment.type) %>%
#   mutate(CountDon = length(payment.type)) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE) %>%
#   ggplot(aes(x = donation.date, y = log(CountDon), colour = factor(payment.type))) + 
#   geom_line() +
#   geom_smooth(method = "lm") +
#   facet_grid(. ~ payment.type) + 
#   theme_economist() +
#   guides(colour = FALSE) +
#   labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Number of Donations", x = "Date", y = "Log # Donations") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 
# 
# ## Amount of donations
# plot2<- dataset_donations %>% 
#   filter(payment.type != 5, payment.type != 10, payment.type != 20, payment.type != 13)  %>%
#   arrange(donation.date) %>%
#   unite(MY, month, year) %>%
#   group_by(MY, payment.type) %>%
#   mutate(SumDon = sum(donation.amount)) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE) %>%
#   ggplot(aes(x = donation.date, y = log(SumDon), colour = factor(payment.type))) + 
#   geom_line() +
#   geom_smooth(method = "lm") +
#   facet_grid(. ~ payment.type) + 
#   theme_economist() +
#   guides(colour = FALSE) +
#   labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of amount of Donations", x = "Date", y = "Log Donations") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 
# 
# grid.arrange(plot1, plot2, nrow=2, ncol=1)


# Digital donations vs physical donations

## Amount Donations
plot1<- dataset_donations %>% 
  mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
                                  payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
  arrange(donation.date) %>%
  unite(MY, month, year) %>%
  group_by(MY, digi.or.phys) %>%
  mutate(SumDon = sum(donation.amount)) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE) %>%
  ggplot(aes(x = donation.date, y = (SumDon), colour = factor(digi.or.phys))) + 
  geom_line() +
  geom_smooth(method = "loess") +
  theme_economist() +
  guides(colour = FALSE) +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of amount of Donations", x = "Date", y = "Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 


## Sum # Donations
plot2<- dataset_donations %>% 
  mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
                                  payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
  distinct(donor.no, .keep_all = TRUE) %>%
  arrange(donation.date) %>%
  unite(MY, month, year) %>%
  group_by(MY, digi.or.phys) %>%
  mutate(CountDon = length(payment.type)) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE) %>%
  ggplot(aes(x = donation.date, y = (CountDon), colour = factor(digi.or.phys))) + 
  geom_line() +
  geom_smooth(method = "loess") +
  theme_economist() +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Number of Donations", x = "Date", y = "# Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

grid.arrange(plot1, plot2, nrow=1, ncol=2)

#################### Geodata analysis #######################

# Analysis of digital and physical payments
## Insight: events are digitally paid, while others not much / distribution similar
## Insight 2: if events payments are digital, but events payments are not that spread, it means that
# digital payments do not drive expantion, but the locality of the event

dataset_maps<- dataset_donations %>% 
  mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
                                  payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
  select(postcode, year, month, donation.amount, digi.or.phys, latitude, longitude, development.income) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(sum.donations = log(sum(donation.amount))) %>%
  filter(!development.income == "Events")

plot1<- ggmap(map2, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = digi.or.phys), alpha = 0.1) +
  geom_point(data = table_events_postcodes, 
             mapping = aes(x = longitude.event, y = latitude.event), colour = "orange") +
  labs(title = "Mapping of digital and physical payments by area", subtitle = "Not events", x = "Longitude", y = "Latitude") 



dataset_maps<- dataset_donations %>% 
  mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
                                  payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
  select(postcode, year, month, donation.amount, digi.or.phys, latitude, longitude, development.income) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(sum.donations = log(sum(donation.amount))) %>%
  filter(development.income == "Events")

plot2<- ggmap(map2, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = digi.or.phys), alpha = 0.1) +
  geom_point(data = table_events_postcodes, 
             mapping = aes(x = longitude.event, y = latitude.event), colour = "orange") +
  labs(title = "", subtitle = "Events", x = "Longitude", y = "Latitude") 

grid.arrange(plot1, plot2, nrow=1, ncol=2)


######################## SDD ######################## 

# Analysis of dispersion of payments by year
## Digital payments drive expansion of donations across geographical areas?

### By year
## Insight: Payments in 2017 are more spread than it used to be in 2012
table_2017<- dataset_donations %>% 
  filter(year == "2017") %>%
  filter(development.income == "Events") %>%
  select(longitude, latitude) %>%
  na.omit()

table_2012<- dataset_donations %>% 
  filter(year == "2012") %>%
  filter(development.income == "Events") %>%
  select(longitude, latitude) %>%
  na.omit()

# 2017
plot(table_2017, 
     xlab="", 
     ylab="", 
     asp=1, 
     axes=FALSE, 
     main="Donations (2017)", type="n")

calc_sdd(id=1, 
         filename="SDD_Output.txt", 
         centre.xy=NULL, 
         calccentre=TRUE, 
         weighted=FALSE, 
         weights=NULL, 
         points=table_2017, verbose=FALSE)


plot_sdd(plotnew=FALSE, 
         plotcentre=FALSE, 
         centre.col="red", 
         centre.pch="1", 
         sdd.col="red",
         sdd.lwd=1,
         titletxt="", 
         plotpoints=TRUE,points.col="black")

# Label the centroid, explicitly using the hidden r.SDD object that was used in plot_sde
text(r.SDD$CENTRE.x, r.SDD$CENTRE.y, "+", col="red")

spatial_st.dev2017<- (r.SDD$SDD)


# 2012
plot(table_2012, 
     xlab="", 
     ylab="", 
     asp=1, 
     axes=FALSE, 
     main="Donations (2012)", type="n")

calc_sdd(id=1, 
         filename="SDD_Output.txt", 
         centre.xy=NULL, 
         calccentre=TRUE, 
         weighted=FALSE, 
         weights=NULL, 
         points=table_2012, verbose=FALSE)


plot_sdd(plotnew=FALSE, 
         plotcentre=FALSE, 
         centre.col="red", 
         centre.pch="1", 
         sdd.col="red",
         sdd.lwd=1,
         titletxt="", 
         plotpoints=TRUE,points.col="black")

# Label the centroid, explicitly using the hidden r.SDD object that was used in plot_sde
text(r.SDD$CENTRE.x, r.SDD$CENTRE.y, "+", col="red")

spatial_st.dev2012<- (r.SDD$SDD)



### By event or not event
## Insight: payments of not events are more dispersed 
## Digital payments don't drive expansion
## What drives expansion?
event<- dataset_donations %>% 
  filter(development.income == "Events") %>%
  select(longitude, latitude) %>%
  na.omit()

not_event<- dataset_donations %>% 
  filter(!development.income == "Events", longitude < 0) %>%
  select(longitude, latitude) %>%
  na.omit()

# Events
plot(event, 
     xlab="", 
     ylab="", 
     asp=1, 
     axes=FALSE, 
     main="Donations (event)", type="n")

calc_sdd(id=1, 
         filename="SDD_Output.txt", 
         centre.xy=NULL, 
         calccentre=TRUE, 
         weighted=FALSE, 
         weights=NULL, 
         points=event, verbose=FALSE)


plot_sdd(plotnew=FALSE, 
         plotcentre=FALSE, 
         centre.col="red", 
         centre.pch="1", 
         sdd.col="red",
         sdd.lwd=1,
         titletxt="", 
         plotpoints=TRUE,points.col="black")

# Label the centroid, explicitly using the hidden r.SDD object that was used in plot_sde
text(r.SDD$CENTRE.x, r.SDD$CENTRE.y, "+", col="red")

spatial_st_dev_event<- (r.SDD$SDD)


# Not events
plot(not_event, 
     xlab="", 
     ylab="", 
     asp=1, 
     axes=FALSE, 
     main="Donations (not event)", type="n")

calc_sdd(id=1, 
         filename="SDD_Output.txt", 
         centre.xy=NULL, 
         calccentre=TRUE, 
         weighted=FALSE, 
         weights=NULL, 
         points=not_event, verbose=FALSE)


plot_sdd(plotnew=FALSE, 
         plotcentre=FALSE, 
         centre.col="red", 
         centre.pch="1", 
         sdd.col="red",
         sdd.lwd=1,
         titletxt="", 
         plotpoints=TRUE,points.col="black")

# Label the centroid, explicitly using the hidden r.SDD object that was used in plot_sde
text(r.SDD$CENTRE.x, r.SDD$CENTRE.y, "+", col="red")

spatial_st_dev_notevent<- (r.SDD$SDD)


### By region, month, events
## There are clusters on the left that don't have any event and can be boosted
## If this clusters get bigger, the SDD should get higher
dataset_maps<- dataset_donations %>%
  select(postcode, year, month, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(sum.donations = log(sum(donation.amount))) 


plot11<- ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) +
  geom_point(data = table_events_postcodes, 
             mapping = aes(x = longitude.event, y = latitude.event), colour = "orange") +
  geom_point(data = table_retail_postcodes, 
             mapping = aes(x = longitude, y = latitude), colour = "red")

plot12<- ggmap(map2, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) +
  geom_point(data = table_events_postcodes, 
             mapping = aes(x = longitude.event, y = latitude.event), colour = "orange") +
  geom_point(data = table_retail_postcodes, 
             mapping = aes(x = longitude, y = latitude), colour = "red")


grid.arrange(plot11, plot12, nrow=1, ncol=2)



### By region, faceted yearly, new retail shops marked
## Growth happens at opening, then dissapears
## Do the hospice stop doing awareness campaign with time?

table_retail_postcodes_filtered<- table_retail_postcodes %>%
  filter(postcode %in% c("DL15 9HT", "DH7 8XD"))


dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  # filter(year %in% c("2014", "2015", "2016", "2017")) %>%
  group_by(region, year) %>% 
  mutate(sum.donations = log(sum(donation.amount))) %>%
  filter(development.income == "Events")


ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) +
  geom_point(data = table_retail_postcodes_filtered, 
             mapping = aes(x = longitude, y = latitude), colour = "red") +
  geom_point(aes(x = -1.607849, y = 54.751254), colour = "red") +
  facet_wrap(~ year)



### ### ### ### ### ### ### ### ### ### ### Conclutions ### ### ### ### ### ### ### ### ### ### ### ###

# Insights: 
## Number of donors: regular givers and online payments have good projections
## Amount of donations: random givers and offline payments take the lead 

## Digital payments outnumber physical payments in number but not amount

# Proposal:
## Make campaign of regular giver online (promote it on events and website, QR code, etc.)
# (https://www.directdebit.co.uk/Resources/Pages/AboutUs.aspx and https://www.paypal.com/gb/webapps/mpp/not-for-profit)
## Boost this clusters that are aound Durham by making events on this areas



### ### ### ### ### ### ### ### ### Donations by event/campaign by awarenes/income ### ### ### ### ### ### ### ### ### ###

