library(gridExtra)

# Average of number of donations by:
# payment type

## Number of donations
plot1<- dataset_donations %>% 
  filter(payment.type != 5, payment.type != 10, payment.type != 20, payment.type != 13)  %>%
  arrange(donation.date) %>%
  unite(MY, month, year) %>%
  group_by(MY, payment.type) %>%
  mutate(CountDon = length(payment.type)) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE) %>%
  ggplot(aes(x = donation.date, y = log(CountDon), colour = factor(payment.type))) + 
  geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(. ~ payment.type) + 
  theme_economist() +
  guides(colour = FALSE) +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of Number of Donations", x = "Date", y = "Log # Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

# Amount of donations
plot2<- dataset_donations %>% 
  filter(payment.type != 5, payment.type != 10, payment.type != 20, payment.type != 13)  %>%
  arrange(donation.date) %>%
  unite(MY, month, year) %>%
  group_by(MY, payment.type) %>%
  mutate(SumDon = sum(donation.amount)) %>%
  arrange(donation.date) %>%
  distinct(MY, .keep_all = TRUE) %>%
  ggplot(aes(x = donation.date, y = log(SumDon), colour = factor(payment.type))) + 
  geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(. ~ payment.type) + 
  theme_economist() +
  guides(colour = FALSE) +
  labs(title = "St. Cuthbert's Hospice", subtitle = "Time Series of amount of Donations", x = "Date", y = "Log Donations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 

grid.arrange(plot1, plot2, nrow=2, ncol=1)


### Digital donations vs physical donations

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



### Digital donations vs physical donations with forecast

## Amount Donations
# dataset_filtered<- dataset_donations %>% 
#   mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
#                                   payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
#   arrange(donation.date) %>%
#   unite(MY, month, year) %>%
#   group_by(MY, digi.or.phys) %>%
#   mutate(SumDon = sum(donation.amount)) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE)

## Sum # Donations
# dataset_filtered<- dataset_donations %>% 
#   mutate(digi.or.phys = case_when(payment.type %in% c(1,2,3,4,5,10,11,12,13,16,18,20) ~ "Phys",
#                                   payment.type %in% c(14,15,17,19) ~ "Digi")) %>%
#   filter(digi.or.phys == "Digi") %>%
#   arrange(donation.date) %>%
#   unite(MY, month, year) %>%
#   group_by(MY, digi.or.phys) %>%
#   mutate(CountDon = length(payment.type)) %>%
#   arrange(donation.date) %>%
#   distinct(MY, .keep_all = TRUE) 
# 
# ## Create filtered xts object
# vectorXTS_filtered<- xts(dataset_filtered$CountDon, order.by = (dataset_filtered$donation.date))
# 
# # vectorXTS_filtered2<- xts(dataset_filtered2$donors.total, order.by = (dataset_filtered2$donation.date))
# 
# # Create xts timeseries to ts
# vectorT<- vectorXTS_filtered %>% 
#   coredata() %>% 
#   as.ts()
# 
# # vectorT2<- vectorXTS_filtered2 %>% 
# #   coredata() %>%
# #   as.ts()
# 
# vectorT <- ts(as.vector(vectorT)) %>% tsclean()
# 
# # vectorT2 <- ts(as.vector(vectorT2)) %>% tsclean()
# 
# # Plot time series
# vectorT %>% ts.plot()
# 
# # vectorT2 %>% ts.plot()
# 
# 
# # Apply Box test to see if time series is White Noise (if p- value less than 0.05 it's not white noise)
# vectorT %>% Box.test()
# 
# # Make the test set
# test_window<- round(length(vectorT)*0.2)
# forecast_window<- 12 #1 year
# train <- subset(vectorT, end = length(vectorT) - test_window) 
# 
# # Fit the desired model to the training data (in this cas an ARIMA and an ETS model)
# lambda<- vectorT %>% BoxCox.lambda()
# classifierT1 <- auto.arima(train, lambda = lambda, stepwise = FALSE, seasonal = FALSE)
# # classifierT2 <- ets(train)
# 
# # Check that both models have white noise residuals
# checkresiduals(classifierT1)
# # checkresiduals(classifierT2)
# 
# ## Use accuracy() to compare the paramethers of auto.arima vs the manual fit
# for_classifierT1 <- forecast(classifierT1, h = forecast_window)
# # for_classifierT11 <- forecast(classifierT11, h = forecast_window)
# accuracy(for_classifierT1, vectorT)
# # accuracy(for_classifierT11, vectorT)
# 
# # Produce forecasts for each model
# ## Train full time series
# classifierFull1 <- auto.arima(vectorT, lambda = lambda, stepwise = FALSE, seasonal = FALSE)
# # classifierFull11 <- vectorT %>% Arima(order = c(0,1,2), lambda = lambda, seasonal = FALSE)
# 
# ## Forecast full time series
# for_classifierFull1 <- forecast(classifierFull1, h = forecast_window)
# # for_classifierFull11 <- forecast(classifierFull11, h = forecast_window)
# 
# ## Plot the train data vs the predicted values, also plot the forecast of the train data comparing it to the real values 
# autoplot(for_classifierFull1) + 
#   autolayer(vectorT) + 
#   autolayer(fitted(for_classifierFull1)) + 
#   # autolayer(vectorT2) + 
#   theme_economist() +
#   labs(title = "St. Cuthbert's Hospice", subtitle = "Number of Regular vs Non Regular donors", x = "Time (Months)", y = "Number of donors", fill = "% of Confidence", colour = "Time Series") +
#   theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 
# 
# autoplot(vectorT) + 
#   autolayer(for_classifierFull1) +
#   theme_economist() +
#   scale_fill_economist() + 
#   labs(title = "St. Cuthbert's Hospice", subtitle = "Evolution of Regular donors", x = "Time (Months)", y = "Donors") +
#   theme(legend.position = "right", plot.title = element_text(size=20), plot.subtitle = element_text(size = 12), text = element_text(family = "Tahoma")) 
# 
# 
# # autoplot(for_classifierFull11) + autolayer(vectorT) + autolayer(fitted(for_classifierFull11))
# 
# # Print the mean of the predicted values to have just one value predicted and not a 80 to 95% confidence range
# for_classifierFull1$mean
# # for_classifierFull11$mean
# 
# 
