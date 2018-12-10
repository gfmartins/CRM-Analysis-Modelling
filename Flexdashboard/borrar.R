

dataset_donations_base <-  dataset_donations %>%
  select(
    donor.no,
    journal.no,
    donation.date,
    nominal,
    source.group,
    source,
    donation.day,
    donation.week,
    donation.month,
    donation.year,
    donation.amount
  ) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) 


## Create reactive objects that will depend on date, nominal, source.group and source code

reactive_viz_donations <- dataset_donations_base %>% 
    group_by_at(vars(MY, source)) %>% 
    mutate(
      sum.donations = sum(donation.amount),
      average.donations = mean(donation.amount),
      number.donations = n()
    ) %>%
    ungroup() %>% 
    distinct(MY, source, .keep_all = TRUE) # In shiny gere goes input$Nominal, source...
  



reactive_viz_newdonors <- dataset_donations_base %>%
    group_by_at(vars(source, donor.no)) %>% # In shiny gere goes input$Nominal, source...
    mutate(donations.per.donor = n()) %>%
    ungroup() %>%
    filter(donations.per.donor == 1) %>%
    group_by_at(vars(MY, source)) %>% 
    mutate(sum.new.donors = n()) %>%
    ungroup() %>% 
    select(MY, sum.new.donors, nominal, source.group, source) %>% 
    distinct(MY, source, .keep_all = TRUE)
  



reactive_viz_totaldonors <- dataset_donations_base %>%
    # group_by(donor.no) %>%
    # ungroup() %>%
    group_by_at(vars(MY, source)) %>% # In shiny gere goes input$Nominal, source...
    distinct(donor.no, .keep_all = TRUE) %>%
    mutate(donors.per.month = n()) %>%
    ungroup() %>% 
    select(MY, donors.per.month, nominal, source.group, source) %>% 
    distinct(MY, source, .keep_all = TRUE) 
  



reactive_viz_general <- reactive_viz_donations %>% 
    left_join(reactive_viz_newdonors, by = c("MY", "nominal", "source.group", "source")) %>% 
    left_join(reactive_viz_totaldonors, by = c("MY", "nominal", "source.group", "source")) %>%
    select(nominal,
           source.group,
           source, 
           donation.month, 
           donation.year,
           donation.date, 
           MY,
           sum.donations,
           average.donations,
           number.donations,
           sum.new.donors,
           donors.per.month) %>%
    gather(index, value, -c(nominal:MY)) %>%
    mutate_if(is.character, funs(as.factor)) %>%
    select(Donation.Date = donation.date, Value = value, 
           Donation.Year = donation.year, Donation.Month = donation.month, everything())
  

## Grouped by source
SourceCode <- "COLBOX"
Index <- "number.donations"

p1 <- reactive_viz_general %>% 
filter(index == Index, source == SourceCode, Donation.Year %in% c("2015", "2016", "2017")) %>%   # In Shiny source = input$grouping variable
  ggplot(aes(Donation.Month, Value, group = Donation.Year, fill = Donation.Year)) + # In shiny gere goes input$sum, mean...
  geom_col(position = "dodge")


ggplotly(p1)



################# Global Time-Series ################# 

# Sum # Donations
p1<- dataset_donations %>%
  arrange(donation.date) %>%
  unite(MY, donation.month, donation.year) %>%
  group_by(MY) %>%
  # mutate(CountDon = length(payment.type)) %>%
  # arrange(donation.date) %>%
  mutate(total.donations = sum(donation.amount)) %>% 
  ungroup() %>% 
  distinct(MY, .keep_all = TRUE) %>% 
  select(donation.date, MY, total.donations) %>% 
  ggplot(aes(donation.date, total.donations)) +
  geom_line(colour = "darkorange1", alpha = 0.8)  +
  geom_smooth(method = "loess", se = FALSE, colour = "grey40") + 
  theme_economist_white() +
  scale_fill_economist() + 
  labs(x = "Date", 
       y = "Value") +
  theme(legend.position = "right", text = element_text(family = "Tahoma"), plot.background = element_rect(fill = "white"), plot.margin=unit(c(1,3,2,2.5),"cm"))


ggplotly(p1)

## Create filtered xts object
vectorXTS_filtered<- xts(dataset_filtered$total.donations, order.by = (dataset_filtered$donation.date))

# vectorXTS_filtered2<- xts(dataset_filtered2$donors.total, order.by = (dataset_filtered2$donation.date))

# Create xts timeseries to ts
vectorT<- vectorXTS_filtered %>%
  coredata() %>%
  as.ts()



p1 <- autoplot(vectorT) + 
  theme_economist_white() +
  scale_fill_economist() + 
  labs(x = "Date", 
       y = "Value") +
  theme(legend.position = "right", text = element_text(family = "Tahoma"), plot.background = element_rect(fill = "white"), plot.margin=unit(c(1,3,2,2.5),"cm"))



dataset_donations %>% 
  filter(nominal == "4080") %>% 
  select(nominal, source.group, source) %>% 
  View()


