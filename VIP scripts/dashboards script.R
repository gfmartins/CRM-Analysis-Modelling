
################# Import data ################# 

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
  select(-c(address1)) %>%
  ### Source codes, group codes, income stream, regular givers details, main dataset
  ## Join main donation dataset with Income Stream and RG
  inner_join(table_income_stream, by ="journal.no") %>%
  left_join(table_RG_details, by = c("donor.no", c("donation.amount" = "instalment.amount"))) %>%
  ### Source codes, group codes, nominal descriptions, income stream, regular givers details, main dataset
  ## Join main donations dataset with Income stream and RG with source codes, group codes, nominal descriptions 
  mutate(day = strftime(.$donation.date, format = "%d")) %>%
  mutate(week = strftime(.$donation.date, format = "%V")) %>%
  mutate(month = strftime(.$donation.date, format = "%m")) %>%
  mutate(year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  left_join(table_nominal_descriptions, by = "source") %>%
  distinct(journal.no, .keep_all = TRUE) %>%
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_at(vars(donor.no), funs(as.factor)) %>%
  select(-source.group.x,
         source.group = source.group.y,
         donor.postcode = postcode, 
         donor.town = town, 
         donor.gender = gender, 
         donation.day = day,
         donation.week = week,
         donation.month = month, 
         donation.year = year,
         everything()) 


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
  select(source, event.postcode = postcode, event.latitude = latitude, event.longitude = longitude)


# Import retail Postcode and join it with UK postcodes to get the longitud and latitude
table_retail_postcodes<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Retail Postcodes.xlsx",
                                    skip = 1) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  inner_join(table_postcodes, by = "postcode") %>%
  select(type, 
         retail.postcode = postcode, 
         date.opened, reatail.latitude, 
         reatail.longitude)


# Include postcodes of donations and events into main dataset
dataset_donations<- dataset_donations %>%
  left_join(table_postcodes, by = "postcode") %>%
  left_join(table_events_postcodes, by = "source")

# # Save the coordinates of a city in a vector (search in google )
# ### In this case Durham 
Coord.City <- c(lon = -1.581517, lat = 54.77952)

################# DataViz  ################# 


### Donations $


## Grouped by source
SourceCode<- "COLBOX"
Index<- "sum.donations"

p1<- dataset_donations %>%
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donation.amount) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  group_by(MY, source) %>% # In shiny gere goes input$Nominal, source...
  mutate(sum.donations = sum(donation.amount), 
         average.donations = mean(donation.amount),
         number.donations = n()) %>%
  ungroup() %>%
  gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(index == Index, source == SourceCode) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()
  
ggplotly(p1)

## Grouped by source.group
SourceGroup<- "COL"
Index<- "sum.donations"

p1<- dataset_donations %>%
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donation.amount) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  group_by(MY, source.group) %>% # In shiny gere goes input$Nominal, source...
  mutate(sum.donations = sum(donation.amount), 
         average.donations = mean(donation.amount),
         number.donations = n()) %>%
  ungroup() %>%
  gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(index == Index, source.group == SourceGroup) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)


## Grouped by nominal
Nominal<- "4100"
Index<- "sum.donations"

p1<- dataset_donations %>%
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donation.amount) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  group_by(MY, nominal) %>% # In shiny gere goes input$Nominal, source...
  mutate(sum.donations = sum(donation.amount), 
         average.donations = mean(donation.amount),
         number.donations = n()) %>%
  ungroup() %>%
  gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(index == Index, nominal == Nominal) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)


### Donors

# New Donors

## Filter by source.group
SourceGroup<- "DOG"

plot1 <- dataset_donations %>% 
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donor.no) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>% 
  # ungroup() %>% 
  group_by(source.group, donor.no) %>% # In shiny gere goes input$Nominal, source...
  mutate(donations.per.donor = n()) %>%
  ungroup() %>%
  # gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(donations.per.donor == 1) %>% 
  group_by(MY, source.group) %>%
  mutate(sum.new.donors = n()) %>%
  ungroup() %>%
  filter(source.group == SourceGroup) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, sum.new.donors, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()
 
ggplotly(plot1)


## No source grouping

dataset_donations %>% 
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donor.no) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>% 
  # ungroup() %>% 
  group_by(donor.no) %>% # In shiny gere goes input$Nominal, source...
  mutate(donations.per.donor = n()) %>%
  ungroup() %>%
  # gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(donations.per.donor == 1) %>% 
  group_by(MY) %>%
  mutate(sum.new.donors = n()) %>%
  ungroup() %>%
  ggplot(aes(donation.month, sum.new.donors, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()



# Total  donors


## Grouped by source.group
dataset_donations %>% 
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donor.no) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>% 
  # ungroup() %>% 
  group_by(MY, source.group) %>% # In shiny gere goes input$Nominal, source...
  distinct(donor.no, .keep_all = TRUE) %>% 
  mutate(donations.per.month = n()) %>%
  ungroup() %>%
  # filter(source.group == SourceCode) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, donations.per.month, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()


## No source grouping

dataset_donations %>% 
  select(journal.no, 
         donation.date, 
         nominal, 
         source.group,
         source,
         donation.day,
         donation.week,
         donation.month,
         donation.year,
         donor.no) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>% 
  # ungroup() %>% 
  group_by(MY) %>% # In shiny gere goes input$Nominal, source...
  distinct(donor.no, .keep_all = TRUE) %>% 
  mutate(donations.per.month = n()) %>%
  ungroup() %>%
  ggplot(aes(donation.month, donations.per.month, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()


################# Try this for ML ################# 

## Shows history of donations of clients and for what was the donation
dataset_donations %>% 
  group_by(donor.no) %>% 
  mutate(N = n()) %>%
  arrange(donor.no) %>% 
  select(donor.no, N, donation.date, source, in.use) 


## Shows how many donations has made a donor per month and source
dataset_donations %>% 
select(journal.no, 
       donation.date, 
       nominal, 
       source.group,
       source,
       donation.day,
       donation.week,
       donation.month,
       donation.year,
       donor.no) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>% 
  # ungroup() %>% 
  group_by(MY, source, donor.no) %>% # In shiny gere goes input$Nominal, source...
  mutate(donations.per.donor = n()) %>%
  ungroup()
  