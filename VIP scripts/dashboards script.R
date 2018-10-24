library(tidyverse)
library(readxl)
library(gridExtra)
library(ggthemes)
library(astsa)
library(forecast)
library(xts) 
library(ggmap)
library(aspace)
library(forecast)
library(TTR)
library(data.table)
library(leaflet)
library(plotly)

################# Import data #################

# Donations dataset

### Group codes
## Import table with group codes
table_group_codes <-
  read_excel(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/nominal ledgers and source groups.xlsx"
  ) %>%
  select(-Recnum) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .)))

### Source codes, group codes
## Import table with sources codes and join with group codes
table_source_codes <-
  read_excel(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/all source codes.xlsx",
    skip = 1
  ) %>%
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
table_nominal_descriptions <-
  read_excel(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/nominal descriptions.xlsx"
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  mutate_all(funs(as.factor)) %>%
  left_join(table_source_codes, by = c("nominal.codes" = "nominal.ledger"))

### Income stream codes
## Import Income Stream table
table_income_stream <-
  read_excel(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/income stream.xlsx",
    skip = 1
  ) %>%
  rename_all(funs(tolower(make.names(.))))

### Regular donors details
## Import Regular Givers details
table_RG_details <-
  read_excel(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Regular givers detail.xlsx"
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  select(-c(start.date, terminated.on)) %>%
  mutate_at(vars(frequency), funs(as.factor))

### Donations
## Import main donations dataset
dataset_donations <-
  as.tibble(
    read_excel(
      "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donations_6Years (nopass).xlsx",
      sheet = "Fernando_6year_3.7.18_1"
    )
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  select(-c(address1)) %>%
  ### Source codes, group codes, income stream, regular givers details, main dataset
  ## Join main donation dataset with Income Stream and RG
  inner_join(table_income_stream, by = "journal.no") %>%
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
  select(
    -source.group.x,
    source.group = source.group.y,
    donor.postcode = postcode,
    donor.town = town,
    donor.gender = gender,
    donation.day = day,
    donation.week = week,
    donation.month = month,
    donation.year = year,
    everything()
  )


# IPU, CS dataset

dataset_oacc<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Cinical Services 2015 - 13.06.2018/New Format Datasets/OACC_dataset.xlsx") %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  mutate_at(vars(q2.pain:q9.practical.matters.addressed, quality.of.life.before.admission.to.spct:lost.control), funs(as.numeric)) %>%
  mutate_if(is.character, funs(as.factor)) %>% 
  mutate_at(vars(anonymous.patient.id), funs(as.factor)) %>%
  mutate_at(vars(date.of.contact), funs(as.Date)) %>%
  group_by(date.of.contact) %>%
  mutate(mean.age = round(mean(patient.age),0)) %>%
  select(anonymous.patient.id, spell, date.of.contact) %>%
  ungroup()

### IPU
## Import IPU dataset
dataset_ipu<- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Cinical Services 2015 - 13.06.2018/New Format Datasets/IPU_dataset.xlsx") %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_at(vars(nhs.number), funs(as.factor)) %>%
  mutate_at(vars(date.of.admission..dd.mm.yyyy.), funs(as.Date)) %>% 
  select(nhs.number, spell, date.of.admission..dd.mm.yyyy., postcode)

### IPU, OACC
## Join datasets IPU and OACC
dataset_ipu_oacc<- dataset_oacc %>%
  left_join(dataset_ipu, by = c("anonymous.patient.id" =  "nhs.number", "spell"))


# Geo dataset

## Import UK postcodes table (open data)
table_postcodes <-
  read_csv(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/ukpostcodes.csv"
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(postcode, latitude, longitude)

## Join IPU, CS data with UK postcodes to get the longitud and latitude
table_CS_postcodes <-
  dataset_ipu_oacc %>%
  inner_join(table_postcodes, by = "postcode") 


# Include postcodes of donations and IPU/CS patients into main dataset
dataset_donations_postcode_ipu <- dataset_donations %>%
  left_join(table_postcodes, by = c("donor.postcode" = "postcode")) %>%
  full_join(table_CS_postcodes, by = c("donor.postcode" = "postcode")) %>%
  select(donor.no, 
         anonymous.patient.id, 
         donor.postcode, 
         latitude.x, longitude.x, 
         latitude.y, longitude.y) %>% 
  mutate_if(is.factor, funs(as.character)) %>% 
  replace_na(list(donor.no = "noMatch", anonymous.patient.id = "noMatch")) %>% 
  mutate(owner.of.postcode = case_when(donor.no == "no match" ~ "justPatient",
                                   anonymous.patient.id == "noMatch" ~ "justDonor",
                                   !anonymous.patient.id == "noMatch" ~ "bothDonorAndPatient"),
         latitude = coalesce(latitude.x, latitude.y),
         longitude = coalesce(longitude.x, longitude.y),
         copy.long = longitude,
         copy.lat = latitude) %>% 
  unite(long.lat, copy.long, copy.lat) %>% 
  distinct(long.lat, .keep_all = TRUE)  %>% 
  mutate_at(vars(owner.of.postcode), funs(as.factor)) %>% 
  select(latitude,
         longitude,
         owner.of.postcode)
  

dataset %>%
  group_by(variable) %>%
  mutate(count = sequence(n()))

remove(table_postcodes)



# # Save the coordinates of a city in a vector (search in google )
# ### In this case Durham
# Coord.City <- c(lon = -1.581517, lat = 54.77952)

################# DataViz  #################

## Create data

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

dataset_viz_donations <- dataset_donations_base %>% 
  group_by(MY, source) %>% # In shiny gere goes input$Nominal, source...
  mutate(
    sum.donations = sum(donation.amount),
    average.donations = mean(donation.amount),
    number.donations = n()
  ) %>%
  ungroup() %>% 
  distinct(MY, source, .keep_all = TRUE) # In shiny gere goes input$Nominal, source...

dataset_viz_newdonors <- dataset_donations_base %>%
  # group_by(donor.no) %>%
  # ungroup() %>%
  group_by(source, donor.no) %>% # In shiny gere goes input$Nominal, source...
  mutate(donations.per.donor = n()) %>%
  ungroup() %>%
  # gather(index, value, -c(journal.no:donation.year, MY)) %>%
  filter(donations.per.donor == 1) %>%
  group_by(MY, nominal) %>% # In shiny gere goes input$Nominal, source...
  mutate(sum.new.donors = n()) %>%
  ungroup() %>% 
  select(MY, sum.new.donors, nominal, source.group, source) %>% 
  distinct(MY, source, .keep_all = TRUE)


dataset_viz_totaldonors <- dataset_donations_base %>%
  # group_by(donor.no) %>%
  # ungroup() %>%
  group_by(MY, source.group) %>% # In shiny gere goes input$Nominal, source...
  distinct(donor.no, .keep_all = TRUE) %>%
  mutate(donors.per.month = n()) %>%
  ungroup() %>% 
  select(MY, donors.per.month, nominal, source.group, source) %>% 
  distinct(MY, source, .keep_all = TRUE) 
  
dataset_viz_general <- dataset_viz_donations %>% 
  left_join(dataset_viz_newdonors, by = c("MY", "source")) %>% # In shiny gere goes input$Nominal, source...
  left_join(dataset_viz_totaldonors, by = c("MY", "source")) # In shiny gere goes input$Nominal, source...
  
  
  

### Donations $


## Grouped by source
SourceCode <- "DONRAF"
Index <- "sum.donations"

  # Plot
  dataset_donations_base %>% 
  filter(index == Index, source == SourceCode) %>%   # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)

## Grouped by source.group
SourceGroup <- "COL"
Index <- "sum.donations"

p1 <- dataset_donations %>%
  select(
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
  unite(MY, month.unite, year.unite) %>%
  group_by(MY, source.group) %>% # In shiny gere goes input$Nominal, source...
  mutate(
    sum.donations = sum(donation.amount),
    average.donations = mean(donation.amount),
    number.donations = n()
  ) %>%
  ungroup() %>%
  gather(index, value,-c(journal.no:donation.year, MY)) %>%
  filter(index == Index, source.group == SourceGroup) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)


## Grouped by nominal
Nominal <- "4100"
Index <- "sum.donations"

p1 <- dataset_donations %>%
  select(
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
  unite(MY, month.unite, year.unite) %>%
  group_by(MY, nominal) %>% # In shiny gere goes input$Nominal, source...
  mutate(
    sum.donations = sum(donation.amount),
    average.donations = mean(donation.amount),
    number.donations = n()
  ) %>%
  ungroup() %>%
  gather(index, value,-c(journal.no:donation.year, MY)) %>%
  filter(index == Index, nominal == Nominal) %>% # In Shiny source = input$grouping variable
  ggplot(aes(donation.month, value, group = donation.year, colour = donation.year)) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)


### Donors

# New Donors

## Filter by nominal
Nominal <- "4100"

 %>%
  filter(nominal == Nominal) %>% # In Shiny source = input$grouping variable
  ggplot(aes(
    donation.month,
    sum.new.donors,
    group = donation.year,
    colour = donation.year
  )) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(plot1)


## Filter by source.group
SourceGroup <- "COL"

plot1 <- dataset_donations %>%
  select(
    journal.no,
    donation.date,
    nominal,
    source.group,
    source,
    donation.day,
    donation.week,
    donation.month,
    donation.year,
    donor.no
  ) %>%
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
  ggplot(aes(
    donation.month,
    sum.new.donors,
    group = donation.year,
    colour = donation.year
  )) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(plot1)


## No source grouping

dataset_donations %>%
  select(
    journal.no,
    donation.date,
    nominal,
    source.group,
    source,
    donation.day,
    donation.week,
    donation.month,
    donation.year,
    donor.no
  ) %>%
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
  ggplot(aes(
    donation.month,
    sum.new.donors,
    group = donation.year,
    colour = donation.year
  )) + # In shiny gere goes input$sum, mean...
  geom_line()



# Total  donors

## Grouped by source.group

Nominal <- "4001"

 %>%
  filter(source.group == Nominal) %>% # In Shiny source = input$grouping variable
  ggplot(
    aes(
      donation.month,
      donors.per.month,
      group = donation.year,
      colour = donation.year
    )
  ) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)

## Grouped by source.group

SourceGroup <- "DOG"

p1 <-  %>%
  filter(source.group == SourceGroup) %>% # In Shiny source = input$grouping variable
  ggplot(
    aes(
      donation.month,
      donors.per.month,
      group = donation.year,
      colour = donation.year
    )
  ) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)

## Grouped by source.group

Source <- "COLBOX"

p1 <- dataset_donations %>%
  select(
    journal.no,
    donation.date,
    nominal,
    source.group,
    source,
    donation.day,
    donation.week,
    donation.month,
    donation.year,
    donor.no
  ) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>%
  # ungroup() %>%
  group_by(MY, source) %>% # In shiny gere goes input$Nominal, source...
  distinct(donor.no, .keep_all = TRUE) %>%
  mutate(donors.per.month = n()) %>%
  ungroup() %>%
  filter(source == Source) %>% # In Shiny source = input$grouping variable
  ggplot(
    aes(
      donation.month,
      donors.per.month,
      group = donation.year,
      colour = donation.year
    )
  ) + # In shiny gere goes input$sum, mean...
  geom_line()

ggplotly(p1)


################# Geo Data ################# 
# Non events

pal <- colorFactor(
  palette = c('red', 'green'),
  domain = dataset_donations_postcode_ipu$owner.of.postcode)


leaflet(dataset_donations_postcode_ipu) %>%
  addTiles() %>%  
  setView(-1.581517, 54.77952, zoom = 9) %>%
  addCircles(
    lng = dataset_donations_postcode_ipu$longitude,
    lat = dataset_donations_postcode_ipu$latitude, 
    label = dataset_donations_postcode_ipu$owner.of.postcode,
    color = ~ pal(owner.of.postcode))




################# Try this for ML #################

## Shows history of donations of clients and for what was the donation
dataset_donations %>%
  group_by(donor.no) %>%
  mutate(N = n()) %>%
  arrange(donor.no) %>%
  select(donor.no, N, donation.date, source, in.use)


## Shows how many donations has made a donor per month and source
dataset_donations %>%
  select(
    journal.no,
    donation.date,
    nominal,
    source.group,
    source,
    donation.day,
    donation.week,
    donation.month,
    donation.year,
    donor.no
  ) %>%
  mutate(year.unite = strftime(.$donation.date, format = "%Y")) %>%
  mutate(month.unite = strftime(.$donation.date, format = "%m")) %>%
  unite(MY, month.unite, year.unite) %>%
  # group_by(donor.no) %>%
  # ungroup() %>%
  group_by(MY, source, donor.no) %>% # In shiny gere goes input$Nominal, source...
  mutate(donations.per.donor = n()) %>%
  ungroup()
