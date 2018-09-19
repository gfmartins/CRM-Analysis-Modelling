library(ggmap)

# Import poscodes table
table_postcodes<- read_csv("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/ukpostcodes.csv") %>% 
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(postcode, latitude, longitude)

# Include postcodes into main dataset
dataset_donations<- dataset_donations %>%
  left_join(table_postcodes, by = "postcode")

# Save the coordinates of a city in a vector (search in google )
Coord.City <- c(lon = -1.323166, lat = 54.765442)

## coordinates, zoom = how zoomed will the map be showed, scale = resolution of the map (1 worst ), map type = tipo de mapa (?getmap para ver todos los tipos), source = de donde se lo descarga (si no se especifica es “google”
map <- get_map(Coord.City, zoom = 10, scale = 1)


####################################### Sum donations ##########

## By postcode, year, events
dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  group_by(postcode, year) %>%
  mutate(sum.donations = sum(donation.amount)) %>%
  filter(development.income == "Events")

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.1) +
  facet_wrap(~ year)



### By region, year, all

map <- get_map(Coord.City, zoom = 8, scale = 1)


dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  group_by(region, year) %>% 
  mutate(sum.donations = sum(donation.amount)) 

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) +
  facet_wrap(~ year)


### By region, month, all

map <- get_map(Coord.City, zoom = 8, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, month, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(sum.donations = sum(donation.amount)) 

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) 


### By region, year, events
map <- get_map(Coord.City, zoom = 10, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  group_by(region, year) %>% 
  mutate(sum.donations = sum(donation.amount)) %>%
  filter(development.income == "Events", sum.donations > 100) 
  


ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) +
  facet_wrap(~ year)

### By region, month, events

map <- get_map(Coord.City, zoom = 10, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, month, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(sum.donations = sum(donation.amount)) %>%
  filter(development.income == "Events", sum.donations > 100)


ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.08) 


####################################### Mean donations ##########
## By postcode, year, events
dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  group_by(postcode, year) %>%
  mutate(mean.donations = log(mean(donation.amount))) %>%
  filter(development.income == "Events")

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = mean.donations), alpha = 0.1) +
  facet_wrap(~ year)



### By region, year, all
### Investigate repeating clear colour in the southern part of the bulk of donations
map <- get_map(Coord.City, zoom = 8, scale = 1)


dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  group_by(region, year) %>% 
  mutate(mean.donations = log(mean(donation.amount))) 
  
ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = mean.donations), alpha = 0.08) +
  facet_wrap(~ year)


### By region, month, all

map <- get_map(Coord.City, zoom = 9, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, month, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(mean.donations = log(mean(donation.amount))) 

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = mean.donations), alpha = 0.08) 


### By region, year, events
map <- get_map(Coord.City, zoom = 9, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  group_by(region, year) %>% 
  mutate(mean.donations = log(mean(donation.amount))) %>%
  filter(development.income == "Events") 



ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = mean.donations), alpha = 0.08) +
  facet_wrap(~ year)

### By region, month, events

map <- get_map(Coord.City, zoom = 9, scale = 1)

dataset_maps<- dataset_donations %>%
  select(postcode, year, month, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  unite(MY, month, year) %>%
  group_by(region, MY) %>% 
  mutate(mean.donations = log(mean(donation.amount))) %>%
  filter(development.income == "Events")


ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = mean.donations), alpha = 0.08)



  