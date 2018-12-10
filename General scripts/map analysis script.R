library(ggmap)

install.packages("devtools")

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

  
# Include postcodes of donations and events into main dataset
dataset_donations<- dataset_donations %>%
  left_join(table_postcodes, by = "postcode") %>%
  left_join(table_events_postcodes, by = "source")


# Save the coordinates of a city in a vector (search in google )
### In this case Durham 
Coord.City <- c(lon = -1.581517, lat = 54.77952)

## coordinates, zoom = how zoomed will the map be showed, scale = resolution of the map (1 worst ), map type = tipo de mapa (?getmap para ver todos los tipos), source = de donde se lo descarga (si no se especifica es “google”
map <- get_map(Coord.City, zoom = 10, scale = 1)


####################################### Sum donations ##########

## By postcode, year, events
dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  group_by(postcode, year) %>%
  mutate(sum.donations = log(sum(donation.amount))) %>%
  filter(development.income == "Events")

ggmap(map, base_layer = ggplot(dataset_maps, aes(longitude, latitude))) +
  geom_point(aes(color = sum.donations), alpha = 0.1) +
  facet_wrap(~ year)



### By region, year, all

# map <- get_map(Coord.City, zoom = 8, scale = 1)


dataset_maps<- dataset_donations %>%
  select(postcode, year, donation.amount, development.income, latitude, longitude) %>%
  na.omit() %>%
  separate(postcode, c("region", "area")) %>%
  group_by(region, year) %>% 
  mutate(sum.donations = log(sum(donation.amount))) %>%
  filter(development.income == "Events")


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


######################## SDD ######################## 

# Analysis of dispersion of payments by year

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

####################  
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
####################  
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
event<- dataset_donations %>% 
  filter(development.income == "Events") %>%
  select(longitude, latitude) %>%
  na.omit()

not_event<- dataset_donations %>% 
  filter(!development.income == "Events") %>%
  select(longitude, latitude) %>%
  na.omit()

####################  
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

####################  

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



