library(readr)
library(tidyverse)

################# Past Import #################


# ### Import data FIRST

# ### Donations
# ## Import main donations dataset
# dataset_donations1 <-
#   as.tibble(
#       read_csv(
#         "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/1st JN's 0-70K.csv"
#         )
#     ) %>%
#   rename_all(funs(tolower(make.names(.)))) %>%
#   mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
#   mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
#   mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
#   mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
#   mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
#   mutate(source.group = substr(source, 1, 3)) %>%
#   mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
#   # mutate_if(is.character, funs(as.factor)) %>%
#   # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
#   rename(
#     donor.postcode = postcode,
#     donor.gender = gender
#   ) %>%
#   select(
#     -c(
#       date.of.last.donation,
#       transaction.date,
#       date.onto.donorflex,
#       address1,
#       town,
#       date.of.first.donation,
#       total.value.of.donations,
#       number.of.donations
#     )
#   ) 
# 
# 
# ### Import data SECOND
# 
# ### Donations
# ## Import main donations dataset
# dataset_donations2 <-
#   as.tibble(
#     read_csv(
#       "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/2nd JN's 70001 - 144855.csv"
#     )
#   ) %>%
#   rename_all(funs(tolower(make.names(.)))) %>%
#   mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
#   mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
#   mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
#   mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
#   mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
#   mutate(source.group = substr(source, 1, 3)) %>%
#   # mutate_if(is.character, funs(as.factor)) %>%
#   # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
#   rename(
#     donor.postcode = postcode,
#     donor.gender = gender
#   ) %>%
#   select(
#     -c(
#       date.of.last.donation,
#       transaction.date,
#       date.onto.donorflex,
#       address1,
#       town,
#       date.of.first.donation,
#       total.value.of.donations,
#       number.of.donations
#     )
#   )
# 
# 
# ### Import data THIRD
# 
# 
# ### Donations
# ## Import main donations dataset
# dataset_donations3 <-
#   as.tibble(
#     read_csv(
#       "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/3rd JN 144855 - 216595.csv"
#     )
#   ) %>%
#   rename_all(funs(tolower(make.names(.)))) %>%
#   mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
#   mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
#   mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
#   mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
#   mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
#   mutate(source.group = substr(source, 1, 3)) %>%
#   mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
#   # mutate_if(is.character, funs(as.factor)) %>%
#   # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
#   rename(
#     donor.postcode = postcode,
#     donor.gender = gender
#   ) %>%
#   select(
#     -c(
#       date.of.last.donation,
#       transaction.date,
#       date.onto.donorflex,
#       address1,
#       town,
#       date.of.first.donation,
#       total.value.of.donations,
#       number.of.donations
#     )
#   )
# 
# 
# ### Create main dataset
# # Join datasets
# dataset_donations_global <- bind_rows(dataset_donations1,
#                                       dataset_donations2,
#                                       dataset_donations3) 
# 
#   
# 
# # Create file of main donations
# write_csv(dataset_donations_global, 
#           "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations.csv")

################# Import update #################

# Import main donations
dataset_main_donations <-
  read_csv(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations.csv"
    
  ) %>% 
## Format all columns, without factors
mutate_at(vars(date.of.birth, 
               donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate_at(vars(journal.no,
                 donation.amount), funs(as.numeric)) %>% 
  mutate_at(
    vars(
      donor.no,
      source:source.group,
      surname,
      forename,
      donor.postcode
    ),
    funs(as.character)
  )

### Donations
## Import main donations dataset
dataset_donations_update <-
  as.tibble(
    read_csv(
      "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/update.csv"
    )
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>% 
  mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
  mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
  mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
  mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  rename(
    donor.postcode = postcode,
    donor.gender = gender
  ) %>%
  select(
    -c(
      date.of.last.donation,
      transaction.date,
      date.onto.donorflex,
      address1,
      town,
      date.of.first.donation,
      total.value.of.donations,
      number.of.donations
    )
  ) %>% 
  ## Format all columns, without factors
  mutate_at(vars(date.of.birth, 
                 donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate_at(vars(journal.no,
                 donation.amount), funs(as.numeric)) %>% 
  mutate_at(
    vars(
      donor.no,
      source:source.group,
      surname,
      forename,
      donor.postcode
    ),
    funs(as.character)
  )


# Bind main dataset with update

dataset_donations_updated <- bind_rows(dataset_main_donations,
                                      dataset_donations_update) %>% 
  ## Format all columns, without factors
  mutate_at(vars(date.of.birth, 
                 donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate_at(vars(journal.no,
                 donation.amount), funs(as.numeric)) %>% 
  mutate_at(
    vars(
      donor.no,
      source:source.group,
      surname,
      forename,
      donor.postcode
    ),
    funs(as.character)
  )
  

# Create up to date file 
write_csv(dataset_donations_updated, 
          "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations.csv")


