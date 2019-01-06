library(readr)
library(tidyverse)
library(readxl)

################# Past Import #################

# 
# ### Import data FIRST
# 
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
#   )
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
#   ) 
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
#           "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations_analysis.csv")

################# Import update #################

# Import main donations
dataset_main_donations <-
  read_csv(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations_analysis.csv"
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
          "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations_analysis.csv")



################# General dataset #################

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
    read_csv(
      "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations_analysis.csv"
    )
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  select(-c(address1)) %>%
  ### Source codes, group codes, income stream, regular givers details, main dataset
  ## Join main donation dataset with Income Stream and RG
  left_join(table_income_stream, by = "journal.no") %>%
  left_join(table_RG_details, by = c("donor.no", c("donation.amount" = "instalment.amount"))) %>%
  ### Source codes, group codes, nominal descriptions, income stream, regular givers details, main dataset
  ## Join main donations dataset with Income stream and RG with source codes, group codes, nominal descriptions
  # mutate(day = strftime(.$donation.date, format = "%d")) %>%
  # mutate(week = strftime(.$donation.date, format = "%V")) %>%
  # mutate(month = strftime(.$donation.date, format = "%m")) %>%
  # mutate(year = strftime(.$donation.date, format = "%Y")) %>%
  # mutate(source.group = substr(source, 1, 3)) %>%
  left_join(table_nominal_descriptions, by = "source") %>%
  distinct(journal.no, .keep_all = TRUE) %>%
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
  rename(
    source.group = source.group.y,
    donor.town = town
  ) 

######## UPDATE WITH 
 # IPU, CS dataset
 
# IPU data

# Import main referrals 
dataset_referrals <- read_csv(
  "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Cinical Services 2015 - 13.06.2018/SystemOne and Excel final/Referral_Report_Final_Complete.csv"
) %>%
  ## Delete this step when I have the ages on the dataset
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  separate(age, c("patient.age", "."), sep = "yrs") %>%
  select(-.) %>% 
  rename(service.offered.before.standarize = service.offered) %>%
  filter(service.offered.before.standarize != "Marie Curie Rapid Response") %>%
  mutate(
    service.offered = case_when(
      service.offered.before.standarize %in% c(
        "In Patient",
        "Inpatient - Emergency Respite",
        "Inpatient - Psyco-social Support",
        "Inpatient - Respite",
        "Inpatient - Symptom Control",
        "Lymphoedema",
        "Advice/consultation",
        "Day Hospice"
      ) ~ "IPU",
      service.offered.before.standarize %in% c(
        "12 Week Program",
        "Living Well Centre",
        "Respiratory Group",
        "Heart Failure Group",
        "Palliative Medicine",
        "Living Well Centre"
      ) ~ "LWC"
    )
  ) %>%
  mutate_at(vars(discharge.date, referral.date), funs(as.Date)) %>%
  mutate_at(
    vars(
      referral.id,
      referral.in.intervention.type,
      service.offered.before.standarize,
      nhs.number
    ),
    funs(as.character)
  ) %>%
  mutate_at(vars(patient.age), funs(as.numeric)) %>%
  arrange(referral.date) %>%
  group_by(nhs.number) %>%
  mutate(spell = sequence(n())) %>%
  ungroup() 



# Geo dataset

## Import UK postcodes table (open data)
table_postcodes <-
  read_csv(
    "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/ukpostcodes.csv"
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(
    postcode, 
    latitude, 
    longitude
  )

## Join IPU, CS data with UK postcodes to get the longitud and latitude
table_CS_postcodes <-
  dataset_referrals %>%
  inner_join(table_postcodes, by = "postcode")


# Include postcodes of donations and IPU/CS patients into main dataset
dataset_donations_postcode_ipu <- dataset_main_donations %>%
  left_join(table_postcodes, by = c("donor.postcode" = "postcode")) %>%
  full_join(table_CS_postcodes, by = c("donor.postcode" = "postcode")) %>%
  select(
    donor.no,
    nhs.number,
    donor.postcode,
    latitude.x,
    longitude.x,
    latitude.y,
    longitude.y
  ) %>%
  mutate_if(is.factor, funs(as.character)) %>%
  replace_na(list(donor.no = "noMatch", nhs.number = "noMatch")) %>%
  mutate(
    owner.of.postcode = case_when(
      donor.no == "no match" ~ "justPatient",
      nhs.number == "noMatch" ~ "justDonor",
      !nhs.number == "noMatch" ~ "bothDonorAndPatient"
    ),
    latitude = coalesce(latitude.x, latitude.y),
    longitude = coalesce(longitude.x, longitude.y),
    copy.long = longitude,
    copy.lat = latitude
  ) %>%
  unite(long.lat, copy.long, copy.lat) %>%
  distinct(long.lat, .keep_all = TRUE)  %>%
  mutate_at(vars(owner.of.postcode), funs(as.factor)) %>%
  select(
    latitude,
    longitude,
    owner.of.postcode
  )

## Remove table containing UK postcodes to free up space
remove(table_postcodes)

