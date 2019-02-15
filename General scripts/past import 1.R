################# Past Import #################


### Import data FIRST

### Donations
## Import main donations dataset
dataset_donations1 <-
  as.tibble(
      read_csv(
        "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/1st JN's 0-70K.csv"
        )
    ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
  mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
  mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
  mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  # mutate_if(is.character, funs(as.factor)) %>%
  # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
  rename(
    donor.postcode = postcode,
    donor.gender = gender
  )

### Import data SECOND

### Donations
## Import main donations dataset
dataset_donations2 <-
  as.tibble(
    read_csv(
      "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/2nd JN's 70001 - 144855.csv"
    )
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
  mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
  mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
  mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  # mutate_if(is.character, funs(as.factor)) %>%
  # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
  rename(
    donor.postcode = postcode,
    donor.gender = gender
  )


### Import data THIRD


### Donations
## Import main donations dataset
dataset_donations3 <-
  as.tibble(
    read_csv(
      "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/3rd JN 144855 - 216595.csv"
    )
  ) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  mutate(donation.day = strftime(.$donation.date, format = "%d")) %>%
  mutate(donation.week = strftime(.$donation.date, format = "%V")) %>%
  mutate(donation.month = strftime(.$donation.date, format = "%m")) %>%
  mutate(donation.year = strftime(.$donation.date, format = "%Y")) %>%
  mutate(source.group = substr(source, 1, 3)) %>%
  mutate_at(vars(date.of.birth, donation.date), funs(as.Date(., format = '%d/%m/%Y'))) %>%
  # mutate_if(is.character, funs(as.factor)) %>%
  # mutate_at(vars(donor.no, payment.type, nominal), funs(as.factor)) %>%
  rename(
    donor.postcode = postcode,
    donor.gender = gender
  )

### Create main dataset
# Join datasets
dataset_donations_global <- bind_rows(dataset_donations1,
                                      dataset_donations2,
                                      dataset_donations3)



# Create file of main donations
write_csv(dataset_donations_global,
          "~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Support Services/Donorflex exports/dataset_main_donations_analysis.csv")
