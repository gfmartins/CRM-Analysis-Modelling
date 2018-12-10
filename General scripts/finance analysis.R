### Import data 

dataset_finance <- read_excel("~/Google Drive/Data Analysis/Bases de Datos/St. Cuthberts/Finance/Sage ledger extract 010413 to date for Fernando.xlsx", skip = 1) %>%
  rename_all(funs(tolower(make.names(.)))) %>%
  rename_all(funs(gsub("_", ".", .))) %>%
  select(type = transactionitemised.type, 
         accountreference = transactionitemised.accountreference,  
         nominalaccountreference = transactionitemised.nominalaccountreference,
         departmentnumber = transactionitemised.departmentnumber,
         date = transactionitemised.date,
         reference = transactionitemised.reference,
         amountnet = transactionitemised.amountnet,
         vatcode = transactionitemised.vatcode,
         amountvat = transactionitemised.amountvat,
         -transactionitemised.details) %>%
  mutate_at(vars(transactionitemised.type:transactionitemised.departmentnumber, transactionitemised.reference, transactionitemised.vatcode), funs(as.factor))

table(dataset_finance$transactionitemised.nominalaccountreference)
table(dataset_finance$transactionitemised.departmentnumber)


colnames(dataset_finance)
