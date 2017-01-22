library(DBI)
library(RMySQL)

host <-  'localhost'
user <- 'crm'
pwd <- 'crm2016@root'

con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd)

length(dbListConnections(RMySQL::MySQL()))

