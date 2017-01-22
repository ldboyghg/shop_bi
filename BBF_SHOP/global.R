#
# global.R
# source by app.R
#

host <-  'localhost'
user <- 'crm'
pwd <- 'crm2016@root'

drv_mysql <- RMySQL::MySQL(max.con=500, fetch.default.rec=1000)

# db:bbf_shop/ecommerce
dbGetQuery0 <- function(db, sql){
    con <- dbConnect(drv_mysql, host=host, user=user, password=pwd, dbname=db)
    df <- dbGetQuery(con, sql)
    dbDisconnect(con)
    return(df)
}

dbWriteTable0 <- function(db, tbl, df) {
    con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd, dbname=db)
    dbWriteTable(con, tbl, df, append = TRUE, row.names = FALSE)
    dbDisconnect(con)
}

dbReadTable0 <- function(db, tbl) {
    con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd, dbname=db)
    df <- dbReadTable(con, tbl)
    dbDisconnect(con)
    return(df)
}
