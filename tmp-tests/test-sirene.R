library(bigdfr)

csv <- "../data/sirc-17804_9075_61173_201807_L_M_20180801_015133163.csv"
# X <- FDF_read(csv)

library(bigreadr)
tmp <- fread2(csv, nrows = 6, encoding = "Latin-1")

# dbWriteTable(append = TRUE)
library(dplyr)

library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), "test2.sqlite")
dbCreateTable(con, "my_table", tmp)
dbAppendTable(con, "my_table", tmp)
dbDisconnect(con)

# grab the db connection from the object created by src_sqlite
# and issue the INSERT That way

res <- dbSendQuery(my_db$con,
                   'INSERT INTO my_table VALUES (9.9, 9.9, 9.9, 9.9, "new")')
