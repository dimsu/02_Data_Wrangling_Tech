# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----

# 1.0 Load librairies ----

# Contains readr
library(tidyverse)

# Excel Connection 
library(readxl)
library(writexl)

#Database Connection 
library(odbc)
library(RSQLite)


# 2.0 readr (readr :: read_csv, read_rds) ----


# 2.1 CSV ----
# col_types used to fix the irregularities that we can find in the csv file
bike_orders_csv_tbl <- readr::read_csv("../00_data/bike_sales/data_wrangled/bike_orderlines.csv", 
    col_types = cols(
        order_id = col_double()
    )) 
 #read::problems --> to have a look on the csv data and see possible anomalie
readr::problems(bike_orders_csv_tbl)


# 2.2 RDS ----

bike_orders_rds_tbl <- readr::read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds"
                                       )
# 3.0 Excel ---- (readxl::read_excel)

bike_orders_xlsx_tbl <- readxl::read_excel("../00_data/bike_sales/data_wrangled/bike_orderlines.xlsx"
                                           , sheet = "Sheet1")

# 4.0 Databases ----

#connect to a database ----
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "../00_data/chinook/Chinook_Sqlite.sqlite")

#List the table contains in the database ----
dbListTables(con)

#display a table contain in the database and register it on local computer ----
#   Function tbl() = to display 
#   Function collect () =  pull the table on the computer to save it 
#   DIsconnect the database at the end = function dbDisconnect()
album_tbl <- tbl(con, "Album") %>% collect()
artist_tbl <- tbl(con, "Artist") %>% collect()
dbDisconnect(con)



