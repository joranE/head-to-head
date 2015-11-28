#FIS <- src_sqlite("fis.sqlite3",create = FALSE)
FIS <- src_mysql(options()$mysql$dbName,
                 options()$mysql$host,
                 options()$mysql$port,
                 options()$mysql$user,
                 options()$mysql$password)
DATA <- tbl(FIS,"main")
XC_FAC <- tbl(FIS,"xc_fac") %>% collect()
RACE_MEDIAN <- tbl(FIS,"median_race_time") %>% collect()