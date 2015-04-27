FIS <- src_sqlite("fis.sqlite3",create = FALSE)
DATA <- tbl(FIS,"main")