# library(statskier2)
# library(dplyr)
# conl <- db_xc_local()
# x <- statskier2::ss_query(con,"select * from main")
# 
# x$cat2[x$cat2 == ""] <- NA
# 
# maj_ind <- x$cat1 %in% c('WC','WSC','OWG','TDS')
# x <- split(x,maj_ind)
# 
# XC_FAC <- load_xc_conv()
# x[[2]] <- x[[2]] %>%
#  mpb() %>%
#  standardize_mpb()
# x[[1]]$mpb <- NA
# 
# x <- do.call("rbind",x)
# x <- dplyr::arrange(x,id)
# 
# x1 <- x %>%
#   group_by(fisid) %>%
#   summarise(N = n_distinct(raceid)) %>%
#   filter(N >= 30)
# x <- dplyr::filter(x,fisid %in% x1$fisid)
# 
# # write.table(x = x,
# #             file = "fis.csv",
# #             sep = ",",
# #             row.names = FALSE,
# #             col.names = TRUE)
# 
# fis_db <- src_sqlite("fis.sqlite3",create = TRUE)
# fis_main <- copy_to(fis_db,
#                     x,
#                     name = "main",
#                     temporary = FALSE,
#                     indexes = list("raceid","fisid","cat1","type"))
# 
# unique_names <- unique(x[,c("name","gender")])
# write.table(x = unique_names,
#             file = "names.csv",
#             sep = ",",
#              row.names = FALSE,
#              col.names = TRUE)
