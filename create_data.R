library(statskier2)
library(dplyr)
conl <- db_xc_local()
x <- statskier2::ss_query(conl,"select * from main")
x <- x %>%
  group_by(raceid) %>%
  mutate(penalty = min(fispoints,na.rm = TRUE)) %>%
  group_by(fisid) %>%
  mutate(n_race = n_distinct(raceid)) %>%
  filter(n_race >= 30) %>%
  select(raceid,date,season,cat1,gender,type,tech,fisid,name,age,rank,fispoints,penalty)

nms <- x %>%
  ungroup() %>%
  select(name,gender) %>%
  distinct()

write.table(x = nms,
            file = "names.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)