library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "fs_db.sqlite")

p_files <- list.files("p_tables", full.names = T)

p_names <- list.files("p_tables")

ps <- list()

for(i in 1:4) {
    ps[[i]] <- readRDS(p_files[i])
}

for(i in 1:4) {
    
    print(dim(ps[[i]]))
}

# p_mamm, p are subsets of p

system.time(readRDS(p_files[4]))
system.time(readRDS(p_files[2]))

p_quick = ps[[4]]
p_tall = ps[[2]]
p_wide = ps[[3]]

# these 3 tables are all you need and are named more usefully.
# I tried converting them to dataframes and storing them as tables; this failed because there were too many columns.

p_quick_serial <- serialize(p_quick, NULL) # could then store this in the db
# but I'm not completely sure how much gain one gets from storing _this_ as sqlite. may be just as well to keep them as .rds files.
