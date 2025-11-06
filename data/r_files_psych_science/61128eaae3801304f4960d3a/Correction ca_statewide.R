

dat <- data.table::fread("ca_statewide_2020_04_01 original.csv")

# clear spelling errors in county names
dat[dat$county_name == "Santa Barbera County",]$county_name <-  "Santa Barbara County"
dat[dat$county_name == "Frenso County",]$county_name <-  "Fresno County"
dat[dat$county_name == "Riverside Coujnty",]$county_name <-  "Riverside County"
dat[dat$county_name == "Siskyou County",]$county_name <-  "Siskiyou County"

write.csv(dat,"ca_statewide_2019_02_25 updated.csv")

