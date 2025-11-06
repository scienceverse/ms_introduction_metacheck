
library(data.table)
library(tidyverse)

wy <- fread("wy_statewide_2020_04_01.csv")

wy[wy$county_name == "Bighorn County"|wy$county_name == "Big Horn Co County" |
        wy$county_name == "Big Horn  County"|wy$county_name == "Bighorn  County" ,]$county_name <- "Big Horn County"
wy[wy$county_name == "Sublett County" | wy$county_name == "Subblett County"|
        wy$county_name == "Sublette Co County",]$county_name <- "Sublette County"
wy[wy$county_name == "Natarona County"| wy$county_name == "Natronia County"| 
        wy$county_name == "Natro County"| wy$county_name == "Natrona Coutny County" |
        wy$county_name == "Natrona Coutny County" | wy$county_name == "Natron County"| 
        wy$county_name == "Natona County"| wy$county_name == "Natrona Co County"|
        wy$county_name == "Natrona Country County",]$county_name <- "Natrona County"
wy[wy$county_name == "Sheridan Coutny County"| wy$county_name == "Sheridan Co County"|
        wy$county_name == "Sherdian County"| wy$county_name == "Shieridan County",]$county_name <- "Sheridan County"
wy[wy$county_name == "Wasahakie County",]$county_name <- "Washakie County"
wy[wy$county_name == "Unita County"|wy$county_name == "Uint Acounty"|
        wy$county_name == "Uinta Co County"| wy$county_name == "Uintal County",]$county_name <- "Uinta County"
wy[wy$county_name == "Carbon Coutny County",]$county_name <- "Carbon County"
wy[wy$county_name == "Platt County",]$county_name <- "Platte County"
wy[wy$county_name == "Alabany Counry County" | wy$county_name == "Ablany County"|
        wy$county_name == "Albany Count County"|wy$county_name == "Albany Co County",]$county_name <- "Albany County"
wy[wy$county_name == "Natrona Coutny County",]$county_name <- "Natrona County"
wy[wy$county_name == "Gremont County"| wy$county_name == "Fremiont County",]$county_name <- "Fremont County"
wy[wy$county_name == "Niobrora County",]$county_name <- "Niobrara County"
wy[wy$county_name == "Goshen Counry County",]$county_name <- "Goshen County"
wy[wy$county_name == "Carban County" | wy$county_name == "Carbn County"|
        wy$county_name == "Carbon Country County" | wy$county_name == "Carbon Couty County"|
        wy$county_name == "Carboncounty",]$county_name <- "Carbon County"
wy[wy$county_name == "Teton  County",]$county_name <- "Teton County"
wy[wy$county_name == "Converse Coutny County"|wy$county_name == "Coverse County"|
        wy$county_name == "Converse Country County"|wy$county_name == "Converse Coutry County"|
        wy$county_name == "Converse Ounty County",]$county_name <- "Converse County"
wy[wy$county_name == "Park Coundy County"|wy$county_name == "Park Cunty County"|
        wy$county_name == "Parkcounty",]$county_name <- "Park County"
wy[wy$county_name == "Capmbell County"|wy$county_name == "Campbell Co County"|
        wy$county_name == "Cambell County"|wy$county_name == "Campbell Co So County"|
        wy$county_name == "Campbell Coundy County"|wy$county_name == "Campbelle County"|
        wy$county_name == "Campell County",]$county_name <- "Campbell County"
wy[wy$county_name == "Lincon County"|wy$county_name == "Linciln County"|
        wy$county_name == "Lincoln  County",]$county_name <- "Lincoln County"
wy[wy$county_name == "Larami County"|wy$county_name == "Laramie  Co County"|
        wy$county_name == "Laram County"|wy$county_name == "Laramia County"|
        wy$county_name == "Laramie Co County"| wy$county_name == "Laramie Co County"|
        wy$county_name == "Laramie Wy County",]$county_name <- "Laramie County"
wy[wy$county_name == "Johnson Co County"|wy$county_name == "Johnson  County",]$county_name <- "Johnson County"
wy[wy$county_name == "Crook Co County",]$county_name <- "Crook County"
wy[wy$county_name == "Cheyenn County",]$county_name <- "Cheyenne County"
wy[wy$county_name == "Goshen Co County",]$county_name <- "Goshen County"
wy[wy$county_name == "Hot  Springs County",]$county_name <- "Hot Springs County"
wy[wy$county_name == "Rock Spirngs County"| wy$county_name == "Rock Springs, County",]$county_name <- "Rock Springs County"
wy[wy$county_name == "Sweetwater Co County"|wy$county_name == "Sweetwater Couinty County"|
        wy$county_name == "Sweetwater Country County"|wy$county_name == "Sweewater County",]$county_name <- "Sweetwater County"


wy$county_name[wy$county_name == "Afton County"] <- "Lincoln" # rename city into county name
wy$county_name[wy$county_name == "Bar Nunn County"] <- "Natrona"# rename city into county name
wy$county_name[wy$county_name == "Basin County"] <- "Big Horn"# rename city into county name
wy$county_name[wy$county_name == "Buffalo County"] <- "Johnson"# rename city into county name
wy$county_name[wy$county_name == "Burlington County"] <- "Big Horn"# rename city into county name
wy$county_name[wy$county_name == "Casper County"] <- "Natrona"# rename city into county name
wy$county_name[wy$county_name == "Cheyenne County"] <- "Laramie"# rename city into county name
wy$county_name[wy$county_name == "Clearmont County"] <- "Sheridan"# rename city into county name
wy$county_name[wy$county_name == "Cody County"] <- "Park"# rename city into county name
wy$county_name[wy$county_name == "Douglas County"] <- "Converse"# rename city into county name
wy$county_name[wy$county_name == "Dubois County"] <- "Fremont"# rename city into county name
wy$county_name[wy$county_name == "Etna County"] <- "Lincoln"# rename city into county name
wy$county_name[wy$county_name == "Evanston County"] <- "Uinta"# rename city into county name
wy$county_name[wy$county_name == "Evansville County"] <- "Natrona"# rename city into county name
wy$county_name[wy$county_name == "Gillette County"] <- "Campbell"# rename city into county name
wy$county_name[wy$county_name == "Green River County"] <- "Sweetwater"# rename city into county name
wy$county_name[wy$county_name == "Guernsey County"] <- "Platte"# rename city into county name
wy$county_name[wy$county_name == "Jackson County"] <- "Teton"# rename city into county name
wy$county_name[wy$county_name == "Kinnear County"] <- "Fremont"# rename city into county name
wy$county_name[wy$county_name == "Lander County"] <- "Fremont"# rename city into county name
wy$county_name[wy$county_name == "Lovell County"] <- "Big Horn"# rename city into county name
wy$county_name[wy$county_name == "Marbleton County"] <- "Sublette"# rename city into county name
wy$county_name[wy$county_name == "Moorcroft County"] <- "Crook"# rename city into county name
wy$county_name[wy$county_name == "Newcastle County"] <- "Weston"# rename city into county name
wy$county_name[wy$county_name == "Pinedale County"] <- "Sublette"# rename city into county name
wy$county_name[wy$county_name == "Powell County"] <- "Park"# rename city into county name
wy$county_name[wy$county_name == "Rawlins County"] <- "Carbon"# rename city into county name
wy$county_name[wy$county_name == "Riverton County"] <- "Fremont"# rename city into county name
wy$county_name[wy$county_name == "Rock Springs County"] <- "Sweetwater"# rename city into county name
wy$county_name[wy$county_name == "Sundance County"] <- "Crook"# rename city into county name
wy$county_name[wy$county_name == "Thermopolis County"] <- "Hot Springs"# rename city into county name
wy$county_name[wy$county_name == "Torrington County"] <- "Goshen"# rename city into county name
wy$county_name[wy$county_name == "Wheatland County"] <- "Platte"# rename city into county name
wy$county_name[wy$county_name == "Worland County"] <- "Washakie"# rename city into county name


write.csv(wy, "wy_statewide_corrected.csv", row.names = FALSE)

