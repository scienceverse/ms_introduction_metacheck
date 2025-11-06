
###Install packages if not already installed.
#install.packages("sp")
#install.packages("rgdal")
#install.packages("geosphere")
#install.packages("dismo")
#install.packages("rgeos")
#install.packages("blockTools")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("foreign")
#install.packages(maptools)
#install.packages(broom)
#install.packages(ggplot2)
#install.packages(ggmap)
#install.packages(RColorBrewer)
#install.packages(haven)
#install.packages(stringr)
#install.packages(ggiraph)

library(sp)
library(rgdal)
library(geosphere)
library(dismo)
library(rgeos)
library(blockTools)
library(dplyr)
library(reshape2)
library(foreign)
library(maptools)
library(broom)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(haven)
library(stringr)
library(ggiraph)

####################              Task #1              ####################
#     -We have 132 villages.
#     -Group the villages into clusters based on their physical location.
###########################################################################

###Tell R where we are working (our working directory):
setwd("P:/7554/Restricted/7554.062 Burkina Faso IE (West Africa IE)/Analysis/_dta/Baseline Data/Cleaned data from CERFODES")

##Load the data:
VillageData <- read.csv(file=paste(getwd(),"Base de données infrastructures apurée finale.csv",sep="/"))

##Sort by NUMQ (VillageID)
VillageData <- arrange(VillageData,NUMQ)


### convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(VillageData$long,VillageData$lat), ncol=2),
  data.frame(ID=seq(1:length(VillageData$long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)
VillageData <- cbind(VillageData,mdist)
# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in meters
d=2000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
# essentially what this does is cluster villages together, requiring that the diameter
# of the cluster be no larger than 5000 meters (5km)
xy$clust <- cutree(hc, h=d)
VillageData$Cluster <- xy$clust


###This part is not necessary, but will help us visualize what we just did:

# expand the extent of plotting frame
xy@bbox[] <- as.matrix(extend(extent(xy),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(xy, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(141)[factor(xy$clust)], add=T)

# See tabulation of clusters
table(VillageData$Cluster)



##########################################################################
#     Before Task #2 we need to organize our variables to match on       #
##########################################################################

###These are the variables we will use for matching. Some data wrangling is necessary:
VillageData$Electric_Grid[VillageData$Q1=="Non"] <- 0
VillageData$Electric_Grid[VillageData$Q1=="Oui"] <- 1
VillageData$Water_Supply[VillageData$Q3=="Non"] <- 0
VillageData$Water_Supply[VillageData$Q3=="Oui"] <- 1
VillageData$Dist_PrimarySchool <- VillageData$Q7
VillageData$Dist_PrimarySchool[VillageData$Q6 == "Oui"] <- 0
VillageData$Dist_SecondarySchool <- VillageData$Q9
VillageData$Dist_SecondarySchool[VillageData$Q8 == "Oui"] <- 0
VillageData$LitCenter[VillageData$Q10=="Non"] <- 0
VillageData$LitCenter[VillageData$Q10=="Oui"] <- 1
VillageData$Dist_Police <- VillageData$Q12
VillageData$Dist_Police[VillageData$Q11 == "Oui"] <- 0
VillageData$Dist_Health <- VillageData$Q14
VillageData$Dist_Health[VillageData$Q13 == "Oui"] <- 0
VillageData$Dist_Market <- VillageData$Q16
VillageData$Dist_Market[VillageData$Q15 == "Oui"] <- 0
VillageData$BadRoad[VillageData$Q40=="Non"] <- 0
VillageData$BadRoad[VillageData$Q40=="Oui"] <- 1
VillageData$Dist_ToPaved <- VillageData$Q41
VillageData$Pop <- VillageData$Q32rec
VillageData$Households <- VillageData$Q33rec
VillageData$Household_Size <- VillageData$Q32rec / VillageData$Q33rec


##Some missing in Dist_SecondarySchool, Dist_Police, Dist_ToPaved, Pop, and Households
##Get average of values for three nearest neighbors to extrapolate values.
VillageData2 <- cbind(VillageData,mdist)
#Village 25 missing Dist_SecondarySchool
VillageData$NUMQ[is.na(VillageData2$Dist_SecondarySchool)]
VillageData2 <- select(VillageData,`25`,Dist_SecondarySchool) %>%
  arrange(`25`) %>% slice(2:4)
Vals <- VillageData2$Dist_SecondarySchool
Weights <- 1 / VillageData2$`25`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Dist_SecondarySchool[VillageData$NUMQ==25] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Village 84 missing Dist_Police
VillageData$NUMQ[is.na(VillageData2$Dist_Police)]
VillageData2 <- select(VillageData,`84`,Dist_Police) %>%
  arrange(`84`) %>% slice(2:4)
Vals <- VillageData2$Dist_Police
Weights <- 1 / VillageData2$`84`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Dist_Police[VillageData$NUMQ==84] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 3 and 125 missing Dist_ToPaved
VillageData$NUMQ[is.na(VillageData2$Dist_ToPaved)]
VillageData2 <- select(VillageData,`3`,Dist_ToPaved) %>% 
  arrange(`3`) %>% slice(2:4)
Vals <- VillageData2$Dist_ToPaved
Weights <- 1 / VillageData2$`3`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Dist_ToPaved[VillageData$NUMQ==3] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 3 and 125 missing Dist_ToPaved
VillageData$NUMQ[is.na(VillageData2$Dist_ToPaved)]
VillageData2 <- select(VillageData,`125`,Dist_ToPaved) %>% 
  arrange(`125`) %>% slice(2:4)
Vals <- VillageData2$Dist_ToPaved
Weights <- 1 / VillageData2$`125`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Dist_ToPaved[VillageData$NUMQ==125] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 9, 16, 129 missing pop
VillageData$NUMQ[is.na(VillageData2$Pop & VillageData2$Households)]
VillageData2 <- select(VillageData,`9`,Pop,Households) %>% 
  arrange(`9`) %>% slice(2:4)
Vals <- VillageData2$Pop
Weights <- 1 / VillageData2$`9`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Pop[VillageData$NUMQ==9] <- Val
Vals <- VillageData2$Households
Val <- sum(Weights*Vals)
VillageData$Households[VillageData$NUMQ==9] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 9, 16, 129 missing pop
VillageData$NUMQ[is.na(VillageData2$Pop & VillageData2$Households)]
VillageData2 <- select(VillageData,`16`,Pop,Households) %>% 
  arrange(`16`) %>% slice(2:4)
Vals <- VillageData2$Pop
Weights <- 1 / VillageData2$`16`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Pop[VillageData$NUMQ==16] <- Val
Vals <- VillageData2$Households
Val <- sum(Weights*Vals)
VillageData$Households[VillageData$NUMQ==16] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 9, 16, 129 missing Pop and Households
VillageData$NUMQ[is.na(VillageData2$Pop & VillageData2$Households)]
VillageData2 <- select(VillageData,`129`,Pop,Households) %>% 
  arrange(`129`) %>% slice(2:4)
Vals <- VillageData2$Pop
Weights <- 1 / VillageData2$`129`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Pop[VillageData$NUMQ==129] <- Val
Vals <- VillageData2$Households
Val <- sum(Weights*Vals)
VillageData$Households[VillageData$NUMQ==129] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 13 and 86 missing Households
VillageData$NUMQ[is.na(VillageData2$Households)]
VillageData2 <- select(VillageData,`13`,Households) %>% 
  arrange(`13`) %>% slice(2:4)
Vals <- VillageData2$Households
Weights <- 1 / VillageData2$`13`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Households[VillageData$NUMQ==13] <- Val

VillageData2 <- cbind(VillageData,mdist)
#Villages 13 and 86 missing Households
VillageData$NUMQ[is.na(VillageData2$Households)]
VillageData2 <- select(VillageData,`86`,Households) %>% 
  arrange(`86`) %>% slice(2:4)
Vals <- VillageData2$Households
Weights <- 1 / VillageData2$`86`
Weights <- Weights / sum(Weights)
Val <- sum(Weights*Vals)
VillageData$Households[VillageData$NUMQ==86] <- Val

rm(VillageData2,Val,Vals,Weights)


###Finally, compute values for average household size
VillageData$Household_Size <- VillageData$Pop / VillageData$Households



####################              Task #2              ####################
#     -We have 132 villages.
#     -Stratify the villages into blocks. 
#        -Blocks will be created by matching villages based on baseline 
#         characteristics. 
#           -Specifically, we use the characteristics to calculate 
#           the Mahalanobis distance between all villages in the commune
#           and construct blocks of two or three most-similar villages on
#           the basis of this distance.
#           -Villages from the same cluster cannot be paired into the same
#           block
###########################################################################

###Here is the R code to do the above:
    ##We will randomize the Dori villages separately. Remove.
    pos = regexpr('Chef-lieu', VillageData$villa)
    VillageData$Dori <- pos
    VillageData$Dori[VillageData$Dori==-1] <- 0
    Dori <- filter(VillageData,Dori==1)
    VillageData <- filter(VillageData,Dori==0)

  ##Create a new variable called Village_ID, a unique village identifier
  VillageData$Village_ID <- 1:nrow(VillageData)
  ##Divide villages into blocks
   blocking <- block(data = VillageData, n.tr = 2,
                    id.vars = c("Village_ID"),
                    block.vars = c("Electric_Grid", "Water_Supply", "Dist_PrimarySchool", 
                          "Dist_SecondarySchool", "LitCenter", "Dist_Police", "Dist_Health", 
                          "Dist_Market", "BadRoad", "Dist_ToPaved", 
                          "Pop", "Households", "Household_Size", 
                          "Cluster"),
                    weight = c(1,1,1, 1,1,1,1, 1,1,1, 1,1,1, .000001),
                    algorithm = "optGreedy", distance = "mahalanobis", 
                    valid.var = "Cluster", valid.range = c(1,nrow(VillageData)), 
                    verbose = TRUE)
          #What the above says, in words: Block the villages in VillageData. Use 2 treatment 
          #classes (treat/control; i.e., 2 obs per block), and block using the variables in block.vars
          #We don't actually want to block on Cluster, but it must be included
          #for an option we use later - to correct for this we weight the other vars at 1 each, and
          #give Cluster a very low weight. We use an optimal greedy algorithm for the matching
          #and match using the mahalanobis distance. We don't want villages from the same cluster
          #to be blocked into the same group, so we use the valid.var and valid.range options.
          #Here, we specify that villages in the same block must have cluster values with a
          #minimum difference of 1 and a maximum difference of the number of villages in the dataset.
          #In other words, a village from cluster 3 and cluster 4 could be in the same block
          #(cluster difference =4-3=1), but two villages from cluter 3 could not be.
  
  
                ###Notes on "algorithm" - The algorithm optGreedy calls an optimal-greedy algorithm, 
                  #repeatedly finding the best remaining match in the entire dataset.
  
  
###We now have an object called "blocking". It is a list of dataframes, which together contain the 
###blocked pairs.
  



####################              Task #3              ####################
#     -We have 132 villages, divided into blocks and clusters.
#           -Clusters are defined by physical location.
#           -Blocks are matched on observable traits and don't allow
#             Villages from the same cluster to be in the same block.
#     -Randomly asssign villages to treatment and control, such that all 
#       villages in the same cluster receive the same condition, and no two
#       villages in the same block receive the same condition.
###########################################################################


##We will need to use the dplyr and reshape2 packages to do some database manipulation first.
###First, export to CSV.
outCSV(blocking)

###Read in the commune-level blocking databases.
BlockedPairs <- read.csv(paste(getwd(), "/Group1.csv",sep =""))  
###Currently each row is a block and columns "Unit.1" and "Unit2" refer to the 2
###villages in the block. Reshape the databases so that rows refer to villages and
###a new column refers to the block number.
BlockedPairs <- melt(BlockedPairs,id.vars=c("X","Distance"))
###Rename variables
BlockedPairs <- rename(BlockedPairs, Block = X, Village_ID = value)
###Sort by Block and Village_ID
BlockedPairs <- arrange(BlockedPairs,Block,Village_ID)
###Merge the complete commune database into the VillageData
VillageData <- full_join(VillageData,BlockedPairs, by="Village_ID")
###Some empty rows where there were an odd number of villages in the commune. Delete empties.
VillageData <- filter(VillageData, !is.na(Village_ID))
###Make Block a factor variable
#VillageData$Block <- as.factor(VillageData$Block)
###Small number of gaps in the Village IDs. We'll need to reset them, or else the randomization could chose IDs that don't exist.
VillageData$Village_ID <- 1:nrow(VillageData)
###Sort by Block and Village_ID
VillageData<- arrange(VillageData,Block,Village_ID)

#Export Blocking and Cluster Assignments
write.dta(select(VillageData, Village_ID, prov, com, villa, Cluster, Block), 
          "BlockingAssignments.dta", version = 12, convert.factors = "string")

###Now we do the randomization task.
###Create a new variable called treatment with all missing values
VillageData$Treatment <- NA
###Set seed for replicability
set.seed(1)
###Index for looping
i = 0
no_selected <- 0


while (i < nrow(VillageData)){
###Randomly select a village and assign to treatment
chosen = sample(nrow(VillageData),1)
if (is.na(VillageData$Treatment[VillageData$Village_ID==chosen])){
  no_selected <- no_selected + 1
}

VillageData$Treatment[VillageData$Village_ID==chosen & is.na(VillageData$Treatment)] <- 1 

check <- 1
while (check != 0 ){
###Save a list of clusters where we have assigned villages
chosen_clusters <- VillageData$Cluster[!is.na(VillageData$Treatment)]
###For each cluster in the list:
for (clust in chosen_clusters){
###Find if it is treatment or control
Treat <- mean(VillageData$Treatment[VillageData$Cluster == clust & !is.na(VillageData$Treatment)])
###Assign the unassigned villages in that cluster to the same status
VillageData$Treatment[VillageData$Cluster==clust & is.na(VillageData$Treatment)] <- Treat 
}

###Save a list of blocks where we have assigned villages
chosen_blocks <- VillageData$Block[!is.na(VillageData$Treatment)]
for (b in chosen_blocks){
###Find if the assigned observation is treatment or control
Treat <- mean(VillageData$Treatment[VillageData$Block == b & !is.na(VillageData$Treatment)])
if (Treat == 1){
VillageData$Treatment[VillageData$Block == b & is.na(VillageData$Treatment)] <- 0 
}
if (Treat == 0){
VillageData$Treatment[VillageData$Block == b & is.na(VillageData$Treatment)] <- 1 
}
}

###Are all observations in blocks and clusters with assigned villages assigned?
chosen_clusters <- VillageData$Cluster[!is.na(VillageData$Treatment)]
chosen_blocks <- VillageData$Block[!is.na(VillageData$Treatment)]
check <- 0
for (clust in chosen_clusters){
check <- check + sum(is.na(VillageData$Treatment[VillageData$Cluster == clust]))
}
for (b in chosen_blocks){
check <- check + sum(is.na(VillageData$Treatment[VillageData$Block == b]))
}
}
i=sum(!is.na(VillageData$Treatment))  
}

##If the above was implemented correctly, all villages in the same cluster should have same treatment status
table(VillageData$Treatment,VillageData$Cluster)
##Also, each block should have one treatment and one control
table(VillageData$Treatment,VillageData$Block)


########Now we need to assign the Dori Villages a treatment and merge them back into the dataset.
###Randomly select a village and assign to treatment
Dori$Village_ID <- 1:nrow(Dori)
Dori$Treatment <- NA

check <- 0
while (check < 3){
  chosen = sample(nrow(Dori),1)
  Dori$Treatment[Dori$Village_ID==chosen & is.na(Dori$Treatment)] <- 1
  check <- sum(!is.na(Dori$Treatment))
}
Dori$Treatment[is.na(Dori$Treatment)] <- 0

##Bring Dori back in to VillageData
VillageData <- select(VillageData, -Block, -Distance, -variable)
VillageData <- rbind(VillageData,Dori)
VillageData <- arrange(VillageData, NUMQ)


###Cleanup
file.remove(paste(getwd(),"/Group1.csv",sep=""))
rm(b,check,chosen,chosen_blocks,chosen_clusters,clust,d,i,no_selected,pos,Treat,blocking,Dori)


##Export
Export <- select(VillageData, NUMQ, lat, long, cont, prov, com, villa, Treatment)
Export$Treatment[Export$Treatment==1] <- "Treatment"
Export$Treatment[Export$Treatment==0] <-"Control"
write.csv(Export,file="P:/7554/Restricted/7554.062 Burkina Faso IE (West Africa IE)/Baseline Data/TreatmentAssignment/TreatmentAssignments.csv")

Export <- select(VillageData, -(`1`:`132`))
write.csv(Export,file="P:/7554/Restricted/7554.062 Burkina Faso IE (West Africa IE)/Baseline Data/Baseline_Data.csv")



###############################################################################################
#################### Draw Map of Treatment Assignments ########################################
###############################################################################################
### convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(VillageData$long,VillageData$lat), ncol=2),
  data.frame(ID=seq(1:length(VillageData$long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
# essentially what this does is cluster villages together, requiring that the diameter
# of the cluster be no larger than 5000 meters (5km)
xy$clust <- cutree(hc, h=d)
VillageData$Cluster <- xy$clust
xy$Treatment <- VillageData$Treatment

###This part is not necessary, but will help us visualize what we just did:
# expand the extent of plotting frame
xy@bbox[] <- as.matrix(extend(extent(xy),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(xy, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

#The shapefile contains polygons in WGS84 projection, so lets define an object to hold that projection.
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#Map Boundaries
bb.sector <- bbox(xy)
#Object "states" is of class SpatialPolygonsDataFrame. Convert to DataFrame.
xy_map <- tidy(xy,region="ID")
Village_map <- left_join(xy_map, VillageData, by = c("ID" = "X"))

#Function that draws map
draw_gmap <- function(map, Treatment, bb) {
  ggmap(get_map(location = bb, zoom=9)) +
    geom_point(data = VillageData, aes(x=long,y=lat, color = factor(Treatment)), 
               size=2, alpha=0.5, shape = 24) +
    plot(ci@polygons, add=T)
}

#Draw map, with fill according to Avg. Years of Schooling, by voting district.
Static_Map <- draw_gmap(Village_map, "Treatment", bb.sector)
Static_Map
