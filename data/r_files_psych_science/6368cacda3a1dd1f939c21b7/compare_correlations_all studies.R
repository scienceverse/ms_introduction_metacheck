#COMPARING CORRELATIONS BETWEEN THOSE WHO DISBELIEVE AND THOSE WHO BELIEVE IN 
#THE OFFICIAL VERSION (+ NOT SURE GROUP IN STUDY 2)

#installing required package
install.packages("cocor")
#loading library
library(cocor)

# comparing correlations for two independent groups
# general formula: cocor.indep.groups(correlation_1, correlation_2,
#sample_size_1, sample_size_2, alpha = .05)

#***** Study 1 *****
  
#corona virus
cocor.indep.groups(.404, .122, 573, 223, alpha = .05)

#climate change
cocor.indep.groups(.468, -.002, 735, 61, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.476, -.134, 637, 159, alpha = .05)

#Maddie McCann
cocor.indep.groups(.546, .082, 615, 181, alpha = .05)

#cancer cure
cocor.indep.groups(.593, .173, 633, 163, alpha = .05)

#Hitler's death
cocor.indep.groups(.485, -.635, 657, 139, alpha = .05)

#Princess Diana
cocor.indep.groups(.281, .001, 572, 224, alpha = .05)

#Epstein's death
cocor.indep.groups(.417, -.209, 383, 413, alpha = .05)


#***** Study 2 *****

#***believers vs. disbelievers***

#corona virus
cocor.indep.groups(.328, .112, 4029, 353, alpha = .05)

#climate change
cocor.indep.groups(.524, .331, 4149, 403, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.404, .110, 3684, 398, alpha = .05)

#Hitler's death
cocor.indep.groups(.525, -.347, 3732, 173, alpha = .05)

#***believers vs. not sure***

#corona virus
cocor.indep.groups(.328, .200, 4029, 966, alpha = .05)

#climate change
cocor.indep.groups(.524, .339, 4149, 792, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.404, .356, 3684, 1261, alpha = .05)

#Hitler's death
cocor.indep.groups(.525, .201, 3732, 1438, alpha = .05)

#***not sure vs. disbelievers***

#corona virus
cocor.indep.groups(.200, .112, 966, 353, alpha = .05)

#climate change
cocor.indep.groups(.339, .331, 792, 403, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.356, .110, 1261, 398, alpha = .05)

#Hitler's death
cocor.indep.groups(.201, -.347, 1438, 173, alpha = .05)



#***** Study 3 *****

#corona virus
cocor.indep.groups(.471, .124, 534, 262, alpha = .05)

#climate change
cocor.indep.groups(.457, -.277, 707, 89, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.464, -.067, 638, 158, alpha = .05)

#cancer cure
cocor.indep.groups(.419, -.214, 586, 210, alpha = .05)

#Hitler's death
cocor.indep.groups(.404, -.268, 693, 103, alpha = .05)

#Epstein's death
cocor.indep.groups(.216, -.187, 287, 509, alpha = .05)

#Osama Bin Laden
cocor.indep.groups(.300, -.467, 750, 39, alpha = .05)

#Elvis Presley
cocor.indep.groups(.486, .223, 742, 29, alpha = .05)


#***** Study 4 *****

#corona virus
cocor.indep.groups(.366, -.022, 477, 311, alpha = .05)

#climate change
cocor.indep.groups(.410, -.437, 662, 126, alpha = .05)

#9/11 terrorist strikes
cocor.indep.groups(.464, -.077, 552, 236, alpha = .05)

#cancer cure
cocor.indep.groups(.384, -.227, 518, 270, alpha = .05)

#Hitler's death
cocor.indep.groups(.409, -.353, 638, 150, alpha = .05)

#Epstein's death
cocor.indep.groups(.286, -.203, 285, 503, alpha = .05)

#Osama Bin Laden
cocor.indep.groups(.418, -.448, 710, 78, alpha = .05)

#Princess Diana
cocor.indep.groups(.409, -.286, 582, 206, alpha = .05)
