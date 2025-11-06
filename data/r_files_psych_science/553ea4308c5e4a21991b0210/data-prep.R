library(car)
d=read.csv("/Users/tholubar/Desktop/DLs/Moral_Rebels_Affirmation-5.csv")
d$White=recode(d$White,"''=0;else=1")
d$ValueImportance=d$ValueImportance-1
d[,18:34]=d[,18:34]-4
d[,60:67]=d[,60:67]-4