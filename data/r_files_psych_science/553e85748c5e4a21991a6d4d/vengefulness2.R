data = read.csv('Lin_April_Sim.csv')
demo = read.csv('SIM_Demographics__April_2012.csv')

#get rid of row of questions
data = data[-(1),]
data = merge(data, demo, by="ID" )

#excluding certain participants for bad responses
#data = data[which(data$mturkcode!='74363'),]

#to find TRIM score
TRIM = cbind(data[,28:45])
TRIM #view the TRIM scale

#to find vengefulness subscale, TRIM items 1, 4, 9, 13, and 17
vengefulness = cbind(TRIM[,1],TRIM[,4],TRIM[,9],TRIM[,13],TRIM[,17])
vengefulness

#to find benevolence subscale, TRIM items 3, 6, 8, 12, 14, and 16
ben = cbind(TRIM[,3],TRIM[,6],TRIM[,8],TRIM[,12],TRIM[,14],TRIM[,16])
ben
benR = cbind(6-ben[,1:6]) #reverse coding benevolence
benR

#to find avoidance subscale, TRIM items 2, 5, 7, 10, 11, 15, and 18
avoid = cbind(TRIM[,2],TRIM[,5],TRIM[,7],TRIM[,10],TRIM[,11],TRIM[,15],TRIM[,18])
avoid

#TRIM total score with benevolence reverse coded
TRIMtot = cbind(vengefulness,benR,avoid)
TRIMtot
data$TRIMscore = rowMeans(TRIMtot)
mean(data$TRIMscore); sd(data$TRIMscore)

#create column in df of vengefulness scores (as calculated by mean of subscale)
data$vengefulness = rowMeans(vengefulness)
mean(data$vengefulness); sd(data$vengefulness)

#capability subscales (1 = capability of committing and equally bad offense, 2 = similarly offense)
data[16:19]
cap1 = (data[16:19])
cap1 = apply(cap1, 2, as.character)
cap1 = apply(cap1, 2, as.numeric)
cap1
data[20:23]
cap2 = (data[20:23])
cap2 = apply(cap2, 2, as.character)
cap2 = apply(cap2, 2, as.numeric)
cap2

#empathy subscale
empathy = (data[,24:27])
empathy
empathy = apply(empathy, 2, as.character)
empathy = apply(empathy, 2, as.numeric)

#create columns in df of capability and empathy scores
data$cap1 = rowMeans(cap1)
mean(data$cap1); sd(data$cap1)
data$cap2 = rowMeans(cap2)
mean(data$cap2); sd(data$cap2)

#checking to see if capability to commit a an equally bad offense and a similar offense are correlated:
cor.test(data$cap1,data$cap2)

#So, we can combine cap1 and cap2
cap = cbind(cap1,cap2)
data$cap = rowMeans(cap)
mean(data$cap); sd(data$cap)

data$empathy = rowMeans(empathy)
mean(data$empathy); sd(data$empathy)

write.csv(data, file = "data_prepped.csv")