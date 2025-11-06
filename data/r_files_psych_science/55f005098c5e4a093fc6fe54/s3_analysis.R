data3a <- read.table("s3a_data.txt", sep = "\t", head = T)
data3b <- read.table("s3b_data.txt", sep = "\t", head = T)

#-------------------------------------------------------------------------
# removal of data and demographics

# study 3a
nrow(data3a) # participants before exclusion
mean(data3a$sex) - 1 # proportion women
range(data3a$age)
median(data3a$age)
apply(data3a[,27:29], 2, sum) # reasons for exclusion
sum(apply(data3a[,7:26], 1, function(x) any(is.na(x)))) # missing values
data3a <- data3a[data3a$remove_answers +
                   data3a$remove_hypothesis +
                   data3a$remove_language == 0, ]
data3a <- data3a[apply(data3a[,7:26], 1, function(x) !any(is.na(x))), ]
nrow(data3a) # participants after exclusion
median(data3a$age)
mean(data3a$sex) - 1 # proportion women

# study 3b
nrow(data3b) # participants before exclusion
mean(data3b$sex == "female") # proportion women
range(data3b$age)
median(data3b$age)
data3b <- data3b[data3b$leave == 1, ]
nrow(data3b) # participants after exclusion
mean(data3b$sex == "female") # proportion women
range(data3b$age)
median(data3b$age)

#-------------------------------------------------------------------------
# preprocessing 

# study 3a
data3a <- data.frame(data3a, 
                     dif_1 = (data3a$ITEM_B - data3a$ITEM_A) / 5,
                     dif_names = (data3a$NAME_B - data3a$NAME_A) / 5)
data3a$version <- ifelse(data3a$version == 1, "additives", "cities")
colnames(data3a)[2] <- "group"

# study 3b
hard <- list(cities = c(1,2,5,6,8),
             additives = c(2,3,4,9,10),
             names = c(1,3,4,5,8))

a <- vector(length = nrow(data3b)) # cities/additives hard
b <- vector(length = nrow(data3b)) # cities/additives easy
ja <- vector(length = nrow(data3b)) # killers hard
jb <- vector(length = nrow(data3b)) # killers easy

for(i in 1:nrow(data3b)) {
  items <- hard[[as.character(data3b$group[i])]]
  a[i] <- rowMeans(data3b[i, items + 2])
  b[i] <- rowMeans(data3b[i, setdiff(1:10, items) + 2])
  ja[i] <- rowMeans(data3b[i, hard[["names"]] + 12])
  jb[i] <- rowMeans(data3b[i, setdiff(1:10, hard[["names"]]) + 12])
}

df <- b - a
dj <- jb - ja

data3b <- data.frame(data3b, dif_1 = df, dif_names = dj)

# study 3a and 3b together
data3 <- data.frame(group = c(data3a$group, as.character(data3b$group)),
                    dif_1 = c(data3a$dif_1, data3b$dif_1),
                    dif_names = c(data3a$dif_names, data3b$dif_names))
  
#-------------------------------------------------------------------------
# analysis

# study 3a
singleTest(data3a$dif_1[data3a$group == "additives"]) # additives
singleTest(data3a$dif_1[data3a$group == "cities"]) # cities
# difference between additives and cities
unpairedTest(data3a$dif_1[data3a$group == "additives"],
             data3a$dif_1[data3a$group == "cities"]) 
# difference killers (between groups)
unpairedTest(data3a$dif_names[data3a$group == "additives"],
             data3a$dif_names[data3a$group == "cities"]) 
singleTest(data3a$dif_names) # pooled killers
# dospert
alpha(data3a[,34:45], check.keys = F)
cor.test(rowMeans(data3a[,34:45], na.rm = T)[data3a$group == "additives"], 
         data3a$dif_1[data3a$group == "additives"])

# study 3b
singleTest(data3b$dif_1[data3b$group == "additives"]) # additives
singleTest(data3b$dif_1[data3b$group == "cities"]) # cities
# difference between additives and cities
unpairedTest(data3b$dif_1[data3b$group == "additives"],
             data3b$dif_1[data3b$group == "cities"]) 
# difference killers (between groups)
unpairedTest(data3b$dif_names[data3b$group == "additives"],
             data3b$dif_names[data3b$group == "cities"]) 
singleTest(data3b$dif_names) # pooled killers

# joint analysis
singleTest(data3$dif_1[data3$group == "additives"]) # additives
singleTest(data3$dif_1[data3$group == "cities"]) # cities
# difference between additives and cities
unpairedTest(data3$dif_1[data3$group == "additives"],
             data3$dif_1[data3$group == "cities"]) 
# difference killers (between groups)
unpairedTest(data3$dif_names[data3$group == "additives"],
             data3$dif_names[data3$group == "cities"]) 
singleTest(data3$dif_names) # pooled killers
# difference between killers and additives
pairedTest(data3$dif_1[data3$group == "additives"],
           data3$dif_names[data3$group == "additives"])


#-------------------------------------------------------------------------
# graph

# study 3a
png("s3a_graph.png", width = 750, height = 500)
makeGraph(list(data3a$dif_1[data3a$group == "additives"], 
               data3a$dif_names[data3a$group == "additives"],
               data3a$dif_1[data3a$group == "cities"], 
               data3a$dif_names[data3a$group == "cities"]),
          c("Additives", "Criminals", "Cities", "Criminals"),
          c("Additives", "Cities")
)
dev.off()

# study 3b
png("s3b_graph.png", width = 750, height = 500)
makeGraph(list(data3b$dif_1[data3b$group == "additives"], 
               data3b$dif_names[data3b$group == "additives"],
               data3b$dif_1[data3b$group == "cities"], 
               data3b$dif_names[data3b$group == "cities"]),
          c("Additives", "Criminals", "Cities", "Criminals"),
          c("Additives", "Cities")
)
dev.off()

# study 3 - joint results
png("s3_graph.png", width = 750, height = 500)
makeGraph(list(data3$dif_1[data3$group == "additives"], 
               data3$dif_names[data3$group == "additives"],
               data3$dif_1[data3$group == "cities"], 
               data3$dif_names[data3$group == "cities"]),
          c("Additives", "Criminals", "Cities", "Criminals"),
          c("Additives", "Cities")
)
dev.off()