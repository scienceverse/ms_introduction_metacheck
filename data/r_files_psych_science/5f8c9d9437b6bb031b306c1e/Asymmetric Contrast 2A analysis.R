# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 2A data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

# Stats #### 

tbl = table(dt$gambled, dt$reference)
print(aggregate(dt$gambled, by = list(dt$reference), FUN = 'mean'), digits = 2)
chisq.test(tbl)