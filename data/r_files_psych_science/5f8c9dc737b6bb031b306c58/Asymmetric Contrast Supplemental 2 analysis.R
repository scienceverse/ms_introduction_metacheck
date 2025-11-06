# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast Supplemental 2 data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

# Analysis ####

t = table(reference = dt$reference, Gambled = dt$gambled)

print(aggregate(dt$gambled == 1, list(reference = dt$reference), mean, na.rm = T), digits = 2)
chisq.test(t)