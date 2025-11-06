# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 2B data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

# Analysis ####

print(aggregate(dt$gambled == 1, list(reference = dt$reference),mean,na.rm=T),digits=2)
t = table(dt$reference, dt$gambled)
chisq.test(t)