# install.packages(c( 'irr'))
library(irr)
load("Fleiss_Kappa.RData")
#1952 sample
kappam.fleiss(osf_dataset_1)
#non-overlapping replication sample
kappam.fleiss(osf_dataset_2)



