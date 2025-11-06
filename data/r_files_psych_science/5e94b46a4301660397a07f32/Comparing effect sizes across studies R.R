#SOI and sexual DS

#soi above, sexual DS below

#vs. Belgian
require(cocor) # load package
cocor.indep.groups(r1.jk=-0.32, r2.hm=-0.31, n1=8129, n2=476, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

require(cocor) # load package
cocor.indep.groups(r1.jk=+0.34, r2.hm=+0.31, n1=8229, n2=476, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)


#vs. Dutch
require(cocor) # load package
cocor.indep.groups(r1.jk=-0.32, r2.hm=-0.40, n1=8129, n2=299, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

require(cocor) # load package
cocor.indep.groups(r1.jk=+0.34, r2.hm=+0.34, n1=8229, n2=299, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)


#Vs. Japanese
require(cocor) # load package
cocor.indep.groups(r1.jk=-0.32, r2.hm=-0.27, n1=8129, n2=296, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

require(cocor) # load package
cocor.indep.groups(r1.jk=+0.34, r2.hm=+0.26, n1=8229, n2=296, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)



#Vs. American students - soi above, sexual DS below
require(cocor) # load package
cocor.indep.groups(r1.jk=-0.32, r2.hm=-0.47, n1=8129, n2=471, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

cocor.indep.groups(r1.jk=+0.34, r2.hm=+0.53, n1=8229, n2=471, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)


#Vs. American Mturk - soi above, sexual DS below
require(cocor) # load package
cocor.indep.groups(r1.jk=-0.32, r2.hm=-0.49, n1=8129, n2=516, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)

require(cocor) # load package
cocor.indep.groups(r1.jk=+0.34, r2.hm=+0.45, n1=8229, n2=516, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
