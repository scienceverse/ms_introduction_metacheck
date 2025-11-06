install.packages("TOSTER")
require("TOSTER")


##Equivalence tests for correlation between socios

##Men and women combined
TOSTr(n=896, r=-0.00751, low_eqbound_r=-0.20, high_eqbound_r=0.20, alpha=0.05)


##Women only
TOSTr(n=563, r=0.01567194, low_eqbound_r=-0.20, high_eqbound_r=0.20, alpha=0.05)


##Men only
TOSTr(n=332, r=-0.04014631, low_eqbound_r=-0.20, high_eqbound_r=0.20, alpha=0.05)


