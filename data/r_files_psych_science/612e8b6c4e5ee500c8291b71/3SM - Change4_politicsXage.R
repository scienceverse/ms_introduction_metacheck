
## Patterns of Implicit and Explicit Attitudes IV. 
## Script version date: September 1st, 2021

####################################################################################################################################

## Runs supplemental analyses exploring intersection of age X politics in explicit age attitudes

####################################################################################################################################

# Load data 
# setwd(**)
load("ALL_demogdataframes.Rdata")

# Load packages
if (!require(plyr)) {install.packages("plyr", dependencies = TRUE); require(plyr)}
if (!require(forecast)) {install.packages("forecast", dependencies = TRUE); require(forecast)}


####################################################################################################################################

## Explicit AGE attitudes ----
## Intersection of age X politics

## Politics-weighted data ----
## First, compute trends by subsetting age from politic-weighted trends
age.libcons$age.grp <- cut(age.libcons$age.new, c(0,20,40,99))
levels(age.libcons$age.grp) <- c("young", "middle", "old")
table(age.libcons$age.grp, age.libcons$politicalid.new)

age.liberals <- age.libcons[as.numeric(age.libcons$politicalid.new) > 4,]
age.conserv <- age.libcons[as.numeric(age.libcons$politicalid.new) < 4,]

# Liberals
age.month.E.libY <- ddply(age.liberals[age.liberals$age.grp=="young",], .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
age.month.E.libO <- ddply(age.liberals[age.liberals$age.grp=="old",], .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

age.WEts.Pol.libNEW_Y <- ts(age.month.E.libY$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.libNEW_Y
age.WEts.Pol.libNEW_O <- ts(age.month.E.libO$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.libNEW_O


# Conservatives
age.month.E.consY <- ddply(age.conserv[age.conserv$age.grp=="young",], .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
age.month.E.consO <- ddply(age.conserv[age.conserv$age.grp=="old",], .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

age.WEts.Pol.consNEW_Y <- ts(age.month.E.consY$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.consNEW_Y
age.WEts.Pol.consNEW_O <- ts(age.month.E.consO$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.consNEW_O


tsmat_polage <- cbind(age.WEts.Pol.consNEW_O, age.WEts.Pol.consNEW_Y, 
                      age.WEts.Pol.libNEW_O, age.WEts.Pol.libNEW_Y)
decomp_polage <- as.data.frame(matrix(nrow = 49, ncol = 4))
start_polage <- vector()
end_polage <- vector()
rch_polage <- vector()
pch_polage <- vector()
arima_polage <- list()

for(i in 1:4){
  decomp_polage[,i] <- decompose(tsmat_polage[,i])$trend
  start_polage[i] <- decomp_polage[7,i]
  end_polage[i] <- decomp_polage[42,i]
  rch_polage[i] <- decompose(tsmat_polage[,i])$trend[7] - decompose(tsmat_polage[,i])$trend[42]
  pch_polage[i] <- rch_polage[i]/decompose(tsmat_polage[,i])$trend[7]
  arima_polage[[i]] <- auto.arima(tsmat_polage[,i], stepwise = FALSE, approximation = FALSE)
  print(i)
}

round(start_polage, 2)
round(end_polage, 2)
round(rch_polage, 2)
round(pch_polage, 2)
arima_polage



pdf(file = "polXage_explicitage_plot1.pdf", width = 7, height = 5)
op <- par(mar = c(3, 6, 5, 3))
plot(age.WEts.Pol.libNEW_Y, col = "cadetblue", lwd = 1,
     ylim = c(-0.15, 1), ylab = "Explicit Attitudes", main = "EXPLICIT AGE", cex.main = 2)
lines(age.WEts.Pol.libNEW_O, col = "darkblue", lwd = 1)
lines(age.WEts.Pol.consNEW_Y, col = "coral", lwd = 1)
lines(age.WEts.Pol.consNEW_O, col = "darkred", lwd = 1)
lines(decomp_polage[,1], col = "darkred", lwd = 3)
lines(decomp_polage[,2], col = "coral", lwd = 3)
lines(decomp_polage[,3], col = "darkblue", lwd = 3)
lines(decomp_polage[,4], col = "cadetblue", lwd = 3)
abline(h = 0, lty = 2)
text(x = 2019, y = 0.26, "Older Liberals", col = "darkblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.95, "Younger Liberals", col = "cadetblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.4, "Younger Conservatives", col = "coral", cex = 1.2, pos = 4)
text(x = 2019, y = -0.15, "Older Conservatives", col = "darkred", cex = 1.2, pos = 4)
par(op)
dev.off()



## Age-weighted data ----
## Second, compute from subsetting politics in age-weighted trends
table(age.ygmidold$pol.grp, age.ygmidold$age.grp)

age.liberals2 <- age.ygmidold[age.ygmidold$pol.grp == "lib",]
age.conserv2 <- age.ygmidold[age.ygmidold$pol.grp == "cons",]
age.liberals2$weightvec.age <- as.numeric(age.liberals2$weightvec.age)
age.conserv2$weightvec.age <- as.numeric(age.conserv2$weightvec.age)

# Liberals
age.month.E.libY2 <- ddply(age.liberals2[age.liberals2$age.grp=="young",], .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))
age.month.E.libO2 <- ddply(age.liberals2[age.liberals2$age.grp=="old",], .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))

age.WEts.Pol.libNEW_Y2 <- ts(age.month.E.libY2$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.libNEW_Y2
age.WEts.Pol.libNEW_O2 <- ts(age.month.E.libO2$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.libNEW_O2


# Conservatives
age.month.E.consY2 <- ddply(age.conserv2[age.conserv2$age.grp=="young",], .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.age.mean = weighted.mean(attnumeric, weightvec.age))
age.month.E.consO2 <- ddply(age.conserv2[age.conserv2$age.grp=="old",], .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.age.mean = weighted.mean(attnumeric, weightvec.age))

age.WEts.Pol.consNEW_Y2 <- ts(age.month.E.consY2$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.consNEW_Y2
age.WEts.Pol.consNEW_O2 <- ts(age.month.E.consO2$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.consNEW_O2

tsmat_polage2 <- cbind(age.WEts.Pol.consNEW_O2, age.WEts.Pol.consNEW_Y2, 
                      age.WEts.Pol.libNEW_O2, age.WEts.Pol.libNEW_Y2)
decomp_polage2 <- as.data.frame(matrix(nrow = 48, ncol = 4))
start_polage2 <- vector()
end_polage2 <- vector()
rch_polage2 <- vector()
pch_polage2 <- vector()
arima_polage2 <- list()

for(i in 1:4){
  decomp_polage2[,i] <- decompose(tsmat_polage2[,i])$trend
  start_polage2[i] <- decomp_polage2[7,i]
  end_polage2[i] <- decomp_polage2[42,i]
  rch_polage2[i] <- decompose(tsmat_polage2[,i])$trend[7] - decompose(tsmat_polage2[,i])$trend[42]
  pch_polage2[i] <- rch_polage2[i]/decompose(tsmat_polage2[,i])$trend[7]
  arima_polage2[[i]] <- auto.arima(tsmat_polage2[,i], stepwise = FALSE, approximation = FALSE)
  print(i)
}

round(start_polage2, 2)
round(end_polage2, 2)
round(rch_polage2, 2)
round(pch_polage2, 2)
arima_polage2


pdf(file = "polXage_explicitage_plot2.pdf", width = 7, height = 5)
op <- par(mar = c(3, 5, 5, 3))
plot(age.WEts.Pol.libNEW_Y2, col = "cadetblue", lwd = 2,
     ylim = c(-0.15, 1), ylab = "Explicit Attitudes", main = "EXPLICIT AGE", cex.main = 2)
lines(age.WEts.Pol.libNEW_O2, col = "darkblue", lwd = 2)
lines(age.WEts.Pol.consNEW_Y2, col = "coral", lwd = 2)
lines(age.WEts.Pol.consNEW_O2, col = "darkred", lwd = 2)
abline(h = 0, lty = 2)
lines(decomp_polage2[,1], col = "darkred", lwd = 3)
lines(decomp_polage2[,2], col = "coral", lwd = 3)
lines(decomp_polage2[,3], col = "darkblue", lwd = 3)
lines(decomp_polage2[,4], col = "cadetblue", lwd = 3)
text(x = 2019, y = 0.26, "Older Liberals", col = "darkblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.95, "Younger Liberals", col = "cadetblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.4, "Younger Conservatives", col = "coral", cex = 1.2, pos = 4)
text(x = 2019, y = -0.15, "Older Conservatives", col = "darkred", cex = 1.2, pos = 4)
par(op)
dev.off()


pdf(file = "polXage_explicitage_plot3.pdf", width = 15, height = 6)
op <- par(mfrow = c(1,2), mar = c(3, 8, 6, 3))
plot(age.WEts.Pol.libNEW_Y2, col = "cadetblue", lwd = 2,
     ylim = c(-0.15, 1), ylab = "Explicit Attitudes", main = "EXPLICIT AGE: age-weighted", cex.main = 2)
lines(age.WEts.Pol.libNEW_O2, col = "darkblue", lwd = 2)
lines(age.WEts.Pol.consNEW_Y2, col = "coral", lwd = 2)
lines(age.WEts.Pol.consNEW_O2, col = "darkred", lwd = 2)
abline(h = 0, lty = 2)
text(x = 2019, y = 0.26, "Older Liberals", col = "darkblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.95, "Younger Liberals", col = "cadetblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.4, "Younger Conservatives", col = "coral", cex = 1.2, pos = 4)
text(x = 2019, y = -0.15, "Older Conservatives", col = "darkred", cex = 1.2, pos = 4)

plot(age.WEts.Pol.libNEW_Y, col = "cadetblue", lwd = 2,
     ylim = c(-0.15, 1), ylab = "Explicit Attitudes", main = "EXPLICIT AGE: politics-weighted", cex.main = 2)
lines(age.WEts.Pol.libNEW_O, col = "darkblue", lwd = 2)
lines(age.WEts.Pol.consNEW_Y, col = "coral", lwd = 2)
lines(age.WEts.Pol.consNEW_O, col = "darkred", lwd = 2)
abline(h = 0, lty = 2)
text(x = 2019, y = 0.26, "Older Liberals", col = "darkblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.95, "Younger Liberals", col = "cadetblue", cex = 1.2, pos = 4)
text(x = 2019, y = 0.4, "Younger Conservatives", col = "coral", cex = 1.2, pos = 4)
text(x = 2019, y = -0.15, "Older Conservatives", col = "darkred", cex = 1.2, pos = 4)
par(op)
dev.off()




