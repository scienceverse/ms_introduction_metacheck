
## Patterns of Implicit and Explicit Attitudes IV. 
## Script version date: January 18th, 2022

####################################################################################################################################

## Recreates all main plots
# First run the Rmd and SM files (overall change and demographics change) to load in all forecasts in the local environment

####################################################################################################################################


## ** EXPLICIT ATTITUDES ----
##  All attitudes plot 
pdf("expfinalplot_forecast.pdf", width = 12, height = 8)
op <- par(mfrow = c(2,3), mar = c(3, 8, 6, 3))
plot(forecast.E.sex, main = "SEXUALITY", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.sex$trend, col = "darkblue", lwd = 3)
lines(sex.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.sexNEW$trend, col = "chocolate3", lwd = 4)


plot(forecast.E.race, main = "RACE", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.race$trend, col = "darkblue", lwd = 3)
lines(race.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.raceNEW$trend, col = "chocolate3", lwd = 4)


plot(forecast.E.skin, main = "SKIN-TONE", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.skin$trend, col = "darkblue", lwd = 3)
lines(skin.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.skinNEW$trend, col = "chocolate3", lwd = 4)


plot(forecast.E.age, main = "AGE", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.age$trend, col = "darkblue", lwd = 3)
lines(age.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.ageNEW$trend, col = "chocolate3", lwd = 4)


plot(forecast.E.dis, main = "DISABILITY", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.dis$trend, col = "darkblue", lwd = 3)
lines(dis.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.disNEW$trend, col = "chocolate3", lwd = 4)


plot(forecast.E.weight, main = "BODY-WEIGHT", 
     ylab = "", ylim = c(-0.1, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 1, by = 0.2), labels = seq(-0.2, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.weight$trend, col = "darkblue", lwd = 3)
lines(weight.WEtsNEW, col = "orange", lwd = 3)
#lines(decomp.E.weightNEW$trend, col = "chocolate3", lwd = 4)

par(op)

dev.off()



## ** IMPLICIT ATTITUDES ----
## All attitudes plot 
pdf("impfinalplot_forecasts.pdf", width = 12, height = 8)
op <- par(mfrow = c(2,3), mar = c(3, 8, 6, 3))
plot(forecast.sex, main = "SEXUALITY", 
     ylab = "", ylim = c(0.1, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.5, by = 0.1), labels = seq(0, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.sex$trend, col = "darkblue", lwd = 3)
lines(sex.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.sexNEW$trend, col = "chocolate3", lwd = 4)

plot(forecast.race, main = "RACE", 
     ylab = "", ylim = c(0.1, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.5, by = 0.1), labels = seq(0, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.race$trend, col = "darkblue", lwd = 3)
lines(race.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.raceNEW$trend, col = "chocolate3", lwd = 4)

plot(forecast.skin, main = "SKIN-TONE", 
     ylab = "", ylim = c(0.1, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.5, by = 0.1), labels = seq(0, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.skin$trend, col = "darkblue", lwd = 3)
lines(skin.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.skinNEW$trend, col = "chocolate3", lwd = 4)

plot(forecast.age, main = "AGE",
     ylab = "", ylim = c(0.1, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.5, by = 0.1), labels = seq(0, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.age$trend, col = "darkblue", lwd = 3)
lines(age.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.ageNEW$trend, col = "chocolate3", lwd = 4)

plot(forecast.dis, main = "DISABILITY",
     ylab = "", ylim = c(0.1, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.6, by = 0.1), labels = seq(0, 0.6, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2021, by = 1), labels = seq(2007, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.dis$trend, col = "darkblue", lwd = 3)
lines(dis.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.disNEW$trend, col = "chocolate3", lwd = 4)

plot(forecast.weightFace, main = "BODY-WEIGHT",
     ylab = "", ylim = c(0.1, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2004, 2021), fcol = "white", flty = 3,
     shadecols = c("white", "white"))
axis(2, at = seq(0, 0.6, by = 0.1), labels = seq(0, 0.6, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2004, 2021, by = 1), labels = seq(2004, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.weightFace$trend, col = "darkblue", lwd = 3, lty = 2)
par(new = TRUE)
plot(forecast.weight, main = "",
     ylab = "", ylim = c(0.1, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2004, 2021), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(0, 0.6, by = 0.1), labels = seq(0, 0.6, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2004, 2021, by = 1), labels = seq(2004, 2021, by = 1), las = 1, cex.axis = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.weight$trend, col = "darkblue", lwd = 3)
lines(weight.WtsNEW, col = "orange", lwd = 3)
#lines(decomp.weightNEW$trend, col = "chocolate3", lwd = 4)

par(op)
dev.off()



## ** FORECASTING to 2025: Explicit Attitudes ----
## All attitudes plot 
pdf("expfinalplot_forecast_2025.pdf", width = 12, height = 9)
op <- par(mfrow = c(2,3), mar = c(5, 8, 6, 3))
plot(forecast.E.sexALL, main = "SEXUALITY", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.sexALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")
#text(x = 2018.5, y = 0.8, labels = "New data", col = "darkgrey")

plot(forecast.E.raceALL, main = "RACE", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.raceALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.E.skinALL, main = "SKIN-TONE", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.skinALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.E.ageALL, main = "AGE", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.ageALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.E.disALL, main = "DISABILITY", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.disALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.E.weightALL, main = "BODY-WEIGHT", 
     ylab = "", ylim = c(-0.3, 1.05), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.3, 1, by = 0.2), labels = seq(-0.3, 1, by = 0.2), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext("Explicit Preference", side = 2, line = 5, cex = 1.7)
lines(decomp.E.weightALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

par(op)
dev.off()



## ** FORECASTING to 2025: Implicit Attitudes ----
## All attitudes plot 
pdf("impfinalplot_forecasts_2025.pdf", width = 12, height = 9)
op <- par(mfrow = c(2,3), mar = c(5, 8, 6, 3))
plot(forecast.sexALL, main = "SEXUALITY", 
     ylab = "", ylim = c(-0.15, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.sexALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.raceALL, main = "RACE", 
     ylab = "", ylim = c(-0.15, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.raceALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.skinALL, main = "SKIN-TONE", 
     ylab = "", ylim = c(-0.15, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.skinALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.ageALL, main = "AGE",
     ylab = "", ylim = c(-0.15, .5), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.ageALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.disALL, main = "DISABILITY",
     ylab = "", ylim = c(-0.15, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2007, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2007, 2025, by = 1), labels = seq(2007, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.disALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")

plot(forecast.weightFace, main = "BODY-WEIGHT",
     ylab = "", ylim = c(-0.15, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2004, 2025), fcol = "white", flty = 3,
     shadecols = c("white", "white"))
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 1, cex.axis = 2)
axis(1, at = seq(2004, 2025, by = 1), labels = seq(2004, 2025, by = 1), las = 2, cex.axis = 1.5)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
abline(h = 0, lty = 2)
lines(decomp.weightFace$trend, col = "darkblue", lwd = 3, lty = 2)
par(new = TRUE)
plot(forecast.weightALL, main = "",
     ylab = "", ylim = c(-0.15, .55), cex.main = 3, axes = FALSE,
     col = "cadetblue", 
     xlim = c(2004, 2025), fcol = "darkblue", flty = 3,
     shadecols = c("lightblue", "cadetblue"))
lines(decomp.weightALL$trend, col = "darkblue", lwd = 3)
abline(v = 2017, lwd = 1.5, lty = 2, col = "darkgrey")
par(op)
dev.off()



## ** DEMOGRAPHIC DIFFERENCES ----

## GENDER ----
pdf(file = "allplot1.gender.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
# Implicit sex, gender 
plot(forecast.Sex.Female, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.Male, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.Wts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(sex.Wts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, gender
plot(forecast.Race.Female, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.Male, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.Wts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(race.Wts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, gender
plot(forecast.Age.Female, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.Male, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.Wts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(age.Wts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)


## Explicit sex, gender
plot(forecast.E.Sex.Female, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.Male, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Sex.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.WEts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(sex.WEts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
mtext("Explicit", side = 2, line = 5, cex = 2)

## Explicit race, gender
plot(forecast.E.Race.Female, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.Male, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Race.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.WEts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(race.WEts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

## Explicit age, gender
plot(forecast.E.Age.Female, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.Male, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.male$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Age.female$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.WEts.Gen.femNEW, col = "darkorchid4", lwd = 3)
lines(age.WEts.Gen.malNEW, col = "darkblue", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()


pdf(file = "allplot1.gender_ALL.pdf", height = 9, width = 11)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
# Implicit sex, gender 
plot(forecast.Sex.FemaleALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.MaleALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, gender
plot(forecast.Race.FemaleALL, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.MaleALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, gender
plot(forecast.Age.FemaleALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.MaleALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)


## Explicit sex, gender
plot(forecast.E.Sex.FemaleALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.MaleALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.E.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
mtext("Explicit", side = 2, line = 5, cex = 2)

## Explicit race, gender
plot(forecast.E.Race.FemaleALL, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.MaleALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.E.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

## Explicit age, gender
plot(forecast.E.Age.FemaleALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.MaleALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.maleALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.E.femaleALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Men", "Women"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()


### RACE (BW) ----
pdf(file = "allplot2.race.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, black-white
plot(forecast.Sex.Black, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.White, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.sex.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(sex.Wts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(sex.Wts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, white-black
plot(forecast.Race.Black,  ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.White,  ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.race.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(race.Wts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(race.Wts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, white-black
plot(forecast.Age.Black, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.White, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.age.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(age.Wts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(age.Wts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)


# Explicit sexuality, black-white
plot(forecast.E.Sex.Black, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.White, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.E.Sex.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(sex.WEts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(sex.WEts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit race, white-black
plot(forecast.E.Race.Black, ylim = c(-1.4, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-1.4, 0.6, by = 0.4), labels = round(seq(-1.4, 0.6, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.White, ylim = c(-1.4, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.E.Race.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(race.WEts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(race.WEts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit age, white-black
plot(forecast.E.Age.Black, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.White, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.black$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.E.Age.white$trend, col = rgb(0.7,0,0), lwd = 3)
lines(age.WEts.Race.blackNEW, col = "darkolivegreen", lwd = 3)
lines(age.WEts.Race.whiteNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()



pdf(file = "allplot2.race_ALL.pdf", height = 9, width = 11)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, black-white
plot(forecast.Sex.BlackALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.WhiteALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.sex.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, white-black
plot(forecast.Race.BlackALL,  ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.WhiteALL,  ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.race.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, white-black
plot(forecast.Age.BlackALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.WhiteALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.age.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)


# Explicit sexuality, black-white
plot(forecast.E.Sex.BlackALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.WhiteALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.sex.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit race, white-black
plot(forecast.E.Race.BlackALL, ylim = c(-1.4, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-1.4, 0.6, by = 0.4), labels = round(seq(-1.4, 0.6, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.WhiteALL, ylim = c(-1.4, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.race.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit age, white-black
plot(forecast.E.Age.BlackALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.WhiteALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.age.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Black", "White"), col = c(rgb(0.2,0.2,0), rgb(0.7,0,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()



pdf(file = "allplot2.race_ALL_withlabels.pdf", height = 9, width = 12)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, black-white
plot(forecast.Sex.BlackALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.WhiteALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.sex.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.35, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = 0.02, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)

# Implicit race, white-black
plot(forecast.Race.BlackALL,  ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.WhiteALL,  ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.race.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.05, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = 0.38, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)

# Implicit age, white-black
plot(forecast.Age.BlackALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.WhiteALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.age.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.55, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = 0.35, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)


# Explicit sexuality, black-white
plot(forecast.E.Sex.BlackALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.WhiteALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.sex.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.7, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = -0.1, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)

# Explicit race, white-black
plot(forecast.E.Race.BlackALL, ylim = c(-1.4, 0.6), main = "RACE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-1.4, 0.6, by = 0.4), labels = round(seq(-1.4, 0.6, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.WhiteALL, ylim = c(-1.4, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.race.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.9, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = 0.4, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)

# Explicit age, white-black
plot(forecast.E.Age.BlackALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.WhiteALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.blackALL$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(decomp.age.E.whiteALL$trend, col = rgb(0.7,0,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.15, pos = 4, col = rgb(0.2,0.2,0), "Black", cex = 1.7)
text(x = 2017, y = 0.65, pos = 4, col = rgb(0.7,0,0), "White", cex = 1.7)

par(op)
dev.off()




## POLITICS ----
pdf(file = "allplot4.politics.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, politics
plot(forecast.Sex.Lib, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.Cons, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.sex.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(sex.Wts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(sex.Wts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, politics
plot(forecast.Race.Lib, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.Cons, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.race.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(race.Wts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(race.Wts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, politics
plot(forecast.Age.Lib, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.Cons, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.age.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(age.Wts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(age.Wts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit sexuality, politics
plot(forecast.E.Sex.Lib, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.Cons, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.E.Sex.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(sex.WEts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(sex.WEts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)),  
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit race, politics
plot(forecast.E.Race.Lib, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.Cons, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.E.Race.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(race.WEts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(race.WEts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("topleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit age, politics
plot(forecast.E.Age.Lib, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.Cons, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.lib$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.E.Age.cons$trend, col = rgb(0.9,0.1,0), lwd = 3)
lines(age.WEts.Pol.libNEW, col = "darkblue", lwd = 3)
lines(age.WEts.Pol.consNEW, col = "darkred", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
par(op)
dev.off()


pdf(file = "allplot4.politics_ALL.pdf", height = 9, width = 11)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, politics
plot(forecast.Sex.LibALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.sex.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, politics
plot(forecast.Race.LibALL, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.race.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, politics
plot(forecast.Age.LibALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.age.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit sexuality, politics
plot(forecast.E.Sex.LibALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.sex.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("left", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)),  
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit race, politics
plot(forecast.E.Race.LibALL, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.race.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("topleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit age, politics
plot(forecast.E.Age.LibALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.age.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Liberals", "Conservatives"), col = c(rgb(0,0.1,0.9), rgb(0.9,0.1,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
par(op)
dev.off()



pdf(file = "allplot4.politics_ALL_withlabels.pdf", height = 9, width = 12)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, politics
plot(forecast.Sex.LibALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Sex.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.sex.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.07, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 0.47, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)

# Implicit race, politics
plot(forecast.Race.LibALL, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Race.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.race.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.15, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 0.45, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)

# Implicit age, politics
plot(forecast.Age.LibALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.ConsALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.age.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.35, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 0.52, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)

# Explicit sexuality, politics
plot(forecast.E.Sex.LibALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Sex.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.sex.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.05, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 1.15, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)


# Explicit race, politics
plot(forecast.E.Race.LibALL, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Race.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.race.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.25, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 0.45, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)


# Explicit age, politics
plot(forecast.E.Age.LibALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0,0.7, alpha = 0.1), rgb(0,0.1,0.9, alpha = 0.1)), 
     col = rgb(0,0,0.7, alpha = 0.7), fcol = rgb(0,0.1,0.9, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.E.Age.ConsALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.9,0.1,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.9,0.1,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.libALL$trend, col = rgb(0,0.1,0.9), lwd = 3)
lines(decomp.age.E.consALL$trend, col = rgb(0.9,0.1,0), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.8, pos = 4, col = rgb(0,0.1,0.9), "Liberals", cex = 1.7)
text(x = 2017, y = 0.1, pos = 4, col = rgb(0.9,0.1,0), "Conservatives", cex = 1.7)

par(op)
dev.off()


## AGE ----
pdf(file = "allplot5.age.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.Sex.Young, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.Old, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.Wts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(sex.Wts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Race.Young, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.Old, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.Wts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(race.Wts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Age.Young, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.Old, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.Wts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(age.Wts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Sex.Young, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Sex.Old, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Sex.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.WEts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(sex.WEts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Race.Young, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Race.Old, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Race.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.WEts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(race.WEts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Age.Young, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Age.Old, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.young$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Age.old$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.WEts.Age.youngNEW, col = "darkblue", lwd = 3)
lines(age.WEts.Age.oldNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("topleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()



pdf(file = "allplot5.age_ALL.pdf", height = 9, width = 11)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
plot(forecast.Sex.YoungALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Race.YoungALL, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Age.YoungALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Sex.YoungALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Sex.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Race.YoungALL, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Race.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Age.YoungALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Age.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("topleft", c("Young", "Old"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

par(op)
dev.off()



pdf(file = "allplot5.age_ALL_withlabels.pdf", height = 9, width = 12)
op <- par(mar = c(5,8,4,1), mfrow = c(2, 3))
plot(forecast.Sex.YoungALL, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.05, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.35, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

plot(forecast.Race.YoungALL, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.15, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.38, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

plot(forecast.Age.YoungALL, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 5, cex = 1.7)
par(new = TRUE)
plot(forecast.Age.OldALL, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.36, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.5, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

plot(forecast.E.Sex.YoungALL, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Sex.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.sex.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.15, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.5, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

plot(forecast.E.Race.YoungALL, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Race.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.race.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = -0.2, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.4, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

plot(forecast.E.Age.YoungALL, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2025))
box()
axis(1, at = c(2007:2025), labels = c(2007:2025), cex.axis = 1.5, las = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Age.OldALL, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2025))
lines(decomp.age.E.youngALL$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.E.oldALL$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
abline(v = 2017, lty = 2, col = "darkgrey", lwd = 2)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
text(x = 2017, y = 0.8, pos = 4, col = rgb(0,0.5,0.8), "Younger", cex = 1.7)
text(x = 2017, y = 0.15, pos = 4, col = rgb(0.7,0.3,0.5), "Older", cex = 1.7)

par(op)
dev.off()




### RACE (WA) ----
pdf(file = "allplot3.raceWA.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
# Implicit sexuality, white-asian
plot(forecast.Sex.Asian, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.White, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.Wts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(sex.Wts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit race, white-asian
plot(forecast.Race.Asian, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.White, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.Wts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(race.Wts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Implicit age, white-asian
plot(forecast.Age.Asian, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Age.White, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.Wts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(age.Wts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit sexuality, white-asian
plot(forecast.E.Sex.Asian, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Sex.White, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Sex.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.WEts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(sex.WEts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit race, white-asian
plot(forecast.E.Race.Asian, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Race.White, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Race.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.WEts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(race.WEts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

# Explicit age, white-asian
plot(forecast.E.Age.Asian, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Age.White, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.asian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Age.white$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.WEts.Race.asianNEW, col = "darkblue", lwd = 3)
lines(age.WEts.Race.whiteNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Asian", "White"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
par(op)
dev.off()



## RELIGION ----
pdf(file = "allplot6.religion.pdf", height = 8, width = 15)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.Sex.Christian, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.Non, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Sex.Other, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Sex.Jewish, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.sex.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.sex.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(sex.Wts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(sex.Wts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(sex.Wts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(sex.Wts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

plot(forecast.Race.Christian, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.Non, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Race.Other, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Race.Jewish, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.race.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.race.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(race.Wts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(race.Wts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(race.Wts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(race.Wts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

plot(forecast.Age.Christian, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Age.Non, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Age.Other, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Age.Jewish, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.age.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.age.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(age.Wts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(age.Wts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(age.Wts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(age.Wts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

plot(forecast.Sex.E.Christian, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.E.Non, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Sex.E.Other, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Sex.E.Jewish, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.E.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.E.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.sex.E.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.sex.E.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(sex.WEts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(sex.WEts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(sex.WEts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(sex.WEts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

plot(forecast.Race.E.Christian, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.E.Non, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Race.E.Other, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Race.E.Jewish, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.E.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.E.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.race.E.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.race.E.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(race.WEts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(race.WEts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(race.WEts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(race.WEts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

plot(forecast.Age.E.Christian, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Age.E.Non, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Age.E.Other, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0, alpha = 0.1), rgb(0.7,0.3,0, alpha = 0.1)), 
     col = rgb(0.7,0,0, alpha = 0.7), fcol = rgb(0.7,0.3,0, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
par(new = TRUE)
plot(forecast.Age.E.Jewish, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.2,0.2,0, alpha = 0.1), rgb(0.2,0.5,0, alpha = 0.1)), 
     col = rgb(0.2,0.2,0, alpha = 0.7), fcol = rgb(0.2,0.2,0, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.E.christian$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.E.non$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(decomp.age.E.other$trend, col = rgb(0.7,0,0), lwd = 3)
lines(decomp.age.E.jewish$trend, col = rgb(0.2,0.2,0), lwd = 3)
lines(age.WEts.Rel.jewishNEW, col = "darkolivegreen", lwd = 3)
lines(age.WEts.Rel.christianNEW, col = "darkblue", lwd = 3)
lines(age.WEts.Rel.nonNEW, col = "darkorchid4", lwd = 3)
lines(age.WEts.Rel.otherNEW, col = "tomato4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("Christian", "Non-religious", "Other", "Jewish"),
       col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5), rgb(0.7,0,0), rgb(0.2,0.2,0)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8, ncol = 2)

par(op)
dev.off()


## EDUCATION ----
pdf(file = "allplot7.education.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.Sex.Coll, ylim = c(-0.1, 0.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Sex.NColl, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.sex.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.sex.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.Wts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(sex.Wts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("> College", "< College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Race.Coll, ylim = c(-0.1, 0.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Race.NColl, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.race.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.race.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.Wts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(race.Wts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("> College", "< College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.Age.Coll, ylim = c(-0.1, 0.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.7, by = 0.2), labels = seq(-0.2, 0.7, by = 0.2), cex.axis = 2, las = 2)
mtext("IAT D-score", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.Age.NColl, ylim = c(-0.1, 0.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.age.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.age.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.Wts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(age.Wts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c(">College", "<College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)


plot(forecast.E.Sex.Coll, ylim = c(-0.4, 1.6), main = "SEXUALITY",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Sex.NColl, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Sex.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Sex.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(sex.WEts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(sex.WEts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("> College", "< College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Race.Coll, ylim = c(-0.4, 1.6), main = "RACE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Race.NColl, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Race.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Race.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(race.WEts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(race.WEts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("> College", "< College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)

plot(forecast.E.Age.Coll, ylim = c(-0.4, 1.6), main = "AGE",
     shadecols = c(rgb(0,0.5,0.5, alpha = 0.1), rgb(0,0.5,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0.5,0.8, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.4, 1.6, by = 0.4), labels = seq(-0.4, 1.6, by = 0.4), cex.axis = 2, las = 2)
mtext("Explicit preference", side = 2, line = 3.2, cex = 1.1)
par(new = TRUE)
plot(forecast.E.Age.NColl, ylim = c(-0.4, 1.6), main = "",
     shadecols = c(rgb(0.7,0,0.5, alpha = 0.1), rgb(0.7,0.3,0.5, alpha = 0.1)), 
     col = rgb(0.7,0.3,0.5, alpha = 0.7), fcol = rgb(0.7,0,0.5, alpha = 0.3),
     cex.main = 2,
     axes = FALSE,
     xlim = c(2007, 2021))
lines(decomp.E.Age.col$trend, col = rgb(0,0.5,0.8), lwd = 3)
lines(decomp.E.Age.ncol$trend, col = rgb(0.7,0.3,0.5), lwd = 3)
lines(age.WEts.Edu.colNEW, col = "darkblue", lwd = 3)
lines(age.WEts.Edu.ncolNEW, col = "darkorchid4", lwd = 3)
abline(h = 0, lty = 2, col = "darkgrey", lwd = 2)
legend("bottomleft", c("> College", "< College"), col = c(rgb(0,0.5,0.8), rgb(0.7,0.3,0.5)), 
       lty = 1, lwd = 3, bty = "n", cex = 1.8)
par(op)
dev.off()



## ** DIFFERENCED SERIES ----
## GENDER ----
pdf(file = "allplotdiff.gender.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.gender, ylim = c(-0.2, 0.2), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.genderNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.gender, ylim = c(-0.2, 0.2), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.genderNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.gender, ylim = c(-0.2, 0.2), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.genderNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diff.gender, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.E.genderNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diff.gender, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.E.genderNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diff.gender, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Men-Women)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diff.gender$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.E.genderNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()



## RACE ----
pdf(file = "allplotdiff.race.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.race, ylim = c(-0.3, 0.5), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.race, ylim = c(-0.3, 0.5), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.race, ylim = c(-0.3, 0.5), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diff.race, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.E.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diff.race, ylim = c(-0.1, 1.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 1.8, by = 0.2), labels = round(seq(-0.2, 1.8, by = 0.2), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.E.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diff.race, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Black)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diff.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.E.raceNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()

## White vs. Asian
pdf(file = "allplotdiff.raceWA.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diffWA.race, ylim = c(-0.2, 0.2), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diffWA.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diffWA.race, ylim = c(-0.2, 0.2), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diffWA.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diffWA.race, ylim = c(-0.2, 0.2), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diffWA.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diffWA.race, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diffWA.E.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diffWA.race, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diffWA.E.raceNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diffWA.race, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(White-Asian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diffWA.race$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diffWA.E.raceNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()



### POLITICS -----
pdf(file = "allplotdiff.pol.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.pol, ylim = c(-0.1, 0.5), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.polNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.pol, ylim = c(-0.1, 0.5), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.polNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.pol, ylim = c(-0.1, 0.5), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.polNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diff.pol, ylim = c(-0.2, 1.4), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 1.4, by = 0.4), labels = round(seq(-0.8, 1.4, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.E.polNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diff.pol, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.E.polNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diff.pol, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Conservative-Liberal)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diff.pol$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.E.polNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()



## EDUCATION -----
pdf(file = "allplotdiff.edu.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.edu, ylim = c(-0.2, 0.2), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.eduNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.edu, ylim = c(-0.2, 0.2), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.eduNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.edu, ylim = c(-0.2, 0.2), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.eduNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diff.edu, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.E.eduNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diff.edu, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.E.eduNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diff.edu, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(>Coll-<Coll)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diff.edu$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.E.eduNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()


## AGE -----
pdf(file = "allplotdiff.age.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.age, ylim = c(-0.2, 0.4), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.ageNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.age, ylim = c(-0.2, 0.4), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.ageNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.age, ylim = c(-0.2, 0.4), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.ageNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Sex.diff.age, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Sex.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.E.ageNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Race.diff.age, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Race.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.E.ageNEW, col = "darkblue", lwd = 3)

plot(forecast.E.Age.diff.age, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Old-Young)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.E.Age.diff.age$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.E.ageNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()



## RELIGION -----
pdf(file = "allplotdiff.rel.pdf", height = 7, width = 11)
op <- par(mar = c(3,8,4,1), mfrow = c(2, 3))
plot(forecast.sex.diff.rel, ylim = c(-0.2, 0.2), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.Wts.diff.relNEW, col = "darkblue", lwd = 3)

plot(forecast.race.diff.rel, ylim = c(-0.2, 0.2), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.Wts.diff.relNEW, col = "darkblue", lwd = 3)

plot(forecast.age.diff.rel, ylim = c(-0.2, 0.2), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = round(seq(-0.2, 0.5, by = 0.1), 3), cex.axis = 2, las = 2)
mtext("IAT D-score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.Wts.diff.relNEW, col = "darkblue", lwd = 3)

plot(forecast.sex.E.diff.rel, ylim = c(-0.8, 0.8), main = "SEXUALITY",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.sex.E.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(sex.WEts.diff.relNEW, col = "darkblue", lwd = 3)

plot(forecast.race.E.diff.rel, ylim = c(-0.8, 0.8), main = "RACE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.race.E.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(race.WEts.diff.relNEW, col = "darkblue", lwd = 3)

plot(forecast.age.E.diff.rel, ylim = c(-0.8, 0.8), main = "AGE",
     shadecols = c(rgb(0,0,0.5, alpha = 0.1), rgb(0,0,0.8, alpha = 0.1)), 
     col = rgb(0,0.5,0.8, alpha = 0.7), fcol = rgb(0,0,0.5, alpha = 0.3),
     cex.main = 3,
     axes = FALSE,
     xlim = c(2007, 2021))
box()
axis(1, at = c(2007:2021), labels = c(2007:2021), cex.axis = 2)
axis(2, at = seq(-0.8, 0.8, by = 0.4), labels = round(seq(-0.8, 0.8, by = 0.4), 3), cex.axis = 2, las = 2)
mtext("Explicit score Difference\n(Nonreligious-Christian)", 2, 4, cex =1.3)
abline(h = 0, lty = 2)
lines(decomp.age.E.diff.rel$trend, col = rgb(0,0,0.5), lwd = 3)
lines(age.WEts.diff.relNEW, col = "darkblue", lwd = 3)

par(op)
dev.off()





