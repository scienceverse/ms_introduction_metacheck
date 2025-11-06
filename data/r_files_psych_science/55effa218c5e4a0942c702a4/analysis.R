#===============================================================================
# LIBRARIES
#===============================================================================
for(pack in c("MBESS", "lme4", "lmerTest", "meta")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)  
  }
  library(pack, character.only = T)
}


#===============================================================================
# HELPER FUNCTIONS
#===============================================================================
# formating p-values
round_p <- function(x) {
  if(x < .001) return("p < .001") 
  if(round(x, 2) == 1) return("p = 1")
  
  if(round(x, 3) >= .01) {
    p <- sprintf("%.2f", x)
  } else {
    p <- sprintf("%.3f", x)
  }
  
  return(paste("p = ", substr(p, 2, nchar(p)), sep = ""))
}

# function for displaying results
results <- function(model, effect, family = "binomial") {
  coefs <- summary(model)$coefficients
  if(family == "binomial") {
    z <- coefs[effect, "z value"]
    p <- coefs[effect, "Pr(>|z|)"]
    e.name <- "OR"
    par <- "z"
  } else {
    z <- coefs[effect, "t value"]
    p <- coefs[effect, "Pr(>|t|)"]        
    e.name <- "b"
    par <- paste0("t(", sprintf("%.1f", coefs[effect, "df"]), ")")
  }
  ES <- fixef(model)[effect]
  CI <- confint.merMod(model, rownames(coefs)[effect], method = "Wald")
  if(family == "binomial") {
    ES <- exp(ES)
    CI <- exp(CI)
  }
  cat(par, " = ", sprintf("%.2f", z), ", ", 
      round_p(p), ", ",
      e.name, " = ", sprintf("%.2f", ES), ", ",
      "95% CI = [", sprintf("%.2f", CI[1]), ", ",
      sprintf("%.2f", CI[2]), "]",
      sep = "")
}

# function for reporting correlations
p.cor <- function(x, y, method = "pearson") {
  shorten <- function(x) {
    s <- sprintf("%.2f", x)
    if(x >= 0) {
      return(substr(s, 2, nchar(s)))
    } else {
      return(paste0("-", substr(s, 3, nchar(s))))
    }
  }
  
  if(method == "pearson") {
    ct <- cor.test(x, y)
    out <- sprintf("r(%i) = %s, 95%% CI = [%s, %s], %s",
                   ct$parameter, shorten(ct$estimate), shorten(ct$conf.int[1]),
                   shorten(ct$conf.int[2]), round_p(ct$p.value))
  } else {
    if(method == "spearman") {
      suppressWarnings(p <- cor.test(x, y, method = "spearman")$p.value)
      sci <- spearman.ci(x, y, nrep = 1000)
      out <- sprintf("rS = %s, 95%% CI = [%s, %s], %s",
                     shorten(sci$estimate), shorten(sci$conf.int[1]),
                     shorten(sci$conf.int[2]), round_p(p))      
    } else {
      out <- "method must be either 'spearman' or 'pearson'!"
    }
  }
  cat(out)
}


#===============================================================================
# STUDY 1
#===============================================================================
d1 <- read.table("s1_data.txt", sep = "\t", head = T, as.is = T)

N1 <- nrow(d1)

study1 <- data.frame(id = paste0("s1_", c(1:N1, 1:N1)),
                     study = rep(1, N1*2),
                     demo = "Czech undergraduates",
                     group = rep("a", N1*2),
                     sample = rep(c(1, 2), each = N1),
                     materials = rep(c("food additives", 
                                       "cities in a war zone"), each = N1),
                     original = rep(c(T, F), each = N1),
                     data = c(((d1$AD_1 - d1$AD_2) / 5),
                              ((d1$CIT_1 - d1$CIT_2)  / 5)),
                     remove = rep((d1$missing == 1 | 
                                     d1$language == 3 | 
                                     d1$suspiciousness > 2 | 
                                     d1$suspiciousness == 2 & d1$random == 1
                     ), 2),
                     stringsAsFactors = F
                     )

s1.item <- data.frame(study = 1,
                      id = rep(paste0("s1_", 1:N1), each = 20),
                      answer = as.numeric(t(as.matrix(d1[,10:29]))),
                      item = rep(1:20, N1),
                      fluent = rep(c(T,F,T,F,F,F,F,T,T,T, F,T,F,T,F,T,T,F,T,F), N1),
                      materials = rep(c("food additives", 
                                        "cities in a war zone"), each = 10, times = N1),
                      sample = rep(c(1, 2), each = 10, times = N1),
                      stringsAsFactors = F
                      )


#===============================================================================
# STUDY 2
#===============================================================================
d2a <- read.table("s2a_data.txt", sep = "\t", head = T, as.is = T)
d2b <- read.table("s2b_data.txt", sep = "\t", head = T, as.is = T)

N2a <- nrow(d2a)
N2b <- nrow(d2b)
N2 <- nrow(d2a) + nrow(d2b)

study2 <- data.frame(id = paste0("s2_", c(rep(1:N2a, 2), rep((N2a+1):(N2a+N2b), 2))),
                     study = rep(2, N2*2),
                     demo = "Czech university students",
                     group = c(ifelse(d2a$version == 1, "a", "b"),
                               rep("a&b", N2a),
                               ifelse(d2b$group == "additives", "a", "b"),
                               rep("a&b", N2b)
                               ),
                     sample = c(ifelse(d2a$version == 1, 1, 2),
                                rep(3, N2a),
                                ifelse(d2b$group == "additives", 1, 2),
                                rep(3, N2b)
                                ),
                     materials = c(ifelse(d2a$version == 1, 
                                          "food additives", 
                                          "cities in a war zone"),
                                   rep("criminals", N2a),
                                   ifelse(d2b$group == "additives", 
                                          "food additives", 
                                          "cities in a war zone"),
                                   rep("criminals", N2b)
                                   ),
                     original = c(ifelse(d2a$version == 1, T, F),
                                  rep(F, N2a),
                                  ifelse(d2b$group == "additives", T, F),
                                  rep(F, N2b)
                                  ),
                     data = c(((d2a$ITEM_B - d2a$ITEM_A) / 5),
                              ((d2a$NAME_B - d2a$NAME_A) / 5),
                              apply(d2b, 1, function(x) if(x[2]=="cities") { 
                                ((sum(as.numeric(x[c(3,4,7,9,10) + 2])) - 
                                    sum(as.numeric(x[c(1,2,5,6,8) + 2])))/5)
                              } else {
                                ((sum(as.numeric(x[c(1,5,6,7,8) + 2])) - 
                                    sum(as.numeric(x[c(2,3,4,9,10) + 2])))/5) 
                              }),
                              apply(d2b, 1, function(x) {
                                (sum(as.numeric(x[c(2,6,7,9,10) + 12])) -
                                   sum(as.numeric(x[c(1,3,4,5,8) + 12])))/5
                                })
                              ),
                     remove = c(rep(d2a$remove_answers + 
                                      d2a$remove_hypothesis + 
                                      d2a$remove_language > 0 |
                                      apply(d2a[,7:26], 
                                            1, 
                                            function(x) any(is.na(x))
                                            ), 
                                    2),
                                rep(d2b$leave == 0, 2)),
                     stringsAsFactors = F
                     )

s2.item <- data.frame(study = 2,
                      id = rep(paste0("s2_", c(1:N2a, (N2a+1):(N2a+N2b))), each = 20),
                      answer = c(as.numeric(t(as.matrix(d2a[,7:26]))),
                                 as.numeric(t(as.matrix(d2b[,3:22])))),
                      item = c(rep(1:20, N2a) + rep(as.numeric(t(as.matrix(
                        cbind(ifelse(d2a$version == 1, 20, 0), 0)))), 
                        each = 10),
                        rep(1:20, N2b) + rep(as.numeric(t(as.matrix(
                          cbind(ifelse(d2b$group == "additives", 20, 0), 0)))), 
                          each = 10)
                      ),
                      fluent = T,
                      sample = c(rep(as.numeric(t(as.matrix(
                        cbind(ifelse(d2a$version == 1, 1, 2), 3)))), 
                        each = 10),
                        rep(as.numeric(t(as.matrix(
                          cbind(ifelse(d2b$group == "additives", 1, 2), 3)))), 
                          each = 10)
                      ),
                      materials = c(rep(as.character(t(as.matrix(
                        cbind(ifelse(d2a$version == 1, 
                                     "food additives", 
                                     "cities in a war zone"), 
                              "criminals")))), 
                        each = 10),
                        rep(as.character(t(as.matrix(
                          cbind(ifelse(d2b$group == "additives", 
                                       "food additives",
                                       "cities in a war zone"), 
                                "criminals")))), 
                          each = 10)
                      ),
                      stringsAsFactors = F
                      )
for(i in 1:(nrow(study2)/2)) {
  if(i <= N2a) {
    if(study2$materials[i] == "food additives") x <- c(T,F,F,F,T,T,T,T,F,F)
    if(study2$materials[i] == "cities in a war zone") x <- c(F,F,T,T,F,F,T,F,T,T) 
  } else {
    if(study2$materials[N2a+i] == "food additives") x <- c(T,F,F,F,T,T,T,T,F,F)
    if(study2$materials[N2a+i] == "cities in a war zone") x <- c(F,F,T,T,F,F,T,F,T,T)   
  }
  s2.item$fluent[(i*20-19):(i*20-10)] <- x
  s2.item$fluent[(i*20-9):(i*20)] <- c(F,T,F,F,F,T,T,F,T,T)
}


#===============================================================================
# STUDY 3
#===============================================================================
d3 <- read.table("s3_data.txt", sep = "\t", head = T, as.is = T)
d3.remove <- read.table("s3_remove.txt", sep = "\t", head = T, as.is = T)
d3.pretest <- read.table("s3_pretest.txt", sep = "\t", as.is = T)

d3.hard <- sapply(gsub("\\.", " ", colnames(d3)[3:ncol(d3)]), 
                  function(x) d3.pretest[d3.pretest == x, 2] > 2.4)

N3 <- nrow(d3)

study3 <- data.frame(id = paste0("s3_", c(1:N3, 1:N3)),
                     study = rep(3, N3*2),
                     demo = "German subject pool",
                     group = c(ifelse(d3$group == "additives", "a", "b"),
                               rep("a&b", N3)),
                     sample = c(ifelse(d3$group == "additives", 1, 2),
                                rep(3, N3)),
                     materials = c(ifelse(d3$group == "additives", 
                                          "food additives", 
                                          "cities in a war zone"),
                                   rep("criminals", N3)),
                     original = c(ifelse(d3$group == "additives", T, F),
                                  rep(F, N3)),
                     data = c((rowMeans(d3[, c(F, F, !d3.hard & 
                                                 rep(c(T, T, F), each = 10))], na.rm = T) -
                                 rowMeans(d3[, c(F, F, d3.hard & 
                                                   rep(c(T, T, F), each = 10))], na.rm = T)),
                              (rowMeans(d3[, c(F, F, !d3.hard & 
                                                 rep(c(F, F, T), each = 10))], na.rm = T) -
                                 rowMeans(d3[, c(F, F, d3.hard & 
                                                   rep(c(F, F, T), each = 10))], na.rm = T))
                     ),
                     remove = (apply(d3.remove[,3:12], 1, 
                                     function (x) {any(matrix(table(x)) > 7)}) | 
                                 apply(d3.remove[,13:22], 1, 
                                       function (x) {any(matrix(table(x)) > 7)}) |
                                 apply(d3, 1, function(x) sum(is.na(x)) > 10)
                     ),
                     stringsAsFactors = F
                     )

s3.item <- data.frame(study = 3,
                      id = rep(paste0("s3_", 1:N3), each = 30),
                      answer = as.numeric(t(as.matrix(d3[,3:32]))),
                      item = rep(1:30, N3),
                      fluent = !rep(d3.hard, N3),
                      sample = rep(c(2,1,3), each = 10, times = N3),
                      materials = rep(c("cities in a war zone",
                                        "additives",
                                        "criminals"
                      ), each = 10, times = N3),
                      stringsAsFactors = F
                      )


#===============================================================================
# STUDY 4
#===============================================================================
d4 <- read.table("s4_data.txt", sep = "\t", head = T, as.is = T)[, c(2:41, 72)]

d4 <- d4[apply(d4, 1, function(x) sum(!is.na(x)) > 2), ]

N4 <- nrow(d4)

study4 <- data.frame(id = paste0("s4_", 1:N4),
                     study = rep(4, N4),
                     demo = "mTurk workers",
                     group = ifelse(d4[,ncol(d4)] %in% c(840189, 840198), "b", "a"),
                     sample = ifelse(d4[,ncol(d4)] %in% c(840189, 840198), 2, 1),
                     materials = ifelse(d4[,ncol(d4)] %in% c(840189, 840198), 
                                        "cities in a war zone", 
                                        "beach resorts"),
                     original = ifelse(d4[,ncol(d4)] %in% c(840189, 840198), F, T),
                     data = apply(d4[,1:ncol(d4)-1], 1, function(x) 
                       (mean(x[!is.na(x)][c(F, F, T, T, F, F, T, F, T, T)]) -   
                          mean(x[!is.na(x)][!c(F, F, T, T, F, F, T, F, T, T)]))),
                     remove = apply(d4[,1:ncol(d4)-1], 1, 
                                    function(x) any(matrix(table((x[!is.na(x)]))) > 8)),
                     stringsAsFactors = F
                     )

s4.item <- data.frame(study = 4,
                      id = rep(paste0("s4_", 1:N4), each = 40),
                      answer = as.numeric(t(as.matrix(d4[,1:40]))),
                      item = rep(c(1:20, 1:20), N4),
                      fluent = rep(c(F, F, T, T, F, F, T, F, T, T), 4*N4),
                      sample = rep(c(2,1,2,1), each = 10, times = N4),
                      materials = rep(rep(c("cities in a war zone",
                                            "beach resorts"), 2),
                                      each = 10, times = N4),
                      stringsAsFactors = F
                      )


#===============================================================================
# NAIVE THEORIES
#===============================================================================
nt <- read.table("naive_theories.txt", sep = "\t", head = T, as.is = T)
round(colMeans(nt[nt$group == 0, 3:ncol(nt)], na.rm = T), 2) # easily pronounceable
round(colMeans(nt[nt$group == 1, 3:ncol(nt)], na.rm = T), 2) # familiar


#===============================================================================
# GRAPH STUDIES 1-4
#===============================================================================
data <- rbind(study1, study2, study3, study4)
idata <- rbind(s1.item, s2.item, s3.item, s4.item)


tiff("Fig1.tif", width = 4000, height = 2000, res = 300)
xoffset <- 2.5

par(las = 1, mar = c(0,0,3,0), cex = 1.15)
plot(0,0, axes = F, ann = F, type = "n", xlim = c(-5.6, 1 + xoffset), ylim = c(0,11))
axis(side = 3, at = (-2:2)*0.5, labels = c("-1", "-0.5", "0", "0.5", "1"), pos = 10, lwd = 2)
lines(c(0, 0), c(0, 10), lty = 2, lwd = 2)

axis(side = 3, at = (-2:2)*0.5 + xoffset, labels = c("-1", "-0.5", "0", "0.5", "1"), pos = 10, lwd = 2)
lines(c(xoffset, xoffset), c(0, 10), lty = 2, lwd = 2)

xs <- c(-5.55, -5.20, -3.33, -2.975, -1.43, -1.39)

y <- 9.5
for(study in 1:4) { 
  for(sample in 1:max(data$sample[data$study == study])) {
    alldat <- data[data$study == study & data$sample == sample, ]
    exp <- data[data$study == study & data$sample == sample & !data$remove, ]
    color <- ifelse(all(exp$original), "black", "grey50")
    pch <- ifelse(all(exp$original), 19, 4)
    t <- t.test(exp$data)
    N <- t$parameter + 1
    points(t$estimate, y, pch = pch, col = color, cex = 1.2, lwd = 3)
    lines(c(t$conf.int[1], t$conf.int[2]), c(y, y), col = color, lwd = 2, lend = 1)
    if(sample == 1) {
      text(xs[1], y, study, adj = c(.5, .5))
      text(xs[2], y, exp$demo[1], adj = c(0, .5))
    }
    text(xs[3], y, exp$group[1], adj = c(.5, .5))
    text(xs[4], y, exp$materials[1], adj = c(0, .5))
    text(xs[5], y, N, adj = c(1, .5))
    text(xs[6], y, paste0("(", nrow(alldat), ")"), adj = c(0, .5))
    
    # mixed-effect models
    iexp <- idata[idata$study == study & idata$sample == sample, ]
    iexp <- iexp[iexp$id %in% unique(exp$id), ]
    iexp <- iexp[!is.na(iexp$answer),]
    mi <- lmer(answer ~ fluent + (fluent|id) + (1|item), data = iexp)
    points(fixef(mi)[2] + xoffset, y, pch = pch, col = color, cex = 1.2, lwd = 3)
    cis <- confint.merMod(mi, 7, method = "Wald")
    lines(c(cis[1] + xoffset, cis[2] + xoffset), c(y, y), col = color, lwd = 2, lend = 1)
    
    y <- y - .7
  }
  y <- y - .9
}

text(xs[1], 10.5, "Study")
text(xs[2], 10.5, "Sample", adj = c(0, .5))
text(xs[3], 10.5, "Group")
text(xs[4], 10.5, "Scenario", adj = c(0, .5))
text(xs[6], 10.5, "N", adj = c(0.5, .5))

mtext("Difference in average rating of dangerousness\n(negative: fluent-safe)", 
      at = xoffset/2, line = 0.65, cex = 1.1)
mtext("Stimuli as a FIXED factor", at = 0, line = -0.7)
mtext("Stimuli as a RANDOM factor", at = xoffset, line = -0.7)
dev.off()


#===============================================================================
# STUDY 5
#===============================================================================
d5 <- read.table("s5_data.txt", sep = "\t", head = T)
nrow(d5)
d5.pron <- data.frame(items = unlist(read.table("s5_items.txt", sep = "\t", 
                                                head = F, as.is = T)), 
                      pronounceability = colMeans(read.table("s5_pronounciation.txt", 
                                                             sep = "\t", 
                                                             head = T)[,2:61]),
                      stringsAsFactors = F
                      )

exclude <- apply(d5[,1:120], 1, function(x) {
  (sum(!is.na(x)) - sum(x[!is.na(x)] == 0) < 9) | (max(table(x[!is.na(x)])) > 7)
  })
sum(exclude)
d5 <- d5[!exclude,]

columnnames <- c("id", "item", "harm", "group", "pronounceability", "original")
d5.long <- matrix(0, ncol = 6, nrow = nrow(d5)*10)
count <- 1
for(row in 1:nrow(d5)) {
  group <- ifelse(d5[row, 121] == 884586, "medicines", "poisons")
  for(col in 1:120) {
    value <- d5[row, col]
    if(is.na(value) | value == 0) next
    n <- col %% 60
    if(n == 0) n <- 60
    item <- d5.pron$items[n]
    fluency <- mean(d5.pron$pronounceability) - d5.pron$pronounceability[n]
    original <- n > 50
    d5.long[count, ] <- c(row, item, value, group, fluency, original)
    count <- count + 1
  }
}
d5.long <- d5.long[1:(count-1),]
d5.long <- data.frame(d5.long, stringsAsFactors = F)
colnames(d5.long) <- columnnames

d5.long$id <- factor(d5.long$id)
d5.long$item <- factor(d5.long$item)
d5.long$harm <- as.numeric(d5.long$harm)
d5.long$group <- factor(d5.long$group)
d5.long$pronounceability <- as.numeric(d5.long$pronounceability)
d5.long$original <- factor(d5.long$original)

# results
#--------
m5a <- lmer(harm ~ pronounceability*group + original*pronounceability + (1|id) + (1|item), 
            data = d5.long)
summary(m5a)
results(m5a, 2, "linear")
results(m5a, 5, "linear")
results(m5a, 6, "linear")
results(m5a, 3, "linear")
results(m5a, 4, "linear")

m5b <- lmer(harm ~ pronounceability*group + (1|id) + (1|item), 
            data = d5.long[d5.long$item %in% items[51:60],])
summary(m5b)
results(m5b, 2, "linear")

m5c <- lmer(harm ~ pronounceability*group + (1|id) + (1|item), 
            data = d5.long[d5.long$item %in% items[1:50],])
summary(m5c)
results(m5c, 2, "linear")

# including random slopes for pronounceability for participants
m5a.pron <- lmer(harm ~ pronounceability*group + original*pronounceability + 
                   (pronounceability|id) + (1|item), data = d5.long)
summary(m5a.pron) # no difference

m5b.pron <- lmer(harm ~ pronounceability*group + (pronounceability|id) + (1|item),
                 data = d5.long[d5.long$item %in% items[51:60],]) # unidentifiable

m5c.pron <- lmer(harm ~ pronounceability*group + (pronounceability|id) + (1|item), 
                 data = d5.long[d5.long$item %in% items[1:50],])
summary(m5c.pron) # no difference


#===============================================================================
# STUDY 5 GRAPH
#===============================================================================
regline <- function(x, col = "black", lwd = 12) {
  int <- x$coefficients[1]
  slope <- x$coefficients[2]
  xs <- x$model[[2]]
  mn <- min(xs)
  mx <- max(xs)
  lines(c(mn, mx), c(int + slope*mn, int + slope*mx), col=col, lwd=lwd)
}

d5.pron.graph <- 6 - d5.pron$pronounceability

tiff("Fig2.tif", width = 2000, height = 3040, res = 300)

par(mfrow = c(2,1), las = 1, mar = c(1, 4.5, 3, 1) + 0.1, oma = c(4,0,0,0), cex = 1.4)
plot(d5.pron.graph, 
     colMeans(d5[,1:60], na.rm = T), 
     pch = c(rep(4,50), rep(19, 10)), 
     col = c(rep("grey50",50), rep("black", 10)),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1.4, 3.5),
     ylim = c(2.9, 5.6),
     xaxs = "i", yaxs = "i",
     lwd = 3
)
regline(lm(colMeans(d5[,1:50], na.rm = T) ~ d5.pron.graph[1:50]), 
        col = "grey50", lwd = 4)
regline(lm(colMeans(d5[,51:60], na.rm = T) ~ d5.pron.graph[51:60]), 
        col = "black", lwd = 4)
mtext("Medicines", 3, line = 0.5, cex = 1.8)
mtext("Harmfulness rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

par(mar = c(1, 4.5, 3, 1) + 0.1)
plot(d5.pron.graph, 
     colMeans(d5[,61:120], na.rm = T), 
     pch = c(rep(4,50), rep(19, 10)),  
     col = c(rep("grey50",50), rep("black", 10)),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1.4, 3.5),
     ylim = c(2.9, 5.6),
     xaxs = "i", yaxs = "i",
     lwd = 3
)
regline(lm(colMeans(d5[,61:110], na.rm = T) ~ d5.pron.graph[1:50]), 
        col = "grey50", lwd = 4)
regline(lm(colMeans(d5[,111:120], na.rm = T) ~ d5.pron.graph[51:60]), 
        col = "black", lwd = 4)
mtext("Poisons", 3, line = 0.5, cex = 1.8)
mtext("Harmfulness rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

mtext("Average pronounceability rating (scale 1-5)", 1, line = 3, cex = 1.5)

dev.off()


#===============================================================================
# STUDY 6
#===============================================================================
d6 <- read.table("s6_harm.txt", head = T, sep = "\t", as.is = T)
d6.novelty <- read.table("s6_novelty.txt", head = T, sep = "\t", as.is = T)
d6.fluency <- read.table("s6_pronounceability.txt", head = T, 
                         sep = "\t", as.is = T)
d6.new.additives <- unlist(read.table("s6_new_additives.txt", as.is = T))
d6.old.additives <- unlist(read.table("s6_old_additives.txt", as.is = T))

length(unique(d6$id))
length(unique(d6.novelty$id))

# pronounceability
d6.pron <- sapply(d6$name, function(x) {mean(d6.fluency$disfluency[d6.fluency$name == x])})
d6.pron.nov <- sapply(d6.novelty$name,  
                      function(x) {mean(d6.fluency$disfluency[d6.fluency$name == x])})

# new/old
d6.original <- d6$name %in% d6.old.additives
d6.original.nov <- d6.novelty$name %in% d6.old.additives

d6 <- cbind(d6, fluency = d6.pron, original = d6.original)
d6.novelty <- cbind(d6.novelty, fluency = d6.pron.nov, original = d6.original.nov)

# exclusion of participants with 12 or more same ratings
d6.retain <- sapply(d6$id, function(x) {max(table(d6[d6$id == x, "harmful"])) < 12})
d6 <- d6[d6.retain, ]

d6.retain.nov <- sapply(d6.novelty$id, 
                        function(x) {max(table(d6.novelty[d6.novelty$id == x, "novel"])) < 12})
d6.novelty <- d6.novelty[d6.retain.nov, ]

length(unique(d6$id))
length(unique(d6.novelty$id))

# results
#--------
# harmfulness
# interaction
m6a <- lmer(harmful ~ fluency*original + (1|id) + (1|name), data = d6)
summary(m6a)
results(m6a, 2, family = "linear")
results(m6a, 4, family = "linear")
# original
m6a.old <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[d6$original,])
summary(m6a.old)
results(m6a.old, 2, family = "linear")
# new
m6a.new <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[!d6$original,])
summary(m6a.new)
results(m6a.new, 2, family = "linear")

# novelty
# interaction
m6b <- lmer(novel ~ fluency*original + (1|id) + (1|name), data = d6.novelty)
summary(m6b)
results(m6b, 4, family = "linear")
results(m6b, 2, family = "linear")
# original
m6b.old <- lmer(novel ~ fluency + (1|id) + (1|name), 
                data = d6.novelty[d6.novelty$original,])
summary(m6b.old)
results(m6b.old, 2, family = "linear")
# new
m6b.new <- lmer(novel ~ fluency + (1|id) + (1|name), 
                data = d6.novelty[!d6.novelty$original,])
summary(m6b.new)
results(m6b.new, 2, family = "linear")

# including random slopes for pronounceability for participants 
m6a.pron <- lmer(harmful ~ fluency*original + (fluency|id) + (1|name), data = d6)
summary(m6a.pron) # no difference
# original
m6a.old.pron <- lmer(harmful ~ fluency + (fluency|id) + (1|name), data = d6[d6$original,])
summary(m6a.old.pron) # no difference
# new
m6a.new.pron <- lmer(harmful ~ fluency + (fluency|id) + (1|name), data = d6[!d6$original,])
summary(m6a.new.pron) # no difference

# similarity
#-----------
d6.similarity <- read.table("s6_similarity.txt", head = T, sep = "\t", 
                            as.is = T, na.strings = c("", "-99"))
d6.similarities <- data.frame(name = colnames(d6.similarity)[3:52],
                              similarity = colMeans(d6.similarity[,3:52], na.rm = T),
                              old = colnames(d6.similarity)[3:52] %in% d6.old.additives)
simmat <- matrix("", ncol = 4, nrow = nrow(d6.similarity)*50)
count <- 1
for(row in 1:nrow(d6.similarity)) {
  for(col in 3:52) {
    value <- d6.similarity[row, col]
    if(!is.na(value)) {
      add <- colnames(d6.similarity)[col]
      simmat[count,] <- c(row, add, value, add %in% d6.old.additives)
      count <- count + 1      
    }
  }
}
simmat <- simmat[1:count-1,]
d6.similarity.ratings <- as.data.frame(simmat)
colnames(d6.similarity.ratings) = c("id", "name", "rating", "old")
d6.similarity.ratings$rating <- as.numeric(as.character(d6.similarity.ratings$rating))

# number of participants
length(unique(d6.similarity.ratings$id))

# averages
max(d6.similarities$similarity)

# difference in similarity between old and new names
m6.similarity <- lmer(rating ~ old + (1|id) + (1|name), data = d6.similarity.ratings)
summary(m6.similarity)
results(m6.similarity, 2, family = "linear")

# difference in averaged similarities
t.test(d6.similarities$similarity[d6.similarities$old],
       d6.similarities$similarity[!d6.similarities$old])

# robustness of the results of study 6
d6.similarities.ordered <- as.character(d6.similarities$name)[
  order(d6.similarities$similarity, decreasing = T)]
d6.sims.ordered <- d6.similarities$similarity[order(d6.similarities$similarity, 
                                                    decreasing = T)]

d6.sensitivity <- matrix(0, nrow = 21, ncol = 6)
for(i in 1:21) {
  subset <- !(d6$name %in% d6.similarities.ordered[0:(i-1)])
  m.temp <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[d6$original & subset,])
  d6.sensitivity[i, 1] <- fixef(m.temp)[2]
  d6.sensitivity[i, 2:3] <- confint.merMod(m.temp, 2, method = "Wald")  
  m.temp <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[!d6$original & subset,])
  d6.sensitivity[i, 4] <- fixef(m.temp)[2]
  d6.sensitivity[i, 5:6] <- confint.merMod(m.temp, 2, method = "Wald")  
}

# graph
png("similarity_sensitivity.png")
par(las = 1)
plot(0, 
     0, 
     type = "n",
     main = "",
     xlab = "Similarity cutoff",
     ylab = "Pronounceability effect",
     ylim = range(d6.sensitivity),
     xlim = c(3, d6.sims.ordered[20])
)
abline(h = 0, lwd = 2, lty = 2)
polygon(c(3, d6.sims.ordered[c(1:20, 20:1)], 3), 
        c(d6.sensitivity[1:21, 2], d6.sensitivity[21:1, 3]),
        col = rgb(1,0,0,0.3),
        border = "red")
polygon(c(3, d6.sims.ordered[c(1:20, 20:1)], 3), 
        c(d6.sensitivity[1:21, 5], d6.sensitivity[21:1, 6]),
        col = rgb(0,0,1,0.3),
        border = "blue")
lines(c(3, d6.sims.ordered[1:20]), d6.sensitivity[1:21, 1], col = "red", lwd = 2)
lines(c(3, d6.sims.ordered[1:20]), d6.sensitivity[1:21, 4], col = "blue", lwd = 2)
dev.off()


#===============================================================================
# STUDY 6 GRAPH
#===============================================================================
d6.pron.graph <- sapply(unique(d6$name), 
                        function(x) {mean(d6.fluency$disfluency[d6.fluency$name == x])})
d6.harm.graph <- sapply(unique(d6$name), function(x) {mean(d6$harmful[d6$name == x])})
d6.nov.graph <- sapply(unique(d6$name), function(x) {mean(d6.novelty$novel[d6.novelty$name == x])})
d6.old.graph <- unique(d6$name) %in% d6.old.additives


tiff("Fig3.tif", width = 2000, height = 3040, res = 300)

par(mfrow = c(2,1), las = 1, mar = c(1, 4.5, 3, 1) + 0.1, oma = c(4,0,0,0), cex = 1.4)
plot(d6.pron.graph, 
     d6.harm.graph, 
     pch = ifelse(!d6.old.graph, 4, 19), 
     col = ifelse(!d6.old.graph, "grey50", "black"),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1, 7),
     ylim = c(3, 5.2),
     xaxs = "i", yaxs = "i",
     lwd = 3
)
regline(lm(d6.harm.graph[!d6.old.graph] ~ d6.pron.graph[!d6.old.graph]), lwd = 4,
        col = "grey50")
regline(lm(d6.harm.graph[d6.old.graph] ~ d6.pron.graph[d6.old.graph]), 
        col = "black", lwd = 4)
mtext("Harmfulness", 3, line = 0.5, cex = 1.8)
mtext("Harmfulness rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

par(mar = c(1, 4.5, 3, 1) + 0.1)
plot(d6.pron.graph, 
     d6.nov.graph, 
     pch = ifelse(!d6.old.graph, 4, 19), 
     col = ifelse(!d6.old.graph, "grey50", "black"),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1, 7),
     ylim = c(3, 5.2),
     xaxs = "i", yaxs = "i",
     lwd = 3
)
regline(lm(d6.nov.graph[!d6.old.graph] ~ d6.pron.graph[!d6.old.graph]), lwd = 4,
        col = "grey50")
regline(lm(d6.nov.graph[d6.old.graph] ~ d6.pron.graph[d6.old.graph]), 
        col = "black", lwd = 4)
mtext("Novelty", 3, line = 0.5, cex = 1.8)
mtext("Novelty rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

mtext("Average pronounceability rating (scale 1-7)", 1, line = 3, cex = 1.5)

dev.off()


#===============================================================================
# STUDY 5 AND 6 INTERACTION META-ANALYSIS
#===============================================================================
s5.t <- summary(m5a)$coefficients[6, "t value"]
s5.df <- summary(m5a)$coefficients[6, "df"]
s6.t <- summary(m6a)$coefficients[4, "t value"]
s6.df <- summary(m6a)$coefficients[4, "df"]

compute.r <- function(t, df) sqrt(t*t / (t*t + df))
metacor(cor = c(compute.r(s5.t, s5.df), compute.r(s6.t, s6.df)), 
        n = c(s5.df + 1, s6.df + 1))


#===============================================================================
# STUDY 7
#===============================================================================
# data load
#----------
d7.wide <- read.table("s7_data.txt", sep = "\t", head = T)
d7.names <- unlist(read.table("s7_names.txt", as.is = T))
d7.pron <- 8 - colMeans(read.table("s7_pronounceability.txt", sep = "\t", head = T), na.rm = T)
d7.length <- nchar(d7.names)

nrow(d7.wide) # N before exclusions

# exclusions
#-----------
exclude <- (
  apply(d7.wide[,c(2:207, 620:825)], 1, function(x) {
    max(table(x[!is.na(x)]), na.rm = T) > 10 | sum(!is.na(x)) < 11
  }) | 
    apply(d7.wide[,c(208:413, 414:619)], 1, function(x) {
      max(table(x[!is.na(x)]), na.rm = T) > 10 | sum(!is.na(x)) < 11
    })
)
sum(exclude) # number of excluded
d7.wide <- d7.wide[!exclude,]
nrow(d7.wide) # N after exclusions

# to a long format
#-----------------
d7 <- matrix(0, ncol = 5, nrow = 22*nrow(d7.wide))
i <- 1
for(row in 1:nrow(d7.wide)) {
  for(col in 2:ncol(d7.wide)) {
    val <- d7.wide[row, col]
    if(!is.na(val)) {
      cond <- (col-2)%/%206
      d7[i,] <- c(row, 
                  ((col-1) %% 206)+1, 
                  val, 
                  (cond %/% 2) - 0.5, 
                  (((cond+1) %/% 2) %% 2) - 0.5
      )
      i <- i+1
    }
  }
}
d7 <- data.frame(d7, stringsAsFactors = F)
colnames(d7) <- c("id", "word", "rating", "order", "scenario")
# order: 0.5 - sick first, -0.5 - adventure first
# scenario: 0.5 - sick, -0.5 - adventure

d7$word <- d7.names[d7$word]
d7$length <- nchar(d7$word)
d7$id <- factor(d7$id)
d7$length.fac <- factor(d7$length)
d7$length <- (d7$length - 9.5)/7
d7$pronounceability <- d7.pron[d7$word] - mean(d7.pron[d7$word])
d7$old <- d7$word %in% d7.names[201:206]
d7$first <- xor(d7$order > 0, d7$scenario > 0)

# main analysis
#--------------
m7 <- lmer(rating ~ scenario*pronounceability + scenario*order + pronounceability*length +
             (length + scenario + pronounceability|id) + (1|word), 
           data = d7[!d7$old,])
summary(m7)
results(m7, 2, "linear") # scenario
results(m7, 3, "linear") # pronounceability
results(m7, 4, "linear") # order
results(m7, 5, "linear") # length
results(m7, 6, "linear") # scenario:pronounceability
results(m7, 7, "linear") # scenario:order
results(m7, 8, "linear") # pronounceability:length

# supplementary analysis
#-----------------------
# the models separately for first and second scenarios (to check the order:scenario interaction)
m7.first <- lmer(rating ~ scenario*pronounceability + pronounceability*length +
                   (pronounceability + length|id) + (1|word),
                 data = d7[!d7$old & d7$first,])
summary(m7.first)
m7.second <- lmer(rating ~ scenario*pronounceability + pronounceability*length +
                    (pronounceability + length|id) + (1|word),
                  data = d7[!d7$old & !d7$first,])
summary(m7.second)

# added scenario:length interaction
m7b <- lmer(rating ~ scenario*pronounceability + scenario*order + pronounceability*length + scenario*length +
              (length + scenario|id) + (1|word), # removed pronounceability slope for better convergence
            data = d7[!d7$old,])
summary(m7b)
results(m7b, 6, "linear")

# correlation of word length and pronounceability
#------------------------------------------------
p.cor(d7.pron, d7.length)

# more supplementary analyses
#----------------------------
# undesirable risk first scenario only
m7.first.risk <- lmer(rating ~ pronounceability*length + 
                        (pronounceability + length|id) + (1|word),
                      data = d7[!d7$old & d7$first & d7$scenario > 0,])
summary(m7.first.risk)
results(m7.first.risk, 2, "linear") # pronounceability
results(m7.first.risk, 3, "linear") # length

# desirable risk (adventure) first scenario only
m7.first.adv <- lmer(rating ~ pronounceability*length + 
                       (pronounceability + length|id) + (1|word),
                     data = d7[!d7$old & d7$first & d7$scenario < 0,])
summary(m7.first.adv)
results(m7.first.adv, 2, "linear") # pronounceability
results(m7.first.adv, 3, "linear") # length

# adding pronounceability and length to simpler models
#-----------------------------------------------------
comparison <- function(model) {
  cat("Chi2(", model$`Chi Df`[2], ") = ", sprintf("%.2f", model$Chisq[2]), ", ", 
      round_p(model$`Pr(>Chisq)`[2]), sep = "")
}

# adding length to a model / undesirable risk
m7.first.risk.pron <- lmer(rating ~ pronounceability + (pronounceability|id) + (1|word),
                           data = d7[!d7$old & d7$first & d7$scenario > 0,])
m7.first.risk.pron.len <- lmer(rating ~ pronounceability + length + (pronounceability|id) + (1|word),
                               data = d7[!d7$old & d7$first & d7$scenario > 0,])
m7.risk.len.added <- anova(m7.first.risk.pron.len, m7.first.risk.pron)
comparison(m7.risk.len.added)

# adding pronounceability to a model / undesirable risk
m7.first.risk.len <- lmer(rating ~ length + (length|id) + (1|word),
                          data = d7[!d7$old & d7$first & d7$scenario > 0,])
m7.first.risk.len.pron <- lmer(rating ~ length + pronounceability + (length|id) + (1|word),
                               data = d7[!d7$old & d7$first & d7$scenario > 0,])
m7.risk.pron.added <- anova(m7.first.risk.len.pron, m7.first.risk.len)
comparison(m7.risk.pron.added)

# adding length to a model / desirable risk
m7.first.adv.pron <- lmer(rating ~ pronounceability + (pronounceability|id) + (1|word),
                          data = d7[!d7$old & d7$first & d7$scenario < 0,])
m7.first.adv.pron.len <- lmer(rating ~ pronounceability + length + (pronounceability|id) + (1|word),
                              data = d7[!d7$old & d7$first & d7$scenario < 0,])
m7.adv.len.added <- anova(m7.first.adv.pron.len, m7.first.adv.pron)
comparison(m7.adv.len.added)

# adding pronounceability to a model / desirable risk
m7.first.adv.len <- lmer(rating ~ length + (length|id) + (1|word),
                         data = d7[!d7$old & d7$first & d7$scenario < 0,])
m7.first.adv.len.pron <- lmer(rating ~ length + pronounceability + (length|id) + (1|word),
                              data = d7[!d7$old & d7$first & d7$scenario < 0,])
m7.adv.pron.added <- anova(m7.first.adv.len.pron, m7.first.adv.len)
comparison(m7.adv.pron.added)

# just pronounceability withouth length
#--------------------------------------
# undesirable risk first scenario only
m7.first.risk.pron <- lmer(rating ~ pronounceability + (pronounceability|id) + (1|word),
                           data = d7[!d7$old & d7$first & d7$scenario > 0,])
summary(m7.first.risk.pron)
results(m7.first.risk.pron, 2, "linear") # pronounceability

# desirable risk (adventure) first scenario only
m7.first.adv.pron <- lmer(rating ~ pronounceability + (pronounceability|id) + (1|word),
                          data = d7[!d7$old & d7$first & d7$scenario < 0,])
summary(m7.first.adv.pron)
results(m7.first.adv.pron, 2, "linear") # pronounceability

# pronoucneability slopes separately for all lengths
#---------------------------------------------------
# undesirable risk
m7.first.risk.slopes <- lmer(rating ~ pronounceability:length.fac + length +
                               (length|id) + (1|word),
                             data = d7[!d7$old & d7$first & d7$scenario > 0,])
summary(m7.first.risk.slopes)

# desirable risk (adventure)
m7.first.adv.slopes <- lmer(rating ~ pronounceability:length.fac + length +
                              (length|id) + (1|word),
                            data = d7[!d7$old & d7$first & d7$scenario < 0,])
summary(m7.first.adv.slopes)

sink("s7_pronounceability_effects.txt")
cat("Risk")
for(i in 3:10) {
  cat("\n", i+3, ":\t", sep = "")
  results(m7.first.risk.slopes, i, family = "linear")
}
cat("\n\nAdventurousness")
for(i in 3:10) {
  cat("\n", i+3, ":\t", sep = "")
  results(m7.first.adv.slopes, i, family = "linear")
}
sink()

#original names
#--------------
round(sapply(d7.names[201:206], function(x) mean(d7$rating[d7$word == x])), 2)
round(sapply(d7.names[201:206], function(x) length(d7$rating[d7$word == x])), 2)
round(sapply(d7.names[201:206], function(x) mean(d7$rating[d7$word == x & d7$scenario > 0])), 2)
round(sapply(d7.names[201:206], function(x) mean(d7$rating[d7$word == x & d7$scenario < 0])), 2)
round(d7.pron[201:206], 2)
s7.original <- data.frame(name = d7.names[201:206],
                          length = nchar(d7.names[201:206]),
                          pronounceability = round(d7.pron[201:206], 2),
                          M_ratings = round(sapply(d7.names[201:206], 
                                                   function(x) mean(d7$rating[d7$word == x])), 2),
                          SE_ratings = round(sapply(d7.names[201:206], function(x) sd(d7$rating[d7$word == x])) /
                                               sqrt(sapply(d7.names[201:206], function(x) length(d7$rating[d7$word == x]))), 2),
                          M_risk = round(sapply(d7.names[201:206], 
                                                function(x) mean(d7$rating[d7$word == x & d7$scenario > 0])), 2),
                          SE_risk = round(sapply(d7.names[201:206], 
                                                 function(x) sd(d7$rating[d7$word == x & d7$scenario > 0])) /
                                            sqrt(sapply(d7.names[201:206], 
                                                        function(x) length(d7$rating[d7$word == x & d7$scenario > 0]))), 2),
                          M_adventure = round(sapply(d7.names[201:206], 
                                                     function(x) mean(d7$rating[d7$word == x & d7$scenario > 0])), 2),
                          SE_adventure = round(sapply(d7.names[201:206], 
                                                      function(x) sd(d7$rating[d7$word == x & d7$scenario > 0])) /
                                                 sqrt(sapply(d7.names[201:206], 
                                                             function(x) length(d7$rating[d7$word == x & d7$scenario > 0]))), 2)
)
write.table(s7.original, "s7_original_descriptives.txt", row.names = F, quote = F, sep = "\t")


#===============================================================================
# STUDY 7 GRAPH
#===============================================================================
d7.risk <- sapply(d7.names, function(x) {mean(d7$rating[d7$word == x & d7$scenario > 0])})
d7.adv <- sapply(d7.names, function(x) {mean(d7$rating[d7$word == x & d7$scenario < 0])})
d7.old <- c(rep(F, 200), rep(T, 6))

tiff("Fig4.tif", width = 4000, height = 3040, res = 300)
par(mfcol = c(2,2), las = 1, mar = c(1.5, 4.5, 3, 0) + 0.1, oma = c(4,0,0,0), cex = 1.4)

plot(d7.pron, 
     d7.risk, 
     pch = 19, 
     col = rgb(0, 1-(d7.length-6)/7, (d7.length-6)/7, 0.5),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1, 7),
     ylim = c(2.5, 6),
     xaxs = "i", yaxs = "i"
)
for(i in 6:13) {
  regline(lm(d7.risk[d7.length == i] ~ d7.pron[d7.length == i]), 
          col = rgb(0, 1-(i-6)/7, (i-6)/7), lwd = 4)  
}
mtext("Riskiness rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

par(mar = c(0.5, 4.5, 3, 0) + 0.1)
plot(d7.pron, 
     d7.adv, 
     pch = 19, 
     col = rgb(0, 1-(d7.length-6)/7, (d7.length-6)/7, 0.5),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(1, 7),
     ylim = c(2.5, 6),
     xaxs = "i", yaxs = "i"
)
for(i in 6:13) {
  regline(lm(d7.adv[d7.length == i] ~ d7.pron[d7.length == i]), 
          col = rgb(0, 1-(i-6)/7, (i-6)/7), lwd = 4)  
}
mtext("Adventurousness rating (scale 1-7)", 2, line = 3, cex = 1.65, las = 0)

mtext("Average pronounceability rating (scale 1-7)", 1, line = 3, cex = 1.5)

par(mar = c(1.5, 3.5, 3, 1) + 0.1)
plot(d7.length + (d7.pron-4)/6, 
     d7.risk, 
     pch = 19, 
     col = rgb((d7.pron-1)/6, 0, 1-(d7.pron-1)/6, 0.5),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(5.5, 13.5),
     ylim = c(2.5, 6),
     xaxs = "i", yaxs = "i"
)
for(i in 7:13) {
  abline(v = i-0.5, lty = 3)
}
points(6:13, sapply(6:13, function(x) mean(d7.risk[d7.length == x])), pch = 3, lwd = 4)
axis(1, 6:13)

par(mar = c(0.5, 3.5, 3, 1) + 0.1)
plot(d7.length + (d7.pron-4)/6,
     d7.adv, 
     pch = 19, 
     col = rgb((d7.pron-1)/6, 0, 1-(d7.pron-1)/6, 0.5),
     main = "",
     xlab = "",
     ylab = "",
     xlim = c(5.5, 13.5),
     ylim = c(2.5, 6),
     xaxs = "i", yaxs = "i"
)
for(i in 7:13) {
  abline(v = i-0.5, lty = 3)
}
points(6:13, sapply(6:13, function(x) mean(d7.adv[d7.length == x])), pch = 3, lwd = 4)
axis(1, 6:13)

mtext("Name length", 1, line = 3, cex = 1.5)

mtext("Desirable risk", side = 3, line = -18.25, outer = TRUE, cex = 1.8)
mtext("Undesirable risk", side = 3, line = -2.25, outer = TRUE, cex = 1.8)

dev.off()