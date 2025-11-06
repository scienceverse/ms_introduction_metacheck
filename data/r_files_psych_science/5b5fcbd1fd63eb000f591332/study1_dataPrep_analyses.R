## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 1: Data preparation and analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("corrplot")) {install.packages("corrplot"); require("corrplot")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("GGally")) {install.packages("GGally"); require("GGally")}
if (!require("smacof")) {install.packages("smacof"); require("smacof")}
if (!require("vegan")) {install.packages("vegan"); require("vegan")}
if (!require("ggfortify")) install.packages("ggfortify"); require("ggfortify")
if (!require("ggrepel")) {install.packages("ggrepel"); require("ggrepel")}
if (!require("grid")) {install.packages("grid"); require("grid")}
if (!require("smacof")) {install.packages("smacof"); require("smacof")}
if (!require("ggdendro")) {install.packages("ggdendro"); require("ggdendro")}
if (!require("quanteda")) {install.packages("quanteda"); require("quanteda")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("boot")) {install.packages("boot"); require("boot")}


## Data preparation
## ----------------------------------------------------------------------------------------------------------- ##

## Load data
load("study1_rawDat.Rda")

## Convert StartDate and EndDate to date format
study1_rawDat$StartDate <- as.POSIXct(study1_rawDat$StartDate, format = "%Y-%m-%d %H:%M:%S")
study1_rawDat$EndDate <- as.POSIXct(study1_rawDat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Turn ResponseId and workerID into character format
study1_rawDat$ResponseId <- as.character(study1_rawDat$ResponseId)
study1_rawDat$workerID <-as.character(study1_rawDat$workerID)

## Change ResponseID to responseID
colnames(study1_rawDat)[3] <- "responseID"

## Label levels in cond
levels(study1_rawDat$cond)

study1_rawDat$cond <- factor(x = study1_rawDat$cond, 
                             levels = c("1", "2"), 
                             labels = c("Man-Woman", "Woman-Man"))

## Turn gender into factor
study1_rawDat$gender <- factor(x = study1_rawDat$gender, 
                               levels = c("1", "2"), 
                               labels = c("Male", "Female"))

## Turn education into factor
study1_rawDat$educ <- factor(x = study1_rawDat$educ, 
                             levels = c("4", "5", "6", "7", "8", "9", "10", "11", "12" ,"13"), 
                             labels = c("HS Grad", "Some College", "Associate's", "Bachelor's", "Some Grad School", 
                                        "Master's", "MBA", "JD", "MD", "PhD"))

## Turn bmonth and byear into numeric
study1_rawDat$bmonth <- as.numeric(levels(study1_rawDat$bmonth))[study1_rawDat$bmonth]
study1_rawDat$byear <- as.numeric(levels(study1_rawDat$byear))[study1_rawDat$byear]

## Turn politicalid into factor
study1_rawDat$politicalid <- factor(x = study1_rawDat$politicalid, 
                          levels = c("1", "2", "3", "4", "5", "6", "7"), 
                          labels = c("Strongly Conservative", 
                                     "Moderately Conservative", 
                                     "Slightly Conservative", 
                                     "Neutral", 
                                     "Slightly Liberal", 
                                     "Moderately Liberal", 
                                     "Strongly Liberal"))

## Create birthday and calculate age
study1_rawDat$birthday <- paste(study1_rawDat$bmonth, 
                                "1",          
                                study1_rawDat$byear, 
                                sep = "/")

study1_rawDat$birthday <- as.POSIXct(study1_rawDat$birthday, 
                                     format = "%m/%d/%Y")

study1_rawDat$Date.of.Completion <- sapply(X = strsplit(x = as.character(study1_rawDat$StartDate), split = " "), 
                                           "[", 
                                           1)

study1_rawDat$Date.of.Completion <- as.POSIXct(study1_rawDat$Date.of.Completion, 
                                     format = "%Y-%m-%d")

study1_rawDat$age <- with(study1_rawDat, (Date.of.Completion - birthday)/365)

study1_rawDat$age <- as.numeric(study1_rawDat$age)

## Rearrange columns
colnames(study1_rawDat)

study1_rawDat <- study1_rawDat[,c("responseID", "workerID", "StartDate", "EndDate", "Date.of.Completion", 
                                  "birthday", "bmonth", "byear", "age", "gender", "educ", "politicalid",
                                  "cond",
                                  "c1.query", "c1.intelligent", "c1.accurate", "c1.fair", "c1.just", 
                                  "c2.query", "c2.intelligent", "c2.accurate", "c2.fair", "c2.just", 
                                  "c1.freeResponse", "c2.freeResponse")]

## Turn main DVs into numeric
study1_rawDat$c1.query <- as.numeric(levels(study1_rawDat$c1.query))[study1_rawDat$c1.query]
study1_rawDat$c2.query <- as.numeric(levels(study1_rawDat$c2.query))[study1_rawDat$c2.query]

study1_rawDat$c1.intelligent <- as.numeric(levels(study1_rawDat$c1.intelligent))[study1_rawDat$c1.intelligent]
study1_rawDat$c2.intelligent <- as.numeric(levels(study1_rawDat$c2.intelligent))[study1_rawDat$c2.intelligent]

study1_rawDat$c1.accurate <- as.numeric(levels(study1_rawDat$c1.accurate))[study1_rawDat$c1.accurate]
study1_rawDat$c2.accurate <- as.numeric(levels(study1_rawDat$c2.accurate))[study1_rawDat$c2.accurate]

study1_rawDat$c1.fair <- as.numeric(levels(study1_rawDat$c1.fair))[study1_rawDat$c1.fair]
study1_rawDat$c2.fair <- as.numeric(levels(study1_rawDat$c2.fair))[study1_rawDat$c2.fair]

study1_rawDat$c1.just <- as.numeric(levels(study1_rawDat$c1.just))[study1_rawDat$c1.just]
study1_rawDat$c2.just <- as.numeric(levels(study1_rawDat$c2.just))[study1_rawDat$c2.just]

## Collapse responses
levels(study1_rawDat$cond)

study1_rawDat$query <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                              yes = study1_rawDat$c1.query, 
                              no = study1_rawDat$c2.query)

study1_rawDat$intelligent <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                                    yes = study1_rawDat$c1.intelligent, 
                                    no = study1_rawDat$c2.intelligent)

study1_rawDat$accurate <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                                 yes = study1_rawDat$c1.accurate, 
                                 no = study1_rawDat$c2.accurate)

study1_rawDat$fair <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                             yes = study1_rawDat$c1.fair, 
                             no = study1_rawDat$c2.fair)

study1_rawDat$just <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                             yes = study1_rawDat$c1.just, 
                             no = study1_rawDat$c2.just)

## Collapse comments
study1_rawDat$c1.freeResponse <- as.character(study1_rawDat$c1.freeResponse)
study1_rawDat$c2.freeResponse <- as.character(study1_rawDat$c2.freeResponse)

study1_rawDat$comment <- ifelse(test = study1_rawDat$cond == "Man-Woman", 
                                yes = study1_rawDat$c1.freeResponse, 
                                no = study1_rawDat$c2.freeResponse)

## Rename rows, 1-199
rownames(study1_rawDat) <- c(1:nrow(study1_rawDat))

## For comparison to Supplemental Study on Other Professions
save(study1_rawDat, file = "doc.surgery.PersonX.Rda")

## =============================================================================================================== ##
## Sample characteristics
  ## N = 199
count(study1_rawDat$gender)
  ## 95 males, 104 females
round(x = mean(study1_rawDat$age), digits = 2)
round(x = sd(study1_rawDat$age), digits = 2)
  ## Mean age = 35.62 years, SD = 12.16 years

## =============================================================================================================== ##
## query: which statement do participants agree with?
count(study1_rawDat$query)
  185/199 ## 93% agree that both man and woman are equally likely to be a doctor
  14/199  ## 7% agree that man is more likely to be a doctor

## =============================================================================================================== ##
## Four main items of interest: intelligent, accurate, fair, and just

## Create dataset with just composite items
dat.composite <- study1_rawDat[,c("intelligent", "accurate", "fair", "just")]

dat.composite <- melt(dat.composite, measure.vars = c("intelligent", "accurate", "fair", "just"))

levels(dat.composite$variable)
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("fair", "intelligent", "just", "accurate"), 
                                 labels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unintelligent ... 7 = Intelligent", 
                                            "1 = Unjust ... 7 = Just",
                                            "1 = Inaccurate ... 7 = Accurate"))

## Visualize
ggplot(data = dat.composite, 
       aes(x = value)) + 
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  facet_wrap(~variable, nrow = 2) + 
  coord_cartesian(xlim = c(0:8)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), labels = percent_format()) + 
  labs(y = "Proprotion of participants") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_blank()) + 
  theme(panel.spacing = unit(2, "lines")) + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  theme(axis.title.y = element_text(vjust = 2, size = 16)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title.x = element_blank())

## Correlations among items
cor(study1_rawDat[,c("intelligent", "accurate", "fair", "just")])
ggpairs(study1_rawDat[,c("intelligent", "accurate", "fair", "just")])

## Cronbach's alpha
psych::alpha(x = var(study1_rawDat[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.92

## Compute composite average
study1_rawDat$composite.avg <- with(study1_rawDat, (intelligent + accurate + fair + just) / 4)

## Inferential statistics on composite average
  ## Mean
round(x = mean(study1_rawDat$composite.avg), digits = 2)
  ## SE
round(x = std.error(study1_rawDat$composite.avg), digits = 2)

  ## One sample t-test against 4
t.test(x = study1_rawDat$composite.avg, 
       mu = 4)

  ## Cohen's d
round(x = cohensD(x = study1_rawDat$composite.avg, 
                  mu = 4), 
      digits = 2)

  ## 95% CI around Cohen's d
set.seed(123)
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

one.sample.t.cohensD.results <- boot(data = study1_rawDat, 
                                     statistic = one.sample.t.cohensD, 
                                     R = 10000)

hist(one.sample.t.cohensD.results$t)

round(x = quantile(x = one.sample.t.cohensD.results$t, 
                   probs = c(0.025, 0.975)), 
      digits = 2)

## Means and standard errors
round(x = apply(X = study1_rawDat[,c("fair", "just", "accurate", "intelligent")], MARGIN = 2, FUN = mean), 
      digits = 2)

round(x = apply(X = study1_rawDat[,c("fair", "just", "accurate", "intelligent")], MARGIN = 2, FUN = std.error), 
      digits = 2)

## Analyses below are not in the manuscript

## --------------------------------------------------------------------------- ##
## Conduct unfolding
## --------------------------------------------------------------------------- ##

## Create separate dataset
dat.unfolding <- study1_rawDat[,c("intelligent", "accurate", "fair", "just")]

summary(dat.unfolding)
  ## Currently, 1 = dissimilar from attribute
  ## Needs to be 0 = similar to attribute

dat.unfolding$intelligent <- 7 - dat.unfolding$intelligent
dat.unfolding$accurate <- 7 - dat.unfolding$accurate
dat.unfolding$fair <- 7 - dat.unfolding$fair
dat.unfolding$just <- 7 - dat.unfolding$just

summary(dat.unfolding[,c("intelligent", "accurate", "fair", "just")])
  ## 0 = similar
  ## 6 = dissimilar

## Unfolding
fit.unfold <- unfolding(delta = dat.unfolding,      ## Data
                        ndim = 2,                   ## 2D solution
                        circle = "none",            ## No restrictions on row or column configurations
                        verbose = TRUE)

fit.unfold
  ## Acceptable stress-1 value of 0.11

## Set random seed to reproduce figures (repel feature)
set.seed(seed = 123)

## Merge subject configurations (i.e,. placement on D1 and D2 with dat)
subj.conf <- as.data.frame(fit.unfold$conf.row)
subj.conf$subject <- c(1:199)
study1_rawDat$subject = c(1:199)

study1_rawDat <- merge(x = study1_rawDat, y = subj.conf, 
                       by = "subject")

plot_unfold <- ggplot() + 
  geom_label_repel(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                                   levels = c("intelligent", "accurate", "fair", "just"), 
                                                   labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                                     D1 = fit.unfold$conf.col[,1], 
                                     D2 = fit.unfold$conf.col[,2]), 
                   mapping = aes(x = D1, y = D2, label = item), size = 5) +
  geom_point(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                             levels = c("intelligent", "accurate", "fair", "just"), 
                                             labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                               D1 = fit.unfold$conf.col[,1], 
                               D2 = fit.unfold$conf.col[,2]), 
             mapping = aes(x = D1, y = D2)) + 
  geom_point(data = study1_rawDat, 
             mapping = aes(x = D1, y = D2, color = composite.avg)) + 
  geom_text_repel(data = study1_rawDat, 
                  mapping = aes(x = D1, y = D2, label = subject, color = composite.avg)) + 
  scale_color_continuous(low = "red", high = "green", 
                         name = "Composite Average\n", 
                         breaks = c(1:7), 
                         labels = c("1: Inaccurate, Unintelligent, Unfair, Unjust", 
                                    "2", "3", "4", "5", "6", 
                                    "7: Accurate, Intelligent, Fair, Just")) + 
  coord_cartesian(xlim = c(-6, 4), ylim = c(-5, 5)) + 
  labs(x = "Dimension 1", y = "Dimension 2") + 
  scale_x_continuous(breaks = seq(-6, 4, 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, 1)) + 
  theme_bw(base_size = 16) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(legend.position = c(0.95, 0.95), legend.justification = c(0.95, 0.95),
        legend.title.align = 0.5) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

plot_unfold

## ============================================================================================================ ##
## Conduct cluster analysis on 199 subjects
  ## Obtain dissimilarity matrix among item (subject) configurations
subject.dissim <- dist(x = fit.unfold$conf.row, 
                       method = "euclidean")

## Conduct cluster analysis
subject.cluster.analysis <- hclust(d = subject.dissim, method = "ward.D2")

## Dendrogram
plot(subject.cluster.analysis, cex = 0.7, main = "Dendrogram",  xlab = "Items", hang = -1)

## Specify 3 clusters in red
rect.hclust(tree = subject.cluster.analysis, k = 3, border = "red")

## Assign each subject to a cluster
subject.clusters <- as.data.frame(cutree(tree = subject.cluster.analysis, k = 3))
subject.clusters$subject <- row.names(subject.clusters)
colnames(subject.clusters)[1] <- "subject.cluster"

## Merge with dat
study1_rawDat <- merge(x = study1_rawDat, y = subject.clusters, 
                       by = "subject")

## Turn cluster into a factor
study1_rawDat$subject.cluster <- factor(study1_rawDat$subject.cluster, 
                                        levels = c(1:3), 
                                        labels = c("a", "b", "c"))

## Relevel subject cluster
study1_rawDat$subject.cluster <- factor(study1_rawDat$subject.cluster, 
                                        levels = c("a", "b", "c"), 
                                        labels = c("Higher on all 4 items", 
                                                   "Lower on all 4 items",
                                                   "More accurate/intelligent than fair/just"))


## Visualize unfolding, color-coded by subject cluster
## This takes a few seconds to render; then zoom to see
ggplot() + 
  geom_label_repel(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                                   levels = c("intelligent", "accurate", "fair", "just"), 
                                                   labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                                     D1 = fit.unfold$conf.col[,1], 
                                     D2 = fit.unfold$conf.col[,2]), 
                   aes(x = D1, y = D2, label = item), size = 5) +
  geom_point(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                             levels = c("intelligent", "accurate", "fair", "just"), 
                                             labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                               D1 = fit.unfold$conf.col[,1], 
                               D2 = fit.unfold$conf.col[,2]), 
             aes(x = D1, y = D2)) + 
  geom_point(data = study1_rawDat,
             aes(x = D1, y = D2, color = subject.cluster)) + 
  geom_text_repel(data = study1_rawDat,
                  aes(x = D1, y = D2, label = subject, color = subject.cluster), 
                  show.legend = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 5, 
                                                  shape = c(rep(15, 3))))) +     ## change 3 to number of clusters
  scale_shape_discrete(solid = FALSE) + 
  scale_color_discrete(name = "Cluster\n") + 
  coord_cartesian(xlim = c(-6, 4), ylim = c(-5, 5)) + 
  labs(x = "Dimension 1", y = "Dimension 2") + 
  scale_x_continuous(breaks = seq(-6, 4, 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, 1)) + 
  theme_bw(base_size = 16) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(legend.position = c(0.95, 0.95), legend.justification = c(0.95, 0.95),
        legend.title.align = 0.5) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

## Sampling of comments
comments.dat <- study1_rawDat[,c("comment", "age", "gender", "educ", "composite.avg")]
comments.dat[117,]
comments.dat[172,]    
comments.dat[141,]    
comments.dat[97,]    
comments.dat[89,]    
comments.dat[35,]     
comments.dat[196,]    
comments.dat[199,]    
comments.dat[185,]   
comments.dat[24,]

## ================================================================== ##

## Package needed
if (!require("ggalt")) {install.packages("ggalt"); require("ggalt")}

## Higher in accuracy and intelligence than fairness and justice
highAcc.lowFair <- subset(study1_rawDat, 
                          subject.cluster == "More accurate/intelligent than fair/just")

ggplot() + 
  geom_label_repel(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                                   levels = c("intelligent", "accurate", "fair", "just"), 
                                                   labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                                     D1 = fit.unfold$conf.col[,1], 
                                     D2 = fit.unfold$conf.col[,2]), 
                   mapping = aes(x = D1, y = D2, label = item), size = 5) +
  geom_point(data = data.frame(item = factor(row.names(fit.unfold$conf.col), 
                                             levels = c("intelligent", "accurate", "fair", "just"), 
                                             labels = c("Intelligent", "Accurate", "Fair", "Just")), 
                               D1 = fit.unfold$conf.col[,1], 
                               D2 = fit.unfold$conf.col[,2]), 
             mapping = aes(x = D1, y = D2)) + 
  geom_point(data = study1_rawDat, 
             mapping = aes(x = D1, y = D2
                           , 
                           color = composite.avg
             )) + 
  geom_text_repel(data = study1_rawDat, 
                  mapping = aes(x = D1, y = D2, label = subject
                                , 
                                color = composite.avg
                  )) + 
  geom_encircle(data = highAcc.lowFair, 
                mapping = aes(x = D1, y = D2), lwd = 8) + 
  coord_cartesian(xlim = c(-6, 4), ylim = c(-5, 5)) + 
  labs(x = "Dimension 1", y = "Dimension 2") + 
  scale_x_continuous(breaks = seq(-6, 4, 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, 1)) + 
  theme_bw(base_size = 16) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(legend.direction = "horizontal") + 
  scale_color_continuous(low = "red", high = "green", 
                         breaks = c(1:7), 
                         labels = c("1\nUnfair\nUnjust\nInaccurate\nUnintelligent", 
                                    " ", " ", " ", " ", " ", 
                                    "7\nFair\nJust\nAccurate\nIntelligent")
                         , 
                         guide = guide_colorbar(title.position = "top",
                                                title = "Composite\nAverage"
                                                , 
                                                title.hjust = 0.5
                         )) + 
  theme(legend.position = c(0.90, 0.95), legend.justification = c(0.90, 0.95)) + 
  theme(legend.title.align = 0.5) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

## ================================================================== ##
## ================================================================== ##
## Text analysis

## Create corpus from comments
comments_corpus <- corpus(x = study1_rawDat$comment)
comments_corpus
  ## 199 documents

## Get summary
summary <- summary(object = comments_corpus, n = 199)
head(summary)

## Add Types, Tokens, and Sentences to study1_rawDat
study1_rawDat$Types <- summary$Types
study1_rawDat$Tokens <- summary$Tokens
study1_rawDat$Sentences <- summary$Sentences

## Look at specific texts
texts(x = comments_corpus)[1] ## Look at first comment; tokens include periods and other punctuation characters (e.g., ampersand)

## Look at a key word in context (kwic)
kwic(x = comments_corpus, 
     keywords = "sexist", 
     window = 5)

## Create document feature matrix (DFM); aka document term matrix (DTM)
comments_dfm <- dfm(x = comments_corpus, 
                    tolower = TRUE, 
                    stem = FALSE, 
                    remove = c(stopwords(kind = "english"), 
                               "what", "impression", 
                               "do", "you", "have", "of", 
                               "person", "X", 'and/or', "and", "or", 
                               "the", "statement", "made"),             ## probably want to add other words, from question stem
                    ngrams = 1,                                         ## Columns/variables are only single words; can do  = c(1, 2) to keep unigrams and bigrams
                    remove_punct=TRUE, 
                    remove_hyphens=FALSE, 
                    remove_numbers=TRUE)

dim(comments_dfm)
  ## 199 rows -- one row/comment by a subject
  ## 545 words

## First 5 rows and columns
comments_dfm[1:5, 1:5]

## Calculate word counts
total_WCs <- colSums(x = comments_dfm) ## Add up the values in each column
total_WCs
sort(total_WCs, decreasing = TRUE)

## Most frequently mentioned words
topfeatures(x = comments_dfm, n = 10, decreasing = TRUE) ## Most frequently mentioned words
topfeatures(x = comments_dfm, n = 10, decreasing = TRUE)/sum(total_WCs) * 100
  ## sexist comprises 3.40% percent of all words used

## ================================================================== ##
## ================================================================== ##
## Save Study 1 data for comparison to Studies 3 and 4

colnames(study1_rawDat)
study1.dat <- study1_rawDat[,c("responseID", 
                               "fair", "just", "accurate", "intelligent", 
                               "query")]

study1.dat$study <- "Study 1"
study1.dat$study <- factor(x = study1.dat$study, 
                           levels = c("Study 1"), 
                           labels = c("Study 1\nn = 199"))

save(study1.dat, file = "study1.dat.Rda")


