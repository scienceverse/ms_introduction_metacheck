################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Replication of Centerbar, Schnall, Clore, & Garvin (2008, JPSP, Study 5)
#@ Coder name: Milan Valasek
#@ Coder e-mail: mivalek@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: none mentioned by replicators
#@ OSF link project: https://osf.io/l8srm/
#@ OSF link replication report: https://osf.io/wcgx5/
#
##@@ REQUIRED PACKAGES @@##
library("car")
library("foreign")
library("heplots")
library("httr")
library("RCurl")

source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

# print() for Anova.mlm objects makes it impossible to access the individual
# values in the printed table. This is an edited version of print.Anova.mlm
# that allows for it.
print2 <- function (x, ...) {
  if ((!is.null(x$singular)) && x$singular) 
    stop("singular error SSP matrix; multivariate tests unavailable\ntry summary(
         object, multivariate=FALSE)")
  test <- x$test
  repeated <- x$repeated
  ntests <- length(x$terms)
  tests <- matrix(NA, ntests, 4)
  if (!repeated) 
    SSPE.qr <- qr(x$SSPE)
  for (term in 1:ntests) {
    eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr, 
                             x$SSP[[term]]), symmetric = FALSE)$values)
    tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(
      eigs, x$df[term], x$error.df), Wilks = Wilks(eigs, x$df[term], x$error.df),
      `Hotelling-Lawley` = HL(eigs, x$df[term], x$error.df),
      Roy = Roy(eigs, x$df[term], x$error.df))
  }
  ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
  ok <- !is.na(ok) & ok
  tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3], 
                                 tests[ok, 4], lower.tail = FALSE))
  rownames(tests) <- x$terms
  colnames(tests) <- c("Df", "test stat", "approx F", "num Df", 
                       "den Df", "Pr(>F)")
  tests <- structure(as.data.frame(tests), heading = paste(
    "\nType ", x$type, if (repeated) " Repeated Measures", " MANOVA Tests: ",
    test, " test statistic", sep = ""
    ), class = c("anova", "data.frame"))
  return(tests)
  invisible(x)
}

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile("https://osf.io/nwbpd/")
df <- read.spss(file, to.data.frame = TRUE)

  ##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL differences between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.

  # select relevant variables: participant number,
  # condition (coherent vs incoherent)
  # number of causal words in story 1, 2, and 3
  df <- df[ ,c(1,9,112:114)]
  names(df) <- c("subject","aff_coh", "Story_1","Story_2","Story_3")

  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or summary call on the resulting object
  # build between-group multivariate model
  # SPSS uses MANOVA for repeated measures ANOVA
  rm_model <- lm(cbind(Story_1, Story_2, Story_3) ~ aff_coh, df)

  # idata with within-subject factor
  rm_idata <- data.frame(factor(c(1,2,3),labels = c("Story_1",
                                                    "Story_2",
                                                    "Story_3")))
  names(rm_idata) <- "Story"

  # final model
  m1 <- Anova(rm_model, idata = rm_idata, idesign = ~ Story)

  # results
  print(m1)


  ##@@ STATISTIC @@##
  print2(m1)[4,3]

  ##@@ P-VALUE @@##
  print2(m1)[4,6]

  ##@@ SAMPLE SIZE @@##
  nrow(df)

  ##@@ EFFECT SIZE @@##
  ## Original replicators DO NOT provide effect size
  etasq(m1)[4,1]

  ##@@ AGREEMENT WITH AUTHORS @@##
  TRUE

  #@ Reason disagreement: 
