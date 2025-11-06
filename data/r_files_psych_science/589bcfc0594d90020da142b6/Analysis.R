#### Load Packages ####
library( psych ) # Used to calculate Cronbach's alpha (i.e., scale reliability)
library( asw.cluster ) # Calculates faultlines
library( parallel ) # Helps the faultlines calculations go faster
library( doParallel ) # Helps the faultlines calculations go faster
library( nlme ) # Runs our multilevel models
library( MicroMacroMultilevel ) # Implements the Croon and van Veldhoven micro-macro multilevel approach
source( "micromacro_expanded.R" ) # Extends the micro-macro multilevel output to include info needed to calculate partial R^2 effect sizes
library( xlsx ) # Used to write output to excel files
library( Hmisc ) # Used to put stars on the correlation table

#### Read Data ####
team.data <- read.csv( "Team Data for Faultlines.csv" )
individual.data <- read.csv( "Individual Data for Faultlines.csv" )

#### Standardizing Testosterone Within Sex ####
# Obtain group means and SDs
T.by.sex.stats <- with( individual.data, rbind(tapply( log.testosterone, Gender, mean, na.rm=T ), 
                                               tapply( log.testosterone, Gender, sd, na.rm=T ) ) )
# Calculate Z-scores within Group
individual.data <- within( individual.data, {
  testosterone.by.sex <- ifelse( Gender=="Male",
                                 (log.testosterone - T.by.sex.stats[,'Male'][1])/T.by.sex.stats[,'Male'][2],
                                 (log.testosterone - T.by.sex.stats[,'Female'][1])/T.by.sex.stats[,'Female'][2]
                                 )
})
# Calculating percentage of females as alternate way of considering sex
team.data <- within( team.data, {
  prop.female <- females/team.size
})

#### Calculating Faultlines ####
# Full faultlines
diversity.data <- with( individual.data, data.frame( Ethnicity, Gender, Country, team.id, stringsAsFactors = F ) )
faultlines.list <- faultlines( data=diversity.data, group.par="team.id", attr.type=c("nominal", "nominal", "nominal" ), method="thatcher" )
faultlines.data <- foreach( team=1:(length(faultlines.list)-3), .combine=rbind ) %dopar% {
  with(faultlines.list[[team]], data.frame(team.id=team, fl.value))
}
team.data <- merge( team.data, faultlines.data, by="team.id", all.x=T )

# Calculating faultlines without gender considered (i.e., only nationality and ethnicity), as was requested during the review process
suggested.diversity.data <- with( individual.data, data.frame( Ethnicity, Country, team.id, stringsAsFactors = F ) )
suggested.faultlines.list <- faultlines( data=suggested.diversity.data, group.par="team.id", attr.type=c("nominal", "nominal" ), method="thatcher" )
suggested.faultlines.data <- foreach( team=1:(length(suggested.faultlines.list)-3), .combine=rbind ) %dopar% {
  with(suggested.faultlines.list[[team]], data.frame(team.id=team, fl.value))
}
names( suggested.faultlines.data )[2] <- "fl.value.nosex"
team.data <- merge( team.data, suggested.faultlines.data, by="team.id", all.x=T )

#### Alternate Way to Determine Diversity (Counts) for Supporting Analyses ####
count.diversity.data <- with(individual.data, data.frame(
  team.id=names(tapply(Ethnicity, team.id, function(x) length(unique(x))) ),
  ethnicity.count=as.numeric(tapply(Ethnicity, team.id, function(x) length(unique(x))) ),
  nationality.count=as.numeric(tapply(Country, team.id, function(x) length(unique(x))) )
))
team.data <- merge( team.data, count.diversity.data, by="team.id", all.x=T )

#### Alternate Ways to Quantify Collective Hormone Profiles for Supporting Analyses ####
# Mean
mean.data <- as.data.frame( with( individual.data, tapply( log.testosterone, team.id, mean, na.rm=T ) ) )
mean.data$team.id <- row.names( mean.data )
names( mean.data )[1] <- "T.mean"

# SD
sd.data <- as.data.frame( with( individual.data, tapply( log.testosterone, team.id, sd, na.rm=T ) ) )
sd.data$team.id <- row.names( sd.data )
names( sd.data )[1] <- "T.sd"

# Min
min.data <- as.data.frame( with( individual.data, tapply( log.testosterone, team.id, min, na.rm=T ) ) )
min.data$team.id <- row.names( min.data )
names( min.data )[1] <- "T.min"

# Max
max.data <- as.data.frame( with( individual.data, tapply( log.testosterone, team.id, max, na.rm=T ) ) )
max.data$team.id <- row.names( max.data )
names( max.data )[1] <- "T.max"

# Empirical Bayes Estimate, used for Footnote 3
ebe.model <- lme(log.testosterone ~ 1, random=~1|team.id, data=individual.data, na.action="na.exclude" )
ebe.data <- as.data.frame(ranef(ebe.model))
names(ebe.data) <- "T.ebe"
ebe.data$team.id <- row.names( ebe.data )

team.data <- merge( team.data, mean.data )
team.data <- merge( team.data, sd.data )
team.data <- merge( team.data, min.data )
team.data <- merge( team.data, max.data )
team.data <- merge( team.data, ebe.data )

#### Methods Section ####

#### Descriptives ####
paste( with( team.data, nrow( individual.data ) ), "Team Members") # Number of participants
paste( round(with( individual.data, mean( Age, na.rm=T ) ),2), "Average Age") # Mean Age
paste( round(with( individual.data, sd( Age, na.rm=T ) ),2), "SD Age") # Age SD
round(with( team.data, table( individual.data$Gender )/sum(table( individual.data$Gender )) )*100,1) # Sex distribution
round(table( individual.data$Ethnicity )/sum(table( individual.data$Ethnicity ) )*100,1) # Ethnic distribution
paste( with( team.data, nrow( team.data ) ), "Teams") # Number of teams
with( team.data, table( team.data$team.size ) ) # Team sizes

#### Center Predictors ####
team.data <- within( team.data, {
  c.team.size <- team.size - mean( team.size, na.rm=T )
  c.females <- females - mean( females, na.rm=T )
  c.prop.female <- prop.female - mean( prop.female, na.rm=T )
  c.nationality <- nationality.count - mean( nationality.count, na.rm=T )
  c.ethnicity <- ethnicity.count - mean( ethnicity.count, na.rm=T )
  c.time.of.day <- time.of.day - mean( time.of.day, na.rm=T )
  c.fl.value <- fl.value - mean( fl.value, na.rm=T )
  c.fl.value.nosex <- fl.value.nosex - mean( fl.value.nosex, na.rm=T )
  c.T.mean <- T.mean - mean( T.mean, na.rm=T )
  c.T.sd <- T.sd - mean( T.sd, na.rm=T )
  c.T.min <- T.min - mean( T.min, na.rm=T )
  c.T.max <- T.max - mean( T.max, na.rm=T )
  c.T.ebe <- T.ebe - mean( T.ebe, na.rm=T )
})

#### Faultlines Descriptives ####
round(with( count.diversity.data, table( ethnicity.count )/sum(table( ethnicity.count )) )*100,1) # Group Ethnic distribution
round(with( team.data, table( females )/nrow(team.data))*100,1) # Group Sex distribution
round(with( count.diversity.data, table( nationality.count )/sum(table( nationality.count )) )*100,1) # Group Nationality distribution
paste( "Average Faultlines:", round(with( team.data, mean( fl.value, na.rm=T ) ),2) ) # Mean Faultlines
paste( "SD Faultlines:", round(with( team.data, sd( fl.value, na.rm=T ) ),2)) # SD Faultlines
paste( c("Min Faultlines","Max Faultlines"), round(with( team.data, range( fl.value, na.rm=T ) ),2),sep="=") # Range for Faultlines

#### ALTERNATE DIVERSITY RECOMMENDATION ####
# The following analyses were requested during the review process as a robustness check but were not reported in the paper, because 
# it is not good to omit categories from the faultlines calculation. We left the syntax in here for transparency.
#### Requested Alternate Primary Analysis ####
individual.data <- individual.data[order(individual.data$team.id),]
team.data <- team.data[order(team.data$team.id),]
# Listwise deletion for the matrix algebra
z.data <- na.omit(team.data[,c( "c.team.size", "c.prop.female", "c.time.of.day", "c.fl.value.nosex", "c.ethnicity", "c.nationality", "c.T.sd", "c.T.min", "c.T.max", "final.performance", "team.id" )])
z.id <- z.data$team.id
z.data <- z.data[,-ncol(z.data)]
x.data <- na.omit(individual.data[,c( "log.testosterone", "log.cortisol", "team.id" )])
x.id <- x.data$team.id
x.data <- x.data[,-ncol(x.data)]

suggested.unbiased.data <- as.data.frame( adjusted.predictors(x.data,z.data,x.id, z.id) )
names( suggested.unbiased.data ) <- sub( "adjusted.group.means.", "", names(  suggested.unbiased.data ), fixed=T )
suggested.unbiased.data <- within( suggested.unbiased.data, {
  c.BLUP.log.testosterone <- BLUP.log.testosterone - mean( BLUP.log.testosterone )
})

#### Alternate Version of Primary Model ####
suggested.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.fl.value.nosex )
suggested.model.output <- micromacro.lm( suggested.model.formula, adjusted.predictors=suggested.unbiased.data, y=suggested.unbiased.data$final.performance, unequal.groups=T )
micromacro.summary( suggested.model.output )

#### Alternate Version of Model Using Interaction Between Sex and T as Covariate ####
suggested.intcov.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female*c.fl.value.nosex + c.BLUP.log.testosterone*c.fl.value.nosex )
suggested.intcov.model.output <- micromacro.lm( suggested.intcov.model.formula, adjusted.predictors=suggested.unbiased.data, y=suggested.unbiased.data$final.performance, unequal.groups=T )
micromacro.summary( suggested.intcov.model.output )

#### Table 2 with Alternate Version of Diversity Thing ####
write.xlsx( data.frame( b=round( suggested.intcov.model.output$statistics[,1], 2), SE=suggested.intcov.model.output$statistics[,3], round(suggested.intcov.model.output$statistics[,c(4:5)], 2), p=suggested.intcov.model.output$statistics[,6], R2=round(suggested.intcov.model.output$statistics[,8], 2) ), "Table S4.xlsx", sheetName="Table 2", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)

#### END ALTERNATE DIVERSITY RECOMMENDATION ####

#### Primary Analysis ####
individual.data <- individual.data[order(individual.data$team.id),]
team.data <- team.data[order(team.data$team.id),]
# Listwise deletion for the matrix algebra
z.data <- na.omit(team.data[,c( "c.team.size", "c.prop.female", "c.time.of.day", "c.fl.value", "c.ethnicity", "c.nationality", "c.T.sd", "c.T.min", "c.T.max", "final.performance", "team.id" )])
z.id <- z.data$team.id
z.data <- z.data[,-ncol(z.data)]
x.data <- na.omit(individual.data[,c( "log.testosterone", "log.cortisol", "team.id" )])
x.id <- x.data$team.id
x.data <- x.data[,-ncol(x.data)]

unbiased.data <- as.data.frame( adjusted.predictors(x.data,z.data,x.id, z.id) )
names( unbiased.data ) <- sub( "adjusted.group.means.", "", names( unbiased.data ), fixed=T )
unbiased.data <- within( unbiased.data, {
  c.BLUP.log.testosterone <- BLUP.log.testosterone - mean( BLUP.log.testosterone )
  c.BLUP.log.cortisol <- BLUP.log.cortisol - mean( BLUP.log.cortisol )
  lo.BLUP.log.cortisol <- c.BLUP.log.cortisol + sd( c.BLUP.log.cortisol )
  hi.BLUP.log.cortisol <- c.BLUP.log.cortisol - sd( c.BLUP.log.cortisol )
  lo.fl.value <- c.fl.value + sd(c.fl.value)
  hi.fl.value <- c.fl.value - sd(c.fl.value)
})

#### Primary Model ####
model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.fl.value )
model.output <- micromacro.lm( model.formula, adjusted.predictors=unbiased.data, y=unbiased.data$final.performance, unequal.groups=T )
micromacro.summary( model.output )

lo.diversity.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*hi.fl.value )
lo.diversity.model.output <- micromacro.lm( lo.diversity.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( lo.diversity.model.output )

hi.diversity.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*lo.fl.value )
hi.diversity.model.output <- micromacro.lm( hi.diversity.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( hi.diversity.model.output )

#### Most Recent Requests from Reviewers ####
addition.covars.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.BLUP.log.cortisol + c.BLUP.log.cortisol*c.fl.value + c.BLUP.log.testosterone*c.fl.value )
addition.covars.model.output <- micromacro.lm( addition.covars.model.formula, adjusted.predictors=unbiased.data, y=unbiased.data$final.performance, unequal.groups=T )
micromacro.summary( addition.covars.model.output )
#### Alternate Table 2 for Editor ####
write.xlsx( data.frame( b=round( addition.covars.model.output$statistics[,1], 2), SE=addition.covars.model.output$statistics[,3], round(addition.covars.model.output$statistics[,c(4:5)], 2), p=addition.covars.model.output$statistics[,6], r=round(addition.covars.model.output$statistics[,7], 2), R2=round(addition.covars.model.output$statistics[,8], 2) ), "Table 2 with Additional Covariates.xlsx", sheetName="Table 2 with new Covariates", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)

#### Using Raw Means ####
lm.r <- function( x ) {
  t.stats = coef(summary(x))[,'t value']
  df = x$df.residual
  effect.sizes = sqrt(t.stats^2/(t.stats^2 + df))
  output <- cbind(coef(summary(x)), data.frame(r=effect.sizes))
  return(output)
}

raw.means.model <- lm( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.T.mean*c.fl.value, data=team.data )
lm.r( raw.means.model)
write.xlsx( cbind(round(lm.r( raw.means.model),2), data.frame(df=rep(raw.means.model$df.residual, length(coef(raw.means.model))))), "Table 2 with Collective T from Raw Means.xlsx", sheetName="Table 2 using Raw Means", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)

#### Footnote 2 ####
footnote.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.BLUP.log.cortisol*c.fl.value )
footnote.model.output <- micromacro.lm( footnote.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( footnote.model.output )
write.xlsx( data.frame( b=round( footnote.model.output$statistics[,1], 2), SE=footnote.model.output$statistics[,3], round(footnote.model.output$statistics[,c(4:5)], 2), p=footnote.model.output$statistics[,6], r=round(footnote.model.output$statistics[,7], 2), R2=round(footnote.model.output$statistics[,8], 2) ), "Footnote 2 Model.xlsx", sheetName="Footnote 2 Mdoel", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)






#### Supporting Analyses ####
# Moderation by Ethnicity Count
ethnicity.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.ethnicity )
ethnicity.model.output <- micromacro.lm( ethnicity.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( ethnicity.model.output )
# Moderation by Female Count
gender.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.BLUP.log.testosterone*c.prop.female )
gender.model.output <- micromacro.lm( gender.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( gender.model.output )
# Moderation by Country Count
nationality.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.nationality )
nationality.model.output <- micromacro.lm( nationality.model.formula, unbiased.data, y=unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( nationality.model.output )


#### Supplemental Analyses ####
#### Table S1: Correlation Table of Key Variables ####
#### Got this off the internet: http://myowelt.blogspot.ca/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
} 

##Create table and insert dataframe below
merged.data <- cbind( team.data, unbiased.data )
measure.correlations <- corstarsl(merged.data[, c("final.performance", "BLUP.log.testosterone", "T.sd", "T.min", "T.max", "fl.value", "time.of.day", "prop.female", "team.size" ) ]) 
measure.correlations <- cbind( measure.correlations, team.size=rep( "", nrow(measure.correlations) ) )
measure.correlations <- as.data.frame( measure.correlations )
descriptives.matrix <- matrix( apply( merged.data[, c("final.performance", "BLUP.log.testosterone", "T.sd", "T.min", "T.max", "fl.value", "time.of.day", "prop.female", "team.size" ) ], 2, min, na.rm=T), nrow=1 )
descriptives.matrix <- rbind( descriptives.matrix, matrix( apply( merged.data[, c("final.performance", "BLUP.log.testosterone", "T.sd", "T.min", "T.max", "fl.value", "time.of.day", "prop.female", "team.size" ) ], 2, max, na.rm=T), nrow=1 ))
descriptives.matrix <- rbind( descriptives.matrix, matrix( apply( merged.data[, c("final.performance", "BLUP.log.testosterone", "T.sd", "T.min", "T.max", "fl.value", "time.of.day", "prop.female", "team.size" ) ], 2, mean, na.rm=T), nrow=1 ))
descriptives.matrix <- rbind( descriptives.matrix, matrix( apply( merged.data[, c("final.performance", "BLUP.log.testosterone", "T.sd", "T.min", "T.max", "fl.value", "time.of.day", "prop.female", "team.size" ) ], 2, sd, na.rm=T), nrow=1 ))
descriptives.matrix <- as.data.frame(round(descriptives.matrix,2))
names( descriptives.matrix ) <- names( measure.correlations )
write.xlsx( measure.correlations, "Table S1.xlsx", sheetName="Correlations", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)
write.xlsx( descriptives.matrix, "Table S1.xlsx", sheetName="Descriptive Stats", col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE)


# Other quantifications
# Listwise Deletion for Matrix Algebra
quant.z.data <- na.omit(team.data[,c( "c.team.size", "c.prop.female", "c.time.of.day", "c.fl.value", "c.ethnicity", "c.nationality", "c.T.sd", "c.T.min", "c.T.max", "final.performance", "team.id" )])
quant.z.id <- quant.z.data$team.id
quant.z.data <- quant.z.data[,-ncol(quant.z.data)]
quant.x.data <- na.omit(individual.data[,c( "log.testosterone", "log.cortisol", "team.id" )])
quant.x.id <- quant.x.data$team.id
quant.x.data <- quant.x.data[,-ncol(quant.x.data)]

quant.unbiased.data <- as.data.frame( adjusted.predictors( quant.x.data, quant.z.data, quant.x.id, quant.z.id ) )
names( quant.unbiased.data ) <- sub( "adjusted.group.means.", "", names( quant.unbiased.data ), fixed=T )
quant.unbiased.data <- within( quant.unbiased.data, {
  c.BLUP.log.testosterone <- BLUP.log.testosterone - mean( BLUP.log.testosterone )
  lo.BLUP.log.testosterone <- c.BLUP.log.testosterone + sd( c.BLUP.log.testosterone )
  hi.BLUP.log.testosterone <- c.BLUP.log.testosterone - sd( c.BLUP.log.testosterone )
  lo.fl.value <- c.fl.value + sd(c.fl.value)
  hi.fl.value <- c.fl.value - sd(c.fl.value)
})

sd.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.T.sd*c.fl.value )
sd.model.output <- micromacro.lm( sd.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( sd.model.output )

min.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.T.min*c.fl.value )
min.model.output <- micromacro.lm( min.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( min.model.output )

max.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.T.max*c.fl.value )
max.model.output <- micromacro.lm( max.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( max.model.output )

ebe.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.T.ebe*c.fl.value )
ebe.model.output <- lm( ebe.model.formula, data=team.data )
summary( ebe.model.output )

quant.unbiased.data <- cbind( quant.unbiased.data, team.data$c.T.ebe )
names(quant.unbiased.data)[ncol(quant.unbiased.data)] <- "c.T.ebe"
print( with( quant.unbiased.data, cor.test( c.T.ebe, c.BLUP.log.testosterone ) )  )
combo.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.log.testosterone*c.fl.value + c.T.sd*c.fl.value + c.T.min*c.fl.value + c.T.max*c.fl.value)
combo.model.output <- micromacro.lm( combo.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( combo.model.output )

#### BIC-based Model Comparison ####
sd.model.output$BIC
min.model.output$BIC
max.model.output$BIC
model.output$BIC

max.model.output$BIC-model.output$BIC

##### Footnote 5: Interactions between T Quantifications and Sex ####
sex.sd.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female * c.T.sd )
sex.sd.model.output <- micromacro.lm( sex.sd.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( sex.sd.model.output )

sex.min.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female * c.T.min )
sex.min.model.output <- micromacro.lm( sex.min.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( sex.min.model.output )

sex.max.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female * c.T.max )
sex.max.model.output <- micromacro.lm( sex.max.model.formula, quant.unbiased.data, y=quant.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary( sex.max.model.output )


#### Table S2 ####
write.xlsx( data.frame( b=round( sd.model.output$statistics[,1], 2), SE=sd.model.output$statistics[,3], round(sd.model.output$statistics[,c(4:5)], 2), p=sd.model.output$statistics[,6], R2=round(sd.model.output$statistics[,8], 2)  ), "Table Other Quantifications.xlsx", sheetName="Table S1 - SD", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)
write.xlsx( data.frame( b=round( min.model.output$statistics[,1], 2), SE=min.model.output$statistics[,3], round(min.model.output$statistics[,c(4:5)], 2), p=min.model.output$statistics[,6], R2=round(min.model.output$statistics[,8], 2) ), "Table Other Quantifications.xlsx", sheetName="Table S1 - MIN", col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE)
write.xlsx( data.frame( b=round( max.model.output$statistics[,1], 2), SE=max.model.output$statistics[,3], round(max.model.output$statistics[,c(4:5)], 2), p=max.model.output$statistics[,6], R2=round(max.model.output$statistics[,8], 2) ), "Table Other Quantifications.xlsx", sheetName="Table S1 - MAX", col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE)
write.xlsx( data.frame( b=round( combo.model.output$statistics[,1], 2), SE=combo.model.output$statistics[,3], round(combo.model.output$statistics[,c(4:5)], 2), p=combo.model.output$statistics[,6], R2=round(combo.model.output$statistics[,8], 2) ), "Table S2.xlsx", sheetName="Table S2", col.names = TRUE, row.names = TRUE, showNA = FALSE)






#### Doing more to control for sex ####
sex.z.data <- na.omit(team.data[,c( "c.team.size", "c.prop.female", "c.time.of.day", "c.fl.value", "c.ethnicity", "c.nationality", "c.T.sd", "c.T.min", "c.T.max", "final.performance", "team.id" )])
sex.z.id <- sex.z.data$team.id
sex.z.data <- sex.z.data[,-ncol(sex.z.data)]
sex.x.data <- na.omit(individual.data[,c( "log.testosterone", "log.cortisol", "testosterone.by.sex", "team.id" )])
sex.x.id <- sex.x.data$team.id
sex.x.data <- sex.x.data[,-ncol(sex.x.data)]

sex.unbiased.data <- as.data.frame( adjusted.predictors( sex.x.data, sex.z.data, sex.x.id, sex.z.id ) )
names( sex.unbiased.data ) <- sub( "adjusted.group.means.", "", names( sex.unbiased.data ), fixed=T )
sex.unbiased.data <- within( sex.unbiased.data, {
  c.BLUP.testosterone.by.sex <- BLUP.testosterone.by.sex - mean( BLUP.testosterone.by.sex )
})

sex.model.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.prop.female + c.BLUP.testosterone.by.sex*c.fl.value )
sex.model.output <- micromacro.lm( sex.model.formula, sex.unbiased.data, y=sex.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary(sex.model.output)
# Checking whether this still holds if we don't control for sex
sex.model.nosexcovar.formula <- as.formula( final.performance ~ c.time.of.day + c.team.size + c.BLUP.testosterone.by.sex*c.fl.value )
sex.model.nosexcovar.output <- micromacro.lm( sex.model.nosexcovar.formula, sex.unbiased.data, y=sex.unbiased.data$final.performance, unequal.groups = T )
micromacro.summary(sex.model.nosexcovar.output)


#### Table S3 ####
write.xlsx( data.frame( b=round( sex.model.output$statistics[,1], 2), SE=sex.model.output$statistics[,3], round(sex.model.output$statistics[,c(4:5)], 2), p=sex.model.output$statistics[,6], R2=round(sex.model.output$statistics[,8], 2) ), "Table S3.xlsx", sheetName="Table S3", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)

#### Figure 2 ####
figure.data <- cbind( team.data, unbiased.data[,c("BLUP.log.testosterone", "c.BLUP.log.testosterone","c.fl.value","hi.fl.value","lo.fl.value")])

pdf( "Figure 2.pdf", width=8.5 )
x <- seq(from=min(figure.data$BLUP.log.testosterone, na.rm=T), to=max(figure.data$BLUP.log.testosterone, na.rm=T), by=.1)
hifl.y <- array(NA, dim=length(x))
hifl.y.upper <- hifl.y
hifl.y.lower <- hifl.y
lofl.y <- array(NA, dim=length(x))
lofl.y.upper <- lofl.y
lofl.y.lower <- lofl.y
for ( i in 1:length(x)) {
  temp.x <- figure.data$BLUP.log.testosterone-x[i]
  figure.data$temp.x <- temp.x
  hifl.raw.y <- micromacro.lm(update(model.formula, .~.-c.BLUP.log.testosterone*c.fl.value+temp.x*hi.fl.value), figure.data, figure.data$final.performance, unequal.groups = T)$statistics[1,1]
  hifl.raw.se <- micromacro.lm(update(model.formula, .~.-c.BLUP.log.testosterone*c.fl.value+temp.x*hi.fl.value), figure.data, figure.data$final.performance, unequal.groups = T)$statistics[1,3]
  hifl.y[i] <- hifl.raw.y
  hifl.y.upper[i] <- hifl.raw.y+hifl.raw.se
  hifl.y.lower[i] <- hifl.raw.y-hifl.raw.se
  lofl.raw.y <- micromacro.lm(update(model.formula, .~.-c.BLUP.log.testosterone*c.fl.value+temp.x*lo.fl.value), figure.data, figure.data$final.performance, unequal.groups = T)$statistics[1,1]
  lofl.raw.se <- micromacro.lm(update(model.formula, .~.-c.BLUP.log.testosterone*c.fl.value+temp.x*lo.fl.value), figure.data, figure.data$final.performance, unequal.groups = T)$statistics[1,3]
  lofl.y[i] <- lofl.raw.y
  lofl.y.upper[i] <- lofl.raw.y+lofl.raw.se
  lofl.y.lower[i] <- lofl.raw.y-lofl.raw.se
}

#### Create Plot of Omnibus Model ####
op <- par(cex.main = 1.5, mar = c(4, 4.5, 2, 2) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.25, font.lab = 2, cex.axis = 1.25, bty = "n", las=1)
plot( x, hifl.y, xlab="", ylab="", type="n", xlim=c(min(x), max(x)), ylim=c(-1*max(abs(range(c(lofl.y.upper,lofl.y.lower,hifl.y.upper,hifl.y.lower)))), max(abs(range(c(lofl.y.upper,lofl.y.lower,hifl.y.upper,hifl.y.lower))))), axes=F )
axis(1)
axis(2)
polygon(c(x,rev(x)), c(lofl.y.upper,rev(lofl.y.lower)), col=adjustcolor("lightsteelblue", alpha.f=.5), border=NA)
polygon(c(x,rev(x)), c(hifl.y.upper,rev(hifl.y.lower)), col=adjustcolor("lightgrey", alpha.f=.5), border=NA)
lines( x, lofl.y, lwd=2, lty="dashed" )
lines( x, hifl.y, lwd=2, lty="solid" )
mtext("Group Testosterone", side=1, line=2.5, cex=1.35)
mtext("Group Performance", side=2, line=3, cex=1.35, las=0)
legend("top", legend=c("High Diversity","Low Diversity"), lty=c("dashed", "solid"), lwd=2, bty = 'n')
dev.off()

#### Table 2 ####
write.xlsx( data.frame( b=round( model.output$statistics[,1], 2), SE=model.output$statistics[,3], round(model.output$statistics[,c(4:5)], 2), p=model.output$statistics[,6], R2=round(model.output$statistics[,8], 2) ), "Table 2.xlsx", sheetName="Table 2", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)

#### Looking at Over Time ###
stability.data <- cbind( team.data, unbiased.data[ ,c( "c.BLUP.log.testosterone", "c.fl.value", "lo.fl.value", "hi.fl.value" ) ] )

long.form.data <- with( stability.data, { data.frame(
  team.id = rep( team.id, 2 ),
  time = c( rep( -1, nrow( stability.data ) ), rep( 1, nrow( stability.data ) ) ),
  performance = c( interim.performance, final.performance ),
  c.time.of.day = rep( c.time.of.day, 2 ),
  c.prop.female = rep( c.prop.female, 2 ),
  c.team.size = rep( c.team.size, 2 ),
  c.fl.value = rep( c.fl.value, 2 ),
  lo.fl.value = rep( lo.fl.value, 2 ),
  hi.fl.value = rep( hi.fl.value, 2 ),
  c.BLUP.log.testosterone = rep( c.BLUP.log.testosterone, 2 ) )
})

# Growth-curve model
growth.model <- lme( performance ~ c.time.of.day + c.prop.female + c.team.size + time * c.BLUP.log.testosterone * c.fl.value, random=~1+time|team.id, na.action="na.exclude", data=long.form.data ); summary( growth.model )
table.growth.curve <- summary(growth.model)$tTable
effect.sizes <- with( anova.lme( growth.model ), (numDF/denDF*`F-value`)/(1+(numDF/denDF*`F-value`)))
write.xlsx(cbind(table.growth.curve,effect.sizes), "Table S5.xlsx", sheetName="Growth Curve", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = FALSE)
