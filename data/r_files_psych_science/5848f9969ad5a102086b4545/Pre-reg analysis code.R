#### This code tests the pre-registered hypotheses found in the main paper. 

# First set your working directory to where you've downloaded the shared data. 
data <- read.csv('PK extension data repository 12-7.csv', stringsAsFactors = FALSE)

require(reshape2); require(ggplot2); require(plyr)
### Main pre-registered analysis ###

### Testing pre-registered hypothesis 1
### This should be read as the mean of 'say' is .179 less than the mean of want, p = .000312, df = 206, t = -3.67
t.test(data$intent.other.say[data$condition == 1], data$intent.other.want[data$condition == 1], paired = TRUE)

### Testing pre-registered hypothesis 1.1
### Do people think others say they want sex more than they themselves say they want sex?
t.test(data$intent.self[data$condition %in% c(1, 2)], data$intent.other.say[data$condition %in% c(1, 2)], paired = TRUE)
### Do people think others want sex more than they themselves say they want sex?
t.test(data$intent.self[data$condition %in% c(1, 2)], data$intent.other.want[data$condition %in% c(1, 2)], paired = TRUE)

### Testing pre-registered hypothesis 2
### In Condition 2, say is .264 less than want, p < .0001, t = -5.71, df = 206
t.test(data$intent.other.say[data$condition == 2], data$intent.other.want[data$condition == 2], paired = TRUE)

### Testing pre-registered hypothesis 3
t.test(data$intent.other.want[data$condition == 1], data$intent.other.want[data$condition == 2])

### Testing pre-registered hypothesis 4
t.test(data$intent.other.say[data$condition == 1], data$intent.other.say[data$condition == 2])

### Testing pre-registered hypothesis 5
### This shows a null effect - the first 'other' question does not differ across condition 1 and 2,
### despite the first one being 'say' and the second 'want' (say is 4.89, want 4.82). The confidence
### intervals put a maximum on the possible difference here, which ranges from -.124 to a maximum
### of .250. So our data suggests that if people think women want sex more than they say they do when
### first asked, this difference is (with 95% confidence) at most .124 on a 7-point scale.
t.test(data$intent.other.first[data$condition == 1], data$intent.other.first[data$condition == 2])


### Testing pre-registered hypothesis 6
### The combined other columns of condition 1 are significantly higher than column 1, p = .002595
t.test(rowMeans(data[data$condition == 1 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE),
       rowMeans(data[data$condition == 2 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE))


### Testing a mixed-effects anova. We pre-registered hypotheses 1, 2, and 5 to directly test simple effects - 
### We did this because both PK's and our hypotheses predicted an interaction between order and condition, and
### so a test of a significant interaction term did not disambiguate the two - they predicted a crossed
### interaction, while we predicted that in the first column, there would be no difference, and then
### conditions would diverge. Nonetheless, it was pointed out to us on review that an anova should be run to
### test that this interaction is there, before running the follow-up simple slopes test in pre-registered
### hypothesis 5. The following code does this, melting the data into long format and fitting a mixed anova.

require(reshape2)
data$PID <- 1:nrow(data)
longdata <- melt(data, measure.vars = c('intent.other.first', 'intent.other.second'))
colnames(longdata) <- gsub('^variable$', 'order', colnames(longdata))
longdata$condition <- as.factor(longdata$condition)
# Essentially we're lengthening the data to have the response to the first and second column for each 
# participant on a different row, with the order variable specifying which is which. Then we run a mixed
# interaction with condition. The anova results for the interaction are reported in the paper. 
require(lme4)
anova(lmer(value ~ order*condition + (1|PID), data = longdata[longdata$condition %in% c(1, 2),]))

# The regression results reveal that, for the first question asked, condition doesn't matter (the simple
# slope of condition when order = intent.other.first is -.06). Responses to the second question go up
# .178 in condition 1, and go down .264 in condition 2. 
summary(lmer(value ~ order*condition + (1|PID), data = longdata[longdata$condition %in% c(1, 2),]))


### Analysis with exploratory conditions 3 and 4 instead of 1 and 2 (so no leading language)

### Testing hypothesis 1
t.test(data$intent.other.say[data$condition == 3], data$intent.other.want[data$condition == 3], paired = TRUE)

### Testing hypothesis 1.1
t.test(data$intent.self[data$condition %in% c(3, 4)], data$intent.other.say[data$condition %in% c(3, 4)], paired = TRUE)
t.test(data$intent.self[data$condition %in% c(3, 4)], data$intent.other.want[data$condition %in% c(3, 4)], paired = TRUE)

### Testing hypothesis 2
t.test(data$intent.other.say[data$condition == 4], data$intent.other.want[data$condition == 4], paired = TRUE)

### Testing hypothesis 3
t.test(data$intent.other.want[data$condition == 3], data$intent.other.want[data$condition == 4])

### Testing hypothesis 4
t.test(data$intent.other.say[data$condition == 3], data$intent.other.say[data$condition == 4])

### Testing hypothesis 5
t.test(data$intent.other.first[data$condition == 3], data$intent.other.first[data$condition == 4])

### Testing hypothesis 6
t.test(rowMeans(data[data$condition == 3 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE),
       rowMeans(data[data$condition == 4 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE))



### Analysis with exploratory and pre-reg samples collapsed (so conds 1 and 3 together, conds 2 and 4 together)
data$condition[data$condition == 3] <-
  1; data$condition[data$condition == 4] <- 2

### Testing pre-registered hypothesis 1
t.test(data$intent.other.say[data$condition == 1], data$intent.other.want[data$condition == 1], paired = TRUE)

### Note I actually need to limit this to conditions 1 and 2

### Testing pre-registered hypothesis 1.1
t.test(data$intent.self, data$intent.other.want, paired = TRUE)
t.test(data$intent.self, data$intent.other.want, paired = TRUE)

### Testing pre-registered hypothesis 2
t.test(data$intent.other.say[data$condition == 2], data$intent.other.want[data$condition == 2], paired = TRUE)

### Testing pre-registered hypothesis 3
t.test(data$intent.other.want[data$condition == 1], data$intent.other.want[data$condition == 2])

### Testing pre-registered hypothesis 4
t.test(data$intent.other.say[data$condition == 1], data$intent.other.say[data$condition == 2])

### Testing pre-registered hypothesis 5
t.test(data$intent.other.first[data$condition == 1], data$intent.other.first[data$condition == 2])

### Testing pre-registered hypothesis 6
t.test(rowMeans(data[data$condition == 1 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE),
       rowMeans(data[data$condition == 2 , c('intent.other.want', 'intent.other.say')], na.rm = TRUE))
