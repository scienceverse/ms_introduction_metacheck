# Bracketing --------------------------------------------------------------
# First run the "SvD_Analysis_1a.R" script

source("Analysis/SvD Analysis_1a.R", echo = TRUE)

# Create a function to find estimates that were bracketed 
b.find <- function(x,q1,q2) {
  ifelse(q1<x & q2>x | q1>x & q2<x,1,0)
}

# Create the dataset with bracketing values 
dbrack <- dlong
dbrack <- dbrack %>% 
  dplyr::mutate(brate = b.find(answer, guess_1, guess_2))

names(dbrack)[names(dbrack) == "condition"] <- "Perspective"

# Bracketing analysis -----------------------------------------------------
table(dbrack$Perspective)
# Overall
ic8 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
            data = dbrack)
summary(ic8)
8.28*sqrt((1/439)+(1/441))

dbrack %>% group_by(Perspective) %>% 
  summarise(mean = mean(brate),
            sd = sd(brate))

# Percentage bracketed
dbrack %>% 
  group_by(Perspective,brate) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))


# To the answer -----------------------------------------------------
# Create a function for second guesses moving away from true answer
b.away <- function(t,g1,g2)
{
  ifelse(g2>g1 & g2<t | g2>t & g2<g1,1,0)
}

dbrackaway <- dbrack %>% 
  filter(brate == 0) %>% 
  mutate(away = b.away(answer, guess_1, guess_2))

dbrackaway %>% 
  group_by(Perspective,away) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

