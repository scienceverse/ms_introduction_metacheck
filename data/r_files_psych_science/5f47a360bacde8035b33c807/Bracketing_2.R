
# First run the SvDvA script 
source("Analysis/SvDvA Analysis_2.R", echo  = TRUE)

# Bracketing --------------------------------------------------------------

# Create a function to find bracketing 
b.find <- function(x,q1,q2) {
  ifelse(q1<x & q2>x | q1>x & q2<x,1,0)
}

dbrack <- dlong

dbrack <- dbrack %>% 
  dplyr::mutate(brate = b.find(answer, guess_1, guess_2))

names(dbrack)[names(dbrack) == "condition"] <- "Perspective"


# Bracketing analysis -----------------------------------------------------
d_a_s <- dbrack %>% 
  filter(Perspective == "Agree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))

d_d_s <- dbrack %>% 
  filter(Perspective == "Disagree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))

d_d_a <- dbrack %>% 
  filter(Perspective == "Disagree" | Perspective == "Agree") %>% 
  mutate(condition_c = ifelse(Perspective == "Agree",0.5,-0.5))


ic12 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic12)
1.16*sqrt((1/462)+(1/463))


ic13 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic13)
12.91*sqrt((1/462)+(1/464))

ic14 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic14)
13.75*sqrt((1/463)+(1/464))

# Percentage bracketed
dbrack %>% 
  group_by(Perspective,brate) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))


# To the answer -----------------------------------------------------
# Create a function for second guesses moving away from true
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
