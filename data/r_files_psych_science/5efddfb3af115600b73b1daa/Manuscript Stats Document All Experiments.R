library(tidyr)
library(tidyverse)
library(ez)

# Experiment 1 Stats

expt1 <- EXPT_1_Parameter_Estimates_After_Exclusions

expt1$Participant <- as.factor(expt1$Participant)
expt1$Pair <- as.factor(expt1$Pair)
expt1$Memory_Load <- as.factor(expt1$Memory_Load)

# Mu comparisons
expt1lowdissim <- expt1 %>% filter(Pair == 2) %>% filter(Memory_Load == 2)
expt1highdissim <- expt1 %>% filter(Pair == 2) %>% filter(Memory_Load == 6)
expt1lowsim <- expt1 %>% filter(Pair == 1) %>% filter(Memory_Load == 2)
expt1highsim <- expt1 %>% filter(Pair == 1) %>% filter(Memory_Load == 6)

t.test(expt1lowdissim$MixMu,expt1highdissim$MixMu, paired = TRUE)
t.test(expt1lowdissim$MixMu,expt1lowsim$MixMu, paired = TRUE)
t.test(expt1highdissim$MixMu,expt1highsim$MixMu, paired = TRUE)
t.test(expt1lowsim$MixMu,expt1highsim$MixMu, paired = TRUE)

t.test(expt1lowdissim$SwapMu,expt1highdissim$SwapMu, paired = TRUE)
t.test(expt1lowdissim$SwapMu,expt1lowsim$SwapMu, paired = TRUE)
t.test(expt1highdissim$SwapMu,expt1highsim$SwapMu, paired = TRUE)
t.test(expt1lowsim$SwapMu,expt1highsim$SwapMu, paired = TRUE)

t.test(expt1lowdissim$NoGuessMu,expt1highdissim$NoGuessMu, paired = TRUE)
t.test(expt1lowdissim$NoGuessMu,expt1lowsim$NoGuessMu, paired = TRUE)
t.test(expt1highdissim$NoGuessMu,expt1highsim$NoGuessMu, paired = TRUE)
t.test(expt1lowsim$NoGuessMu,expt1highsim$NoGuessMu, paired = TRUE)

# 2x2 ANOVA

expt1factorial <- expt1 %>% group_by(Participant, Memory_Load, Pair)

# SD 

ezANOVA(expt1factorial,
        dv = MixSD,
        wid = Participant,
        within = c(Pair, Memory_Load))

ezANOVA(expt1factorial,
        dv = SwapSD,
        wid = Participant,
        within = c(Pair, Memory_Load))

ezANOVA(expt1factorial,
        dv = NoGuessSD,
        wid = Participant,
        within = c(Pair, Memory_Load))

# Guess 

ezANOVA(expt1factorial,
        dv = MixGuess,
        wid = Participant,
        within = c(Pair, Memory_Load))

ezANOVA(expt1factorial,
        dv = SwapGuess,
        wid = Participant,
        within = c(Pair, Memory_Load))

# Swap 

ezANOVA(expt1factorial,
        dv = SwapB,
        wid = Participant,
        within = c(Pair, Memory_Load))

## Follow-up T-tests
expt1lowdissim <- expt1 %>% filter(Pair == 2) %>% filter(Memory_Load == 2)
expt1highdissim <- expt1 %>% filter(Pair == 2) %>% filter(Memory_Load == 6)
expt1lowsim <- expt1 %>% filter(Pair == 1) %>% filter(Memory_Load == 2)
expt1highsim <- expt1 %>% filter(Pair == 1) %>% filter(Memory_Load == 6)

#Swap Model SD 
t.test(expt1lowdissim$SwapSD,expt1highdissim$SwapSD, paired = TRUE)
t.test(expt1lowdissim$SwapSD,expt1lowsim$SwapSD, paired = TRUE)
t.test(expt1lowdissim$SwapSD,expt1highsim$SwapSD, paired = TRUE)
t.test(expt1highdissim$SwapSD,expt1lowsim$SwapSD, paired = TRUE)
t.test(expt1highdissim$SwapSD,expt1highsim$SwapSD, paired = TRUE)
t.test(expt1lowsim$SwapSD,expt1highsim$SwapSD, paired = TRUE)

#No Guess Model SD 
t.test(expt1lowdissim$NoGuessSD,expt1highdissim$NoGuessSD, paired = TRUE)
t.test(expt1lowdissim$NoGuessSD,expt1lowsim$NoGuessSD, paired = TRUE)
t.test(expt1lowdissim$NoGuessSD,expt1highsim$NoGuessSD, paired = TRUE)
t.test(expt1highdissim$NoGuessSD,expt1lowsim$NoGuessSD, paired = TRUE)
t.test(expt1highdissim$NoGuessSD,expt1highsim$NoGuessSD, paired = TRUE)
t.test(expt1lowsim$NoGuessSD,expt1highsim$NoGuessSD, paired = TRUE)

#Standard Model Guess 
t.test(expt1lowdissim$MixGuess,expt1highdissim$MixGuess, paired = TRUE)
t.test(expt1lowdissim$MixGuess,expt1lowsim$MixGuess, paired = TRUE)
t.test(expt1lowdissim$MixGuess,expt1highsim$MixGuess, paired = TRUE)
t.test(expt1highdissim$MixGuess,expt1lowsim$MixGuess, paired = TRUE)
t.test(expt1highdissim$MixGuess,expt1highsim$MixGuess, paired = TRUE)
t.test(expt1lowsim$MixGuess,expt1highsim$MixGuess, paired = TRUE)

#Swap Model B 
t.test(expt1lowdissim$SwapB,expt1highdissim$SwapB, paired = TRUE)
t.test(expt1lowdissim$SwapB,expt1lowsim$SwapB, paired = TRUE)
t.test(expt1lowdissim$SwapB,expt1highsim$SwapB, paired = TRUE)
t.test(expt1highdissim$SwapB,expt1lowsim$SwapB, paired = TRUE)
t.test(expt1highdissim$SwapB,expt1highsim$SwapB, paired = TRUE)
t.test(expt1lowsim$SwapB,expt1highsim$SwapB, paired = TRUE)


# Experiment 2A 

expt2A <- EXPT_2A_Parameter_Estimates_After_Exclusions

expt2Alow <- expt2A %>% filter(Rating == 1)
expt2Amed <- expt2A %>% filter(Rating == 2)
expt2Ahigh <- expt2A %>% filter(Rating == 3)

# Mu T-tests

t.test(expt2Alow$MixMu,expt2Amed$MixMu, paired = TRUE)
t.test(expt2Alow$MixMu,expt2Ahigh$MixMu, paired = TRUE)
t.test(expt2Amed$MixMu,expt2Ahigh$MixMu, paired = TRUE)

t.test(expt2Alow$SwapMu,expt2Amed$SwapMu, paired = TRUE)
t.test(expt2Alow$SwapMu,expt2Ahigh$SwapMu, paired = TRUE)
t.test(expt2Amed$SwapMu,expt2Ahigh$SwapMu, paired = TRUE)

t.test(expt2Alow$NoGuessMu,expt2Amed$NoGuessMu, paired = TRUE)
t.test(expt2Alow$NoGuessMu,expt2Ahigh$NoGuessMu, paired = TRUE)
t.test(expt2Amed$NoGuessMu,expt2Ahigh$NoGuessMu, paired = TRUE)

# SD 
expt2A$Participant <- as.factor(expt2A$Participant)
expt2A$Rating <- as.factor(expt2A$Rating)

ezANOVA(expt2A,
        dv = MixSD,
        wid = Participant,
        within = c(Rating))

ezANOVA(expt2A,
        dv = SwapSD,
        wid = Participant,
        within = Rating)

ezANOVA(expt2A,
        dv = NoGuessSD,
        wid = Participant,
        within = Rating)

# SD T-tests

t.test(expt2Alow$MixSD,expt2Amed$MixSD, paired = TRUE)
t.test(expt2Alow$MixSD,expt2Ahigh$MixSD, paired = TRUE)
t.test(expt2Amed$MixSD,expt2Ahigh$MixSD, paired = TRUE)

t.test(expt2Alow$SwapSD,expt2Amed$SwapSD, paired = TRUE)
t.test(expt2Alow$SwapSD,expt2Ahigh$SwapSD, paired = TRUE)
t.test(expt2Amed$SwapSD,expt2Ahigh$SwapSD, paired = TRUE)

t.test(expt2Alow$NoGuessSD,expt2Amed$NoGuessSD, paired = TRUE)
t.test(expt2Alow$NoGuessSD,expt2Ahigh$NoGuessSD, paired = TRUE)
t.test(expt2Amed$NoGuessSD,expt2Ahigh$NoGuessSD, paired = TRUE)

# Guess
expt2A$Participant <- as.factor(expt2A$Participant)
expt2A$Rating <- as.factor(expt2A$Rating)

ezANOVA(expt2A,
        dv = MixGuess,
        wid = Participant,
        within = c(Rating))

ezANOVA(expt2A,
        dv = SwapGuess,
        wid = Participant,
        within = Rating)

# Guess T-tests

t.test(expt2Alow$MixGuess,expt2Amed$MixGuess, paired = TRUE)
t.test(expt2Alow$MixGuess,expt2Ahigh$MixGuess, paired = TRUE)
t.test(expt2Amed$MixGuess,expt2Ahigh$MixGuess, paired = TRUE)

t.test(expt2Alow$SwapGuess,expt2Amed$SwapGuess, paired = TRUE)
t.test(expt2Alow$SwapGuess,expt2Ahigh$SwapGuess, paired = TRUE)
t.test(expt2Amed$SwapGuess,expt2Ahigh$SwapGuess, paired = TRUE)

# Swap Rates
expt2A$Participant <- as.factor(expt2A$Participant)
expt2A$Rating <- as.factor(expt2A$Rating)

ezANOVA(expt2A,
        dv = SwapB,
        wid = Participant,
        within = c(Rating))

# Swap T-tests

t.test(expt2Alow$SwapB,expt2Amed$SwapB, paired = TRUE)
t.test(expt2Alow$SwapB,expt2Ahigh$SwapB, paired = TRUE)
t.test(expt2Amed$SwapB,expt2Ahigh$SwapB, paired = TRUE)


# Experiment 2B

expt2B <- EXPT_2B_Parameter_Estimates_After_Exclusions

expt2Blow <- expt2B %>% filter(Rating == 1)
expt2Bmed <- expt2B %>% filter(Rating == 2)
expt2Bhigh <- expt2B %>% filter(Rating == 3)

# Mu T-tests

t.test(expt2Blow$MixMu,expt2Bmed$MixMu, paired = TRUE)
t.test(expt2Blow$MixMu,expt2Bhigh$MixMu, paired = TRUE)
t.test(expt2Bmed$MixMu,expt2Bhigh$MixMu, paired = TRUE)

t.test(expt2Blow$SwapMu,expt2Bmed$SwapMu, paired = TRUE)
t.test(expt2Blow$SwapMu,expt2Bhigh$SwapMu, paired = TRUE)
t.test(expt2Bmed$SwapMu,expt2Bhigh$SwapMu, paired = TRUE)

t.test(expt2Blow$NoGuessMu,expt2Bmed$NoGuessMu, paired = TRUE)
t.test(expt2Blow$NoGuessMu,expt2Bhigh$NoGuessMu, paired = TRUE)
t.test(expt2Bmed$NoGuessMu,expt2Bhigh$NoGuessMu, paired = TRUE)

# SD 
expt2B$Participant <- as.factor(expt2B$Participant)
expt2B$Rating <- as.factor(expt2B$Rating)

ezANOVA(expt2B,
        dv = MixSD,
        wid = Participant,
        within = c(Rating))

ezANOVA(expt2B,
        dv = SwapSD,
        wid = Participant,
        within = Rating)

ezANOVA(expt2B,
        dv = NoGuessSD,
        wid = Participant,
        within = Rating)

# SD T-tests

t.test(expt2Blow$MixSD,expt2Bmed$MixSD, paired = TRUE)
t.test(expt2Blow$MixSD,expt2Bhigh$MixSD, paired = TRUE)
t.test(expt2Bmed$MixSD,expt2Bhigh$MixSD, paired = TRUE)

t.test(expt2Blow$SwapSD,expt2Bmed$SwapSD, paired = TRUE)
t.test(expt2Blow$SwapSD,expt2Bhigh$SwapSD, paired = TRUE)
t.test(expt2Bmed$SwapSD,expt2Bhigh$SwapSD, paired = TRUE)

t.test(expt2Blow$NoGuessSD,expt2Bmed$NoGuessSD, paired = TRUE)
t.test(expt2Blow$NoGuessSD,expt2Bhigh$NoGuessSD, paired = TRUE)
t.test(expt2Bmed$NoGuessSD,expt2Bhigh$NoGuessSD, paired = TRUE)

# Guess
expt2B$Participant <- as.factor(expt2B$Participant)
expt2B$Rating <- as.factor(expt2B$Rating)

ezANOVA(expt2B,
        dv = MixGuess,
        wid = Participant,
        within = c(Rating))

ezANOVA(expt2B,
        dv = SwapGuess,
        wid = Participant,
        within = Rating)

# Guess T-tests

t.test(expt2Blow$MixGuess,expt2Bmed$MixGuess, paired = TRUE)
t.test(expt2Blow$MixGuess,expt2Bhigh$MixGuess, paired = TRUE)
t.test(expt2Bmed$MixGuess,expt2Bhigh$MixGuess, paired = TRUE)

t.test(expt2Blow$SwapGuess,expt2Bmed$SwapGuess, paired = TRUE)
t.test(expt2Blow$SwapGuess,expt2Bhigh$SwapGuess, paired = TRUE)
t.test(expt2Bmed$SwapGuess,expt2Bhigh$SwapGuess, paired = TRUE)


# Swap Rates
expt2B$Participant <- as.factor(expt2B$Participant)
expt2B$Rating <- as.factor(expt2B$Rating)

ezANOVA(expt2B,
        dv = SwapB,
        wid = Participant,
        within = c(Rating))

# Swap T-tests

t.test(expt2Blow$SwapB,expt2Bmed$SwapB, paired = TRUE)
t.test(expt2Blow$SwapB,expt2Bhigh$SwapB, paired = TRUE)
t.test(expt2Bmed$SwapB,expt2Bhigh$SwapB, paired = TRUE)
