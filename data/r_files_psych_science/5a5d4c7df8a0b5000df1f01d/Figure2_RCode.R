library(ggplot2)

df = data.frame(Condition=rep(c("Friend Gain | Parent Lose", "Parent Gain | Friend Lose"), each=2),
     Type=rep(c("Simulated", "Real"),2),
    Likelihood=c(2.545597, 2.620146, 2.76434, 2.639456), 
    SE = c(.053, .068, .035, .041)) #Values in these two latter lines taken from sample specific analyses 

ggplot(data=df, aes(x=Condition, y=Likelihood, group=Type)) +
  geom_line(aes(linetype = Type, group=Type))+
  geom_point()+
  geom_errorbar(aes(ymin = Likelihood-SE, ymax = Likelihood+SE), width=.04)+
  theme_bw()


