# Compare Study 2 and GSS 2021
#———————————————————————————————————————————————————————————————————————————————
gss <- read_csv("Data/GSS Comparison/s2_data_gss.csv")

describe(gss$grncon)
describe(gss$polviews)

s2$grncon <- s2$envConc / (100/4) + 1
s2$polviews <- s2$politics / (100/6) + 1

gssComp <- rbind(gss %>% transmute(sample="GSS 2021", grncon),
                 s2 %>% transmute(sample="Study 2", grncon))

gssComp %>% 
  group_by(sample) %>%
  summarise(m = mean(grncon, na.rm=T),
            ci = 1.96 * sd(grncon, na.rm=T) / sqrt(n())) %>% 
  ggplot(aes(factor(sample), m)) +
  geom_violin(data=gssComp, aes(sample, grncon), 
              fill="grey", color=NA, alpha=.5, bw=.4) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=m-ci, ymax=m+ci), width=0) +
  scale_y_continuous(breaks=c(1,5), labels=c("Not at all", "Very"),
                     sec.axis = sec_axis(~., breaks=c(1,3,5), 
                                         labels=c("Not at all", 
                                                  "Moderately",
                                                  "Extremely"))) +
  theme_bw() +
  theme(axis.text = element_text(size=11, color="black"),
        axis.title = element_text(size=12, color="black")) +
  labs(x= NULL, y=NULL,
       title="Generally speaking, how concerned \nare you about environmental issues?”")