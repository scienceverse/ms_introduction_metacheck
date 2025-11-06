
require(lavaan)

multipleivMediation<-'
#direct effect
std_immigpolicy~b1*std_symthreatavg + b2*std_realthreatavg + c1*std_genthreat + c2*std_persthreat
#mediator
std_symthreatavg ~ a1*std_genthreat +a2*std_persthreat
std_realthreatavg ~ a3*std_genthreat + a4*std_persthreat
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b1
indirect3 := a3*b2
indirect4 := a4*b2
#total effects
total1 := c1 + (a1*b1) + (a3*b2) 
total2 := c2 + (a2*b1) +(a4*b2)
std_symthreatavg ~~ std_realthreatavg
'

fit<-sem(model = multipleivMediation, data=study2c)
summary(fit)
