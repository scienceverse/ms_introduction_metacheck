library(BayesFactor)
library(lsr)

df <- rio::import("Data_public.xls")
df_trans <- rio::import("RTs_after_boxcox.xls")

# RTs_after_boxcox.xls stores numbers as text, for some reason.
for (f in names(df_trans)) {
  df_trans[, f] <- as.numeric(df_trans[, f])
}

# p. 476: Dot-probe tasks.
wilcox.test(df$Masked_Valid_RT, df$Masked_Invalid_RT, paired=TRUE)    # p=0.030; doesn't seem to match article

t.test(df_trans$RT_Con_Valid, df_trans$RT_Con_Invalid, paired=TRUE)
cohensD(df_trans$RT_Con_Valid, df_trans$RT_Con_Invalid, method="paired")
ttestBF(df_trans$RT_Con_Valid, df_trans$RT_Con_Invalid, paired=TRUE)

# p. 476: Identification tasks.
t.test(df$Masked_D_prime, mu=0)
ttestBF(df$Masked_D_prime, mu=0)

t.test(df$Umasked_D_prime, mu=0)
cohensD(df$Umasked_D_prime)
ttestBF(df$Umasked_D_prime, mu=0)

# p. 476: ERP results.
t.test(df$Masked_contr_Early, df$Masked_ipsi_Early, paired=TRUE)
cohensD(df$Masked_contr_Early, df$Masked_ipsi_Early, method="paired")
ttestBF(df$Masked_contr_Early, df$Masked_ipsi_Early, paired=TRUE)

t.test(df$Unmask_contr_Early, df$Unmasked_Ipsi_Early, paired=TRUE)
cohensD(df$Unmask_contr_Early, df$Unmasked_Ipsi_Early, method="paired")
ttestBF(df$Unmask_contr_Early, df$Unmasked_Ipsi_Early, paired=TRUE)

t.test(df$Unmask_contr_Late, df$Unmask_ipsi_Late, paired=TRUE)
cohensD(df$Unmask_contr_Late, df$Unmask_ipsi_Late, method="paired")
ttestBF(df$Unmask_contr_Late, df$Unmask_ipsi_Late, paired=TRUE)

# p. 477: Divided sample into two groups based on masked D-prime performance.
df$Masked_D_prime_chance <- (df$Masked_D_prime < 0.18)   # or we could sort and split into two
df1 <- df[(df$Masked_D_prime_chance),]
df2 <- df[(! df$Masked_D_prime_chance),]
t.test(df1$masked_N2pc_E, df2$masked_N2pc_E, var.equal=T)
ttestBF(df1$masked_N2pc_E, df2$masked_N2pc_E)

# p. 477: N2pc and d-prime correlation.
cor.test(df$masked_N2pc_E, df$Masked_D_prime)     # in the CSV file, this is "maksed_N2pc_E" [sic]
correlationBF(df$masked_N2pc_E, df$Masked_D_prime)

cor.test(df$unmasked_N2pc_E, df$Umasked_D_prime)
correlationBF(df$unmasked_N2pc_E, df$Umasked_D_prime)
