# Comparing the correlation coefficients ----------------------------------
dcor <- dlongw2

# Create the correlation variables of the signed errors
dcor$guess_1_cor <- dcor$guess_1 - dcor$answer
dcor$guess_2_cor <- dcor$guess_2 - dcor$answer

# A function for coefficient comparisons 
cor.diff.test = function(x1, x2, y1, y2, method="pearson") {
  cor1 = cor.test(x1, x2, method=method)
  cor2 = cor.test(y1, y2, method=method)
  
  r1 = cor1$estimate
  r2 = cor2$estimate
  n1 = sum(complete.cases(x1, x2))
  n2 = sum(complete.cases(y1, y2))
  fisher = ((0.5*log((1+r1)/(1-r1)))-(0.5*log((1+r2)/(1-r2))))/((1/(n1-3))+(1/(n2-3)))^0.5
  
  p.value = (2*(1-pnorm(abs(fisher))))
  
  result= list(
    "cor1" = list(
      "estimate" = as.numeric(cor1$estimate),
      "p.value" = cor1$p.value,
      "n" = n1
    ),
    "cor2" = list(
      "estimate" = as.numeric(cor2$estimate),
      "p.value" = cor2$p.value,
      "n" = n2
    ),
    "p.value.twosided" = as.numeric(p.value),
    "p.value.onesided" = as.numeric(p.value) / 2
  )
  cat(paste(sep="",
            "cor1: r=", format(result$cor1$estimate, digits=3), ", p=", format(result$cor1$p.value, digits=3), ", n=", result$cor1$n, "\n",
            "cor2: r=", format(result$cor2$estimate, digits=3), ", p=", format(result$cor2$p.value, digits=3), ", n=", result$cor2$n, "\n",
            "diffence: p(one-sided)=", format(result$p.value.onesided, digits=3), ", p(two-sided)=", format(result$p.value.twosided, digits=3), "\n"
  ))
  return(result);
}

# Calculate the difference
dcorself <- subset(dcor, dcor$condition == "Self")
dcordis <- subset(dcor, dcor$condition == "Disagree")
dcorag <- subset(dcor, dcor$condition == "Agree")


cor.diff.test(dcorself$guess_1_cor,
              dcorself$guess_2_cor,
              dcordis$guess_1_cor,
              dcordis$guess_2_cor)

cor.diff.test(dcorag$guess_1_cor,
              dcorag$guess_2_cor,
              dcordis$guess_1_cor,
              dcordis$guess_2_cor)

cor.diff.test(dcorag$guess_1_cor,
              dcorag$guess_2_cor,
              dcorself$guess_1_cor,
              dcorself$guess_2_cor)
