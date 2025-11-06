# adjusted from the Lakens workshop by Gilad Feldman
# mini meta analyses functions

PrintMetaResult  <- function (metadataframe) {
  print(summary(metadataframe))

}

PrintPlotMetaR  <- function (metadataframe) {
  forest(metadataframe, leftcols=c("studlab"), studlab=TRUE, xlab="Hedges' g", col.square="grey", col.square.lines="black", col.study="black", fontsize=11, comb.fixed = FALSE, text.random = "Random", hetstat = FALSE)
}

PrintFunnelMetaR  <- function (metadataframe) {

  funnel(metadataframe, comb.fixed=TRUE,
         level=0.95, contour=c(0.9, 0.95, 0.99),
         col.contour=c("darkgrey", "grey", "lightgrey"),
         lwd=2, cex=2, pch=16, studlab=TRUE, cex.studlab=1.25,
         xlim=c(-1,1))
  legend(-.6, 0.0,
         c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
         fill=c("darkgrey", "grey", "lightgrey"))
  
  trimfill(metadataframe)
}

ExpMetaR  <- function (Study, N1, N2, M1, M2, SD1, SD2) {
  
  if(!require(MBESS)){install.packages('MBESS')}
  if(!require(meta)){install.packages('meta')}
  
  bias<-1-3/(4*(N1+ N2-2)-1) #correction for bias
  ESg <-numeric(length(N1))
  
  #Use MBESS to calc d unbiased (Hedges g)
  for(i in 1:length(N1)){ 
    ESg[i]<-smd(Mean.1= M1[i], Mean.2=M2[i], s.1=SD1[i], s.2=SD2[i], n.1=N1[i], n.2=N2[i], Unbiased=FALSE) 
  }
  
  #Daniel Lakens:Some formulas add *((n+n)/(n+n-2)) - we don't so that results are consistent with meta-analysis
  ESdv <-(((N1+N2)/(N1*N2))+(ESg^2/(2*(N1+N2))))*bias^2  
  
  #Calculate Standard Errors ES
  SE<-sqrt(ESdv)
  
  meta<-metagen(ESg, SE, studlab=Study)
  
  return (meta)
}
