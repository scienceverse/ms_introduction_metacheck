library(boot)

meanboot <- function(d,i)
{
  return(mean(d[i]))
}


bootstrapci <- function(x,R)
{
  bt <- boot(data = x, statistic = meanboot, R)
  if (R > 1)
  { 
    bic <- boot.ci(bt, type="basic")
    sbt <- sort(bt$t)
    return(c(sbt[floor(R*0.025)], sbt[round(R/2)], sbt[ceiling(R*0.975)]))
  }
  else
  {
    return(rep(mean(x),3))
  }
}

ciTS <- function (df, var, R, noRes=FALSE)
{
  if (noRes)
  {df %>% split(.$date) %>% lapply(function(x) bootstrapci(x[[var]],R)) %>% do.call(rbind,.) -> vts}  
  else
  {df %>% split(.$date) %>% lapply(function(x) bootstrapci(x[[var]]/x$n,R)) %>% do.call(rbind,.) -> vts}  
  
  dvts <- data.frame(date = rownames(vts), low=vts[,1], mid=vts[,2], hi = vts[,3], row.names = NULL)
  return(dvts)
}

plotts <- function(ts, ylab="Normalized Daily Average", col="black", 
                   bgcolor="gray", zero=TRUE,
                   add=FALSE, nolog=FALSE, plt=NULL, dtbreaks="2 weeks")
{
  if (is.null(plt))
  { 
    if (nolog)
    {
      plt <- ggplot(data = ts, aes(x=as.Date(date), y=mid))
      plt <- plt + geom_ribbon(aes(ymin=low, ymax=hi), fill=bgcolor) 
      plt <- plt + geom_line(aes(x=as.Date(date), y=mid),colour=col)
      plt <- plt + geom_point(aes(x=as.Date(date), y=ts$mid),colour=col, size=0.75)
    }
    if (nolog == FALSE)
    {
      plt <- ggplot(data = ts, aes(x=as.Date(date), y=log(mid/bl)))
      plt <- plt + geom_ribbon(aes(ymin=log(low/bl), ymax=log(hi/bl)), fill=bgcolor) 
      plt <- plt + geom_line(aes(x=as.Date(date), y=log(mid/bl)),colour=col)
      plt <- plt + geom_point(aes(x=as.Date(date), y=log(ts$mid/ts$bl)),colour=col, size=0.75)
    }
    plt <- plt + scale_x_date("Day", date_breaks = dtbreaks) + scale_y_continuous(name=ylab)
    plt <- plt + theme_bw() + geom_vline(xintercept=as.numeric(as.Date("2015-11-13")), col=rgb(1,0,0,0.5), lwd=1, lty=2) 
    if (zero)
      plt <- plt + geom_hline(yintercept = 0, col=rgb(0,0,0,0.5), lwd=1,lty=2)
  }
  else
  {
    if (nolog)
    {
      plt <- plt + geom_ribbon(data=ts, aes(ymin=low, ymax=hi), fill=bgcolor) 
      plt <- plt + geom_line(data=ts, aes(x=as.Date(date), y=mid),colour=col)
      plt <- plt + geom_point(data=ts, aes(x=as.Date(date), y=mid),colour=col, size=0.75)
    }
    if (nolog==FALSE)
    {
      plt <- plt + geom_ribbon(data=ts, aes(ymin=log(low/bl), ymax=log(hi/bl)), fill=bgcolor) 
      plt <- plt + geom_line(data=ts, aes(x=as.Date(date), y=log(mid/bl)),colour=col)
      plt <- plt + geom_point(data=ts, aes(x=as.Date(date), y=log(mid/bl)),colour=col, size=0.75)
    }
  }
  return(plt)
}


TSmodel1 <- function(ts, zp)
{
  t <- seq(1, length(ts))
  z <- t==zp 
  post <- t>zp
  z2 <- t == zp+1
  ti <- 2:length(ts); 
  tp <- 1:(length(ts)-1)
  y <- ts
  yt <- y[ti]; 
  ypre <- y[tp]
  z <- z[ti]
  z2 <- z2[ti]
  post <- post[ti]
  reg <- bayesglm(yt~ypre:post+z+z2)
  return(reg)
}

PairTSW <- function (df, sel, var, w=1, R=100)
{
  dfr <- inner_join(data.frame(date=df$date,x=df[[var]]/df$n, user=df$user), sel, by="user")
  dfT <- filter(dfr, dfr$sel==TRUE)
  dfF <- filter(dfr, dfr$sel==FALSE)
  
  dates <- sort(unique(dfr$date))
  TS <- data.frame()
  for (i in seq(w+1, length(dates)))
  {
    print(i)
    datessel <- dates[seq(i-w,i)]
    dfT %>% filter(date %in% datessel) -> dfTsel
    dfF %>% filter(date %in% datessel) -> dfFsel
    Tboot <- bootstrapci(dfTsel$x,R)
    Fboot <- bootstrapci(dfFsel$x,R)
    TS <- rbind(TS, data.frame(date=datessel[w], 
                               Tlow=Tboot[1], Tmid=Tboot[2], Thi = Tboot[3],
                               Flow=Fboot[1], Fmid=Fboot[2], Fhi=Fboot[3]))
  }
  
  return(TS)
}


PairTSDifW <- function (df, sel, var, w=1, R=100)
{
  dfr <- inner_join(data.frame(date=df$date,x=df[[var]]/df$n, user=df$user), sel, by="user")
  dfT <- filter(dfr, dfr$sel==TRUE)
  dfF <- filter(dfr, dfr$sel==FALSE)
  
  dates <- sort(unique(dfr$date))
  TS <- data.frame()
  for (i in seq(w+1, length(dates)))
  {
    print(i)
    datessel <- dates[seq(i-w,i)]
    dfT %>% filter(date %in% datessel) -> dfTsel
    dfF %>% filter(date %in% datessel) -> dfFsel
    
    Tboot <- boot(data = dfTsel$x, statistic = meanboot, R)
    Fboot <- boot(data = dfFsel$x, statistic = meanboot, R)
    
    sbt <- sort(Tboot$t-Fboot$t)
    TS <- rbind(TS, data.frame(date=datessel[w], 
               low=sbt[floor(R*0.025)], mid=sbt[round(R/2)], hi=sbt[ceiling(R*0.975)]))
      
  }
  
  return(TS)
}




