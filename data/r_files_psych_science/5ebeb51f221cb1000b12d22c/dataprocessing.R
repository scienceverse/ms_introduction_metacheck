##### Reanalysis of #####
##### Quoidbach, J., Taquet, M., Desseilles, M., de Montjoye, Y.-A., & Gross, J. J. (2019). 
##### Happiness and Social Behavior. Psychological Science. http://doi.org/10.1177/0956797619849666
##### osf.io/bxgn4
##### Dataprocessing

## computing alternative happiness 
ids <- unique(dat$user_id)

for(id in ids){ # loop on the individual level
  # test case:
  #id = "00393e8f5b979bc82ac28a059c8b5b3dc6752f09c75f0663fc5eed2c"
  # day = "2013-02-13"
  dat.id <- dat[dat$user_id == id,]
  # total average
  dat[dat$user_id == id, "totalAverage_nocurrent"] <- sapply(1:nrow(dat.id),function(x) mean(dat.id$Happiness_t1[-x]))
  dat[dat$user_id == id, "totalAverage_past_nocurrent"] <- sapply(1:nrow(dat.id),function(x) mean(dat.id$Happiness_t1[0:(x-1)]))
  dat[dat$user_id == id, "n.obs.user"][1] <- nrow(dat.id)
  for(day in unique(dat.id$date)){ # loop on the daily level
    dat.day <- dat.id[dat.id$date == day,] # subset to the current day
    # day average
    dat.day[,c("time","Happiness_t0","Happiness_t1","day_avg_happiness_no_current","Delta_happiness")]
    dat[dat$user_id == id & dat$date == day,"dayAverage_nocurrent"] <- sapply(1:nrow(dat.day),function(x) mean(dat.day$Happiness_t1[-x]))
    dat[dat$user_id == id & dat$date == day,"n.obs.day"][1] <- nrow(dat.day)
    
    if( nrow(dat.day) > 1){
      dat[dat$user_id == id & dat$date == day,"dayAverage_min2"] <- sapply(1:nrow(dat.day),function(x) mean(dat.day$Happiness_t1[-x]))
    }
    
    if( nrow(dat.day) >= 3){
      dat[dat$user_id == id & dat$date == day,"Happiness_t-1"] <-c(NA,NA,dat.day$Happiness_t1[1:(nrow(dat.day)-2)])
    }
    
    if( nrow(dat.day) >= 4){
      dat[dat$user_id == id & dat$date == day,"Happiness_t-2"] <-c(NA,NA,NA,dat.day$Happiness_t1[1:(nrow(dat.day)-3)])
    }
    
    # average of all previously reported happiness measures on a given day excluding t1 #
    dat[dat$user_id == id & dat$date == day,"dayAverage_past"] <- # compute the daily average of all happiness reports excluding T0 and T1
      sapply(1:nrow(dat.day),function(x) {
        if(x <= 1){ # if there s only onexisting measures that day (Happiness T1)
          return(NA)   # report an NA
        }else{ # if there are more measures than the Happiness T1 measure, report their mean
          return(mean(dat.day$Happiness_t1[1:(x-1)]))
        }
      })
    dat[dat$user_id == id & dat$date == day,"dayAverage_past_n.obs"] <- # count number of observations for this measure
      sapply(1:nrow(dat.day),function(x) {
        if(x <= 1){ 
          return(NA)   
        }else{ 
          return(length(dat.day$Happiness_t1[1:(x-1)]))
        }
      })
    
    # average of all previously reported happiness measures on a given day excluding t0 and t1 #
    dat[dat$user_id == id & dat$date == day,"dayAverage_past_noT0"] <- # compute the daily average of all happiness reports excluding T0 and T1
      sapply(1:nrow(dat.day),function(x) {
      if(x <= 2){ # if there are only two existing measures that day (Happiness T0 and Happiness T1)
        return(NA)   # report an NA
      }else{ # if there are more than the Happiness T0 and Happiness T1 measures, report their mean
        return(mean(dat.day$Happiness_t1[1:(x-2)]))
      }
    })
    dat[dat$user_id == id & dat$date == day,"dayAverage_past_noT0_n.obs"] <- # count number of observations for this measure
      sapply(1:nrow(dat.day),function(x) {
      if(x <= 2){ 
        return(NA)   
      }else{ 
        return(length(dat.day$Happiness_t1[1:(x-2)]))
      }
    })
    
    # past week
    dat.week <- dat.id[as.POSIXct(dat.id$date) %in% (as.POSIXct(day)-1:7*24*60*60),] # subset to the past week
    dat[dat$user_id == id & dat$date == day,"PastWeekAverage_nocurrent"] <-  mean(dat.week$Happiness_t1)
    dat[dat$user_id == id & dat$date == day,"n.obs.week"] <-  length(dat.week$Happiness_t1)
  }
  
  for(row in 1:nrow(dat.id)){
    # past day average
    dat.past.day <- dat.id[as.POSIXct(dat.id$timestamp, origin = "1970-01-01") >= as.POSIXct(dat.id$timestamp[row]-24*60*60, origin = "1970-01-01") &
                             as.POSIXct(dat.id$timestamp, origin = "1970-01-01") < as.POSIXct(dat.id$timestamp[row], origin = "1970-01-01") ,] # subset to the past day
    dat[dat$user_id == id & dat$timestamp == dat.id$timestamp[row],"n.obs.PastDay"][1] <- nrow(dat.past.day)
    dat[dat$user_id == id & dat$timestamp == dat.id$timestamp[row],"PastDayAverage_nocurrent"] <- mean(dat.past.day$Happiness_t1)
  }

  cat(paste0("\r ", which(ids == id), " out of ", length(ids)))
}

# create the person-mean centerd happiness variable 
dat$Happiness_t1_pmc <- pcenter(dat$user_id, dat$Happiness_t1)
dat$Happiness_t0_pmc <- pcenter(dat$user_id, dat$Happiness_t0)

# create the "being in an interaction" variable from the alone variable
dat$int_t0 <- abs(1-dat$alone_t0)
dat$int_t1 <- abs(1-dat$alone_t1) 

save(dat, file = paste0("dat_",Sys.Date(),".RData"))
