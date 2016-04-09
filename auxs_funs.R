

timeCumsum <- function(mydata = sib2in %>% select(date,LE,eci),
                       ave.time = "month"){
  
  require(plyr);require(dplyr)
  
  if(ave.time == "year") form = "%Y"
  if(ave.time == "month") form = "%Y-%m"
  if(ave.time == "day") form = "%Y-%m-%d"
  
  if(ave.time == "season") {
    cat("Warning: Fixed for southern hemisphere")
    
    mydata <- cutData(mydata,type = "season",hemisphere = "southern")
      period <- expand.grid(unique(format(mydata$date,"%Y")), unique(mydata$season))
        n.period <- nrow(period)
        
     new.mydata <- llply(1:n.period,.progress = "text", function(i){ # i <- 2
          
          if(period[i,2] == "summer (DJF)"){
            
            rows.period <- c(which(format(mydata$date,"%Y") == as.numeric(as.vector(period[i,1])) -1 
                                 &
                                 format(mydata$date,"%m") == 12 
                                 &
                                 mydata$season == period[i,2])
                             ,
                             which(format(mydata$date,"%Y") == as.numeric(as.vector(period[i,1])) 
                                 &
                                 format(mydata$date,"%m") %in% c("01","02") 
                                 &
                                 mydata$season == period[i,2])
            )
              
          } else { 
            
          rows.period <- which(format(mydata$date,"%Y") == period[i,1] 
                               &
                              mydata$season == period[i,2])
          }
       
       
            subs <- cumsum(mydata[rows.period,-which(names(mydata) %in% c("date","season"))])
          subs[1,] <- NA
          mydata[rows.period,-which(names(mydata) %in% c("date","season"))] <- subs
          mydata$id <- period[i,2]
          mydata$season <- NULL
          return(mydata[rows.period,])
        })     
    
  } else {
  
        period <- unique(format(mydata$date,form))
        n.period <- length(period)
        
        new.mydata <- llply(period,.progress = "text", function(i){ # i <- period[3]
          rows.period <- which(format(mydata$date,form) == i)
          subs <- cumsum(mydata[rows.period,-which(names(mydata)=="date")])
          subs[1,] <- NA
          mydata[rows.period,-which(names(mydata)=="date")] <- subs
          mydata$id <- i
          return(mydata[rows.period,])
        })
  
  }

  my.data <- bind_rows(new.mydata)
  return(my.data)
  
}


make.barplot <- function(bars = comp.le,
                         sequential = rainbow(4),
                         main = "",
                         beside = FALSE){
  
  barplot(t(bars[,-1]), 
          names.arg = bars$date %>% format("%Y-%m"),
          cex.names = 0.8,
          col = sequential, 
          beside = beside,
          width = 1, 
          las = 2,
          space = ifelse(beside,c(1,1),0.2),
          xlim = c(0, (length(bars$date)+18)+ beside*ncol(bars)),
          main = main,
          axes = FALSE
  )
  box(bty = "l")
   axis(2,padj = 1)
  legend(ifelse(beside,"top","bottomright"),
         bty = "n",cex = 0.75,ncol = ifelse(beside,ncol(bars)-1,1),
         legend = names(bars[,-1]), #in order from top to bottom
         fill = sequential, # 6:1 reorders so legend order matches graph
         title = "LE comp")
  
  
}

# Function to convert from Wm-2 to mm for nhour*3600s 
wm2.to.mm <- function(var,nhour=1){
  if("date" %in% names(var)) {
    var[,-which(names(var) == "date")] <- var[,-which(names(var) == "date")]*nhour*3600/2454000
  } else {
    var <- var*nhour*3600/2454000
  }
  return(var)}
