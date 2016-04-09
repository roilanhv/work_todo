

est.to.dbhm <- function(data.in = est83669, 
                        vars = "rsum",
                        vars2 = "prec",
                        mult = 10,
                        roun = 0,
                        dir_out = "./"){
    
    for.dbhm <- data.frame( std_id = data.in$site,
                            ano    = as.integer(format(as.Date(data.in$date), "%Y")),
                            mes    = as.integer(format(as.Date(data.in$date), "%m")),
                            dia    = as.integer(format(as.Date(data.in$date), "%d")),
                            tm     = 32766,
                            tmax   = 32766,
                            tmin   = 32766,
                            um     = 32766,
                            umin   = rep(32766,times = length(data.in$date)),
                            n_sum  = rep(32766,times = length(data.in$date)),
                            n_lown = rep(32766,times = length(data.in$date)),
                            fsm    = 32766,
                            fmaxx  = 32766,
                            fmaxs  = 32766,
                            rsum   = 32766,
                            d0m    = 32766,
                            sun    = 32766,
                            E01    = 32766,
                            snow   = 32766,
                            mslp   = 32766,
                            patm   = 32766
    )
    # "prec","tar","tmax","tmin","patm","pnmm",  "wsx",  "n", "ur", "ws"
    aa <-  data.frame(prec = c(0.0,300),
                      tar = c(-90.0,70),
                      tmax= c(-90.0,70),
                      tmin= c(-90.0,70),
                      ws = c(0.0,100),
                      wsx = c(0.0,100),
                      n = c(0.0,24.0),
                      ur = c(1.0,100.0),
                      patm = c(100,2000),
                      pnmm = c(100,2000)
    )
    
    for(i in 1:length(vars)){ #i = 8
      data.in[which(data.in[,vars2[i]] < aa[1,vars2[i]]) ,vars2[i]] <- NA
      data.in[which(data.in[,vars2[i]] > aa[2,vars2[i]]) ,vars2[i]] <- NA
        for.dbhm[,vars[i]] <- round(data.in[,vars2[i]]*mult[i],roun)
        cat("Var", vars2[i],"in var",vars[i],". Factor *",mult[i],"\n")
    }
    
    
    # for.dbhm         <- data.frame( std_id = data.in$site,
    #                                ano    = as.integer(format(as.Date(data.in$date), "%Y")),
    #                                mes    = as.integer(format(as.Date(data.in$date), "%m")),
    #                                dia    = as.integer(format(as.Date(data.in$date), "%d")),
    #                                tm     = round(data.in$tar*10,0),
    #                                tmax   = round(data.in$tmax*10,0),
    #                                tmin   = round(data.in$tmin*10,0),
    #                                um     = round(data.in$ur,0),
    #                                umin   = rep(32766,times = length(data.in$date)),
    #                                n_sum  = rep(32766,times = length(data.in$date)),
    #                                n_lown = rep(32766,times = length(data.in$date)),
    #                                fsm    = round(data.in$ws*10,0),
    #                                fmaxx  = round(data.in$wd,0),
    #                                fmaxs  = rep(32766,times = length(data.in$date)),
    #                                rsum   = round(data.in$prec*10,0),
    #                                d0m    = rep(32766,times = length(data.in$date)),
    #                                sun    = round(data.in$cc*10,0),
    #                                E01    = rep(32766,times = length(data.in$date)),
    #                                snow   = rep(32766,times = length(data.in$date)),
    #                                mslp   = rep(32766,times = length(data.in$date)),
    #                                patm   = round(data.in$patm,0)
    # )
    for.dbhm[is.na(for.dbhm)]  <-  32766
    for.dbhm[for.dbhm == Inf]  <-  32766
    for.dbhm[for.dbhm == -Inf] <-  32766
    
    write.table(x = for.dbhm, 
                file = paste0(dir_out,unique(data.in$site),".dbhm"),
                sep = "\t ", 
                row.names = F,
                col.names = F 
                )  
    
     return(cat("FILE created:",paste0(dir_out,unique(data.in$site),".dbhm","\n")))
}

info.est.to.dbhm <- function(lon,
                             lat,
                             code,
                             alt,
                             dir_out = dir_out){
    
    all.stn <- data.frame(code,round(lon,4),round(lat,5),alt)
    
    write.table(x = all.stn,
                file = paste0(dir_out,"all.stn.txt"),
                quote = FALSE,
                sep = "     \t",
                row.names = FALSE,col.names = FALSE )
    return(cat("FILE:", paste0(dir_out,"all.stn.txt","\n")))
}









