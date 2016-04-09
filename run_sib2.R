
sib2 <- function(date1="1995010101",
                 date2="2008123124",
                 infile2="/home/hidro2roilan/Dropbox/Dissertacao/torre2",
                 dtt="3600",
                 itrunk = "20",
                 ilw = "5",
                 ivtype = "2",
                 istype = "3",
                 isnow = "0",
                 ipbl = "1",
                 idirr = "0",         #!
                 decay = "0.85",      #!variando
                 zlong = "-47.63",
                 zlat = "-21.61",
                 poros = "0.458"  ,   #! soilpara(1,k)
                 phsat = "-0.2"  ,    #! soilpara(2,k)
                 satco = "3.5E-06",   #! soilpara(3,k)
                 bee = "7.797"   ,    #! soilpara(4,k)
                 slope = "0.08"  ,    #! soilpara(5,k)
                 #!speyield = 0.20  , #! soilpara(6,k)
                 slpp = "1.0",
                 tc = "298.0",
                 tg = "298.0",
                 td = "297.0",
                 capac = c("0.0", "0.0"),
                 snoww = c("0.0", "0.0"),
                 www = c("0.24", "0.17", "0.17"),
                 surdep = "0.0",
                 gwdep = "6.0",
                 app = "0.0001",
                 bpp = "20.0",
                 cpp = "0.9999",
                 tc_ini = "298.0",
                 tg_ini = "298.0",
                 td_ini = "297.0",
                 www_ini = c("0.34", "0.27", "0.27"),
                 zwind = "45.0",
                 zmet = "45.0",
                 gmudmu = "1.0",
                 green = "0.54",
                 vcover = "0.97",
                 id = "001",
                 verbose = FALSE,
                 vars_out = "",
                 dir_out = paste0("/home/",system("echo $USER",intern = TRUE),"/Dropbox/Dissertacao/")){
 
    if(verbose){
        cat(">","\n","Arquivo com dados de entrada:","\n")
        cat(infile2,"\n")
        cat("diretorio de saida: ",dir_out,"\n")
        
    }    
    infile2 =  paste0("'",infile2,"'") 
    dir_out =  paste0("'",dir_out,"'") 
# Parâmetros de entrada para o DBHM
     paras <- paste(date1, date2, infile2, dtt, itrunk, ilw, ivtype, istype, isnow, 
         ipbl, idirr, decay, zlong, zlat, poros, phsat, satco, bee, slope, slpp, 
         tc, tg, td, capac[1], capac[2], snoww[1], snoww[2], www[1],www[2],www[3], 
         surdep, gwdep, app, bpp, cpp, tc_ini, 
         tg_ini, td_ini, www_ini[1], www_ini[2], www_ini[3], zwind, zmet, gmudmu,
         vcover,green,
         id, dir_out)
# Linha de comando para rodar o SiB2
     command_line <- paste("cd ~/Dropbox/Dissertacao/sib2_offline/; echo",
                     "\"", 
                     paras,
                     "\"",
                     "| time -p ./sib2_offline.out")
    if(id == "666") command_line <- paste("cd ~/Dropbox/Dissertacao/sib2_offline/; echo",
                     "\"", 
                     paras,
                     "\"",
                     "| time -p ./sib2_offlinesinsun.out")
     if(verbose){
        cat("=======================================================================================","\n")
         cat(paras,"\n")
         cat("=======================================================================================","\n")
         cat(command_line, "\n")
         cat("=======================================================================================","\n")
     }         
# Chamada para rodada do DBHM
     system(command = command_line,
            intern = verbose,
            ignore.stdout = !verbose,ignore.stderr = !verbose )
# Lendo a saída do DBHM
     date = data.frame(date = seq.POSIXt(from = as.POSIXct(date1,tz = "GMT","%Y%m%d%H")- 3600,
                                            to =  as.POSIXct(date2,tz = "GMT","%Y%m%d%H")-3600,
                                            by = "hour"))
    if(verbose) cat("Reading sib2diag.tx","\n")
    fww <- read.table(file = gsub(pattern = "'",replacement = "",
                                  x = paste0(dir_out,"sib2diag",id,".txt")),
          header = TRUE,nrows = length(date[,1]))
        
# Calculando variáveis de conteúdo de água no solo [camada 0-1m, 1-2m]
    if(verbose) cat("Calculando conteudo de água nas camadas","\n")
    if( (fww$zdepth1[1] + fww$zdepth2[1]) >= 1.0){
        fww$swc1 <- fww$poros *(fww$www1 * fww$zdepth1 + fww$www2 * (1.0 - fww$zdepth1))*1000
        fww$swc2 <- fww$poros *(fww$www2 * (fww$zdepth1+fww$zdepth2 - 1.0) + 
                                    fww$www3 * (2.0-(fww$zdepth1+fww$zdepth2)))*1000
    } else {
        fww$swc1 <- fww$poros *(fww$www1 * fww$zdepth1 + fww$www2 * fww$zdepth2 + 
                                    (1.0 - fww$zdepth1 - fww$zdepth2) * fww$www3 )*1000
        fww$swc2 <- fww$poros * fww$www3 *1000
    }
    fww$swc12=fww$swc1+fww$swc2 
    # Componentes de LE em mm/h

    if("LE.mm" %in% vars_out) fww$LE.mm <- fww$LE * 1.4688e-3
    if("ec.mm" %in% vars_out) fww$ec.mm <- fww$ec * 1.4688e-3
    if("eci.mm" %in% vars_out) fww$eci.mm <- fww$eci * 1.4688e-3
    if("ect.mm" %in% vars_out) fww$ect.mm <- fww$ect * 1.4688e-3
    if("eg.mm" %in% vars_out) fww$eg.mm <- fww$eg * 1.4688e-3
    if("egi.mm" %in% vars_out) fww$egi.mm <- fww$egi * 1.4688e-3
    if("egs.mm" %in% vars_out) fww$egs.mm <- fww$egs * 1.4688e-3
    # Selecionando variáveis específicas se for o caso
    # # Para todas as variáveis
    
    if(vars_out[1] == "") vars_out <- names(fww)
      if(verbose) cat("selecionando variáveis","\n")
         out <- cbind(date,fww[,vars_out])
           names(out) <- c("date",vars_out)
    
    return(out)
}












