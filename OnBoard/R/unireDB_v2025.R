## MERGE ACCESS COMING FROM DIFFERENTE PC AND THE MEASURES TAKEN ONBOARD IN HAULS EXCEL SHEET
# It allows merging hauls also in case the same haul is present in more than one PC

# Before start it is needed to change the working directory and the access names based on the present year

library(magrittr)
library(dplyr)
library(purrr)
library(readxl)
library(ggplot2)
library(RODBC)
library(gridExtra)
library(stringr)
library(grid)
rm(list = ls())
'%ni%'=Negate('%in%')
wd_acces="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2025/OnBoard"
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

source_file <- paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_template.accdb")
file.copy(source_file,  paste0(wd_acces,"/access/Maschera inserimento SOLEMON_commerciale.accdb"), overwrite = TRUE)
file.copy(source_file,  paste0(wd_acces,"/access/Maschera inserimento SOLEMON_benthos.accdb"), overwrite = TRUE)

#file.copy(source_file,  paste0(wd_acces,"/bio_data_v2024_SOLEMON_BENTHOS_complete.accdb"), overwrite = TRUE)
target_species<-read.csv(paste0(wd_acces,"/data/target_species.csv"))%>%filter(target==1)


store.hauls=list()

# get info from db 1
MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_ENA.accdb")
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH)
acctables=sqlTables(channel) # look for tables
close(channel)

# tables to be checked
tables_check=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
tables_check=tables_check[-grep('template|test', tables_check)]
tables_check=data.frame(original_name=tables_check,format_name=tables_check)
tables_check$original_name=str_remove(tables_check$original_name, 'cala_')


# get info from db 2
MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_FRA.accdb")
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH)
acctables=sqlTables(channel) # look for tables
close(channel)

# tables to be checked
tables_check2=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
tables_check2=tables_check2[-grep('template|test', tables_check2)]
tables_check2=data.frame(original_name=tables_check2,format_name=tables_check2)
tables_check2$original_name=str_remove(tables_check2$original_name, 'cala_')


# find hauls in databases
common_hauls=unique(c(tables_check[tables_check$original_name %in% tables_check2$original_name,]$original_name,
tables_check2[tables_check2$original_name %in% tables_check$original_name,]$original_name))

hauls_db_ENA=tables_check[tables_check$original_name %ni% common_hauls,]
hauls_db_ENA$id=seq(1:nrow(hauls_db_ENA))

hauls_db_FRA=tables_check2[tables_check2$original_name %ni% common_hauls,]
hauls_db_FRA$id=seq(1:nrow(hauls_db_FRA))

# find hauls with data taken onboard and to be added
dir.target.ob="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2025/OnBoard/data/onboard_measures"
x.files.ob=data.frame(target.file=list.files(path=dir.target.ob))
x.files.ob$hauls.ob=str_remove(str_remove(x.files.ob$target.file,'_onboard_meas.csv'),'haul_')

# upload on final db ####
# hauls only in db 0
cat("Check hauls in db ENA")
for(icheck in 1:nrow(hauls_db_ENA)){
  
  haul=hauls_db_ENA[icheck, ]$format_name
  haul.name=paste('cala',hauls_db_ENA[icheck, ]$original_name,sep='_')
  cat(paste0(haul.name," "))
  # import table 1
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_ENA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  close(channel)
  
  # # check if more data needs to be added #
  # if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
  #   tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
  #   xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
  #   
  #   xdat.template=xdat[1,]
  #   xdat.template[1,]=NA
  #   xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
  #   rownames(xdat.template)=NULL
  #   xdat.template$gear=toupper(xdat.ob$Gear)
  #   xdat.template$gear=ifelse(xdat.template$gear=="",NA,xdat.template$gear)
  #   xdat.template$species_name=toupper(xdat.ob$Species)
  #   xdat.template$species_name=ifelse(xdat.template$species_name=="",NA,xdat.template$species_name)
  #   xdat.template$length_mm=xdat.ob$Lenght_mm
  #   xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
  #   xdat.template$Sex=toupper(xdat.ob$Sex)
  #   xdat.template$Mat=xdat.ob$Mat
  #   xdat.template$weight_g=ifelse(is.na(xdat.ob$Weight_g),xdat.ob$Total.weight_g,
  #                                 xdat.ob$Weight_g)
  #   xdat.template$total_number=xdat.ob$Number
  #   xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
  #   xdat=rbind(xdat, xdat.template)
  # }
  
  # save on final db
  
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat), tablename = haul.name)
  #close(channel)
  xdat$ID=1:nrow(xdat)
  store.hauls[[icheck]]=xdat
  names(store.hauls)[icheck]=haul
}


# hauls only in db 1
cat("Check hauls in db FRA")
for(icheck in 1:nrow(hauls_db_FRA)){
  
  haul=hauls_db_FRA[icheck, ]$format_name
  haul.name=paste('cala',hauls_db_FRA[icheck, ]$original_name,sep='_')
  cat(paste0(haul.name," "))
  # import table 1
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_FRA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  
  close(channel)
  
  # # check if more data needs to be added #
  # if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
  #   tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
  #   xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
  #   
  #   xdat.template=xdat[1,]
  #   xdat.template[1,]=NA
  #   xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
  #   rownames(xdat.template)=NULL
  #   xdat.template$gear=toupper(xdat.ob$Gear)
  #   xdat.template$gear=ifelse(xdat.template$gear=="",NA,xdat.template$gear)
  #   xdat.template$species_name=toupper(xdat.ob$Species)
  #   xdat.template$species_name=ifelse(xdat.template$species_name=="",NA,xdat.template$species_name)
  #   xdat.template$length_mm=xdat.ob$Lenght_mm
  #   xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
  #   xdat.template$Sex=toupper(xdat.ob$Sex)
  #   xdat.template$Mat=xdat.ob$Mat
  #   xdat.template$weight_g=ifelse(is.na(xdat.ob$Weight_g),xdat.ob$Total.weight_g,
  #                                 xdat.ob$Weight_g)
  #   xdat.template$total_number=xdat.ob$Number
  #   xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
  #   xdat=rbind(xdat, xdat.template)
  # }
  
  xdat$ID=1:nrow(xdat)
  # save on final db
  store.hauls=append(store.hauls, list(xdat))
  names(store.hauls)[length(store.hauls)]=haul
  
  
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat), tablename = haul.name)
  #close(channel)
  
}


# hauls in common
cat("Check hauls in common \n")
for(icheck in 1:length(common_hauls)){
  
  haul=tables_check[tables_check$original_name==common_hauls[icheck], ]$format_name
  haulbis=tables_check2[tables_check2$original_name==common_hauls[icheck], ]$format_name
  haul.name=paste('cala',tables_check[tables_check$original_name==common_hauls[icheck], ]$original_name,sep='_')
  cat(paste0(haul.name," "))
  # Table DB BIS
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_FRA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  
  close(channel)
  
  # Table DB principale
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_ENA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdatbis <- sqlQuery(channel,
                      paste0("SELECT * FROM [", paste0(haulbis), "] ORDER BY [ID]"),
                      stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  print(paste(haul, identical(xdat, xdatbis)))
  
  # if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
  #   tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
  #   xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
  #   
  #   xdat.template=xdat[1,]
  #   xdat.template[1,]=NA
  #   xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
  #   rownames(xdat.template)=NULL
  #   xdat.template$gear=toupper(xdat.ob$Gear)
  #   xdat.template$gear=ifelse(xdat.template$gear=="",NA,xdat.template$gear)
  #   xdat.template$species_name=toupper(xdat.ob$Species)
  #   xdat.template$species_name=ifelse(xdat.template$species_name=="",NA,xdat.template$species_name)
  #   xdat.template$length_mm=xdat.ob$Lenght_mm
  #   xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
  #   xdat.template$Sex=toupper(xdat.ob$Sex)
  #   xdat.template$Mat=xdat.ob$Mat
  #   xdat.template$weight_g=ifelse(is.na(xdat.ob$Weight_g),xdat.ob$Total.weight_g,
  #                                 xdat.ob$Weight_g)
  #   xdat.template$total_number=xdat.ob$Number
  #   xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
  #   xdat=rbind(xdat, xdat.template)
  # }
  
  xdatbis$ID=seq(max(xdat$ID)+1,(max(xdat$ID)+nrow(xdatbis)),1)
  xdat.combined=rbind(xdat, xdatbis)
  
  # save on final db
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat.combined), tablename = haul.name)
  #close(channel)
  
  store.hauls=append(store.hauls, list(xdat.combined))
  names(store.hauls)[length(store.hauls)]=haul
  
}


#### check if there is something to add from the otolith db and then save
get_tables=function(db,wd_acces="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2025/OnBoard/access"){
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb") 
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  acctables=sqlTables(channel) # look for tables
  close(channel)
  tables_check=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
  #tables_check=tables_check[-grep('template', tables_check)]
  #tables_check=tables_check[-grep('test', tables_check)]
  return(tables_check)
}
tables.oto=get_tables(db='OTO')
cat("Check hauls in db OTO \n")
for(icheck in 1:length(store.hauls)){
  i.haul=store.hauls[[icheck]]
  i.nm=names(store.hauls)[icheck]
  cat(paste0(i.nm," "))
  if(i.nm %in% tables.oto){
    MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_OTO.accdb")
    PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
    channel <- odbcDriverConnect(PATH)
    xdat <- sqlQuery(channel,
                     paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                     stringsAsFactors = FALSE) # Load data into R dataframe
    close(channel)
    xdat$Sex=ifelse(xdat$Sex=='FALSE','F',xdat$Sex)
    xdat$ID=seq(max(i.haul$ID)+1,(max(i.haul$ID)+nrow(xdat)),1)
    i.haul=rbind(i.haul, xdat)
  }
  
  ### upload data

  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_commerciale.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(i.haul), tablename = i.nm, rownames = TRUE)
  close(channel)
}



##### merge benthos
benthos.dat=list()

tab.1=get_tables('Benthos_FRA')
tab.1=tab.1[-grep('template', tab.1)]
tab.1=tab.1[-grep('test', tab.1)]

for(icheck in 1:length(tab.1)){
    i.nm=tab.1[icheck]
    cat("Check hauls in db FRA benthos \n")
    cat(paste0(i.nm," "))
    MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_Benthos_FRA.accdb")
    PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
    channel <- odbcDriverConnect(PATH)
    xdat <- sqlQuery(channel,
                     paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                     stringsAsFactors = FALSE) # Load data into R dataframe
    close(channel)
    xdat$Sex=ifelse(xdat$Sex=='FALSE','F',xdat$Sex)
    benthos.dat[[icheck]]=xdat
    names(benthos.dat)[icheck]=i.nm
}


tab.2=get_tables('Benthos_ENA')
tab.2=tab.2[-grep('template', tab.2)]
tab.2=tab.2[-grep('test', tab.2)]

for(icheck in 1:length(tab.2)){
  i.nm=tab.2[icheck]
  cat("Check hauls in db ENA benthos \n")
  cat(paste0(i.nm," "))
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_Benthos_ENA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  xdat$Sex=ifelse(xdat$Sex=='FALSE','F',xdat$Sex)
  benthos.dat=append(benthos.dat, list(xdat))
  names(benthos.dat)[length(benthos.dat)]=i.nm
  
}


for(icheck in 1:length(benthos.dat)){
  i.haul=benthos.dat[[icheck]]
  i.nm=names(benthos.dat)[icheck]
  cat("Join hauls benthos \n")
  cat(paste0(i.nm," "))

  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_benthos.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(i.haul), tablename = i.nm, rownames = TRUE)
  close(channel)
}

# Move commercial species records from benthos to the commercial one
source_file <- paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_template.accdb")
file.copy(source_file,  paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_complete.accdb"), overwrite = TRUE)
file.copy(source_file,  paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_BENTHOS_complete.accdb"), overwrite = TRUE)

list_hauls<-get_tables(db="commerciale")
#list_hauls<-list_hauls[list_hauls!="cala_45bis"]
cat("Move record from benthos to commercial \n")
for(haul in 1:length(list_hauls)){
  cat(paste0(list_hauls[haul]," "))
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_commerciale.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat_commercial <- sqlQuery(channel,
                   paste0("SELECT * FROM [", list_hauls[haul], "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  close(channel)
  
  if(list_hauls[haul]!="cala_45bis"){
  MDBPATH <- paste0(wd_acces,"/access/Maschera inserimento SOLEMON_benthos.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  
  xdat_benthos <- sqlQuery(channel,
                           paste0("SELECT * FROM [", list_hauls[haul], "] ORDER BY [ID]"),stringsAsFactors = FALSE)%>% 
    tidyr::fill(species_name, .direction = "down") %>%
    tidyr::fill(gear, .direction = "down")
  xdat_benthos_c<-xdat_benthos%>%
    filter(length_mm>0|!is.na(Sex),!species_name %in% c("AEQUOPE","CHLAGLA","LIOCDEP"))
  
  xdat_final<-rbind(xdat_commercial,xdat_benthos_c)%>%select(-rownames)
  xdat_final$ID=seq(max(xdat_final$ID)+1,(max(xdat_final$ID)+nrow(xdat_final)),1)
  }else{
    xdat_final<-xdat_commercial%>%select(-rownames)
  }
  
  MDBPATH <- paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_complete.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(xdat_final), tablename = list_hauls[haul], rownames = F)
  close(channel)
  
  if(list_hauls[haul]!="cala_45bis"){
  xdat_benthos_b<-setdiff(xdat_benthos,xdat_benthos_c)%>%select(-rownames)
  xdat_benthos_b$ID=seq(max(xdat_benthos_b$ID)+1,(max(xdat_benthos_b$ID)+nrow(xdat_benthos_b)),1)
  xdat_benthos_b$Sex=ifelse(xdat_benthos_b$Sex==FALSE,"F",xdat_benthos_b$Sex)
  
  MDBPATH <- paste0(wd_acces,"/access/bio_data_v2025_SOLEMON_BENTHOS_complete.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(xdat_benthos_b), tablename = list_hauls[haul], rownames = F)
  close(channel)
}
}





