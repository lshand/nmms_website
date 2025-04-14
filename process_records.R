library(dplyr)
library(lubridate)
library(hms)

convert_to_time <- function(x){
  z = gregexpr(":",x)[[1]][1]
  if(length(z)>1){ #> 1 hour 
    s=substr(x,z[2]+1, nchar(x))
    m=as.numeric(substr(x,z[1]+1,z[2]-1))
    h=as.numeric(substr(x,1, z[1]-1))
  }else{
    h=0
    s=as.numeric(substr(x,z[1]+1, nchar(x)))
    m=ifelse(z>0, as.numeric(substr(x,1, z[1]-1)),0)
  }
  
  tmp_hms = hms::hms(seconds = s , minutes = m, hours = h)
  return(as.character(tmp_hms))
}

#LMSC Top 10
recordfiles <- list.files(paste0("../records/",pool_len,"/LMSC_top10"))

records <- c()
for(i in 1:length(recordfiles)){
  tmp_records = read.csv(paste0("../records/",pool_len,"/LMSC_top10/",recordfiles[i]))
  tmp_records[,'Season'] = substr(recordfiles[i], nchar(recordfiles[i])-7,nchar(recordfiles[i])-4)
  records <- rbind(records,tmp_records)
}

records$Time <- sapply(records$Time, convert_to_time)
records$Sex <- ifelse(records$Sex=="Women ","Women","Men")

records_lmsc10 <- records
#best_times_lmsc10 <- records %>% 
#  group_by(Sex, Age.Group,Distance,Stroke) %>% 
#  summarise(top_time=min(Time)) 


#USMS Top 10
recordfiles <- list.files(paste0("../records/",pool_len,"/USMS_top10"))

records <- c()
for(i in 1:length(recordfiles)){
  tmp_records = read.csv(paste0("../records/",pool_len,"/USMS_top10/",recordfiles[i]), skip=1)
  tmp_records[,'Season'] = substr(recordfiles[i], nchar(recordfiles[i])-7,nchar(recordfiles[i])-4)
  records <- rbind(records,tmp_records)
}

#reformat Age.Group
records[,'Sex'] <- sapply(records$Age.Group, function(x) ifelse(substr(x,1,1)=="W","Women","Men"))
records$Age.Group <- sapply(records$Age.Group, function(x) substr(x,2,nchar(x)))
records[,'break_ind'] <- sapply(records$Event, function(x) regexpr(" ",x)[1])
records <- records %>% mutate(Distance=substr(Event,1,break_ind-1),
                                Stroke=substr(Event,break_ind+1,nchar(Event)))
records$Stroke <- recode(records$Stroke, "IM"="Individual Medley", "Free"="Freestyle",
                         "Breast"= "Breaststroke", "Back"="Backstroke", "Fly"="Butterfly")
records$Time <- sapply(records$Time, function(x) substr(x, 1, nchar(x)-1))
records$Time <- sapply(records$Time, convert_to_time)
records_usms10 <- records
#best_times_usms10 <- records %>% 
#  group_by(Sex, Age.Group,Distance,Stroke) %>% 
#  summarise(top_time=min(Time))

#create state record tables
vars=c("Sex","Age.Group","Distance","Stroke","Time","Name","Season")
records <- rbind(records_lmsc10[,vars], records_usms10[,vars])
best_times <- records %>% 
  group_by(Sex, Age.Group,Distance,Stroke) %>% 
  summarise(top_time=min(Time))
staterecords <- c()
for(i in 1:nrow(best_times)){
  tmp_rec = records %>% filter(Sex == best_times$Sex[i],
                               Age.Group == best_times$Age.Group[i],
                               Distance == best_times$Distance[i],
                               Stroke == best_times$Stroke[i],
                               Time == best_times$top_time[i])
  staterecords <- rbind(staterecords,tmp_rec)
}

staterecords$Distance <- as.numeric(staterecords$Distance )
agegroups = unique(staterecords$Age.Group)

#time formatting

staterecords$Time <- sapply(staterecords$Time, function(x){
  if(nchar(x)<11){
    if(nchar(x)==10){
      x=paste0(x,"0")
    }
    if(nchar(x)==9){
      x=paste0(x,"00")
    }
    if(nchar(x)==8){
      x=paste0(x,".00")
    }
  }
  if(substr(x,1,5)=="00:00"){
    y=substr(x,7,nchar(x))
  }else if(substr(x,1,2)=="00"){
    y=substr(x,4,nchar(x))
  }else{
    y=x
  }
  
  return(y)
})

#staterecords$Date <- ifelse(is.na(staterecords$Date),staterecords$Season,staterecords$Date)

list=ls()[! ls() %in% c("staterecords","agegroups")]
