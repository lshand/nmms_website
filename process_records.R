library(dplyr)
library(lubridate)
library(hms)

recordfiles <- list.files("../records/")[grep(".csv",list.files("../records/"))]

records <- c()
for(i in 1:length(recordfiles)){
  tmp_records = read.csv(paste0("../records/",recordfiles[i]))
  tmp_records[,'Season'] = substr(recordfiles[i], nchar(recordfiles[i])-7,nchar(recordfiles[i])-4)
  records <- rbind(records,tmp_records)
}

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

records$Time <- sapply(records$Time, convert_to_time)

best_times <- records %>% 
  group_by(Sex, Age.Group,Distance,Stroke) %>% 
  summarise(top_time=min(Time))

#create tables
staterecords <- c()
for(i in 1:nrow(best_times)){
  tmp_rec = records %>% filter(Sex == best_times$Sex[i],
                               Age.Group == best_times$Age.Group[i],
                               Distance == best_times$Distance[i],
                               Stroke == best_times$Stroke[i],
                               Time == best_times$top_time[i])
  staterecords <- rbind(staterecords,tmp_rec)
}

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

staterecords$Date <- ifelse(is.na(staterecords$Date),staterecords$Season,staterecords$Date)

list=ls()[! ls() %in% c("staterecords","agegroups")]
