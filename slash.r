source("make_sword.r")
library(dplyr)
library(stringr)

## Check if the file already exists else download 
download_daily_data<-function(start_date,end_date, run_num, data_dir = "data/"){
  files<- list.files(path = "data/")
  print(start_date)
  dates <- seq(from=start_date, to=end_date, by='days')
  print("downloading data")
  for(i in seq_along(dates)){
    if(sum(str_detect(files,paste("samurai_",dates[i],"_*[0-9]*_",".csv", sep=""))) ==0){
      write.csv(fxddr(dates[i],"samurai_"), paste(paste(data_dir,"palace",sep=""),dates[i],run_num,".csv", sep="_"), row.names = FALSE)
    }
  }
}
