
longitude=c(-110,-90,-100,-85)
latitude=c(33,40,37,34)

sensorID=c("SN100","SN200","SN300","SN400")


i=0
while(i<100){
dat=data.frame(timestamp=as.numeric(Sys.time()),sensorID=sensorID,longitude=longitude,latitude=latitude,
               temperature=rnorm(n=4,mean=30,sd=1))
write.csv(dat,paste0("sensorData",gsub("[^0-9]","",Sys.time()),".csv"),
          row.names = FALSE)
i=i+1
Sys.sleep(2)
}

while(TRUE){
        dat=data.frame(timestamp=as.numeric(Sys.time()),sensorID=sensorID[1:3],longitude=longitude[1:3],latitude=latitude[1:3],
                       temperature=rnorm(n=3,mean=30,sd=1))
        write.csv(dat,paste0("sensorData",gsub("[^0-9]","",Sys.time()),".csv"),
                  row.names = FALSE)
        Sys.sleep(2)
}