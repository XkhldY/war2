library(RMySQL)

 mydb = dbConnect(MySQL(), user='root', password='', dbname='MeshliumDB', host='localhost')
dbListTables(mydb)

dbListFields(mydb, 'sensorparser')

res<-dbSendQuery(mydb, "SELECT * FROM meshliumdb.sensorparser  where sensor='GP_NO-8001-1' and timestamp >'2016-03-16'")
data <- dbFetch(res, n = 10)

# data$timestamp <- as.Date(data$timestamp, "%YYYY-%mm-%dd")
data$timestamp = as.POSIXct(data$timestamp,tz = "GMT") 

date=order(data$timestamp)
range(date)
plot(date,data$value, type = "l")


dbListConnections(MySQL())
for(con in list(mydb))
   dbDisconnect(con)
