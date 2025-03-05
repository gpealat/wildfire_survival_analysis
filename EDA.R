library(RSQLite)
library(dbplyr)
library(dplyr)
library(lubridate)

load_data<-function(){
  # create db connection
  conn <- dbConnect(SQLite(), './data/data.sqlite')
  
  # pull the fires table into RAM
  fires <- tbl(conn, "Fires") %>% collect()
  
  # disconnect from db
  dbDisconnect(conn)

  return(fires)  
}

data <-load_data()

# check size
print(object.size(data), units = 'Gb')

data_filtered <- data %>%
                  select(DISCOVERY_DATE, DISCOVERY_DOY, DISCOVERY_TIME, 
                         CONT_DATE,CONT_TIME, FIRE_SIZE)

head(data_filtered)

data_filtered <- data_filtered %>% 
                  mutate(FIRE_START_DATE = as.POSIXct(paste(DISCOVERY_DATE, DISCOVERY_TIME), format = "%m/%d/%Y %H%M"),
                         FIRE_END_DATE = as.POSIXct(paste(CONT_DATE, CONT_TIME), format = "%m/%d/%Y %H%M"))  %>%
                  mutate(FIRE_TIME_HOUR = as.numeric(difftime(FIRE_START_DATE, FIRE_END_DATE, units="hours")))

                         