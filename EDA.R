library(RSQLite)
library(dbplyr)
library(dplyr)
library(lubridate)
library(survival)
library(survminer)

#Create a function to load the data from the /data folder
load_data<-function(){
  # create db connection
  conn <- dbConnect(SQLite(), './data/data.sqlite')
  
  # pull the fires table into RAM
  fires <- tbl(conn, "Fires") %>% collect()
  
  # disconnect from db
  dbDisconnect(conn)

  return(fires)  
}

# Calling the function
data <-load_data()

#Selecting some columns of interest
data_filtered <- data %>%
                  select(DISCOVERY_DATE, DISCOVERY_DOY, DISCOVERY_TIME, 
                         CONT_DATE,CONT_TIME, FIRE_SIZE, FIRE_SIZE_CLASS, STATE,
                         NWCG_GENERAL_CAUSE, NWCG_CAUSE_CLASSIFICATION)

# Creating new features
# FIRE_TIME_HOUR: this is the length of time a fire will burn between its FIRE_START_DATE and FIRE_END_DATE
data_filtered <- data_filtered %>% 
                  mutate(FIRE_START_DATE = as.POSIXct(paste(DISCOVERY_DATE, DISCOVERY_TIME), format = "%m/%d/%Y %H%M"),
                         FIRE_END_DATE = as.POSIXct(paste(CONT_DATE, CONT_TIME), format = "%m/%d/%Y %H%M"))  %>%
                  mutate(FIRE_TIME_HOUR = as.numeric(difftime(FIRE_END_DATE, FIRE_START_DATE, units="hours"))) %>%
                  mutate(DISCOVERY_TIME = as.numeric(substr(DISCOVERY_TIME, 1, 2))) %>%
                  select(-DISCOVERY_DATE, -CONT_DATE, -CONT_TIME) %>%
                  mutate(DISCOVERY_SLOT = )


data_filtered$event <- 1  # No censored data (all fires finished)

surv_object <- Surv(time = data_filtered$FIRE_TIME_HOUR, 
                    event = data_filtered$event)


km_fit <- survfit(surv_object ~ NWCG_CAUSE_CLASSIFICATION, data = data_filtered)

ggsurvplot(km_fit, conf.int = TRUE, pval = TRUE, 
           legend.title = "Fire Type",
           xlab = "Fire Running Time (Hours)", 
           title = "Kaplan-Meier Curve by Fire Type")

