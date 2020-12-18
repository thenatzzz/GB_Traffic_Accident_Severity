# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")
########################

casualties <- read.csv("uk_accidents.csv", header=T)
head(casualties, 2)

###
#check missing val
library(naniar)
# vis_miss(casualties, warn_large_data=FALSE)

###
# drop row with Age = 0
# casualties$Casualty.Age[casualties$Casualty.Age==0]=1111111
casualties=subset(casualties, Casualty.Age!=0)

##
casualties$Accident.Date
class(casualties$Accident.Date)
casualties$Accident.Date <- as.factor(casualties$Accident.Date)

# Convert 'Accident.Date' from a factor to a date
casualties$Accident.Date <- as.Date(casualties$Accident.Date, "%d/%m/%Y")
casualties$Accident.Date

# Extract the month
casualties$month <- format(casualties$Accident.Date, format="%B")
casualties$month <- as.factor(casualties$month)
casualties$month <- factor(casualties$month,levels=month.name)
summary(casualties$month)

# Extract the day of the week
casualties$day <- format(casualties$Accident.Date, format="%A")
casualties$day <- factor(casualties$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
summary(casualties$day)


# Add an hour band variable
casualties$Time <- gsub("[ [:punct:]]", "" , casualties$Time) # remove punctuation from 'Time' variable
casualties$Time <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", casualties$Time) # add a colon after 2nd value
casualties$hour<- as.POSIXlt(casualties$Time, format="%H:%M")$hour # add a new column called 'hour' with hours band
# casualties[1:3,c(1, 24, 26, 44)] # check the results


#####################################
# Relabel the 'Accident.Severity' categories
class(casualties$Accident.Severity)
table(casualties$Accident.Severity)

# casualties$Accident.Severity <- factor(casualties$Accident.Severity)
# levels(casualties$Accident.Severity) <-c("Fatal", "Serious", "Slight")

casualties$Accident.Severity[casualties$Accident.Severit=='1 FATAL']  <- "2 SERIOUS" 
casualties$Accident.Severity <- factor(casualties$Accident.Severity,
                                    levels= c("2 SERIOUS", "3 SLIGHT"),
                                    labels= c( "Serious", "Slight")  )
table(casualties$Accident.Severity)

###
# Relabel the 'Mode.of.Travel' categories

table(casualties$Mode.of.Travel)

casualties$Mode.of.Travel[casualties$Mode.of.Travel=='9 PRIVATE HIRE']  <- "8 OTHER VEHICLE" 

# casualties$Mode.of.Travel
casualties$Mode.of.Travel <- factor(casualties$Mode.of.Travel,
                                    levels= c("1 PEDESTRIAN", "2 PEDAL CYCLE", "3 POWERED 2 WHEELER", "4 CAR",
                                              "5 TAXI", "6 BUS OR COACH", "7 GOODS VEHICLE", "8 OTHER VEHICLE"),
                                    labels= c("Pedestrian", "Pedal Cycle", "Powered 2 Wheeler", "Car",
                                              "Taxi", "Bus or Coach", "Goods Vehicle", "Other Vehicle")  )
table(casualties$Mode.of.Travel)


# Relabel the 'Casualty.Sex' categories
table(casualties$Casualty.Sex)

casualties$Casualty.Sex <- factor(casualties$Casualty.Sex,
                                  levels= c("1 MALE", "2 FEMALE","'-1 UNKNOWN"),
                                  labels= c("Male", "Female","Unknown"))
table(casualties$Casualty.Sex)


####
# Relabel the 'Light.Conditions..Banded.' categories
table(casualties$Light.Condtions..Banded.)
# colnames(casualties)

# levels(casualties$Light.Condtions..Banded.)
casualties$Light.Conditions.Banded <- factor(casualties$Light.Condtions..Banded.,
                                             levels= c("1 DARK", "2 DAYLIGHT"),
                                             labels= c("Dark", "Daylight"))
casualties[,c("Light.Condtions..Banded.")] <- list(NULL)

table(casualties$Light.Conditions.Banded)

####
# Create age bands
# bands <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
           # "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84")
# casualties$ageband <- cut(casualties$Casualty.Age, 
                          # breaks=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84), 
                          # labels = bands)
bands <- c("0-9","10-19","20-29","30-39",
           "40-49","50-59","60-69","70-79","80-89",'90-99')
casualties$ageband <- cut(casualties$Casualty.Age, 
                          breaks=c(0,9,19,29,39,49,59,69,79,89,99), 
                          labels = bands)
class(casualties$ageband)
####
colnames(casualties)

# Relabel the 'Casualty.Class' categories
table(casualties$Casualty.Class)

# levels(casualties$Casualty.Class)
casualties$Casualty.Class <- factor(casualties$Casualty.Class,
                                             levels= c("1 DRIVER/RIDER", "2 PASSENGER",'3 PEDESTRIAN'),
                                             labels= c("Driver/Rider", "Passenger",'Pedestrian'))

table(casualties$Casualty.Class)

###
# Relabel the 'Highway' categories
table(casualties$Highway)

# levels(casualties$Highway)
casualties$Highway <- factor(casualties$Highway,
                                    levels= c("1 TLRN", "2 HA",'3 BOR'),
                                    labels= c("TLRN", "HA",'BOR'))

table(casualties$Highway)


###
# Relabel the 'Road.Type' categories
table(casualties$Road.Type)

# levels(casualties$Road.Type)
casualties$Road.Type <- factor(casualties$Road.Type,
                             levels= c("1 ROUNDABOUT", "2 ONE-WAY ST",'3 DUAL CWY','6 SINGLE CWY','7 SLIP RD','9 UNKNOWN'),
                             labels= c("Roundabout", "One-way ST",'Dual CWY','Single CWY','Slip RD','Unknown'))

table(casualties$Road.Type)

###
# Relabel the 'Weather' categories
table(casualties$Weather)
casualties$Weather[casualties$Weather=='9 UNKNOWN']  <- "8 OTHER" 
casualties$Weather[casualties$Weather=='3 SNOWING']  <- "8 OTHER" 
casualties$Weather[casualties$Weather=='4 FINE/HIGH WINDS']  <- "8 OTHER" 
casualties$Weather[casualties$Weather=='5 RAINING/HIGH WINDS']  <- "8 OTHER" 
casualties$Weather[casualties$Weather=='6 SNOWING/HIGH WINDS']  <- "8 OTHER" 
casualties$Weather[casualties$Weather=='7 FOG/MIST']  <- "8 OTHER" 


# levels(casualties$Weather)
casualties$Weather <- factor(casualties$Weather,
                               levels= c("1 FINE", "2 RAINING",'8 OTHER'),
                               labels= c("Fine", "Raining",'Other'))
table(casualties$Weather)

###
# Relabel the 'Road.Surface' categories
table(casualties$Road.Surface)
casualties$Road.Surface[casualties$Road.Surface=='9 UNKNOWN (S/R)']  <- "OTHER" 
casualties$Road.Surface[casualties$Road.Surface=='5 ROAD-FLOOD']  <- "OTHER" 
casualties$Road.Surface[casualties$Road.Surface=='4 ROAD-FROST/ICE']  <- "OTHER" 
casualties$Road.Surface[casualties$Road.Surface=='3 ROAD-SNOW']  <- "OTHER" 



# levels(casualties$Road.Surface)
casualties$Road.Surface <- factor(casualties$Road.Surface,
                             levels= c("1 ROAD-DRY", "2 ROAD-WET",'OTHER'),
                             labels= c("Road-Dry", "Road-Wet",'Other'))
table(casualties$Road.Surface)

###
# Relabel the 'Junction.Detail' categories
table(casualties$Junction.Detail)
casualties$Junction.Detail[casualties$Junction.Detail=='02 MINI']  <- "OTHER" 
casualties$Junction.Detail[casualties$Junction.Detail=='05 SLIP ROAD']  <- "OTHER" 
casualties$Junction.Detail[casualties$Junction.Detail=='07 MULTI JUN']  <- "OTHER" 
casualties$Junction.Detail[casualties$Junction.Detail=='08 PRIV DRIVE']  <- "OTHER" 
casualties$Junction.Detail[casualties$Junction.Detail=='99 UNKNOWN (S/R)']  <- "OTHER" 
casualties$Junction.Detail[casualties$Junction.Detail=='09 OTHER JUN']  <- "OTHER" 


# levels(casualties$Junction.Detail)
casualties$Junction.Detail <- factor(casualties$Junction.Detail,
                                  levels= c("00 NO JUN IN 20M", "01 ROUNDABOUT",'03 T/STAG JUN','06 CROSSROADS','OTHER'),
                                  labels= c("No JUN in 20M", "Roundabout",'T/STAG JUN','Crossroads','Other'))
table(casualties$Junction.Detail)

#################################
###############################
################################

# casualties$ageband <- data.frame(casualties$ageband, stringsAsFactors = FALSE)
class(casualties$Time)

write.table(casualties, sep=",",dec = " ",file='uk_accidents_processed.csv', row.names = FALSE)


# casualties2 <- read.csv("uk_accidents_processed.csv", header=T)

