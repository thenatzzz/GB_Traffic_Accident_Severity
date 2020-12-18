# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")

##############################
# road accident UK
# https://tfl.gov.uk/corporate/publications-and-reports/road-safety

####

# Load the casualty data
# casualties <- read.csv("http://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-casualty.csv", header=T)
casualties <- read.csv("2019-gla-data-extract-casualty.csv", header=T)
colnames(casualties)[which(names(casualties) == "ï..AREFNO")] <- "AREFNO"

head(casualties, 2)
# casualties$X <- NULL # remove the redundant 'X' variable

# Load the attendant data
# attendant <- read.csv("http://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-attendant.csv", header=T)
attendant <- read.csv("2019-gla-data-extract-attendant.csv", header=T)
colnames(attendant)[which(names(attendant) == "ï..AREFNO")] <- "AREFNO"

colnames(attendant)
head(attendant, 2)

########
# Merge the attendant data with the casualty data using the AREFNO
casualties <- merge(casualties, attendant, by="AREFNO")
names(casualties)
head(casualties, 2)
#####
rm(attendant) # remove the attendant dataset to save memory


table(casualties$Junction.Detail)

### drop some column
col_drop = c('Borough.x','Boro.x','Easting.x','Northing.x','CREFNO',
             'Casualty.Age..Banded.','Ped.Location','Ped.Movement',
             'Borough.y','Boro.y','Easting.y','Northing.y','Location',
             'Junction.Control','APOLICER_DECODED','ADATES_FULL',
             'C.W.Hazard','Special.Conditions',
             'Road.Class.1','Road.No..1','Road.Class.2','Road.No.2',
             'Ped..Crossing.Decoded','Casualty.Severity','Day',
             'No..of.Casualties.in.Acc.')
# casualties=within(casualties, rm('Borough.x'))
casualties[,col_drop] <- list(NULL)

###
# install.packages("naniar")
library(naniar)

vis_miss(casualties, warn_large_data=FALSE)
# drop nan
casualties=na.omit(casualties)

head(casualties, 2)
write.csv(casualties,file='uk_accidents.csv', row.names = FALSE)

###################################