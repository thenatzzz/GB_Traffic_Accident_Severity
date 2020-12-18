# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")
########################
# Load the relevant packages
library(ggvis)
library(dplyr)
# 
# casualties <- read.csv("uk_accidents_processed.csv", header=T)
# casualties <- read.csv("10kuk_accidents_processed.csv", header=T)
# casualties <- read.csv("smote_uk_accidents_processed.csv", header=T)
# casualties <- read.csv("smote10k_uk_accidents_processed.csv", header=T)
casualties <- read.csv("full_data.csv", header=T)

# 
# veh =read.csv("2019-gla-data-extract-vehicle.csv", header=T)
# attendant=read.csv("2019-gla-data-extract-attendant.csv", header=T)
# casualty= read.csv("2019-gla-data-extract-casualty.csv", header=T)
# dim(veh)
# dim(attendant)
# dim(casualty)
# table(casualties$Accident.Severity)
# casualties <- SMOTE(Accident.Severity ~ .,casualties, perc.over = 600,perc.under=100)

# casualties = sample_n(casualties, 10000)
table(casualties$Accident.Severity)
prop.table(table(casualties$Accident.Severity))

# vis_miss(casualties, warn_large_data=FALSE)

head(casualties, 2)


# write.table(casualties, sep=",",dec = " ",file='10kuk_accidents_processed.csv', row.names = FALSE)

#####
# Number of casualties by mode of travel
table(casualties$Mode.of.Travel)

# Number of casualties by mode and casualty severity
with(casualties, table(Mode.of.Travel, Accident.Severity))


# Calculate the mean age of KSI casualties by mode of travel
casualties[casualties$Accident.Severity == "Fatal" | casualties$Accident.Severity == "Serious", ] %>%
  group_by(Mode.of.Travel) %>% 
  summarise(mean = round(mean(Casualty.Age), 0))

# Casualties by mode of travel
casualties %>% 
  ggvis(x = ~Mode.of.Travel, fill = ~factor(Mode.of.Travel) ) %>% 
  layer_bars() %>%
  add_axis("x", title = "Mode of Travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Mode of Travel")

# Casualties by month
casualties %>%
  
  # mutate(y=as.character(casualties$month), x=as.numeric(casualties$month)) %>% 
  # arrange(desc(casualties$month)) %>% 
  
  ggvis(~month, fill :="#e5f5f9") %>% 
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties")

# Casualties by month and mode of travel
casualties %>%
  ggvis(x = ~month, fill = ~as.factor(Mode.of.Travel)) %>%
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Mode of Travel")

# Casualties by mode and severity
casualties %>%
  group_by(Accident.Severity) %>%
  ggvis(x = ~Mode.of.Travel, fill = ~Accident.Severity, fillOpacity := 0.7) %>%
  layer_bars() %>%
  scale_nominal("fill", range = c("#1b9e77", "#d95f02", "#7570b3")) %>%
  add_axis("x", title = "Mode of travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")

# KSIs by mode of travel
casualties[casualties$Accident.Severity == "Slight" | casualties$Accident.Severity == "Serious",] %>%
  group_by(Mode.of.Travel) %>%
  ggvis(~Mode.of.Travel, fill = ~Accident.Severity) %>%
  add_axis("x", title = "Mode of travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")

# x_fac = factor(casualties$month, levels = month.name)
casualties %>% arrange(match(month, month.name))
# Casualty severity by month amongst pedal cyclists
casualties[casualties$Mode.of.Travel == "Pedal Cycle", ] %>%
  group_by(Accident.Severity) %>%
  ggvis(~month, fill = ~Accident.Severity) %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")

# KSIs by month amongst pedal cyclists
casualties[casualties$Mode.of.Travel == "Pedal Cycle" & 
             casualties$Accident.Severity == "Slight" | casualties$Accident.Severity == "Serious", ] %>%
  group_by(Accident.Severity) %>%
  ggvis(~month, fill = ~Accident.Severity) %>%
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")

# Powered 2 Wheeler casualties by age band and gender
casualties[casualties$Mode.of.Travel == "Powered 2 Wheeler", ] %>% 
  group_by(Casualty.Sex) %>%
  ggvis(x = ~ageband, fill = ~Casualty.Sex, fillOpacity := 0.8) %>%
  layer_bars() %>%
  add_axis("x", title = "Casualty age band") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty sex")


# Powered 2 Wheeler casualties by month and light conditions
casualties[casualties$Mode.of.Travel == "Powered 2 Wheeler", ] %>% 
  group_by(Light.Conditions.Banded) %>%
  ggvis(x = ~month, fill = ~Light.Conditions.Banded) %>%
  layer_bars() %>%
  scale_nominal("fill", range = c("Black", "Yellow")) %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Light conditions")

#####################
# Mean age of casualties by mode of travel
casualties %>% 
  ggvis(~factor(Mode.of.Travel), ~Casualty.Age) %>% 
  layer_boxplots(width = 0.5)

# Casualties by day
casualties %>%
  group_by(Accident.Date) %>%
  summarize(count = n()) %>%
  ggvis(~Accident.Date, ~count) %>%
  layer_lines()

########
# Number of casualties by mode of travel
casualty_freq <- casualties %>%
  group_by(Mode.of.Travel, Accident.Severity) %>%
  summarize(count = n()) 
casualty_freq <- as.data.frame(casualty_freq)
head(casualty_freq)


###
freq <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(x[4], collapse = "<br />")
}

casualty_freq %>% ggvis(~Mode.of.Travel, ~count, fill = ~factor(Accident.Severity)) %>% 
  layer_bars() %>%
  scale_nominal("fill", range = c("#e41a1c", "#377eb8", "#4daf4a")) %>%
  add_axis("x", title = "Mode of travel", properties = axis_props(labels=list(angle=90, align="left")) ) %>% 
  add_axis("y", title = "") %>%
  add_legend("fill", title="Casualty severity") %>%
  add_tooltip(freq, "hover")


###
# Filtering casualties by age band and mode of travel
casualty_ages <- casualties %>%
  group_by(Mode.of.Travel, ageband) %>%
  summarize(count = n()) 

casualty_ages %>% 
  ggvis(~ageband, ~count) %>% 
  filter(Mode.of.Travel %in% eval(input_select(c("Pedestrian", "Pedal Cycle", "Powered 2 Wheeler", 
                                                 "Car", "Taxi", "Bus or Coach", "Goods Vehicle", 
                                                 "Other Vehicle",'Private Hire'), selected = "Pedestrian"))) %>% 
  add_axis("x", title = "Casualty age band") %>%
  add_axis("y", title = "Number of casualties") %>%
  layer_bars(fill = ~Mode.of.Travel) %>%
  hide_legend("fill")
