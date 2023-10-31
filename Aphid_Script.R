# Curation Script --------------------------------------------------------------

# Dataset: Complex life histories predispose aphids to recent abundance declines
# Location: North-eastern US
# Curator: Qianyu
# Date: March 2023 to April 2023

# Set up -----------------------------------------------------------------------

# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)


rm(list=ls()) # clear up the environment before starting

# import datasets
dt_abundance <- read.csv("Compiled_Trap_Records.csv")
dt_species <- read_excel('Species_Names_Codes_Info.xlsx', sheet=1, col_names=T, na='')
dt_location <- read_excel('Location_Coordinates.xlsx', sheet=1, col_names=T, na='')

#Merge Dataset -----------------------------------------------------------------

names(dt_location)[1] <- "LOCATION"
dt_location$LOCATION <- toupper(dt_location$LOCATION)
dt <- merge(dt_abundance, dt_species, by = "CODE", all.x = TRUE)
dt <- merge(dt,dt_location, by = "LOCATION", all.x = TRUE)

View(dt)


dim(dt) # check dimensions, returns row and column counts
summary(dt)

#check sampling duration condition
years <- substr(dt$END_DATE, 1, 4)                                          #sampling duration fulfilled:)
n_distinct(years) >= 2 


#Field check--------------------------------------------------------------------

# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$TOTAL)                                                        #checked, passed:)

# Secondary fields such as trawl, plotmust be factors or integers?          #NA
# Secondary fields such as elevation or depth will normally be numeric.     #NA

# Date should be POSIXct (not applicable in this case) NA, just year
is.POSIXct(dt$END_DATE)                                                     #checked, not in POSIXct format

# Taxonomic fields must be characters or factors?
is.factor(dt$Species.name) | is.character(dt$Species.name)
is.factor(dt$Species.common.name) | is.character(dt$Species.common.name)    #checked, passed:)
is.factor(dt$CODE) | is.character(dt$CODE)

#Fix Date Type------------------------------------------------------------------

dt$END_DATE <- as.POSIXct(dt$END_DATE, format = "%m/%d/%Y")                 #done:)
print(dt$END_DATE)

#Delete Unwanted Column---------------------------------------------------------

#store information we don't need for raw data in a new dataset in case needed
#CODE AND TIME are also stored to identify records
extradata <- dt[, c("CODE","END_DATE","Species.common.name", "Number.of.occurences.in.record","Native?","Hosts","Summer_host","Primary host","Crop Pest","Heteroecious/Monoecious**","Other")]
View(extradata)

#delete columns
dt$comment <- NULL
dt$name <- NULL
dt$KEEP_DROP.x <- NULL
dt$KEEP_DROP.y <- NULL
dt$Number.of.occurences.in.record <- NULL
dt$Hosts <- NULL
dt$Summer_host <- NULL
dt$`Primary host` <- NULL
dt$`Crop Pest` <- NULL
dt$Species.common.name <- NULL
dt$`Holocyclic ***` <- NULL
dt$`Host-breadth*` <- NULL
dt$`Heteroecious/Monoecious**` <- NULL
dt$Other <- NULL
dt$`Native?` <- NULL

#Primary Field Check------------------------------------------------------------

# Abundance
dt <- dt[!is.na(dt$TOTAL),]                                                 #NA value removed 
dt <- subset(dt, dt$TOTAL > 0)                                              #zero or negative removed
min(dt$TOTAL) > 0 # no zeroes?                                              #zero or negative value passed:)
sum(dt$TOTAL=="") > 0 # no blanks                                           #NA value passed:)

#Date
valid_dates <- list()

# Year < 2021, month < 12, day < 31
for (i in seq_along(dt$END_DATE)) {                                         #check date condition
  year <- as.numeric(format(dt$END_DATE[[i]], "%Y"))
  month <- as.numeric(format(dt$END_DATE[[i]], "%m"))
  date <- as.numeric(format(dt$END_DATE[[i]], "%d"))
  valid_dates[i] <- year <= 2023 & month <= 12 & date <= 31
}
sum(valid_dates == 0) > 0                                                   #date passed:)

#Coordinate 
#latitude has to be constrained between -90 to 90, and longitude from -180 to 180
dt <- dt[!is.na(dt$`Location long`),]                                       #remove row with empty long
dt <- dt[!is.na(dt$`Location lat`),]                                        #remove row with empty lat
sum(dt$`Location long`=="") > 0                                             #check empty, passed:)
sum(dt$`Location lat`=="") > 0 
min(dt$`Location long`) >= -180 & max(dt$`Location long`) <= 180            #checked, passed:)
min(dt$`Location lat`) >= -90 & max(dt$`Location lat`) <= 90                #checked, passed:)

#Plot---------------------------------------------------------------------------
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=`Location long`, y=`Location lat`), shape=21, size=3, color='orange')

points + coord_fixed(xlim=c(-100,-140), ylim=c(30,55))                     #graph plotted:)

#Taxa---------------------------------------------------------------------------

dt$Species <- dt$Species.name                                               #operate in new column
dt$Species <- str_remove_all(dt$Species, "\\(.*?\\)")                       #remove the name of people who name these species

unique_Species <- unique(dt$Species)                                        #find all different genus+species names
unique_Species                                                              #check spelling manually
length(unique_Species)

#delete unidentified records(without genus and species)
dt <- dt[!(dt$Species == "retained without ID. this code appeared 18 times in the record, but was never identified . Retained but there is no ID unless it is Brachycaudus cardui  "), ]
dt <- dt[!(dt$Species == "Retained without ID. The count was 85 individuals 7/18/97 in Burley, no ID - presume this is some sort of mistake but not obvious what"), ]
dt <- dt[!is.na(dt$Species.name),]

indices <- which(dt$Species == "Macrospihum rosae ")                        #identify wrong spelling records
dt$Species[indices] <- "Macrosiphum rosae"                                  #correct spelling replaced:)
                                
dt$Genus <- word(dt$Species, 1, sep = fixed(' '))                           #seperate genus and species
dt$Species <- word(dt$Species, 2, sep = fixed(' '))
dt$Family <- as.character(rep('Aphididae', nrow(dt)))                       #add family
indices1 <- which(dt$Species == "sp.")                                        
dt$Species[indices1] <- "sp"                                                #replace sp. with sp

#manually add species to those who cannot be added by word() due to 2 empty space between words
dt$Species[dt$Species.name == "Hyperomyzus (Neonasonovia) nigricornis (Knowlton)"] <- "nigricornis"
dt$Species[dt$Species.name == "Sipha (Rungsia) elegans del Guercio"] <- "elegans"
dt$Species[dt$Species.name == "Uroleucon (Uromelan) taraxaci (Kaltenbach)"] <- "taraxaci"
dt$Species[dt$Species.name == "Rhopalomyzus (Judenkoa) lonicerae (Siebold)"] <- "lonicerae"
dt$Species[dt$Species.name == "Dysaphis (Pomaphis) plantaginea (Passerini)"] <- "plantaginea"
dt$Species[dt$Species.name == "Uroleucon (Lambersius) erigeronensis (Thomas)"] <- "erigeronensis"
dt$Species[dt$Species.name == "Dysaphis (Pomaphis) crataegi (Kaltenbach)"] <- "crataegi"

unique_Species <- unique(dt$Species)                                        
unique_Species                                                              
length(unique_Species)

#length unique species is 3 lower than initial value(84 versus 87) because there are three pairs 
#of species which has same epithet but belong to different genus. They are
#1.Hyperomyzus lactucae       Acyrthosiphon lactucae
#2.Periphyllus americanus     Prociphilus americanus 
#3.Durocapillata utahensis    Drepanaphis utahensis
#ps. this confuses me a lot initially

View(unique(dt[, c("Species.name", "Genus","Species")]))                    #do a visual check   
                                                                            #taxa passed:)

#Prepare for export---------------------------------------------------
View(dt)

dt$Species.name <- NULL                

#extract Year & Month & Day
dt$Year <- format(dt$END_DATE, "%Y")                          
dt$Month <- format(dt$END_DATE, "%m")
dt$Day <- format(dt$END_DATE, "%d")

dt$Year <- as.integer(dt$Year)                                             #convert year & month & day to integer
dt$Month <- as.integer(dt$Month)
dt$Day <- as.integer(dt$Day)

#Rename the columns
names(dt)[names(dt) == "TOTAL"] <- "Abundance"                
names(dt)[names(dt) == "Location lat"] <- "Latitude"
names(dt)[names(dt) == "Location long"] <- "Longitude"
names(dt)[names(dt) == "END_DATE"] <- "Date"
names(dt)[names(dt) == "LOCATION"] <- "Location"
names(dt)[names(dt) == "CODE"] <- "Code"

#Reorder the columns to the proper format
dt <- dt[c('Abundance',
           'Family',
           'Genus',
           'Species',
           'Location',
           'Latitude',
           'Longitude',
           'Date',
           'Day',
           'Month',
           'Year',
           'Code')] %>% arrange(Year, Family, Genus, Species)

head(dt) # final check!

setwd(file.choose() %>% dirname()) # choose a file in the directory you want this saved in

# specify the output file name
dataset_name <- "Aphid_Raw_Data"
output <- paste0(dataset_name, "_rawdata_CC.csv")

# write the data table to a CSV file
write.csv(dt, output, row.names = FALSE, na = "")


                                    #HAHAHA WE MADE IT! ٩(๑> ₃ <)۶







