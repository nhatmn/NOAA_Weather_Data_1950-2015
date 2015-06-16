source("to2011.R")
source("2012.R")
source("2013.R")
source("2014.R")
source("2015.R")

stormDataTo2011 <- read.csv("data/StormData_to2011.csv")
stormData2012 <- read.csv("data/StormData_2012.csv")
stormData2013 <- read.csv("data/StormData_2013.csv")
stormData2014 <- read.csv("data/StormData_2014.csv")
stormData2015 <- read.csv("data/StormData_2015.csv")

stormDataTo2012 <- rbind(stormDataTo2011,stormData2012)
stormDataTo2013 <- rbind(stormDataTo2012,stormData2013)
stormDataTo2014 <- rbind(stormDataTo2013,stormData2014)
stormDataTo2015 <- rbind(stormDataTo2014,stormData2015)

TotalStormData <- select(stormDataTo2015, YEAR, STATE_ABBREVIATION, STATE_NAME, EVENT_TYPE, COUNT, FATALITIES, INJURIES, PROPERTY_DAMAGE, CROPS_DAMAGE)
str(TotalStormData)

# Write CSV in R
write.csv(TotalStormData, file = "data/StormData_to2015.csv")


# Clean data
TotalStormData <- read.csv("data/StormData_to2015.csv")

# Remove leading, trailing and repeated whitspaces from events labels.
length(unique(TotalStormData$EVENT_TYPE)) #991
TotalStormData$EVENT_TYPE <- toupper(gsub("(^\\s*)|(\\s*$)|((?<=\\s)\\s+)", "", TotalStormData$EVENT_TYPE, perl = T))
length(unique(TotalStormData$EVENT_TYPE)) #889


unique(TotalStormData$EVENT_TYPE)

# Remove summary rows
exclude.list <- c("NONE", "?", "OTHER", "MARINE ACCIDENT", "WET MONTH", "WET YEAR", 
                  "APACHE COUNTY", "NO SEVERE WEATHER", "MONTHLY PRECIPITATION", "UNSEASONABLY WARM YEAR", 
                  "DROWNING", "SOUTHEAST", "EXCESSIVE", "HIGH", "MILD PATTERN", "NORTHERN LIGHTS", 
                  "RECORD TEMPERATURES", "RECORD TEMPERATURE", "SEVERE TURBULENCE", "MONTHLY TEMPERATURE", 
                  "TEMPERATURE RECORD")
TotalStormData <- TotalStormData[!grepl("(^SUMMARY)|(SUMMARY$)", TotalStormData$EVENT_TYPE) & !TotalStormData$EVENT_TYPE %in% exclude.list, ]
length(unique(TotalStormData$EVENT_TYPE)) #801

# Combining thunderstorm realated entries
thuderstorm.pattern <- "^(THUN?D?EE?RE?STORM)|(TSTMW)|(TSTM)|(THUNDERSTORMW)|(THUNDERTORM)|(THUNDERTSORM)|(THUNDERSTROM)|(TUNDERSTORM)|(SEVERE THUNDERSTORM)|(GUSTNADO)|(GUSTY THUNDERSTORM WINDS)|(^GUSTY THUNDERSTORM WIND$)|(^THUNDESTORM WINDS$)|(MICR?OBURST)|(^DOWNBURST)"
grep(thuderstorm.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(thuderstorm.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "THUNDERSTORM / THUNDERSTORM WIND"
length(unique(TotalStormData$EVENT_TYPE)) #676

# Combining hail realated entries
hail.pattern <- "HAIL"
grep(hail.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(hail.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "HAIL"
length(unique(TotalStormData$EVENT_TYPE)) #640

# Combining ornado realated entries
tornado.pattern <- "(TORNADO)|(TORNDAO)|(WAY?TER\\s?SPOUT)|(FUNNEL)|(LANDSPOUT)|(WHIRLWIND)"
grep(tornado.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(tornado.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "TORNADO"
length(unique(TotalStormData$EVENT_TYPE)) #608

# Combining wildfire realated entries
wildfire.pattern <- "(^((BRUSH)|(WILD)|(FOREST)|(GRASS)).*FIRES?$)|(^RED FLAG CRITERIA)|(^RED FLAG FIRE WX)"
grep(wildfire.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(wildfire.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "WILDFIRE"
length(unique(TotalStormData$EVENT_TYPE)) #598

# Combining surge realated entries
surge.pattern <- "SURGE"
grep(surge.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(surge.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "SURGE"
length(unique(TotalStormData$EVENT_TYPE)) #596

# Combining flash flood realated entries
flash.flood.pattern <- "(FLASH.*FLOOD)|(FLOOD.*FLASH)|(^FLASH FLOOODING)"
grep(flash.flood.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(flash.flood.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "FLASH FLOOD"
length(unique(TotalStormData$EVENT_TYPE)) #572

# Combining coatal flood, beach erosion and tide realated entries
coastal.flood.pattern <- "((COASTAL)|(CSTL)|(TIDAL)|(BEACH)).*((EROSION)|(FLOOD))|(ASTRONOMICAL.*TIDE)|(^HIGH TIDE)|(^BEACH EROSIN)|(^RAPIDLY RISING WATER)|(^RAPIDLY RISING WATER)|(^BLOW-OUT TIDE)"
grep(coastal.flood.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(coastal.flood.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "COASTAL FLOOD OR EROSION / TIDE"
length(unique(TotalStormData$EVENT_TYPE)) #551

# Combining remaining flood realated entries
other.flood.pattern <- "(^FLOOD((S)|(ING))?$)|(^LAKE(SHORE)? FLOOD$)|(^(ICE JAM)|(SNOWMELT)|(MAJOR)|(MINOR)|(RIVER( AND STREAM )?)|(RURAL) FLOOD(ING)?$)|(^FLOOD & HEAVY RAIN$)|(^HIGH WATER$)|(^SMA?L?L STREAM)|(^LOCAL FLOOD)|(^HIGHWAY FLOODING)|(^FLOOD WATCH)|(^BREAKUP FLOODING)|(^STREAM FLOODING)|(URBAN)|(^STREET FLOOD)"
grep(other.flood.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(other.flood.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "FLOOD"
length(unique(TotalStormData$EVENT_TYPE)) #493

# Combining tropical storm related entries
tropical.strom.pattern <- "(TROPICAL STORM)|(HURRICANE)|(TYPHOON)|(TROPICAL DEPRESSION)|(^REMNANTS OF FLOYD)"
grep(tropical.strom.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(tropical.strom.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "TROPICAL STORM / HURRICANE / TYPHOON"
length(unique(TotalStormData$EVENT_TYPE)) #474

# Combining snow and ice related entries
ice.and.snow.pattern <- "(SNOW)|(ICE)|(GLAZE)|(FREEZING DRIZZLE)|(FREEZE)|(FROST)|(HEAVY MIX)|(SLEET)|(ICY ROADS)|(MIXED PRECIPITATION)|(^MIXED PRECIP)|(^FREEZING SPRAY)"
print(grep(ice.and.snow.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T))
TotalStormData[grepl(ice.and.snow.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "ICE / SNOW / FROST"
length(unique(TotalStormData$EVENT_TYPE)) #315

# Combining dust realated entries
dust.storm.pattern <- "DUST"
grep(dust.storm.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(dust.storm.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "DUST STORM / DEVIL"
length(unique(TotalStormData$EVENT_TYPE)) #308

# Combining cold realated entries
cold.pattern <- "(COLD)|(CHILL)|(COOL)|(LOW TEMPERATURE)|(HYPOTHERMIA)|(^UNSEASONAL LOW TEMP)|(^RECORD LOW$)"
grep(cold.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(cold.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "COLD AND WINDCHILL"
length(unique(TotalStormData$EVENT_TYPE)) #261

# Combining generic wind realated entries
wind.pattern <- "(((GRADIENT)|(HIGH)|(GUSTY)|(STRONG)) WIND)|(^WINDS?$)|(^NON-SEVERE WIND DAMAGE$)|(^WIND DAMAGE$)|(^STORM FORCE WINDS$)|(^WND$)|(^WIND GUSTS)|(^WIND ADVISORY)|(^WIND STORM)|(^GUSTY LAKE WIND)|(^WAKE LOW WIND)"
grep(wind.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(wind.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "WIND"
length(unique(TotalStormData$EVENT_TYPE)) #208

# Combining rain and wet weather realated entries
rain.pattern <- "(RAIN)|(HEAVY PRECIPITATION)|(HEAVY SHOWER)|(^METRO STORM, MAY 26)|(^HEAVY PRECIPATATION)|(^UNSEASONABLY WET)|(^EXCESSIVE PRECIPITATION)|(^NORMAL PRECIPITATION)|(^WET WEATHER)|(^EXCESSIVE WETNESS)|(^EXTREMELY WET)|(^RECORD PRECIPITATION)|(ABNORMALLY WET)"
grep(rain.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(rain.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "RAIN OR WET"
length(unique(TotalStormData$EVENT_TYPE)) #156

# Combining mudslide, rock slide and landslide realated entries
land.slide.pattern <- "(MUD\\s?SLIDE)|(ROCK SLIDE)|(LANDSLUMP)|(LANDSLIDES?)"
grep(land.slide.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(land.slide.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "LANDSLIDE / MUDSLIDE / ROCK SLIDE"
length(unique(TotalStormData$EVENT_TYPE)) #147

# Combining lightning realated entries
lightning.pattern <- "(LIGHTNING)|(LIGHTING)|(LIGNTNING)"
grep(lightning.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(lightning.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "LIGHTNING"
length(unique(TotalStormData$EVENT_TYPE)) #135

# Combining dust realated entries
fog.pattern <- "FOG"
grep(fog.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(fog.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "FOG"
length(unique(TotalStormData$EVENT_TYPE)) #131

# Combining winter storm and blizzard realated entries
blizzard.pattern <- "(BLIZZARD)|(WINTER STORM)"
grep(blizzard.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(blizzard.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "BLIZZARD / WINTER STORM"
length(unique(TotalStormData$EVENT_TYPE)) #125

# Combining winter weather realated entries
winter.pattern <- "(WINTER WEATHER)|(WINTRY MIX)|(^WINTER MIX)|(^WINTERY MIX)"
grep(winter.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(winter.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "WINTER WEATHER"
length(unique(TotalStormData$EVENT_TYPE)) #120

# Combining heat realated entries
heat.pattern <- "(HEAT)|(RECORD WARMTH)|(HOT)|(WARM)|(^RECORD HIGH)|(HYPERTHERMIA)|(HIGH TEMPERATURE RECORD)"
grep(heat.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(heat.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "HEAT"
length(unique(TotalStormData$EVENT_TYPE)) #80

# Combining drought realated entries
dry.pattern <- "(DROUGHT)|(DRY)|(DRIEST)|(^BELOW NORMAL PRECIPITATION)"
grep(dry.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(dry.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "DROUGHT"
length(unique(TotalStormData$EVENT_TYPE)) # 63

# Combining volcanic activity realated entries
volcanic.pattern <- "(VOLCANIC ((ASH)|(ERUPTION)))"
grep(volcanic.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(volcanic.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "VOLCANIC ACTIVITY"
length(unique(TotalStormData$EVENT_TYPE)) #60

# Combining various marine events
marine.pattern <- "(HIGH SEAS)|(SURF)|(RIP CURRENT)|(SWELLS)|(MARINE)|(HEAVY SEAS)|(^ROUGH SEAS)|(^HIGH WAVES)|(^WIND AND WAVE)|(^ROGUE WAVE)|(HIGH SURF)|(SEICHE)"
grep(marine.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(marine.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "MARINE WIND / SWELL / SURF"
length(unique(TotalStormData$EVENT_TYPE)) #38

# Combining vog and smoke realated entries
vog.and.smoke.pattern <- "(VOG)|(SMOKE)"
grep(vog.and.smoke.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(vog.and.smoke.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "VOG / SMOKE"
length(unique(TotalStormData$EVENT_TYPE)) #36

# Combinine avalanche realated entries
avalanche.pattern <- "(AVALANCH?E)"
grep(avalanche.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(avalanche.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "AVALANCHE"
length(unique(TotalStormData$EVENT_TYPE)) #35

# Combining dam failure realated entries
dam.pattern <- "(DAM)"
grep(dam.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(dam.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "DAM FAILURE"
length(unique(TotalStormData$EVENT_TYPE)) #34

# Combining coastal storm realated entries
coastalstorm.pattern <- "(COASTAL\\s?STORM)"
grep(coastalstorm.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(coastalstorm.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "COASTAL STORM"
length(unique(TotalStormData$EVENT_TYPE)) #33

# Combining wall cloud realated entries
wallcloud.pattern <- "(WALL CLOUD)"
grep(wallcloud.pattern, sort(unique(TotalStormData$EVENT_TYPE)), value = T)
TotalStormData[grepl(wallcloud.pattern, TotalStormData$EVENT_TYPE), ]$EVENT_TYPE <- "WALL CLOUD"
length(unique(TotalStormData$EVENT_TYPE)) #31
unique(TotalStormData$EVENT_TYPE)
dim(TotalStormData)

TotalStormDataAfterCleaning <- select(TotalStormData, YEAR, STATE_ABBREVIATION, STATE_NAME, EVENT_TYPE, COUNT, FATALITIES, INJURIES, PROPERTY_DAMAGE, CROPS_DAMAGE)
str(TotalStormDataAfterCleaning)
write.csv(TotalStormDataAfterCleaning, file = "data/StormData_to2015_after_cleaning.csv")

# Data aggregation
# Cleaned dataset has been aggregated by year to obtain insight about 
# general trends in number of reported events, number of affected people 
# and amount of property damage.
TotalStormDataAfterCleaning <- read.csv("data/StormData_to2015_after_cleaning.csv")
TotalStormDataAfterCleaning <- data.table(TotalStormDataAfterCleaning)

str(TotalStormDataAfterCleaning)
TotalStormDataAfterCleaning$YEAR <- as.character(TotalStormDataAfterCleaning$YEAR)

TotalStormDataAfterAggregating <-aggregate(list(TotalStormDataAfterCleaning$COUNT, TotalStormDataAfterCleaning$FATALITIES, TotalStormDataAfterCleaning$INJURIES, TotalStormDataAfterCleaning$PROPERTY_DAMAGE, TotalStormDataAfterCleaning$CROPS_DAMAGE), 
                 by=list(TotalStormDataAfterCleaning$YEAR, TotalStormDataAfterCleaning$STATE_ABBREVIATION, TotalStormDataAfterCleaning$STATE_NAME, TotalStormDataAfterCleaning$EVENT_TYPE), 
                 FUN=sum, na.rm=TRUE)

colnames(TotalStormDataAfterAggregating) <- c("YEAR", "STATE_ABBREVIATION", "STATE_NAME", "EVENT_TYPE", "COUNT", "FATALITIES", "INJURIES", "PROPERTY_DAMAGE", "CROPS_DAMAGE")
str(TotalStormDataAfterAggregating)
TotalStormDataAfterAggregating <- data.table(TotalStormDataAfterAggregating)
TotalStormDataAfterAggregating <- TotalStormDataAfterAggregating[order(YEAR, STATE_NAME, EVENT_TYPE),]
write.csv(TotalStormDataAfterAggregating, file = "data/StormData_to2015_after_aggregating.csv")

number.of.events.by.year <- TotalStormDataAfterCleaning[, list(COUNT = sum(COUNT)), by = list(YEAR)]

population.damage.by.year <- rbind(TotalStormDataAfterCleaning[, list(total = sum(FATALITIES, na.rm = TRUE), 
                                             category = "FATALITIES"), by = list(YEAR)], TotalStormDataAfterCleaning[, list(total = sum(INJURIES, 
                                                                                                               na.rm = TRUE), category = "INJURIES"), by = list(YEAR)])

population.damage.by.year$category <- factor(population.damage.by.year$category, 
                                             labels = c("FATALITIES", "INJURIES"))

property.and.crop.damage.by.year <- rbind(TotalStormDataAfterCleaning[, list(total = sum(CROPS_DAMAGE, 
                                                                na.rm = TRUE), category = "CROPS_DAMAGE"), by = list(YEAR)], TotalStormDataAfterCleaning[, list(total = sum(PROPERTY_DAMAGE, 
                                                                                                                                              na.rm = TRUE), category = "PROPERTY_DAMAGE"), by = list(YEAR)])

property.and.crop.damage.by.year$category <- factor(property.and.crop.damage.by.year$category, 
                                                    labels = c("CROPS_DAMAGE", "PROPERTY_DAMAGE"))


