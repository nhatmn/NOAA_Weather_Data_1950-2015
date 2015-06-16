library(data.table)
library(dplyr)
StormData <- read.csv(gzfile('data/StormEvents_details-ftp_v1.0_d2013_c20150601.csv.gz'))
str(StormData)
levels(StormData$STATE)

#'x' is the column of a data.frame that holds STATE NAME
stateFromAbbreviationToName <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
                      "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                      "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                      "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                      "UT", "VT", "VA", "WA", "WV", "WI", "WY", "LC", "PH", "GM", "PZ", 
                      "AM", "AN", "PK", "LH", "LM", "LS", "SL", "LO", "LE", "VI", "AS", 
                      "GU", "PR")),
    full=as.factor(c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", 
                     "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", 
                     "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", 
                     "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", 
                     "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", 
                     "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", 
                     "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", 
                     "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", 
                     "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING", "LAKE ST CLAIR", 
                     "HAWAII WATERS", "GULF OF MEXICO", "E PACIFIC", "ATLANTIC SOUTH", 
                     "ATLANTIC NORTH", "GULF OF ALASKA", "LAKE HURON", "LAKE MICHIGAN", 
                     "LAKE SUPERIOR", "ST LAWRENCE R", "LAKE ONTARIO", "LAKE ERIE", "VIRGIN ISLANDS", 
                     "AMERICAN SAMOA", "GUAM", "PUERTO RICO"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(full=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$state[match(st.x$full,st.codes$full)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

substrRight <- function(x, n){
  if (x != '' && length(x) != 0){
    substr(x, nchar(x)-n+1, nchar(x))
  } else {
    return('')
  }
}

removeLastCharacter <- function(x){
  if (x != '' && length(x) != 0){
    substr(x, 1, nchar(x)-1)
  } else {
    return('')
  }
}

# (Hundred (H), Thousand (K), Million (M), Billion (B))
# M = H/10000
# M = K/1000
# M = B*1000
# This function is to standardize the PROPERTY and CROPS damage number to Million
standardizeToMillion<-function(value, mag) {
  if (value != '' && length(value) != 0){
    if(mag=="H"){ 
      value / 10000;
    } else if(mag=="K") { 
      value / 1000;
    } else if (mag == "M"){
      value * 1;
    } else if (mag == "B") { 
      value * 1000;
    } else {
      0;
    }
  } else {
    return('')
  }
}

convertStringToNumber <- function(x){
  if (x != '' && length(x) != 0){
    as.numeric(x)
  } else {
    return(0)
  }
}

StormData$STATE_ABBREVIATION <- stateFromAbbreviationToName(StormData$STATE)
StormData$COUNT <- 1
StormData$FATALITIES <-  StormData$DEATHS_DIRECT + StormData$DEATHS_INDIRECT
StormData$INJURIES <-	StormData$INJURIES_DIRECT + StormData$INJURIES_INDIRECT
StormData$PROPERTY_DAMAGE <-	mapply(removeLastCharacter, as.character(StormData$DAMAGE_PROPERTY))
StormData$CROPS_DAMAGE <-	mapply(removeLastCharacter, as.character(StormData$DAMAGE_CROPS))
StormData$PROPERTY_DAMAGE <-  mapply(convertStringToNumber, StormData$PROPERTY_DAMAGE)
StormData$CROPS_DAMAGE <-	mapply(convertStringToNumber, StormData$CROPS_DAMAGE)
StormData$PROPERTY_DAMAGE_EXP <-  mapply(substrRight, as.character(StormData$DAMAGE_PROPERTY), 1)
StormData$CROPS_DAMAGE_EXP <-	mapply(substrRight, as.character(StormData$DAMAGE_CROPS), 1)
StormData$PROPERTY_DAMAGE <- mapply(standardizeToMillion, StormData$PROPERTY_DAMAGE, toupper(StormData$PROPERTY_DAMAGE_EXP))
StormData$CROPS_DAMAGE <- mapply(standardizeToMillion, StormData$CROPS_DAMAGE, toupper(StormData$CROPS_DAMAGE_EXP))
StormData$EVENT_TYPE <- toupper(StormData$EVENT_TYPE)

ShortStormData <- select(StormData, YEAR, STATE_ABBREVIATION, STATE, EVENT_TYPE, COUNT, FATALITIES, INJURIES, PROPERTY_DAMAGE, CROPS_DAMAGE)
colnames(ShortStormData) <- c("YEAR", "STATE_ABBREVIATION", "STATE_NAME", "EVENT_TYPE", "COUNT", "FATALITIES", "INJURIES", "PROPERTY_DAMAGE", "CROPS_DAMAGE")

# Write CSV in R
write.csv(ShortStormData, file = "data/StormData_2013.csv")



