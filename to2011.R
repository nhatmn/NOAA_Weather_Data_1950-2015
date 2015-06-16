library(data.table)
library(dplyr)
StormData <- read.csv(bzfile('data/repdata_data_StormData.csv.bz2'))
str(StormData)
StormData$COUNT <- 1
StormData$YEAR <- year(strptime(StormData$BGN_DATE, "%m/%d/%Y %H:%M:%S"))

levels(StormData$PROPDMGEXP)
levels(StormData$CROPDMGEXP)

# (Hundred (H), Thousand (K), Million (M), Billion (B))
# M = H/10000
# M = K/1000
# M = B*1000
# This function is to standardize the PROPERTY and CROPS damage number to Million
standardizeToMillion<-function(value, mag) {
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
}

StormData$PROPERTY_DAMAGE <- mapply(standardizeToMillion, StormData$PROPDMG, toupper(StormData$PROPDMGEXP))
StormData$CROPS_DAMAGE <- mapply(standardizeToMillion, StormData$CROPDMG, toupper(StormData$CROPDMGEXP))

levels(StormData$STATE)

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
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
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

StormData$STATE_NAME <- stateFromLower(StormData$STATE)

ShortStormData <- select(StormData, YEAR, STATE, STATE_NAME, EVTYPE, COUNT, FATALITIES, INJURIES, PROPERTY_DAMAGE, CROPS_DAMAGE)
colnames(ShortStormData) <- c("YEAR", "STATE_ABBREVIATION", "STATE_NAME", "EVENT_TYPE", "COUNT", "FATALITIES", "INJURIES", "PROPERTY_DAMAGE", "CROPS_DAMAGE")

# Write CSV in R
write.csv(ShortStormData, file = "data/StormData_to2011.csv")
