# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, lubridate)

# Clean Accident Data By Year --------------------------------------------------

states.fars <- c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", 
                 "California", "", "Colorado", "Connecticut", "Delaware", 
                 "District of Columbia", "Florida", "Georgia", "Guam", 
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                 "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                 "Vermont", "Virginia", "Virgin Islands (Since 2004)", "Washington", 
                 "West Virginia", "Wisconsin", "Wyoming")

weather.conditions.1979 <- c('Clear', 'Rain', 'Sleet', 'Snow', '', '', 'Cloudy')
weather.conditions.1981 <- c('Clear', 'Rain', 'Sleet', 'Snow', 'Fog, Smog, Smoke', '', '', 'Other')
weather.conditions.2006 <- c('Clear', 'Rain', 'Sleet', 'Snow', 'Fog, Smog, Smoke', 'Rain', 'Sleet', 'Other')
weather.conditions.2009 <- c('Clear', 'Rain', 'Sleet', 'Snow', 'Fog, Smog, Smoke', 'Wind', 'Blowing Sand, Soil, Dirt', 'Other')
weather.conditions.2012 <- c('Clear', 'Rain', 'Sleet', 'Snow', 'Fog, Smog, Smoke', 'Wind', 'Blowing Sand, Soil, Dirt', 'Other', '', 'Cloudy', 'Snow')
weather.conditions.later <- c('Clear', 'Rain', 'Sleet', 'Snow', 'Fog, Smog, Smoke', 'Wind', 'Blowing Sand, Soil, Dirt', 'Other', '', 'Cloudy', 'Snow', 'Freezing Rain')

years <- seq(1975, 2019, 1)
for (year in years) {
  accident.df <- read.csv(paste0('./data/', year, '/accident.csv'))
  
  print(paste0('Cleaning accident data for ', year))
  
  lookup <- c("ROUTE" = "CL_TWAY")
  cleaned.accident.df <- accident.df %>%
    rename(any_of(lookup)) %>%
    mutate(LATITUDE = ifelse(rep('LATITUDE' %in% names(.), nrow(.)), LATITUDE, NA),
           RUR_URB = ifelse(rep(year <= 1986, nrow(.)), LAND_USE,
                                ifelse(rep(year <= 2014, nrow(.)), ROAD_FNC, RUR_URB)),
           LONGITUD =  ifelse(rep('LONGITUD' %in% names(.), nrow(.)), LONGITUD, NA),
           NO_LANES = ifelse(rep('NO_LANES' %in% names(.), nrow(.)), NO_LANES, NA),
           SP_LIMIT = ifelse(rep('SP_LIMIT' %in% names(.), nrow(.)), SP_LIMIT, NA),
           ALIGNMNT = ifelse(rep('ALIGNMNT' %in% names(.), nrow(.)), ALIGNMNT, NA),
           PROFILE = ifelse(rep('PROFILE' %in% names(.), nrow(.)), PROFILE, NA),
           SUR_COND = ifelse(rep('SUR_COND' %in% names(.), nrow(.)), SUR_COND, NA),
           Year = year) %>%
    select(ST_CASE, Year, STATE, PERSONS, DAY, MONTH, DAY_WEEK, 
           HOUR, MINUTE, ROUTE, RUR_URB, LATITUDE, LONGITUD, 
           MAN_COLL, LGT_COND, WEATHER, FATALS, DRUNK_DR, NO_LANES, SP_LIMIT,
           ALIGNMNT, PROFILE, SUR_COND) %>%
    rename(CaseId = ST_CASE,
           StateId = STATE,
           NumberOfPersonsInvolved = PERSONS,
           Month = MONTH, # 99 is unknown
           Day = DAY, # 99 is unknown
           DayOfTheWeek = DAY_WEEK, # 9 is unknown
           Hour = HOUR, # 88, 99 is unknown
           Minute = MINUTE, # 88, 99 is unknown
           RouteSigningId = ROUTE, # Can be Other/Unknown
           LandUseId = RUR_URB, # Usually Rural or Urban, but could be other/unknown/not report
           Latitude = LATITUDE, # 88, 99, 88.88, 99.99, 77.7777, 88.888, 99.999 are unknown/not reported
           Longitude = LONGITUD, # 88, 99, 88.88, 99.99, 77.7777, 88.888, 99.999 are unknown/not reported
           MannerOfCollisionId = MAN_COLL, # 9, 98, 99 are unknown/not reported for MAN_COLL
           LightConditionId = LGT_COND, # 9, 8 are unknown/not reported for LGT_COND
           WeatherCondition = WEATHER, # 98, 99 are unknown/not reported for WEATHER
           NumberOfFatalities = FATALS,
           NumberOfDrunkDriversInvolved = DRUNK_DR, # Beginning in 2008, DRUNK_DR is for drivers only. For 1999-2007, it was incorrectly derived as anybody in the crash testing positive for alcohol at a certain level. Before that, it was not super reliable
           TotalLanesInRoadway = NO_LANES,
           PostedSpeedLimit = SP_LIMIT,
           RoadwayAlignment = ALIGNMNT,
           RoadProfile = PROFILE,
           RoadwaySurfaceCondition = SUR_COND
     ) %>%
    mutate(
      RouteSigning = as.factor(RouteSigningId),
      LandUse = as.factor(LandUseId),
      MannerOfCollision = as.factor(MannerOfCollisionId),
      LightCondition = as.factor(LightConditionId),
      WeatherCondition = as.factor(WeatherCondition),
    ) %>%
    mutate(
      State = states.fars[StateId],
      Month = ifelse(Month == 99, NA, Month),
      Day = ifelse(Day == 99, NA, Day),
      DayOfTheWeek = ifelse(DayOfTheWeek == 9, NA, DayOfTheWeek),
      Hour = ifelse(Hour %in% c(88, 99), NA, Hour),
      Minute = ifelse(Minute %in% c(88, 99), NA, Minute),
      RouteSigningId = ifelse(RouteSigningId %in% c(8, 9), NA, RouteSigningId),
      LandUseId = ifelse(LandUseId %in% c(8, 9), NA, LandUseId),
      Latitude = ifelse(Latitude %in% c(88888888, 99999999), NA, Latitude),
      Longitude = ifelse(Longitude %in% c(888888888, 999999999), NA, Longitude),
      MannerOfCollisionId = ifelse(Year <= 2001, 
                                   ifelse(MannerOfCollisionId == 9, NA, MannerOfCollisionId),
                                   ifelse(MannerOfCollisionId %in% c(98, 99), NA, MannerOfCollisionId)),
      LightConditionId = ifelse(LightConditionId %in% c(8, 9), NA, LightConditionId),
      WeatherCondition = ifelse(WeatherCondition %in% c(9, 99, 98), NA, WeatherCondition),
      TotalLanesInRoadway = ifelse(TotalLanesInRoadway == 9, NA, TotalLanesInRoadway),
      PostedSpeedLimit = ifelse(PostedSpeedLimit %in% c(98, 99), NA, PostedSpeedLimit),
      RoadwayAlignment = ifelse(RoadwayAlignment == 9, NA, RoadwayAlignment),
      RoadProfile = ifelse(RoadProfile  == 9, NA, RoadProfile),
      RoadwaySurfaceCondition = ifelse(RoadwaySurfaceCondition %in% c(8, 9), NA, RoadwaySurfaceCondition)
    ) %>%
    filter(State %in% state.name) %>%
    mutate(StateAbbv = state.abb[match(State, state.name)],
           DateOfAccident = make_date(Year, Month, Day)) %>%
    select(-StateId)
  
  # Convert weather data to values
  cleaned.accident.df <- cleaned.accident.df %>%
    mutate(WeatherConditionId = WeatherCondition) %>%
    mutate(WeatherCondition = ifelse((Year <= 1979) & (Year >= 1975) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.1979[WeatherCondition], WeatherCondition)) %>%
    mutate(WeatherCondition = ifelse((Year <= 1981) & (Year >= 1980) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.1981[WeatherCondition], WeatherCondition)) %>%
    mutate(WeatherCondition = ifelse((Year <= 2006) & (Year >= 1982) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.2006[WeatherCondition], WeatherCondition)) %>%
    mutate(WeatherCondition = ifelse((Year <= 2009) & (Year >= 2007) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.2009[WeatherCondition], WeatherCondition)) %>%
    mutate(WeatherCondition = ifelse((Year <= 2012) & (Year >= 2010) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.2012[WeatherCondition], WeatherCondition)) %>%
    mutate(WeatherCondition = ifelse((Year >= 2013) & (WeatherCondition != '') & (!is.na(WeatherCondition)), weather.conditions.later[WeatherCondition], WeatherCondition))
  
  cleaned.accident.df[is.na(cleaned.accident.df)] <- ""
  
  print(paste0('Cleaned accident data for ', year))
  
  write.csv(cleaned.accident.df, paste0('./data/', year, '/cleaned-accident.csv'), row.names = FALSE)
}

print('Completed cleaning accident data')

# Clean Vehicle Data By Year ---------------------------------------------------

for (year in years) {
  vehicle.df <- read.csv(paste0('./data/', year, '/vehicle.csv'))
  print(paste0('Cleaning vehicle data for ', year))
  
  lookup <- c("L_COMPL" = "L_CL_VEH", 
              "BODY_TYP" = "PBODYTYP",
              "MOD_YEAR" = "PMODYEAR")
  cleaned.vehicle.df <- vehicle.df %>%
    rename(any_of(lookup)) %>%
    mutate(L_COMPL = ifelse(rep('L_COMPL' %in% names(.), nrow(.)), L_COMPL, NA),
           Factors1 = ifelse(rep('DR_CF1' %in% names(.), nrow(.)), DR_CF1, NA),
           Factors2 = ifelse(rep('DR_CF2' %in% names(.), nrow(.)), DR_CF2, NA),
           Factors3 = ifelse(rep('DR_CF3' %in% names(.), nrow(.)), DR_CF3, NA),
           Factors4 = ifelse(rep('DR_CF4' %in% names(.), nrow(.)), DR_CF4, NA),
           Factors5 = ifelse(rep('DR_SF1' %in% names(.), nrow(.)), DR_SF1, NA),
           Factors6 = ifelse(rep('DR_SF2' %in% names(.), nrow(.)), DR_SF2, NA),
           Factors7 = ifelse(rep('DR_SF3' %in% names(.), nrow(.)), DR_SF3, NA),
           Factors8 = ifelse(rep('DR_SF4' %in% names(.), nrow(.)), DR_SF4, NA),
           VNUM_LAN = ifelse(rep('VNUM_LAN' %in% names(.), nrow(.)), VNUM_LAN, NA),
           VSPD_LIM = ifelse(rep('VSPD_LIM' %in% names(.), nrow(.)), VSPD_LIM, NA),
           VPROFILE = ifelse(rep('VPROFILE' %in% names(.), nrow(.)), VPROFILE, NA),
           VSURCOND = ifelse(rep('VSURCOND' %in% names(.), nrow(.)), VSURCOND, NA),
           VALIGN = ifelse(rep('VALIGN' %in% names(.), nrow(.)), VALIGN, NA)) %>%
    select(ST_CASE, VEH_NO, MAKE, BODY_TYP, MOD_YEAR, TRAV_SP, L_STATUS, L_COMPL, 
           Factors1, Factors2, Factors3, Factors4, Factors5, Factors6, 
           Factors7, Factors8, VNUM_LAN, VSPD_LIM, VALIGN, VPROFILE, VSURCOND) %>%
    rename(CaseId = ST_CASE,
           VehicleId = VEH_NO,
           Make = MAKE,
           CarBodyType = BODY_TYP,
           VehicleModelYear = MOD_YEAR,
           TravelingSpeed = TRAV_SP,
           LicenseStatus = L_STATUS,
           LicenseCompliance = L_COMPL,
           TotalLanesInRoadway = VNUM_LAN,
           PostedSpeedLimit = VSPD_LIM,
           RoadwayAlignment = VALIGN,
           RoadProfile = VPROFILE,
           RoadwaySurfaceCondition = VSURCOND
    ) %>%
    mutate(Make = ifelse(Make == 99, NA, Make),
           CarBodyType = ifelse(CarBodyType %in% c(98, 99), NA, CarBodyType),
           VehicleModelYear = ifelse(VehicleModelYear %in% c(9998, 9999), NA, VehicleModelYear),
           TravelingSpeed = ifelse(TravelingSpeed %in% c(98, 99, 998, 999), NA, TravelingSpeed),
           LicenseStatus = ifelse(LicenseStatus == 9, NA, LicenseStatus),
           LicenseCompliance = ifelse(LicenseCompliance %in% c(6,7,8,9), NA, LicenseCompliance),
           TotalLanesInRoadway = ifelse(TotalLanesInRoadway == 9, NA, TotalLanesInRoadway),
           PostedSpeedLimit = ifelse(PostedSpeedLimit %in% c(98, 99), NA, PostedSpeedLimit),
           RoadwayAlignment = ifelse(RoadwayAlignment %in% c(8, 9), NA, RoadwayAlignment),
           RoadProfile = ifelse(RoadProfile %in% c(8, 9), NA, RoadProfile),
           RoadwaySurfaceCondition = ifelse(RoadwaySurfaceCondition %in% c(98, 99), NA, RoadwaySurfaceCondition),
           DriverRelatedFactors1 = ifelse(rep(year <= 2009, nrow(.)), Factors1, Factors5),
           DriverRelatedFactors2 = ifelse(rep(year <= 2009, nrow(.)), Factors2, Factors6),
           DriverRelatedFactors3 = ifelse(rep(year <= 2009, nrow(.)), Factors3, Factors7),
           DriverRelatedFactors4 = ifelse(rep(year <= 2009, nrow(.)), Factors4, Factors8)) %>%
    select(-Factors1, -Factors2, -Factors3, -Factors4, 
           -Factors5, -Factors6, -Factors7, -Factors8)
  cleaned.vehicle.df[is.na(cleaned.vehicle.df)] <- ""
  
  print(paste0('Cleaned vehicle data for ', year))
  
  write.csv(cleaned.vehicle.df, paste0('./data/', year, '/cleaned-vehicle.csv'), row.names = FALSE)
}

print('Completed cleaning vehicle data')

# Clean Person Data By Year ----------------------------------------------------

for (year in years) {
  person.df <- read.csv(paste0('./data/', year, '/person.csv'))
  print(paste0('Cleaning person data for ', year))
  
  cleaned.person.df <- person.df %>%
    mutate(DRUGS = ifelse(rep('DRUGS' %in% names(.), nrow(.)), DRUGS, NA),
           Year = year) %>%
    select(ST_CASE, VEH_NO, AGE, SEX, Year, PER_TYP, 
           INJ_SEV, SEAT_POS, DRINKING, DRUGS) %>%
    rename(CaseId = ST_CASE,
           VehicleId = VEH_NO,
           Age = AGE,
           Sex = SEX,
           PersonType = PER_TYP,
           SeverityOfInjury = INJ_SEV,
           SeatingPosition = SEAT_POS,
           PoliceReportedAlcoholInvolvement = DRINKING,
           PoliceReportedDrugInvolvement = DRUGS) %>%
    mutate(Age = ifelse(Year <= 2008,
                        ifelse(Age == 99, NA, Age),
                        ifelse(Age %in% c(998, 999), NA, Age)),
           Sex = ifelse(Sex %in% c(8, 9), NA, Sex),
           PersonType = ifelse(Year <= 1993,
                               ifelse(PersonType == 9, NA, PersonType),
                               ifelse(PersonType %in% c(88, 99), NA, PersonType)),
           SeverityOfInjury = ifelse(SeverityOfInjury %in% c(8, 9), NA, SeverityOfInjury),
           SeatingPosition = ifelse(SeatingPosition %in% c(98, 99), NA, SeatingPosition),
           PoliceReportedAlcoholInvolvement = ifelse(PoliceReportedAlcoholInvolvement %in% c(8, 9), NA, PoliceReportedAlcoholInvolvement),
           PoliceReportedDrugInvolvement = ifelse(PoliceReportedDrugInvolvement %in% c(8, 9), NA, PoliceReportedDrugInvolvement)
     ) %>%
    mutate(Sex = ifelse(Age == 1, 'Male', 'Female'))
  cleaned.person.df[is.na(cleaned.person.df)] <- ""
  
  print(paste0('Cleaned person data for ', year))
  
  write.csv(cleaned.person.df, paste0('./data/', year, '/cleaned-person.csv'), row.names = FALSE)
  
}

print('Completed cleaning person data')

# Combine Data Together --------------------------------------------------------

for (year in years) {
  print(paste0('Combining data for ', year))
  
  cleaned.person.df <- read.csv(paste0('./data/', year, '/cleaned-person.csv'))
  cleaned.vehicle.df <- read.csv(paste0('./data/', year, '/cleaned-vehicle.csv'))
  cleaned.accident.df <- read.csv(paste0('./data/', year, '/cleaned-accident.csv'))
  
  combined.df <- cleaned.person.df %>%
    left_join(cleaned.vehicle.df, by = c('CaseId', 'VehicleId')) %>%
    left_join(cleaned.accident.df, by = 'CaseId') %>%
    mutate(RoadwaySurfaceCondition = ifelse(rep(year <= 2009, nrow(.)), RoadwaySurfaceCondition.y, RoadwaySurfaceCondition.x),
           RoadProfile = ifelse(rep(year <= 2009, nrow(.)), RoadProfile.y, RoadProfile.x),
           RoadwayAlignment = ifelse(rep(year <= 2009, nrow(.)), RoadwayAlignment.y, RoadwayAlignment.x),
           PostedSpeedLimit = ifelse(rep(year <= 2009, nrow(.)), PostedSpeedLimit.y, PostedSpeedLimit.x),
           TotalLanesInRoadway = ifelse(rep(year <= 2009, nrow(.)), TotalLanesInRoadway.y, TotalLanesInRoadway.x)) %>%
    rename(Year = Year.x) %>%
    select(-RoadwaySurfaceCondition.y, -RoadwaySurfaceCondition.x, 
           -RoadProfile.y, -RoadProfile.x, 
           -RoadwayAlignment.y, -RoadwayAlignment.x, 
           -PostedSpeedLimit.y, -PostedSpeedLimit.x,
           -TotalLanesInRoadway.y, -TotalLanesInRoadway.x, -Year.y)
  combined.df[is.na(combined.df)] <- ""
  
  print(paste0('Combined data for ', year))
  
  write.csv(combined.df, paste0('./data/', year, '/combined-data.csv'), row.names = FALSE)
}
