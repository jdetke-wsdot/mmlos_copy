# Multimodal Level of Service
# Data processing 

library(foreign) # to read DBF file
library(tidyverse) # data munging 

# retrieve data
# Environmental Protection Agency. (2024). Smart Location Database. Retrieved from https://www.epa.gov/smartgrowth/smart-location-mapping#SLD on June 9, 2024. 
# Direct link to zip file of data base: https://edg.epa.gov/EPADataCommons/public/OA/SLD/SmartLocationDatabaseV3.zip
# Need to unzip, then bring in, comes as a geodatabase, with one block group file containing the entire database
# zip file comes with data dictionary 
# Quick and dirty... Open GDB in Arc, use 'Table to dBASE tool', open DBF here... 

EPA <- foreign::read.dbf("data/SmartLocationDatabaseV3/EPA_SLD.dbf", as.is = F)

# Just the data we need...
EPA_WA <- EPA %>% 
  dplyr::filter(STATEFP==53) %>%
  dplyr::select(OBJECTID,
                GEOID10,
                GEOID20,
                COUNTYFP,
                TRACTCE,
                Ac_Land,
                TotPop,
                CountHU,
                HH,
                TotEmp,
                E5_Ind,
                D1A, #HU/acre
                D1C, #jobs/acre
                D1D, #employment + HU / acre
                D5DR, #transit employment accessibility vs MSA access
                D3AMM, # links per square mile multimodal
                D3APO) %>% # links per square mile pedestrian
  dplyr::rename(County = COUNTYFP,
                Tract = TRACTCE,
                Population = TotPop,
                HousingUnits = CountHU,
                Households = HH,
                Employment = TotEmp,
                Industrial_Employment = E5_Ind,
                HousingUnitsAcre = D1A,
                JobsAcre = D1C,
                ActivityUnitsAcre = D1D,
                DestinationAccessibility = D5DR,
                DesignMultimodal = D3AMM,
                DesignPedestrian = D3APO) %>%
  dplyr::mutate(Design = ifelse(DesignMultimodal > DesignPedestrian,
                                DesignMultimodal,
                                DesignPedestrian)) %>% # Copying process done in Oregon to choose the higher measure to handle missing and bad data. ODOT flyer https://www.oregon.gov/lcd/CL/Documents/PlaceTypesFlyer.pdf
  dplyr::mutate(AreaType = dplyr::case_when(ActivityUnitsAcre < 1 ~ "rural_temp", # creating this just to not adjust to transit with very low activity units later, will remove the temp on back end
                                            ActivityUnitsAcre >= 1 & ActivityUnitsAcre < 4 ~ "rural",
                                            ActivityUnitsAcre >= 4 & ActivityUnitsAcre < 8 ~ "suburban",
                                            ActivityUnitsAcre >= 8 & ActivityUnitsAcre < 30 ~ "urban",
                                            ActivityUnitsAcre >=30 ~ "urban core")) %>%
  dplyr::mutate(AreaTypeAT = base::ifelse(ActivityUnitsAcre >= 8 & ActivityUnitsAcre < 30 & Design > 30,
                                         "urban core",
                                         AreaType)) %>%
  dplyr::mutate(FreightIntensive = dplyr::case_when(Industrial_Employment >= 2000 ~ "frieght intensive",
                                                    Industrial_Employment < 2000 ~ "")) %>%
  dplyr::group_by(County) %>%
  dplyr::mutate(county_emp = sum(Employment, na.rm = T)) %>%
  dplyr::mutate(bg_emp_share = Employment / county_emp) %>%
  dplyr::mutate(EmpShare = base::ifelse(bg_emp_share > .05, "emp", "")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-county_emp, -bg_emp_share) %>%
  dplyr::mutate(AreaTypeATjobs = base::ifelse(AreaTypeAT == "suburban" & 
                                                EmpShare == "emp",
                                              "urban",
                                              AreaTypeAT))

# bring in data on whether block groups have existing fixed route transit service
# this was created outside of R by taking an intersect of the fixed route transit service
# File at: https://gisdata-wsdot.opendata.arcgis.com/datasets/9b920f5ac2e04dff980d5463bd88bed5_3/explore
# the polygon file comes from the EPA geodatabase downloaded earlier or under the folder data/epa...

transit <- utils::read.csv("data/block_groups_transit.csv") %>%
  dplyr::select(GEOID20) %>%
  dplyr::distinct() %>%
  dplyr::mutate(transit = "transit")
transit$GEOID20 <- base::as.factor(transit$GEOID20)

EPA_WA <- dplyr::left_join(EPA_WA, transit)

EPA_WA$transit[is.na(EPA_WA$transit)] <- "no"

EPA_WA <- EPA_WA %>%
  dplyr::mutate(AreaTypeATjobsTransit = base::ifelse(AreaTypeATjobs == "rural" &
                                                       transit == "transit",
                                                     "suburban",
                                                     AreaTypeATjobs))

# and remove rural temp post transit adjustment. 
EPA_WA$AreaTypeATjobsTransit <- base::ifelse(EPA_WA$AreaTypeATjobsTransit == "rural_temp",
                                             "rural",
                                             EPA_WA$AreaTypeATjobsTransit)

  
# Write table for giggles if wanted
write.csv(EPA_WA, "data/EPA_WA.csv")

# outside of R join this DF to block group shapefile. 
# dissolve layer for easy load in shiny app. 

