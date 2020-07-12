library(tidyverse)
library(lubridate)
library(rgdal)
library(rsatscan)
library(rJava)
library(mailR)
library(keyring)
library(rmarkdown)
library(leaflet)
library(kableExtra)
library(DT)


####IMPORTANT NOTES:
#If the case history file is ever manipulated directly in Excel, be sure to save the RunDate column as a Custom Format type 'm/d/yyyy' before closing
#Same as above for the cluster history file and the StartDate and EndDate columns

#Set email settings - only needs to be done once per computer
#key_set("satscan_sender")  ##Email address to appear in 'From' line of emails
#key_set("email_host")      ##Email smtp host name
#key_set("email_id")        ##Email smtp email address 
#key_set("email_pw")        ##Eamil smtp password



#=================SCRIPT SET-UP================#

#Setting working directory
setwd('S:/Enhanced Surveillance/General CD/COVID-19/SatScan')

#Setting options for Markdown reports
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")



#=================PREPARING FILES AND PARAMETERS================#

#Importing Business Objects Report and clean variables
#Note: Report pulls last 14 days for household exclusions (see notes below) then is filtered to correct analysis dates
satscanLineList <- list.files("C:/Users/kbemis/Downloads", pattern = "^COVID_Line_List_for_SatScan", full.names = T)
events <- read_csv(satscanLineList) %>% 
  distinct() %>%
  rename_all(~str_replace_all( ., "([[:punct:]])|[[:space:]]", "_" )) %>%
  rename(Last = Current_Last_Name,
         First = Current_First_Name,
         Age = Current_Age__yrs_,
         Race = Race_at_Onset_List,
         Ethnicity = Ethnicity_at_Onset,
         Admitted = Admitted_to_Hospital,
         Specimen_Date = Earliest_Specimen_Collection_Date,
         Attends_Facility = Day_Care_Facility_Name,
         Lat = Latitude__Home_,
         Long = Longitude__Home_,
         Employer = Name_of_Employer,
         City = Current_City) %>%
  mutate(Specimen_Date = ymd(str_remove(Specimen_Date, '[[:space:]].*'))) 


#Filter out cases without clean addresses
events <- events %>%
  filter(Address_Verified__Home_ == "YES") %>%
  filter(!grepl("* box *", Current_Address_Line_1, ignore.case = T)) #Remove PO Box addresses

  
#Filter out cases linked to congregate settings, other outbreaks, or likely household transmission 
#Goal is to detect community transmission
#Note: How should assumptions/goals change under extensive contact tracing?

#Household transmission
events <- events %>%
  group_by(Last, Lat, Long) %>% 
  arrange(Last, Lat, Long, Specimen_Date) %>% 
  mutate(Household_Lag = Specimen_Date - first(Specimen_Date)) %>%
  ungroup %>%
  filter(Household_Lag < 2)
  
#Congregate settings and outbreaks
congregate_settings <- c("Long-term/Skilled Care Facility", "Assisted/Supported Living Facility", "Independent/Senior Living Facility", 
                         "Developmental Disability Facility", "Other Long-Term Facility", "Group Home", 
                         "Mental Health Facility", "Alcohol/Drug Treatment Facility", "Correction Facility or Jail",
                         "Military Facility")

events <- events %>%
  filter(Current_Address_Type == "Home") %>%
  filter(!Patient_Attends_Resides %in% congregate_settings) %>%
  filter(is.na(Is_Part_of_Outbreak) | Is_Part_of_Outbreak != "Yes" )


#Filter to correct analysis dates (6 days with a 2 day lag)
events <- events %>%
  filter((Specimen_Date < Sys.Date() - 2) & (Specimen_Date > Sys.Date() - 9))


#Setting disease name for incorporation in history files
disease_run_name <- "COVID"
  

#=================RUNNING SATSCAN================#
  
#Creating cases file for SatScan
casfile <- events %>%
    mutate(Count = 1) %>%
    select(EventID = State_Case_Number,
           Count,
           Specimen_Date)
  
#Creating coordinates file for SatScan
geofile <- events %>%
    select(EventID = State_Case_Number,
           Lat,
           Long)
  

#Reset the options for the parameter file
invisible(ss.options(reset=TRUE))

#Set options related to the input tab 
ss.options(list(CaseFile = "SessionR.cas", 
                PrecisionCaseTimes = 3,
                StartDate = format(Sys.Date()- 9, "%Y/%m/%d"),
                EndDate = format(Sys.Date()- 2, "%Y/%m/%d"),
                CoordinatesFile = "SessionR.geo",
                CoordinatesType = 1
                ))

#Set options related to the analysis tab 
ss.options(list(AnalysisType = 4,
                ModelType = 2,
                ScanAreas = 1,
                TimeAggregationUnits = 3,
                TimeAggregationLength = 1
))

#Set options related to the output tab 
ss.options(list(ResultsFile = "S:/SatScan Project/ScanResult.txt",  ###doesn't appear to actually be writing?
                OutputGoogleEarthKML = "n",
                OutputShapefiles = "y",
                MostLikelyClusterEachCentroidASCII = "y",
                MostLikelyClusterEachCentroidDBase = "y",
                MostLikelyClusterCaseInfoEachCentroidASCII = "n",
                MostLikelyClusterCaseInfoEachCentroidDBase = "n",
                CensusAreasReportedClustersASCII = "y",
                CensusAreasReportedClustersDBase = "y",
                IncludeRelativeRisksCensusAreasASCII = "n",
                IncludeRelativeRisksCensusAreasDBase = "n",
                SaveSimLLRsASCII = "n",
                SaveSimLLRsDBase = "n"
))

#Set advanced options 
ss.options(list(UseDistanceFromCenterOption = "n",
                MaxSpatialSizeInDistanceFromCenter = 1,
                NonCompactnessPenalty = 1,
                MaxTemporalSize = 3, #NOTE: NYC also setting min temporal window (2) but option not available in Rsatscan list, not sure why
                MaxTemporalSizeInterpretation = 1,  
                ProspectiveStartDate = format(Sys.Date()-8, "%Y/%m/%d"),  #Not sure if this parameter is correct...maybe doesn't matter?
                CriteriaForReportingSecondaryClusters = 1,
                LogRunToHistoryFile = "y"#,
                #Version = "9.4.4"  #actually running version 9.6 but when correct and enabled, script fails? Works when not specified. 
))


#Write files
td <- tempdir()
write.ss.prm(td, "SessionR")
write.cas(as.data.frame(casfile), td, "SessionR")
write.geo(as.data.frame(geofile), td, "SessionR")

#Run SatScan and store results

session <- try(satscan(td, "SessionR", sslocation = "C:/Program Files/SaTScan", cleanup = FALSE, verbose = TRUE), silent = T)

if (class(session) == "try-error"){
  sink(file = 'satscan_function_log.txt', append = T)
  cat('No results returned for', disease_run_name, '\n')
  sink()
  
  write(paste0(disease_run_name, "_", Sys.Date(), " - ", session, "\n"), file = 'try_error_log.txt', append = T)
  
  return()
}



#=================PACKAGING SATSCAN RESULTS================#

#If/Else determines if a significant cluster has been detected and executes different steps depending on answer
if (max(session$col$RECURR_INT) < 100) {             ####REDUCE NUMBER IF TESTING OUTPUT/LOOPS
  
  #Add result to log file
  sink(file = 'satscan_function_log.txt', append = T)
  cat('No significant cluster for', disease_run_name, '\n')
  sink()
  
  #Creating log of most likely cluster to add to history file       
  cluster_temp <- as.data.frame(session$col) %>% 
    filter(CLUSTER == 1) %>%  
    mutate(ClusterID = paste(Sys.Date(), disease_run_name, "1", sep = "-"),
           StartDate = ymd(START_DATE),
           EndDate = ymd(END_DATE),
           Disease = disease_run_name) %>%   
    select(ClusterID, Disease, LATITUDE, LONGITUDE, RADIUS, StartDate, EndDate, P_VALUE, RECURR_INT, OBSERVED, EXPECTED, ODE)
  
  #Identifying case list of most likely cluster (current and past - filtered to just current involved in next step)
  all_involved <- session$gis %>% filter(CLUSTER == 1) %>% select(LOC_ID)
  
  #Saving cases from involved tracts to cluster log
  cluster_temp$CaseList <- events %>% 
    filter(State_Case_Number %in% all_involved$LOC_ID & Specimen_Date >= cluster_temp$StartDate & Specimen_Date <= cluster_temp$EndDate) %>% 
    select(State_Case_Number) %>% 
    toString()
  
  #Adding cluster to history file
  write_csv(cluster_temp, 'Cluster History File.csv', append = TRUE)

  
}  else {
  
  
  #Determine number of significant clusters
  cluster_num <- sum(session$col$RECURR_INT > 99)   ####REDUCE NUMBER IF TESTING OUTPUT/LOOPS
  
  #Logging each cluster to history file
  for (i in 1:cluster_num) {
    
    #Creating log of cluster to add to history file      
    cluster_temp <- as.data.frame(session$col) %>% 
      filter(CLUSTER == i) %>%  
      mutate(ClusterID = paste(Sys.Date(), disease_run_name, i, sep = "-"),
             StartDate = ymd(START_DATE),
             EndDate = ymd(END_DATE),
             Disease = disease_run_name) %>%   
      select(ClusterID, Disease, LATITUDE, LONGITUDE, RADIUS, StartDate, EndDate, P_VALUE, RECURR_INT, OBSERVED, EXPECTED, ODE)
    
    #Identifying case list of most likely cluster (current and past - filtered to just current involved in next step)
    all_involved <- session$gis %>% filter(CLUSTER == 1) %>% select(LOC_ID)
    
    #Identifying just current cases involved in cluster
    cases_involved <- events %>% 
      filter(State_Case_Number %in% all_involved$LOC_ID & Specimen_Date >= cluster_temp$StartDate & Specimen_Date <= cluster_temp$EndDate) %>% 
      select(State_Case_Number) 
    
    #Saving cases from involved tracts to cluster log
    cluster_temp$CaseList <- toString(cases_involved)
    
    #Adding cluster to history file
    write_csv(cluster_temp, 'Cluster History File.csv', append = TRUE)
    
    #Creating log for case history file
    cases_involved <- cases_involved %>% 
      mutate(ClusterID_FirstIdentified = paste(Sys.Date(), disease_run_name, i, sep = "-"),
             RunDate = Sys.Date()) %>% 
      select(RunDate, ClusterID_FirstIdentified, State_Case_Number)
    
    #Importing case history file to assist in determining if clusters are new or ongoing
    case_hist <- read_csv('Case History File.csv', col_types = "Dccc")
    
    #Determine if cluster is all new or new cases added to ongoing cluster
    if (any(cases_involved$State_Case_Number %in% case_hist$State_Case_Number) == FALSE) {
      
      #Add result to log file
      sink(file = 'satscan_function_log.txt', append = T)
      cat('Significant cluster detected for', disease_run_name, '- NEW \n')
      sink()
      
      #Add new cases to history file
      write_csv(cases_involved,'Case History File.csv', append = TRUE)
      
      #Select variables of interest for report
      line_list <- events %>% 
        filter(State_Case_Number %in% cases_involved$State_Case_Number) %>%
        mutate(Attends_Facility = ifelse(is.na(Attends_Facility), Other_Facility, Attends_Facility),
               Occupation = ifelse(is.na(Occupation) | Occupation == "Other", Other_Occupation, Occupation)) %>%
        select(State_Case_Number,
               Specimen_Date,
               Last:Deceased,
               Attends_Resides = Patient_Attends_Resides,
               Attends_Facility,
               Occupation,
               Employer,
               Epi_Comment,
               Reporter_Comment,
               City,
               Lat,
               Long)
      
      #Prepare line list
      line_list_table <- line_list %>%
        select(-Lat, -Long) %>%
        select_if(function(col) !(all(is.na(col)))) %>% #Will eliminate any columns that are empty for all cases, e.g. Serogroup
        arrange(Specimen_Date)
      
      #Prepare map
      cluster <- session$shapeclust[session$shapeclust$CLUSTER == i, ] 
      
      line_list_map <- SpatialPointsDataFrame(coords=line_list[, c("Long", "Lat")], line_list, 
                                              proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
      
      cluster_map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      
        addPolygons(data = cluster,
                     popup = sprintf("%s<br/>%s<br/>%s", 
                                     paste("Date Range:", cluster$START_DATE, "to", cluster$END_DATE), 
                                     paste("Observed Cases:", cluster$OBSERVED), 
                                     paste("Expected Cases:", round(cluster$EXPECTED, 2))) %>% lapply(htmltools::HTML),
                    group = "SatScan Cluster")  %>%
        
        addCircleMarkers(data = line_list_map,
                   fillColor = 	'black',
                   fillOpacity = .8,
                   stroke = FALSE,
                   radius = 5,
                   label = sprintf("%s<br/>%s", line_list_map$State_Case_Number, 
                                   paste(line_list_map$Age, " year old", line_list_map$Sex)) %>% lapply(htmltools::HTML),
                   group = "Cases in Cluster") %>%
        
        addLayersControl(overlayGroups = c("SatScan Cluster", "Cases in Cluster"))
      
      #Packaging data for final products
      report_data <- list(line_list_table, cluster_map, cluster_temp)
      
      #Save data --- IF THIS ENDS UP TAKING UP TOO MUCH SPACE, IT'S PROBABLY NOT NECESSARY SINCE THE MARKDOWN FILE WILL BE SAVED
      save(report_data, file = paste0("Clusters/Cluster ", cluster_temp$ClusterID, " Files.Rdata"))
      
      #Render report on cluster in R Markdown
      rmarkdown::render(input = "New Cluster Report.Rmd", 
                        output_file = paste0("Clusters/Cluster Report ID ", cluster_temp$ClusterID, ".html"),
                        params = list(data = report_data))
      
      #Email report to epidemiologist
      # send.mail(from = key_get("satscan_sender"),
      #           to = "",                       
      #           subject = paste("SECURELOCK: SatScan has detected a new cluster for", disease_run_name),
      #           body = "Please review the attached line list and determine if the cluster should be investigated further.",   
      #             smtp = list(host.name = key_get("email_host"), port = 25,
      #                         user.name = key_get("email_id"),
      #                         passwd = key_get("email_pw"), tls = TRUE),
      #           attach.files = paste0("Clusters/Cluster Report ID ", cluster_temp$ClusterID, ".html"),
      #           authenticate = TRUE,
      #           send = TRUE) 

      
    } else {      #new case(s) in ongoing cluster
      
      #Determine which cluster new cases are part of
      which_clust <- case_hist %>% 
        filter(case_hist$State_Case_Number %in% cases_involved$State_Case_Number) %>% 
        arrange(RunDate) %>%
        slice(1) %>%
        select(ClusterID_FirstIdentified) %>%
        as.character()
      
      #Determine which cases are new and assign ongoing cluster ID
      new_cases_involved <- cases_involved %>% 
        filter(!cases_involved$State_Case_Number %in% case_hist$State_Case_Number) %>%
        mutate(ClusterID_Assigned = which_clust)
      
      #Only generate output if new case was added to the cluster (as opposed to ongoing cluster, no new cases)
      if (nrow(new_cases_involved) >= 1) {
        
        #Add result to log file
        sink(file = 'satscan_function_log.txt', append = T)
        cat('Significant cluster detected for', disease_run_name, '- ONGOING - NEW CASE(S) \n')
        sink()
        
        #Add new cases to history file
        write_csv(new_cases_involved,'Case History File.csv', append = TRUE)
        
        #Prepare line list
        ongoing_case_list <- rbind(new_cases_involved, filter(case_hist, ClusterID_FirstIdentified == which_clust | ClusterID_Assigned == which_clust)) 
        
        line_list <- inner_join(events, ongoing_case_list) %>% 
          mutate(Attends_Facility = ifelse(is.na(Attends_Facility), Other_Facility, Attends_Facility),
                 Occupation = ifelse(is.na(Occupation) | Occupation == "Other", Other_Occupation, Occupation)) %>%
          select(Date_Added = RunDate,
                 State_Case_Number,
                 Specimen_Date,
                 Last:Deceased,
                 Attends_Resides = Patient_Attends_Resides,
                 Attends_Facility,
                 Occupation,
                 Employer,
                 Epi_Comment,
                 Reporter_Comment,
                 City,
                 Original_Cluster = ClusterID_FirstIdentified,
                 Lat,
                 Long) 
        
        line_list_table <- line_list %>%
          select(-Lat, -Long) %>%
          select_if(function(col) !(all(is.na(col)))) %>% #Will eliminate any columns that are empty for all cases, e.g. Serogroup
          arrange(Specimen_Date)
        
        #Prepare map
        cluster <- session$shapeclust[session$shapeclust$CLUSTER == i, ]   
        
        line_list_map <- SpatialPointsDataFrame(coords=line_list[, c("Long", "Lat")], line_list, 
                                                proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
        
        line_list_map@data <- line_list_map@data %>%   #Flagging cases added today
          mutate(new_case_flag = ifelse(line_list_map$State_Case_Number %in% new_cases_involved$State_Case_Number, 1, 0))
        
        cluster_map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
          
          addPolygons(data = cluster,
                      popup = sprintf("%s<br/>%s<br/>%s", 
                                      paste("Date Range:", cluster$START_DATE, "to", cluster$END_DATE), 
                                      paste("Observed Cases:", cluster$OBSERVED), 
                                      paste("Expected Cases:", round(cluster$EXPECTED, 2))) %>% lapply(htmltools::HTML),
                      group = "New Area of Cluster")  %>%
          
          addCircleMarkers(data = line_list_map,
                           fillColor = 	~ifelse(line_list_map$new_case_flag == 1, "red", "black"),
                           fillOpacity = .8,
                           stroke = FALSE,
                           radius = 5,
                           label = sprintf("%s<br/>%s", line_list_map$State_Case_Number, 
                                           paste(line_list_map$Age, " year old", line_list_map$Sex, ", Specimen Date:", format(line_list_map$Specimen_Date, "%m/%d/%y"))) 
                           %>% lapply(htmltools::HTML),
                           group = "Cases in Cluster") %>%
          
          addLayersControl(overlayGroups = c("New Area of Cluster", "Cases in Cluster"))
        
        #Packaging data for final products
        report_data <- list(which_clust, line_list_table, cluster_map, cluster_temp, new_cases_involved[ , 3])
        
        #Saving data (currently replacing original file, might need to change later if all records wanted)
        save(report_data, file = paste0("Clusters/Cluster ", which_clust, " Files.Rdata"))
        
        #Render report on cluster in R Markdown
        rmarkdown::render(input = "Ongoing Cluster Report.Rmd", 
                          output_file = paste0("Clusters/Cluster Report ID ", which_clust, "_Updated.html"),
                          params = list(data = report_data))

        #Email report to epidemiologist
        # send.mail(from = key_get("satscan_sender"),
        #           to = "",                       
        #           subject = paste("SECURELOCK: SatScan has detected new cases in an ongoing", disease_run_name, "cluster"),
        #           body = paste0("New cases have been detected for cluster ", which_clust, ". Please review the updated line list and map attached."),   
        #             smtp = list(host.name = key_get("email_host"), port = 25,
        #                         user.name = key_get("email_id"),
        #                         passwd = key_get("email_pw"), tls = TRUE),
        #           attach.files = paste0("Clusters/Cluster Report ID ", which_clust, "_Updated.html"),
        #           authenticate = TRUE,
        #           send = TRUE) 
        
      } else {
        
        #Add result to log file
        sink(file = 'satscan_function_log.txt', append = T)
        cat('Significant cluster detected for', disease_run_name, '- ONGOING \n')
        sink()
        
        
      } #ongoing cluster, new cases if/else closure
      

      
    } #new or ongoing cluster if/else closure
    
    
    
  } #significant cluster for loop closure
  
  
  
} #is cluster significant if/else closure







#=================NOTIFYING PROGRAMMER OF JOB RUN RESULTS================#

# #Notify programmer via email of the scan results
# if(as.Date(file.mtime("Cluster History File.csv")) == Sys.Date()){
#   
#   send.mail(from = key_get("satscan_sender"),
#             to = "",
#             subject = "Daily SatScan Report - Exact Locations",
#             body = paste("Results of the run for", Sys.Date(), "are attached."),   
#             smtp = list(host.name = key_get("email_host"), port = 25,
#                         user.name = key_get("email_id"),
#                         passwd = key_get("email_pw"), tls = TRUE),
#             attach.files = c("satscan_function_log.txt", "try_error_log.txt"),
#             authenticate = TRUE,
#             send = TRUE) 
#   
# } else{ 
#   
#   send.mail(from = key_get("satscan_sender"),
#             to = "",
#             subject = "ATTENTION: Daily SatScan Cluster Detection Failed - Exact Locations",
#             body = "Please check the script and troubleshoot.",
#             smtp = list(host.name = key_get("email_host"), port = 25,
#                         user.name = key_get("email_id"),
#                         passwd = key_get("email_pw"), tls = TRUE),
#             authenticate = TRUE,
#             send = TRUE)
#   
# }

#Print results of run to console(if email not being used, else can disable)
readLines('satscan_function_log.txt')
#cluster_temp$RECURR_INT   #check recurrence interval if cluster detected

#Delete log file after emailed
unlink('satscan_function_log.txt')
unlink('try_error_log.txt')

#Delete SatScan Download
unlink(satscanLineList)



