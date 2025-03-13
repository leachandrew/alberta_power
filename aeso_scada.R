source("power_paper_base.R")

library(rvest)
library(httr)
library(janitor)

files<-list.files("data/Hourly") 

zip_folder <- "data/Hourly"

# Get list of ZIP files
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)

# Folder for extracted files
extract_folder <- file.path(zip_folder, "unzipped")
dir.create(extract_folder, showWarnings = FALSE)

# Loop through each ZIP file
for (zip_file in zip_files)
  # Unzip files into the extraction folder
  unzip(zip_file, exdir = extract_folder)
  
  # Get a list of extracted files (csv)
  csv_files <- list.files(extract_folder, pattern = "\\.csv$", full.names = TRUE)
  data_store<-NULL
  # Read each Excel file
for (csv_file in csv_files) {
    data_store <- bind_rows(data_store,read_csv(csv_file)%>%clean_names()%>%rename("ID"="asset_short_name"))
    print(paste("Read:", csv_file))
  }

  get_plant_info <-function() {
    #load plant data
    plant_data <- read.xlsx(xlsxFile = "data/AB_Plant_Info_New.xlsx", sheet = "Plant_info", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
    colnames(plant_data)<-gsub("\\.", " ", colnames(plant_data)) 
    plant_info<-data.frame(t(plant_data[(1:10),-1]))
    #fix names
    plant_info<-setNames(plant_info,t(plant_data[(1:10),1]))
    plant_info$Capacity <- as.numeric(as.character(plant_info$Capacity))
    
    #Some of the names had errors or inconsistencies so fixed here
    
    plant_info<-arrange(plant_info,NRG_Stream)
    #put plant info in the Global Environment
    assign("plant_info", plant_info, envir = .GlobalEnv) #dump it to the global environment
  }
  
  get_plant_info()
  
  data_store<-data_store %>%left_join(plant_info)
  save(data_store,file="data/aeso_scada.rdata")