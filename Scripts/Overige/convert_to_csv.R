

# 

install.packages("readxl")
library(readxl)

source_dir <- "G:/Gedeelde drives/Team_Boseco_BR/PRJ_BR_Gegevensverwerking/Hoogtemodellen"
destination_dir <- "G:/Gedeelde drives/Team_Boseco_BR/PRJ_BR_Gegevensverwerking/Hoogtemodellen/csv"

excel_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)

for (file in excel_files) {
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read Excel file
  data <- read_excel(file)
  
  # Create path for CSV file in destination directory
  csv_file <- file.path(destination_dir, paste0(file_name, ".csv"))
  
  # Write data to CSV file
  write.csv2(data, csv_file, row.names = FALSE)
}