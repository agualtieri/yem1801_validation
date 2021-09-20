rm(list = ls())

# library
library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(cluster)

# souce
source("./R/minimum_standards.R")
source("./R/data_falsification.R")
source("./R/check_log.R")


# load
data <- "./inputs/42.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August 2021.xlsx"

sheets <- openxlsx::getSheetNames(data)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
names(SheetList) <- sheets

clean <- SheetList[["Cleaned Data"]]
clean <- clean %>% mutate(index = row_number())
clean <- dplyr::rename(clean, uuid = X_uuid)

log <- SheetList[["Cleaning Log - Fixed"]]


tool <- read.xlsx("./inputs/REACH_JMMI_Tool_April_2021.xlsx")
#raw <- read.csv("./inputs/REACH_JMMI_Tool_April_2021_-_all_versions_-_False_-_2021-04-22-08-37-57.csv", stringsAsFactors = FALSE)


#enum.false <- calculateEnumeratorSimilarity(raw, tool, "select_one_organization_name_All", "district_ID_All")
enum.false2 <- calculateDifferences(clean, tool)
write.xlsx(enum.false2, paste0("./outputs/yemen_jmm_similar surveys_",lubridate::today(),".xlsx"))

# min standards
vec <- names(select(clean, starts_with("calc_price")))
min.stand <- minium_standards(clean, "district_name", vec) %>% filter(minimum_standards == "requirement not met")

clean.m <- clean %>% select(district_name) %>% melt()

min.dist <- clean.m %>% group_by(district_name) %>% summarise(., n=n()) %>% mutate(minimum_standards = ifelse(n >= 3, "requirement met", "requirement not met"))
write.xlsx(min.dist, paste0("./outputs/yemen_jmm_min standards_",lubridate::today(),".xlsx"))


# cleaning log
clean.i <- inspect_all(clean) %>% filter(!is.na(index)) %>% left_join(select(clean, "uuid", "district_name", "index"), by = "index")

clean.i <- semi_join(clean.i, log, c("uuid"= "ids"))

write.csv(clean.i, paste("./outputs/yemen_outliers_",lubridate::today(),".csv"))

# check log
log$old_Var <- 1
log <- log %>%  filter(variables %in% names(clean))
  
log.c <- check_log(clean, log, variable = "variables", 
                   old_log_var = "old_Var", new_log_var = "new_values", 
                   uuid_data = "uuid", uuid_log = "ids")


