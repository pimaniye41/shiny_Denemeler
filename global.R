library(readxl)
dataset <- read_excel("~/R_files/r4ds_final-main/xutum_data.xlsx")
dataset[,c(4:11)] <- sapply(dataset[,c(4:11)], as.numeric)
dataset$Dates <- as.character(dataset$Dates)
