#Liam Byrne
# Data 608 - Week 3

# ***Working Diretory must have folder 'BORO_zip_files_csv' within***

BK <- read.csv("BORO_zip_files_csv\\BK.csv")
BX <- read.csv("BORO_zip_files_csv\\BX.csv")
MN <- read.csv("BORO_zip_files_csv\\MN.csv")
QN <- read.csv("BORO_zip_files_csv\\QN.csv")
SI <- read.csv("BORO_zip_files_csv\\SI.csv")

NYC <- merge(BK, BX, all =TRUE)
NYC <- merge(NYC, MN, all =TRUE)
NYC <- merge(NYC, QN, all =TRUE)
NYC <- merge(NYC, SI, all =TRUE)

save(NYC, file="NYC_all.Rda")