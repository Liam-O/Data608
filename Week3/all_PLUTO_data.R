#Liam Byrne
# Data 608 - Week 3

BK <- read.csv("https://github.com/Liam-O/Data608/blob/master/Week3/BORO_zip_files_csv/BK.csv?raw=true")
BX <- read.csv("https://github.com/Liam-O/Data608/blob/master/Week3/BORO_zip_files_csv/BX.csv?raw=true")
MN <- read.csv("https://github.com/Liam-O/Data608/blob/master/Week3/BORO_zip_files_csv/MN.csv?raw=true")
QN <- read.csv("https://github.com/Liam-O/Data608/blob/master/Week3/BORO_zip_files_csv/QN.csv?raw=true")
SI <- read.csv("https://github.com/Liam-O/Data608/blob/master/Week3/BORO_zip_files_csv/SI.csv?raw=true")

NYC <- merge(BK, BX, all =TRUE)
NYC <- merge(NYC, MN, all =TRUE)
NYC <- merge(NYC, QN, all =TRUE)
NYC <- merge(NYC, SI, all =TRUE)

write.csv(NYC, file="NYC_pluto_all.csv")