#Liam Byrne
# Data 608 - Week 3

BK <- read.csv("https://media.githubusercontent.com/media/Liam-O/Data608/master/Week3/BORO_zip_files_csv/BK.csv")
BX <- read.csv("https://media.githubusercontent.com/media/Liam-O/Data608/master/Week3/BORO_zip_files_csv/BX.csv")
MN <- read.csv("https://media.githubusercontent.com/media/Liam-O/Data608/master/Week3/BORO_zip_files_csv/MN.csv")
QN <- read.csv("https://media.githubusercontent.com/media/Liam-O/Data608/master/Week3/BORO_zip_files_csv/QN.csv")
SI <- read.csv("https://media.githubusercontent.com/media/Liam-O/Data608/master/Week3/BORO_zip_files_csv/SI.csv")

NYC <- merge(BK, BX, all =TRUE)
NYC <- merge(NYC, MN, all =TRUE)
NYC <- merge(NYC, QN, all =TRUE)
NYC <- merge(NYC, SI, all =TRUE)

save(NYC, file="NYC_pluto_all.csv")