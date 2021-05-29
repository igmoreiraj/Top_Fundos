library(fs)

folders<-c(
  "00_Scripts",
  "00_Data",
  "01_Functions",
  "02_Visualizations",
  "03_Reports",
  "04_Models"
)


fs::dir_create(folders)

