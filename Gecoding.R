library(stringr)
library(readxl)

New_construction_clean <- read_csv("Documents/Polimi/Non Parametric/New_construction_clean.csv")

New_construction_clean$street_name <- str_replace_all(New_construction_clean$street_name, c("01"="1", "02"="2", "03"="3","04"="4","05"="5","06"="6","07"="7","08"="8","09"="9"))
New_construction_clean$address <- paste(New_construction_clean$street_number, New_construction_clean$street_number_suffix, New_construction_clean$street_name, New_construction_clean$street_suffix, ", San Francisco, CA, USA")
New_construction_clean$address <- str_replace(New_construction_clean$address, " NA ", " ")[1:4226]
New_construction_clean$address <- str_replace(New_construction_clean$address, " BL ", " BLVD ")
New_construction_clean$address <- str_replace(New_construction_clean$address, " TR ", " TERR ")
New_construction_clean$address <- str_replace(New_construction_clean$address, " PZ ", " Plaza ")

#my_df = New_construction_clean$address

#write.csv(my_df[1:500,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses1.csv", row.names = FALSE)
#write.csv(my_df[501:1000,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses2.csv", row.names = FALSE)
#write.csv(my_df[1001:1500,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses3.csv", row.names = FALSE)
#write.csv(my_df[1501:2000,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses4.csv", row.names = FALSE)
#write.csv(my_df[2001:2500,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses5.csv", row.names = FALSE)
#write.csv(my_df[2501:3000,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses6.csv", row.names = FALSE)
#write.csv(my_df[3001:3500,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses7.csv", row.names = FALSE)
#write.csv(my_df[3501:4000,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses8.csv", row.names = FALSE)
#write.csv(my_df[4001:4226,],"/Users/saratonazzi/Documents/Polimi/Non Parametric/addresses9.csv", row.names = FALSE)

Geocoded <- read_excel("Documents/Polimi/Non Parametric/Geocoded.xlsx")
#Geocoded <-  read_excel("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Geocoded.xlsx")

Geocoded <- Geocoded[!duplicated(Geocoded),]

New_construction_clean <- merge(New_construction_clean,subset(Geocoded, select=c("original_address", "lat", "lon")), by.x="address", by.y="original_address")
