##FIND NH CENTROID
SFNeighborhoods <- read_sf("Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
poly <- st_transform(SFNeighborhoods$geometry, 4326)
centroids <- st_centroid(poly, of_largest_polygon=FALSE)
# SFNeighborhoods$centroid <- as.character(SFNeighborhoods$centroid) #??
for(i in 1:37) {
  SFNeighborhoods[i,"lon"] <- centroids[[i]][1]
  SFNeighborhoods[i,"lat"] <- centroids[[i]][2]
}
st_write(SFNeighborhoods,"/Users/saratonazzi/Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")

##FIND NH AREA
SFNeighborhoods$area <- st_area(poly)
SFNeighborhoods$area <- as.numeric(substring(SFNeighborhoods$area,1, nchar(SFNeighborhoods$area)-6))
st_write(SFNeighborhoods,"/Users/saratonazzi/Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
#write.csv(SFNeighborhoods,"/Users/saratonazzi/Documents/Polimi/Non Parametric/SFNeighborhoods_new.csv", row.names = FALSE)

##NEIGHBORHOOD OF RENT
rent_clean <- read_csv("Documents/Polimi/Non Parametric/rent_clean_old.csv")
unique(rent_clean$nhood[order(rent_clean$nhood)])
B=rent_clean$nhood[order(rent_clean$nhood)]
length(unique(B))
unique(B)
D=Buyout_Agreements$Analysis.Neighborhood[order(Buyout_Agreements$Analysis.Neighborhood)]
unique(D)
setdiff(B,D)
setdiff(D,B)

rent_clean$nhood <- str_replace(rent_clean$nhood, "castro", "Castro/Upper Market")
rent_clean$nhood <- str_replace(rent_clean$nhood, "alamo square", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "bayview", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "hunters point", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "bernal", "Bernal Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "candlestick point", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "civic / van ness", "Tenderloin")
rent_clean$nhood <- str_replace(rent_clean$nhood, "haight ashbury", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "hayes valley", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "cole valley", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "glen park", "Glen Park/Noe Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "noe valley", "Glen Park/Noe Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "diamond heights", "Glen Park/Noe Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "excelsior / outer mission", "Excelsior/Outer Mission")
rent_clean$nhood <- str_replace(rent_clean$nhood, "lower pac hts", "Pacific Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "marina / cow hollow", "Marina")
rent_clean$nhood <- str_replace(rent_clean$nhood, "mission district", "Mission")
rent_clean$nhood <- str_replace(rent_clean$nhood, "north beach / telegraph hill", "North Beach")
rent_clean$nhood <- str_replace(rent_clean$nhood, "parkside", "Sunset/Parkside")
rent_clean$nhood <- str_replace(rent_clean$nhood, "outer sunset", "Sunset/Parkside")
rent_clean$nhood <- str_replace(rent_clean$nhood, "presidio hts / laurel hts / lake st", "Presidio Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "sea cliff", "Seacliff")
rent_clean$nhood <- str_replace(rent_clean$nhood, "lower haight", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "ingleside", "Oceanview/Merced/Ingleside")

rent_clean$nhood <- mapply(function(x) str_to_title(x), rent_clean$nhood)

rent_clean$nhood <- str_replace(rent_clean$nhood, "Usf / Anza Vista", "Lone Mountain/USF")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Nopa", "Lone Mountain/USF")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Financial District", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Downtown", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Soma / South Beach", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "West Portal / Forest Hills", "West of Twin Peaks")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Ccsf", "West of Twin Peaks")

write.csv(rent_clean,"/Users/saratonazzi/Documents/Polimi/Non Parametric/rent_clean_nh.csv", row.names = FALSE)

###!!!!Glen Park/Noe Valley!!!!
###!!!!Financial District/South Beach/SOMA!!!!
###!!!!Excelsior/Outer Mission!!!!
###!!!!Haight Ashbury/Hayes Valley!!!!

###FIX SFNEIGHBORHOOD

SFNeighborhoods <- read_sf("Documents/Polimi/Non Parametric/Analysis Neighborhoods.geojson")
SFNeighborhoods[nrow(SFNeighborhoods) + 1,] <- c("Glen Park/Noe Valley",st_union(SFNeighborhoods[SFNeighborhoods$nhood=='Glen Park',2],SFNeighborhoods[SFNeighborhoods$nhood=='Noe Valley',2]))
SFNeighborhoods[nrow(SFNeighborhoods) + 1,] <- c("Financial District/South Beach/SOMA",st_union(SFNeighborhoods[SFNeighborhoods$nhood=='Financial District/South Beach',2],SFNeighborhoods[SFNeighborhoods$nhood=='South of Market',2]))
SFNeighborhoods[nrow(SFNeighborhoods) + 1,] <- c("Excelsior/Outer Mission",st_union(SFNeighborhoods[SFNeighborhoods$nhood=='Excelsior',2],SFNeighborhoods[SFNeighborhoods$nhood=='Outer Mission',2]))
SFNeighborhoods[nrow(SFNeighborhoods) + 1,] <- c("Haight Ashbury/Hayes Valley",st_union(SFNeighborhoods[SFNeighborhoods$nhood=='Haight Ashbury',2],SFNeighborhoods[SFNeighborhoods$nhood=='Hayes Valley',2]))

SFNeighborhoods <- SFNeighborhoods %>%  filter(!row_number() %in% c(5,13,17,31,32,35,36,37))

###FIX NH OF BUYOUT

Buyout <- read_csv("Documents/Polimi/Non Parametric/Buyout_Agreements_Clean.csv")
A=Buyout$nhood[order(Buyout$nhood)]
unique(A)
Buyout$nhood <- str_replace(Buyout$nhood, "Glen Park", "Glen Park/Noe Valley")
Buyout$nhood <- str_replace(Buyout$nhood, "Noe Valley", "Glen Park/Noe Valley")
Buyout$nhood <- str_replace(Buyout$nhood, "Glen Park/Glen Park/Noe Valley", "Glen Park/Noe Valley")
Buyout$nhood <- str_replace(Buyout$nhood, "Financial District/South Beach", "Financial District/South Beach/SOMA")
Buyout$nhood <- str_replace(Buyout$nhood, "South of Market", "Financial District/South Beach/SOMA")
Buyout$nhood <- str_replace(Buyout$nhood, "Excelsior", "Excelsior/Outer Mission")
Buyout$nhood <- str_replace(Buyout$nhood, "Outer Mission", "Excelsior/Outer Mission")
Buyout$nhood <- str_replace(Buyout$nhood, "Excelsior/Excelsior/Outer Mission", "Excelsior/Outer Mission")
Buyout$nhood <- str_replace(Buyout$nhood, "Haight Ashbury", "Haight Ashbury/Hayes Valley")
Buyout$nhood <- str_replace(Buyout$nhood, "Hayes Valley", "Haight Ashbury/Hayes Valley")
Buyout$nhood <- str_replace(Buyout$nhood, "Haight Ashbury/Haight Ashbury/Hayes Valley", "Haight Ashbury/Hayes Valley")
write.csv(Buyout,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Buyout_Agreements_Clean_nh.csv", row.names = FALSE)

###FIX NH OF EVICTIONS

eviction_monthly <- read_csv("Documents/Polimi/Non Parametric/eviction_nhood_monthly.csv")
eviction_yearly <- read_csv("Documents/Polimi/Non Parametric/Eviction_Notices_Clean.csv")

A=eviction_monthly$nhood[order(eviction_monthly$nhood)]
B=eviction_yearly$nhood[order(eviction_yearly$nhood)]
unique(A)
unique(B)

eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Glen Park", "Glen Park/Noe Valley")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Noe Valley", "Glen Park/Noe Valley")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Glen Park/Glen Park/Noe Valley", "Glen Park/Noe Valley")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Financial District/South Beach", "Financial District/South Beach/SOMA")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "South of Market", "Financial District/South Beach/SOMA")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Excelsior", "Excelsior/Outer Mission")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Outer Mission", "Excelsior/Outer Mission")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Excelsior/Excelsior/Outer Mission", "Excelsior/Outer Mission")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Haight Ashbury", "Haight Ashbury/Hayes Valley")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Hayes Valley", "Haight Ashbury/Hayes Valley")
eviction_monthly$nhood <- str_replace(eviction_monthly$nhood, "Haight Ashbury/Haight Ashbury/Hayes Valley", "Haight Ashbury/Hayes Valley")

eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Glen Park", "Glen Park/Noe Valley")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Noe Valley", "Glen Park/Noe Valley")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Glen Park/Glen Park/Noe Valley", "Glen Park/Noe Valley")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Financial District/South Beach", "Financial District/South Beach/SOMA")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "South of Market", "Financial District/South Beach/SOMA")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Excelsior", "Excelsior/Outer Mission")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Outer Mission", "Excelsior/Outer Mission")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Excelsior/Excelsior/Outer Mission", "Excelsior/Outer Mission")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Haight Ashbury", "Haight Ashbury/Hayes Valley")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Hayes Valley", "Haight Ashbury/Hayes Valley")
eviction_yearly$nhood <- str_replace(eviction_yearly$nhood, "Haight Ashbury/Haight Ashbury/Hayes Valley", "Haight Ashbury/Hayes Valley")

write.csv(eviction_monthly,"/Users/saratonazzi/Documents/Polimi/Non Parametric/eviction_monthly_nh.csv", row.names = FALSE)
write.csv(eviction_yearly,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Eviction_Notices_Clean_nh.csv", row.names = FALSE)
