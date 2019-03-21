#Uploading Mayoral Race data
data <- read_excel("Precinct_level_District_Canvass.xls", skip = 6, sheet = 1)
data1 <- read_excel("Precinct_level_District_Canvass.xls", skip = 6, sheet = 2)

#Changing colnames
a <- colnames(data)
a[1] <- "Ward_Precinct"
colnames(data) <- a

#Combining tables
data <- data[-c(2, 6, 7, 10)]
data1 <- data1[c(8, 9, 11, 12)]
data <- data.frame(cbind(data, data1))
rm(a, data1)

#Manipulating data
#Remove spacer rows
data <- data[!is.na(data$Ward_Precinct),]

#Manipulate colnames
a <- colnames(data[2:18])
colnames(data[2:18]) <- a

#Make cand. votes numeric; NA -> 0's
for(i in 1:17){
  data[a[[i]]] <- as.numeric(unlist(data[a[[i]]]))
  data[a[i]][is.na(data[a[i]])] <- 0
}

#Establish Precinct/Ward DFs
#PRECINCT
precinct_data <- data[data$Ward_Precinct %like% "Precinct",]

precinct_data <- cbind(data.frame(str_split_fixed(precinct_data$Ward_Precinct, " Precinct ", 2)), precinct_data[2:18])
precinct_data[,1] <- str_extract(precinct_data[,1], "\\d+")
colnames(precinct_data)[1:2] <- c("ward", "precinct")
precinct_data$ward <- as.numeric(precinct_data$ward)
precinct_data$precinct <- as.numeric(precinct_data$precinct)
colnames(precinct_data)[5] <- "Turnout"

#WARD
ward_data <- data[!(data$Ward_Precinct %like% "Precinct"),]
ward_data <- unique(ward_data)

ward_data$Ward_Precinct <- str_extract(ward_data$Ward_Precinct, "\\d+")
colnames(ward_data)[1] <- "ward"
ward_data$ward <- as.numeric(ward_data$ward)
colnames(ward_data)[4] <- "Turnout"

#Visualize data
#Download shapefiles of wards; precincts
#WARDS
chi_wards <- readOGR(dsn = "Wards_2015", layer = "geo_export_c2d7f343-16a2-405e-a3f7-7bf64cf4a3b8")
View(chi_wards@data)
summary(chi_wards)

chi_wardsLL <- spTransform(chi_wards, "+init=epsg:4326")
plot(chi_wardsLL)

#PRECINCTS
chi_prec <- readOGR(dsn = "Precincts_2015", layer = "geo_export_bd03998c-b2ac-45d8-9db7-3196d08ca1ac")
View(chi_prec@data)
summary(chi_prec)

chi_precLL <- spTransform(chi_prec, "+init=epsg:4326")
plot(chi_precLL)

plot(chi_wardsLL, border = 'grey') 
axis(1)
axis(2)

#Top Vote Getters
cand <- ward_data[51, 5:18]
cand <- data.frame(t(cand))
colnames(cand) <- "Votes"
cand$Candidates <- rownames(cand)
cand <- cand %>%
  arrange(desc(Votes))

grid.table(cand)

jpeg("cand_vote_totals.jpeg", 250, 350)
grid.table(cand)
dev.off()
