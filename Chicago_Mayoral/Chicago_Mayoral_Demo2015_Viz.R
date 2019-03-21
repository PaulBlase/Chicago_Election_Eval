#Add demographics
demo <- read.csv("demo_est.csv")
demo <- demo[, c(1, 8:13)]
str(demo)

demo[, 3] <- demo[, 3] * 100 / demo[, 2]
demo[, 4] <- demo[, 4] * 100 / demo[, 2]
demo[, 5] <- demo[, 5] * 100 / demo[, 2]
demo[, 6] <- demo[, 6] * 100 / demo[, 2]
demo[, 7] <- demo[, 7] * 100 / demo[, 2]

colnames(demo)[1] <- "ward"

ward_data <- merge(ward_data, demo, by = "ward")

#2015 Results
initial_15 <- read.csv("Voting_data_2015/init_2015_mayoral_ward.csv")
colnames(initial_15) <- c("ward", "Votes_init", "Registered_init", "Emanuel_init", 
                          "Emanuel_init_Perc", "Wilson_init", "Wilson_init_Perc", "Fioretti_init", 
                          "Fioretti_init_Perc", "Garcia_init", "Garcia_init_Perc", "Walls_init", 
                          "Walls_init_Perc")
str(initial_15)
initial_15$Fioretti_init <- as.numeric(initial_15$Fioretti_init)


runoff_15 <- read.csv("Voting_data_2015/runoff_2015_mayoral_ward.csv")
colnames(runoff_15) <- c("ward", "Registered_RO", "Votes_RO", "Emanuel_RO", "Emanuel_RO_Perc", 
                         "Garcia_RO", "Garcia_RO_Perc")
str(runoff_15)

#Structuring 2015 results into data
ward_data <- merge(ward_data, initial_15, by = "ward")
ward_data <- merge(ward_data, runoff_15, by = "ward")
str(ward_data)
ward_data <- ward_data[, -c(28, 30, 32, 34, 36, 40, 42)]

#Visualizations
chi_wards_merge <- merge(chi_wards, ward_data, by = "ward")
tmaptools::palette_explorer()

#Turnout
break_turnout <- c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000)

#2019 Init
init_2019 <- tm_shape(chi_wards_merge) +
  tm_fill("Ballots.Cast",
          breaks = break_turnout, palette = "YlGnBu", n = 8) +
  tm_borders() + 
  tm_legend(show=FALSE) +
  tm_layout("Feb. 2019", 
            title.size = 1.25,
            title.position = c("right", "bottom"))

#2015 RO
RO_2015 <- tm_shape(chi_wards_merge) +
  tm_fill("Votes_RO",
          breaks = break_turnout, palette = "YlGnBu", n = 8) +
  tm_borders() + 
  tm_legend(show=FALSE) +
  tm_layout("Apr. 2015", 
            title.size = 1.25,
            title.position = c("right", "bottom"))
#2015 Init
init_2015 <- tm_shape(chi_wards_merge) +
  tm_fill("Votes_init", title = "Voter Turnout",
          breaks = break_turnout, palette = "YlGnBu", n = 8) +
  tm_borders() +
  tm_layout("Feb. 2015", 
            title.size = 1.25,
            title.position = c("right", "bottom"),
            legend.title.size = 1.5,
            legend.text.size = 0.6)

tmap_arrange(init_2015, RO_2015, init_2019)

jpeg("Turnout.jpeg", 896, 500)
tmap_arrange(init_2015, RO_2015, init_2019)
dev.off()

#Demographics
breaks_demo <- c(0, 20, 40, 60, 80, 100)

#White
pop_wh <- tm_shape(chi_wards_merge) +
  tm_fill("White2016", title = "% of Population", 
          breaks = breaks_demo, palette = "YlGnBu") +
  tm_borders() +
  tm_layout("White", 
            title.size = 1.25,
            title.position = c("right", "top"),
            legend.title.size = 1.5,
            legend.text.size = 0.8)

#Black
pop_bl <- tm_shape(chi_wards_merge) +
  tm_fill("Black2016", title = "% of Population", 
          breaks = breaks_demo, palette = "YlGnBu") +
  tm_borders() +
  tm_legend(show=FALSE) +
  tm_layout("Black", 
            title.size = 1.25,
            title.position = c("right", "top"),
            legend.title.size = 1.5,
            legend.text.size = 0.8)

#Hispanic
pop_hs <- tm_shape(chi_wards_merge) +
  tm_fill("Latino2016", title = "% of Population", 
          breaks = breaks_demo, palette = "YlGnBu") +
  tm_borders() +
  tm_legend(show=FALSE) +
  tm_layout("Latino", 
            title.size = 1.25,
            title.position = c("right", "top"),
            legend.title.size = 1.5,
            legend.text.size = 0.8)

tmap_arrange(pop_wh, pop_bl, pop_hs)

jpeg("Demographics.jpeg", 896, 500)
tmap_arrange(pop_wh, pop_bl, pop_hs)
dev.off()
