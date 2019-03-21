#PRECINCT VOTING VIZUALIZATION
#Merge frames
chi_prec_merge <- merge(chi_prec, precinct_data, by = c("precinct","ward"))
View(chi_prec_merge@data)

#Turnout %
tm_shape(chi_prec_merge) +
  tm_fill("Turnout") +
  tm_borders()

#Ballots Cast
tm_shape(chi_prec_merge) +
  tm_fill("Ballots.Cast") +
  tm_borders()

#Preckwinkle
tm_shape(chi_prec_merge) +
  tm_fill("TONI.PRECKWINKLE") +
  tm_borders()

#Lightfoot
tm_shape(chi_prec_merge) +
  tm_fill("LORI.LIGHTFOOT") +
  tm_borders()

#DR. WILLIE
tm_shape(chi_prec_merge) +
  tm_fill("WILLIE.L..WILSON") +
  tm_borders()

#Daley
tm_shape(chi_prec_merge) +
  tm_fill("WILLIAM.M..DALEY") +
  tm_borders()

#Mendoza
tm_shape(chi_prec_merge) +
  tm_fill("SUSANA.A..MENDOZA") +
  tm_borders()

#Enyia
tm_shape(chi_prec_merge) +
  tm_fill("AMARA.ENYIA") +
  tm_borders()

#ISOLATING WARD RESULTS
#Ward numbering
tm_shape(chi_prec_merge) +
  tm_borders(col = "grey75") +
  tm_shape(chi_wards_merge) +
  tm_text("ward") +
  tm_borders() 

#COMPARING PRECKWINKLE vs LIGHTFOOT
#Overall
plot1 <- tm_shape(prec_cand_merge) +
  tm_fill("Lightfoot over Preckwinkle", palette = "RdYlBu") +
  tm_borders()

plot1

plot2 <- tm_shape(chi_prec_merge) +
  tm_fill("Turnout", palette = "RdYlBu") +
  tm_borders()

#Plotting Cand vs Turnout
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot1, vp=viewport(layout.pos.col = 1))
print(plot2, vp=viewport(layout.pos.col = 2))

tmap_arrange(plot1, plot2)

jpeg("lightfoot_vs_preckwinkle.jpeg", 800, 700)
tmap_arrange(plot1, plot2)
dev.off()

#Precinct Winners
colnames(precinct_data)[precinct_data[1,] == max(precinct_data[1, 6:19])]

precinct_data_win <- precinct_data

for(i in 1:nrow(precinct_data)){
  precinct_data_win$winner[i] <- colnames(precinct_data[, 6:19])[precinct_data[i, 6:19] == max(precinct_data[i, 6:19])]
}

precinct_data_win$run_off <- ifelse(precinct_data_win$winner == "LORI.LIGHTFOOT", "LORI.LIGHTFOOT", "OTHER")
precinct_data_win$run_off[precinct_data_win$winner == "TONI.PRECKWINKLE"] <- "TONI.PRECKWINKLE"

precinct_data_win_merge <- merge(chi_prec, precinct_data_win, by = c("ward", "precinct"))
table(precinct_data_win$winner)
table(precinct_data_win$run_off)

winner <- tm_shape(precinct_data_win_merge) +
  tm_fill("winner") +
  tm_borders()

runoff <- tm_shape(precinct_data_win_merge) +
  tm_fill("run_off") +
  tm_borders()

tmap_arrange(winner, runoff)
