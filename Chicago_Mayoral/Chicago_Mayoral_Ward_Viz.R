#WARD VOTING VIZUALIZATION
#Merge frames
chi_wards_merge <- merge(chi_wards, ward_data, by = "ward")
View(chi_wards_merge@data)

head(chi_wards_merge@data)

tm_shape(chi_wards_merge) +
  tm_text("ward") +
  tm_borders()

#Turnout %
tm_shape(chi_wards_merge) +
  tm_fill("Turnout") +
  tm_borders()

#Ballots Cast
tm_shape(chi_wards_merge) +
  tm_fill("Ballots.Cast") +
  tm_borders()

#Preckwinkle
preckwinkle_ward <- tm_shape(chi_wards_merge) +
  tm_fill("TONI.PRECKWINKLE") +
  tm_borders()

#Lightfoot
lightfoot_ward <- tm_shape(chi_wards_merge) +
  tm_fill("LORI.LIGHTFOOT") +
  tm_borders()

#DR. WILLIE
wilson_ward <- tm_shape(chi_wards_merge) +
  tm_fill("WILLIE.L..WILSON") +
  tm_borders()

#Daley
daley_ward <- tm_shape(chi_wards_merge) +
  tm_fill("WILLIAM.M..DALEY") +
  tm_borders()

#Mendoza
mendoza_ward <- tm_shape(chi_wards_merge) +
  tm_fill("SUSANA.A..MENDOZA") +
  tm_borders()

#Enyia
enyia_ward <- tm_shape(chi_wards_merge) +
  tm_fill("AMARA.ENYIA") +
  tm_borders()

#Plotting Cand vs Turnout
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,3)))
print(lightfoot_ward, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(preckwinkle_ward, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(daley_ward, vp=viewport(layout.pos.col = 3, layout.pos.row = 1))
print(wilson_ward, vp=viewport(layout.pos.col = 1, layout.pos.row = 2))
print(mendoza_ward, vp=viewport(layout.pos.col = 2, layout.pos.row = 2))
print(enyia_ward, vp=viewport(layout.pos.col = 3, layout.pos.row = 2))

tmap_arrange(lightfoot_ward, preckwinkle_ward, daley_ward, wilson_ward, mendoza_ward, enyia_ward)

jpeg("top_six_cand_ward.jpeg", 750, 600)
tmap_arrange(lightfoot_ward, preckwinkle_ward, daley_ward, wilson_ward, mendoza_ward, enyia_ward)
dev.off()
