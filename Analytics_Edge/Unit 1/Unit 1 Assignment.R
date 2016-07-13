str(mvt)

max(mvt$ID)

min(mvt$Beat)

summary(mvt$Arrest)

# How many observations have a LocationDescription value of ALLEY?
Alley<- subset(mvt, LocationDescription == "ALLEY")
nrow(Alley)
HighSodium$Description

#Prob 2.1 Understanding dates in R

header(mtv)
