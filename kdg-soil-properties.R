# ----------------------------------
# Analysis of soil properties for intensive vs. extensive culivation
# Kedougou, Senegal
# By: Stephen Wood
# -----------------------------------

## Read data
sn_borlaug_soil_data <- read.csv("/Volumes/My Passport for Mac/Data/sn_borlaug_soil_data.csv",header=T)


## Plots of relevant variables
# Make function for ANOVA, Tukey test, and boxplot
land_use_analysis <- function(variable,label,data=sn_borlaug_soil_data){
  boxplot(variable ~ land_use,data=data,ylab=label)
  summary(aov(variable ~ land_use,data=data))
  TukeyHSD(aov(variable ~ land_use,data=data))
}

# Physical and chemical properties
# pH
land_use_analysis(sn_borlaug_soil_data$pH.H2O,"pH (H2O)")
# Cation exchange capacity
land_use_analysis(sn_borlaug_soil_data$CEC,"Cation exchange capacity")

# Nitrogen
# Percent N
land_use_analysis(sn_borlaug_soil_data$X.N,"Total Nitrogen (%)")
# NO3
land_use_analysis(sn_borlaug_soil_data$N.NO3.,"N(NO3)")
# NH4
land_use_analysis(sn_borlaug_soil_data$N.NH4.,"N(NH4)")
# Percent N, POM
land_use_analysis(sn_borlaug_soil_data$POM_percN,"Particulate N (%)")
# Percent N, MAOM
land_use_analysis(sn_borlaug_soil_data$MAOM_percN,"Mineral-associated N (%)")

# Carbon
# Percent C
land_use_analysis(sn_borlaug_soil_data$X.C,"Total Carbon (%)")
# Organic C
land_use_analysis(sn_borlaug_soil_data$organic_C,"Organic C")
# Percent C, POM
land_use_analysis(sn_borlaug_soil_data$POM_percC,"Particulate C (%)")
# Percent C, MAOM
land_use_analysis(sn_borlaug_soil_data$MAOM_percC,"Mineral-associated C (%)")

# Phosphorus
# Total P
land_use_analysis(sn_borlaug_soil_data$total_P,"Total P")
# Assimilable P
land_use_analysis(sn_borlaug_soil_data$Assim_P,"Assimilable P")

# Calcium
land_use_analysis(sn_borlaug_soil_data$X.Ca,"Calcium")



<<<<<<< HEAD
# # Analyses tried, but code to exclude
=======

>>>>>>> 7933cae985acc3a9ef5a401ff0efd4f9ee361dbd
# # ------------------------------------------------------------------------------------
# # No interesting differences
# 
# # Small minerals
# small_minerals <- sn_borlaug_soil_data$X._clay + sn_borlaug_soil_data$X._silt
# land_use_analysis(small_minerals,"Clay + Silt (%)")
# # Salinity
# land_use_analysis(sn_borlaug_soil_data$Salinity,"Salinity")


