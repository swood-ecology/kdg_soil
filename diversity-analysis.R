# -------------------------------
# OTU diversity metric analysis
# Microbial biomass and enzymatic analysis
# Stephen Wood
# -------------------------------

library(vegan)    # For PCA and bi-plot, for enzyme data

setwd("/Volumes/My Passport for Mac/Data/MiSeq/Senegal_16S/Data_and_analysis/joined/joined_demultiplexed/alpha_collated")

# read in diversity data
chao <- read.table("chao1.txt",header=T,sep='\t')[,-1]
species <- read.table("observed_species.txt",header=T,sep='\t')[,-1]
pd <- read.table("PD_whole_tree.txt",header=T,sep='\t')[,-1]
shannon <- read.table("shannon.txt",header=T,sep='\t')[,-1]

# read in sample type data
mapping <- read.table("/Volumes/My Passport for Mac/Data/MiSeq/Senegal_16S/Data_and_analysis/EMPAfricamappingfile.txt",header=F,sep='\t')
mapping <- mapping[,c(1,6)]
names(mapping) <- c('Sample','LandUse')

# create function to subset to rarefied level, average across iterations, and merge with land use
rare.iter <- function(data,rare,metric,type){
  data.iter <- as.data.frame(colMeans(subset(data,sequences.per.sample == rare)[,-c(1:2)],na.rm=T))
  data.iter[,2] <- rownames(data.iter)
  names(data.iter) <- c(metric,'Sample')
  rownames(data.iter) <- seq(from=1, to=nrow(data.iter))
  return(merge(data.iter,type,by='Sample'))
}

# rarefy variables to 200
chao.format <- rare.iter(data=chao,rare=200,metric='Chao',type=mapping)
species.format <- rare.iter(data=species,rare=200,metric='ObservedRichness',type=mapping)
pd.format <- rare.iter(data=pd,rare=200,metric='PD',type=mapping)
shannon.format <- rare.iter(data=shannon,rare=200,metric='Shannon',type=mapping)

# analyze for diversity
c.plot <- boxplot(Chao~LandUse,data=chao.format,main="Chao")
r.plot <- boxplot(ObservedRichness~LandUse,data=species.format,main="Observed Richness")
pd.plot <- boxplot(PD~LandUse,data=pd.format,main = "Phylogenetic Diversity")
h.plot <- boxplot(Shannon~LandUse,data=shannon.format,main="Shannon")


# Analyze for microbial biomass and enzymatic potential
setwd("/Volumes/My Passport for Mac/Data/")
sn_borlaug_soil_data <- read.csv("sn_borlaug_soil_data.csv",header=T)

# Microbial biomass
boxplot(Microbial_biomass ~ land_use,data=sn_borlaug_soil_data,main="Microbial biomass")
summary(aov(Microbial_biomass ~ land_use,data=sn_borlaug_soil_data))
TukeyHSD(aov(Microbial_biomass ~ land_use,data=sn_borlaug_soil_data))

# Enzymatic potential
enzyme <- sn_borlaug_soil_data[,25:29]
rda.enzyme <- rda(enzyme ~ land_use,data=sn_borlaug_soil_data,scale=T)
ordiplot(rda.enzyme,main="Enzymatic potential")
points(rda.enzyme,"sites",pch=20,cex=0.5)
text(rda.enzyme,"bp",cex=.8)
text(rda.enzyme,"species",cex=.8,col="red")
