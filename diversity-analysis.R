# -------------------------------
# OTU diversity metric analysis
# Stephen Wood
# -------------------------------


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


