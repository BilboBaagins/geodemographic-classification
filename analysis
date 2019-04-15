##########################
#
#   ERNIE_2016_SAPS
#
##########################
library(rgdal)
library(GISTools)
library(leaflet)
library(leaflet.extras)
# Going to emulate ENRIE 2011 SAPS that Jan Rigby, Martin Charlton & Chris Brunsdon carried out, using their code for the 2011 study. 

# Join SAPS shapefile to the SAPS csv with all variables. 
saps_shp <- readOGR(dsn="K:\\Spatial\\01_Data\\01_Census_Data\\02_2016_Census\\02_Data", layer="Small_Areas__Generalised_20m__OSi_National_Boundaries", stringsAsFactors = FALSE)
saps_csv <- read.csv("K:\\Spatial\\01_Data\\01_Census_Data\\02_2016_Census\\02_Data\\SAPS2016_SA2017.csv", stringsAsFactors = FALSE, strip.white = TRUE)
# Join on "GUID"
saps <- merge(saps_shp, saps_csv, by = "GUID")
# Read in settlements shapefile and county shapefile
settle <- readOGR(dsn="K:\\Spatial\\01_Data\\01_Census_Data\\02_2016_Census\\02_Data", layer="Settlements_Generalised_20m__OSi_National_Boundaries", stringsAsFactors=FALSE)
county <- readOGR(dsn="K:\\Spatial\\01_Data\\01_Census_Data\\02_2016_Census\\02_Data", layer="Admin_Counties_Generalised_20m__OSi_National_Boundaries", stringsAsFactors=FALSE)

colnames(saps_csv)[1] <- "GEOGID"

# Create variables when SAPS data is introduced. 
CreateVariables <- function(CensusData,na.rm=TRUE) {
 attach(saps_csv)
  Age0_4    <- 100 * ( T1_1AGE0T + T1_1AGE1T + T1_1AGE2T + T1_1AGE3T + T1_1AGE4T ) /
                      T1_1AGETT
  Age5_14   <- 100 * ( T1_1AGE5T + T1_1AGE6T + T1_1AGE7T + T1_1AGE8T + T1_1AGE9T +
                         T1_1AGE10T + T1_1AGE11T + T1_1AGE12T + T1_1AGE13T + T1_1AGE14T) / 
                       T1_1AGETT
  Age25_44  <- 100 * ( T1_1AGE25_29T + T1_1AGE30_34T + T1_1AGE35_39T + T1_1AGE40_44T ) /
                       T1_1AGETT
  Age45_64  <- 100 *( T1_1AGE45_49T + T1_1AGE50_54T + T1_1AGE55_59T + T1_1AGE60_64T ) / T1_1AGETT
  Age65over <- 100 *( T1_1AGE65_69T + T1_1AGE70_74T + T1_1AGE75_79T + T1_1AGE80_84T + T1_1AGEGE_85T ) / T1_1AGETT

  EU_National           <- 100 * (T2_1UKN + T2_1PLN + T2_1LTN + T2_1EUN) / T2_1TN
  ROW_National          <- 100 * (T2_1RWN) / T2_1TN
  Born_outside_Ireland  <- 100 * (T2_1TBP - T2_1IEBP) / T2_1TBP

  Separated            <- 100 * (T1_2SEPT + T1_2DIVT) / T1_2T
  SinglePerson         <- 100 * (T5_2_1PP - T4_5RP  ) / T5_2_TP
  Pensioner            <- 100 * T4_5RP / T4_5TP
  LoneParent           <- 100 * (T4_3FOPFCT + T4_3FOPFCT) / T4_5TF
  DINK                 <- 100 * T4_5PFF / T4_5TF
  NonDependentKids     <- 100 * T4_4AGE_GE20F / T4_4TF

  RentPublic         <- 100 * T6_3_RLAH / T6_3_TH
  RentPrivate        <- 100 * T6_3_RPLH / T6_3_TH
  #Terraced           <-
  #Detached           <-
  Flats              <- 100 * T6_1_FA_H / T6_1_TH
  NoCenHeat          <- 100 * T6_5_NCH / T6_5_T
  RoomsHH            <- (T6_4_1RH + 2*T6_4_2RH + 3*T6_4_3RH + 4*T6_4_4RH + 5*T6_4_5RH + 6*T6_4_6RH + 7*T6_4_7RH + 8*T6_4_GE8RH) / T6_4_TH
  PeopleRoom         <- T1_1AGETT / (T6_4_1RH + 2*T6_4_2RH + 3*T6_4_3RH + 4*T6_4_4RH + 5*T6_4_5RH + 6*T6_4_6RH + 7*T6_4_7RH + 8*T6_4_GE8RH)
  SepticTank         <- 100 * T6_7_IST / T6_7_T

  HEQual              <-  100 * ((T10_4_ODNDT + T10_4_HDPQT + T10_4_PDT + T10_4_DT) / T10_4_TT) # educ to degree or higher
  Employed            <-  100 * T8_1_WT / T8_1_TT
  TwoCars             <-  100 * (T15_1_2C + T15_1_3C + T15_1_GE4C) / (T15_1_NC + T15_1_1C + T15_1_2C + T15_1_3C + T15_1_GE4C)
  JTWPublic           <-  100 * (T11_1_BUT + T11_1_TDLT) / T11_1_TT
  HomeWork            <-  100 * T9_2_PH / T9_2_PT
  LLTI                <-  100 * (T12_3_BT + T12_3_VBT) / T12_3_TT
  UnpaidCare          <-  100 * (T12_2_M + T12_2_F)/ (T1_1AGETT)

  Students           <- 100 * T8_1_ST / T8_1_TT
  Unemployed         <- 100 * T8_1_ULGUPJT / T8_1_TT
  # PartTime           <- 100 *
  EconInactFam       <- 100 * T8_1_LAHFT / T8_1_TT
  Agric              <- 100 * (T14_1_AFFM + T14_1_AFFF) / (T14_1_TM + T14_1_TF)
  Construction       <- 100 * (T14_1_BCM  + T14_1_BCF ) / (T14_1_TM + T14_1_TF)
  Manufacturing      <- 100 * (T14_1_MIM  + T14_1_MIF ) / (T14_1_TM + T14_1_TF)
  Commerce           <- 100 * (T14_1_CTM  + T14_1_CTF ) / (T14_1_TM + T14_1_TF)
  Transport          <- 100 * (T14_1_TCM  + T14_1_TCF ) / (T14_1_TM + T14_1_TF)
  Public             <- 100 * (T14_1_PAM  + T14_1_PAF ) / (T14_1_TM + T14_1_TF)
  Professional       <- 100 * (T14_1_PSM  + T14_1_PSF ) / (T14_1_TM + T14_1_TF)

  ### MISC
  Broadband          <- 100 * T15_3_B / (T15_3_B + T15_3_OTH)            # Internet connected HH with Broadband
  Internet           <- 100 * (T15_3_B + T15_3_OTH) / T15_3_T            # Households with Internet

detach(saps_csv)

### Bringing it all together

  Place <- data.frame(CensusData[,1],stringsAsFactors=FALSE)
  colnames(Place)[1] <- 'GEOGID'
  Demographic <- data.frame(Age0_4, Age5_14, Age25_44, Age45_64, Age65over, EU_National, ROW_National, Born_outside_Ireland)
  HouseholdComposition <- data.frame(Separated, SinglePerson, Pensioner, LoneParent, DINK, NonDependentKids)
  Housing <- data.frame(RentPublic, RentPrivate, Flats, NoCenHeat, RoomsHH, PeopleRoom, SepticTank)
  SocioEconomic <- data.frame(HEQual, Employed, TwoCars, JTWPublic, HomeWork, LLTI, UnpaidCare)
  Employment <- data.frame(Students, Unemployed, EconInactFam,Agric,Construction,Manufacturing,Commerce,Transport,Public,Professional)
  Misc  <- data.frame(Internet, Broadband)
    
  # DerivedData <- data.frame(Demographic,HouseholdComposition,Housing,SocioEconomic,Employment,Misc)
    
  DerivedData <- data.frame(Place,Demographic,HouseholdComposition,Housing,SocioEconomic,Employment,Misc)
  if (na.rm) DerivedData[which(is.na(DerivedData),arr.ind=T)] <- 0                            # there are a few NAs - not when I do it? - CB

  DerivedData
}




# Raw data
SAData <- saps_csv
# CreateVariables function transforms raw data. Compute the derived variables
SAVars_2 <- CreateVariables(SAData)

# The Area Classification Code
# This provides a working data-set. Now use k-means cluster analysis to derive the classification. 

# First off, we need to subject the variables to Principal Components Analysis (PCA) to reduce redundancy in the data and also - very importantly - to remove any correlations between variables. 
# Correlations between variables causes issues when running k-means clustering on the data as correlated data screws up Euclidean distance calculations between variables in attribute space. 

# PCA (Princial Components Analysis)
PCA <- princomp(SAVars[, -1], cor = T, scores = T)
PCA$sdev^2/sum(PCA$sdev^2)
#   Comp.1       Comp.2       Comp.3       Comp.4       Comp.5       Comp.6       Comp.7       Comp.8       Comp.9      Comp.10      Comp.11      Comp.12      Comp.13 
#   0.2434411516 0.1353789931 0.0851507223 0.0683584000 0.0406508217 0.0358303462 0.0313613158 0.0274321087 0.0246595288 0.0239387555 0.0218132097 0.0217413186 0.0201233149 
#   Comp.14      Comp.15      Comp.16      Comp.17      Comp.18      Comp.19      Comp.20      Comp.21      Comp.22      Comp.23      Comp.24      Comp.25      Comp.26 
#   0.0190922862 0.0176065668 0.0173098885 0.0163003675 0.0147535847 0.0136895297 0.0129356444 0.0125224825 0.0113936662 0.0103565271 0.0091733015 0.0090555977 0.0075025771 
#   Comp.27      Comp.28      Comp.29      Comp.30      Comp.31      Comp.32      Comp.33      Comp.34      Comp.35      Comp.36      Comp.37      Comp.38      Comp.39 
#   0.0072218716 0.0064550674 0.0059831230 0.0052395406 0.0043067307 0.0041053853 0.0029561296 0.0028136777 0.0025260607 0.0023654972 0.0017494455 0.0014781407 0.0009165733 
#   Comp.40 
#   0.0003107501
cumsum(PCA$sdev^2/sum(PCA$sdev^2))
#   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7    Comp.8    Comp.9   Comp.10   Comp.11   Comp.12   Comp.13   Comp.14   Comp.15   Comp.16   Comp.17   Comp.18 
#   0.2434412 0.3788201 0.4639709 0.5323293 0.5729801 0.6088104 0.6401718 0.6676039 0.6922634 0.7162021 0.7380154 0.7597567 0.7798800 0.7989723 0.8165788 0.8338887 0.8501891 0.8649427 
#   Comp.19   Comp.20   Comp.21   Comp.22   Comp.23   Comp.24   Comp.25   Comp.26   Comp.27   Comp.28   Comp.29   Comp.30   Comp.31   Comp.32   Comp.33   Comp.34   Comp.35   Comp.36 
#   0.8786322 0.8915679 0.9040903 0.9154840 0.9258405 0.9350138 0.9440694 0.9515720 0.9587939 0.9652489 0.9712321 0.9764716 0.9807783 0.9848837 0.9878399 0.9906535 0.9931796 0.9955451 
#   Comp.37   Comp.38   Comp.39   Comp.40 
#   0.9972945 0.9987727 0.9996892 1.0000000 




# Note that the first 15 components account for 81.7% of the variance in total. We will use these for a k-means cluster analysis. 
set.seed(290162)  # Reproducible outcome
library(lubridate)
then <- now()  # Time this - it takes a while
# Create a progress bar also to monitor where ae are. 
pb <- winProgressBar(title = "progress bar", min = 0, max = 100, width = 300)

smallest.clus <- wss <- rep(0, 100)

for (i in 1:100) {

	Sys.sleep(0.1)
	setWinProgressBar(pb, i, title=paste(round(i/100*100, 0), "% done"))

    clus <- kmeans(PCA$scores[, 1:15], i, iter.max = 100, nstart = 20)
    wss[i] <- clus$tot.withinss
    smallest.clus[i] <- min(clus$size)
}
close(pb)
# How long did the calculation take?
elapsed <- now() - then
elapsed


plot(1:100, wss[1:100], type = "h", main = "Cluster Scree Plot", xlab = "Number of Clusters", ylab = "Within Cluster Sum of Squares")


plot(1:100, smallest.clus[1:100], type = "h", main = "Smallest Cluster Plot", 
    xlab = "Number of Clusters", ylab = "Smallest Cluster Size")

# set seed to get the same answer each time!
set.seed(32767)
then <- now()
SAclus <- kmeans(PCA$scores[, 1:15], 17, iter.max = 100, nstart = 20)
elapsed <- now() - then
elapsed
SAclusters <- SAclus$cluster


# We need this for the 'ddply' function
library(plyr)
# Compute a data frame (one row per cluster) containing the means of each
# variable in that cluster
mean_by_cluster <- ddply(SAVars, .(SAclusters), numcolwise(mean))[, -1]
# Compute the column-wise means for *all* observations
mean_by_col <- apply(SAVars[, -1], 2, mean)
# Compute the column-wise *sample* sd's for *all* observations
sd_by_col <- apply(SAVars[, -1], 2, sd)
# Create the z-scores via the 'scale' function
z_scores <- scale(mean_by_cluster, center = mean_by_col, scale = sd_by_col)





library(RColorBrewer)
heatmap(t(z_scores_2),
        scale = 'none',
        col=brewer.pal(6,'BrBG'),
        breaks=c(-1e10,-2,-1,0,1,2,+1e10),
        xlab='Cluster Number',
        add.expr=abline(h=(0:40)+0.5,v=(0:17)+0.5,col='white'))



barplot(sort(table(SAclusters)), las = 3)






# Cluster the Small Areas using Kaufman and Rousseeuw's PAM (Partitioning Around Medoids) algorithm.
# A key difference between this and \( k \)-means is that the total absolute distances within clusters are minimised, rather than the sum of squared distances. The contribution to this sum from outliers is proportionally less, and so there is less tendency to form small clusters of outlying areas. However, it should be noted that a single run of this algorithm for every small area in Ireland takes notably longer than an equivalent run of \( k \)-means.

# set seed to get the same answer each time.
# Num clusters = 17
library(cluster)
set.seed(32767)
then2 <- now()
PAMclus <- pam(x = PCA$scores[, 1:15], k = 17, pamonce = 2)
elapsed2 <- now() - then2
PAMclusters <- PAMclus$clustering












# set seed to get the same answer each time.
library(cluster)
set.seed(32766)
then2 <- now()
PAMclus_2 <- pam(x = PCA$scores[, 1:15], k = 18, pamonce = 2)
elapsed2 <- now() - then2
PAMclusters_2 <- PAMclus_2$clustering

# set seed to get the same answer each time.
library(cluster)
set.seed(32766)
then2 <- now()
PAMclus_3 <- pam(x = PCA$scores[, 1:15], k = 2, pamonce = 2)
elapsed2 <- now() - then2
PAMclusters_3 <- PAMclus_3$clustering

# Repeat z-score analysis with pam-based clusters

# Compute a data frame (one row per cluster) containing the means of each
# variable in that cluster
mean_by_cluster2 <- ddply(SAVars, .(PAMclusters), numcolwise(mean))[, -1]

# Create the z-scores via the 'scale' function
z_scores2 <- scale(mean_by_cluster2, center = mean_by_col, scale = sd_by_col)

heatmap(t(z_scores2),
        scale = 'none',
        col=brewer.pal(7,'BrBG'),
        breaks=c(-1e10,-2,-1,-0.5,0.5,1,2,+1e10),
        xlab='Cluster Number',
        add.expr=abline(h=(0:40)+0.5,v=(0:22)+0.5,col='white'))

barplot(sort(table(PAMclusters)), las = 3)




library(MASS)
mds.locs2 <- sammon(dist(z_scores2))
plot(mds.locs2$points, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
    asp = 1)
text(mds.locs2$points, label = paste("Group", 1:22))

