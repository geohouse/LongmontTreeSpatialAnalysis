
library(ggplot2)
library(dplyr)

# read in the exported attribute table
inputTrees <- read.table("C:/Users/Geoffrey House User/Documents/GitHub/LongmontTreeSpatialAnalysis/Longmont_trees_withAnnexationDate_speciesLabels_forAnalyze_wGreenwayBoolean_v2_forRGithub.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Convert 0/1 greenway boolean to string instead
inputTrees$inGreenwayBuffer <- ifelse(test = inputTrees$inGreenwayBuffer == 1, yes = "In greenway", no = "Not in greenway")

# Remove any trees not assigned a year bin (annexation date couldn't be looked up). Affects
# 259 of 20663 records (1%)
inputTrees_v2 <- inputTrees[which(!is.na(inputTrees$YearAnnexed_num)),]

# Change the blank entry for tree type (trees not in the bins considered here) to 'Other'
inputTrees_v2$majorTreeType[which(inputTrees_v2$majorTreeType=="")] <- "Other"

treesForSpeciesPlot <- inputTrees_v2 %>% group_by(majorTreeType) %>% summarise(treeCount = n()) %>% 
  arrange(desc(treeCount))

speciesPlot <- ggplot(data = treesForSpeciesPlot, mapping = aes(x =  reorder(majorTreeType,-treeCount), y = treeCount)) + 
  geom_col() + theme_bw() + xlab("Tree group") + ylab("Number of trees") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

speciesPlot


# import the annexation information in order to calculate the area of annexation
# for each of the time bins in the map:
# 1872-1920
# 1921-1950
# 1951-1960
# 1961-1970
# 1971-1980
# 1981-1990
# 1991-2000
# 2001-2010
# 2011-2021

inputAnnexation <- read.table("C:/Users/Geoffrey House User/Documents/GitHub/LongmontTreeSpatialAnalysis/Longmont_annexationInfo_forAnalyze_forRGithub.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

assignYearBin <- function(inputYearEntry){
  
  if(is.na(inputYearEntry)){
    return(NA)
  }
  
  if(inputYearEntry >=1872 && inputYearEntry <=1920){
    return("1872-1920")
  } else if(inputYearEntry >=1921 && inputYearEntry <=1950){
    return("1921-1950")
  } else if(inputYearEntry >=1951 && inputYearEntry <=1960){
    return("1951-1960")
  } else if(inputYearEntry >=1961 && inputYearEntry <=1970){
    return("1961-1970")
  } else if(inputYearEntry >=1971 && inputYearEntry <=1980){
    return("1971-1980")
  } else if(inputYearEntry >=1981 && inputYearEntry <=1990){
    return("1981-1990")
  } else if(inputYearEntry >=1991 && inputYearEntry <=2000){
    return("1991-2000")
  } else if(inputYearEntry >=2001 && inputYearEntry <=2010){
    return("2001-2010")
  } else if(inputYearEntry >=2011 && inputYearEntry <=2021){
    return("2011-2021")
  } 
  
}

inputAnnexation$yearBin <- sapply(X = inputAnnexation$YearAnnexed_num, FUN = assignYearBin)

areaByYearBin <- inputAnnexation %>% group_by(yearBin) %>% summarize(totalArea_sqft=sum(Shape_Area))
# Get the area annexed during each time period in acres
areaByYearBin$totalArea_acres <- areaByYearBin$totalArea_sqft / 43560

# These are now removed above.
# 259 trees couldn't have a year annexed assigned (some because they're out of Longmont city limits)
# sum(is.na(inputTrees$YearAnnexed_num))

# Also assign year bins to the tree data
inputTrees_v2$yearBin <- sapply(X = inputTrees_v2$YearAnnexed_num, FUN = assignYearBin)

#numTreesByYearBin <- inputTrees %>% group_by(yearBin) %>% summarize(numberTreesYearBin = n())

numTreesByYearBin_wGreenway <- inputTrees_v2 %>% group_by(yearBin, inGreenwayBuffer) %>% summarize(numberTreesYearBin = n())


#join the area and the tree info based on the year bin

combinedAnnexTreeCounts_yearBin <- dplyr::inner_join(areaByYearBin, numTreesByYearBin_wGreenway, by = "yearBin")

combinedAnnexTreeCounts_yearBin$numTreesPerAcre <- combinedAnnexTreeCounts_yearBin$numberTreesYearBin / combinedAnnexTreeCounts_yearBin$totalArea_acres

# Need to get the total number of trees for each year bin in each row. Because there
# are 2 rows per year bin (greenway vs. not), here it extracts the total per year bin
# from a tibble into a vector, then repeats each element in the vector twice into the 
# new variable. Makes calculating the percentage of trees in greenway vs. not for each
# year bin very easy.
combinedAnnexTreeCounts_yearBin$yearBinTotal <- rep(x = combinedAnnexTreeCounts_yearBin %>% group_by(yearBin) %>% summarize(total = sum(numberTreesYearBin)) %>% select(total) %>% unlist(use.names = FALSE), each = 2)

combinedAnnexTreeCounts_yearBin$percentageOfYearBinTotal <- (combinedAnnexTreeCounts_yearBin$numberTreesYearBin / combinedAnnexTreeCounts_yearBin$yearBinTotal) * 100

propGreenwayThroughTimeGraph <- ggplot(data = combinedAnnexTreeCounts_yearBin, mapping = aes(x = yearBin, y = percentageOfYearBinTotal, group = inGreenwayBuffer, color = inGreenwayBuffer)) + 
  geom_line(size=1) + geom_point(size=2) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Percentage of total") + xlab("Year range") + labs(color="") + 
  scale_color_manual(values = c("#4d9221", "#c51b7d"))

propGreenwayThroughTimeGraph

# NEED plotting for number of trees per acre
# Collapse the greenway info to just get the overall density of trees (#/acre) for
# each time bin
treeDensityWithTime <- combinedAnnexTreeCounts_yearBin %>% group_by(yearBin) %>% 
  summarize(totalTrees = sum(numberTreesYearBin)) %>% 
  left_join(areaByYearBin, by="yearBin") %>% 
  mutate(numTreesPerAcre = totalTrees/totalArea_acres)

treeDensityTimeGraph <- ggplot(data = treeDensityWithTime, mapping = aes(x = yearBin, y = numTreesPerAcre, group = 1)) + 
  geom_line(size=1) + geom_point(size=2) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Number of trees per acre") + xlab("Year range")

treeDensityTimeGraph

# Percentage of the total number of each species planted in each of the time bins
treesForSpeciesPercPlotWithTime <- inputTrees_v2 %>% group_by(majorTreeType, yearBin) %>% 
  summarize(numTypePerYearBin = n())

# Add the total number of trees per species through a join and re-name the count column
treesForSpeciesPercPlotWithTime_v2 <- dplyr::left_join(x = treesForSpeciesPercPlotWithTime, y = treesForSpeciesPlot, by = "majorTreeType") %>%
  rename(totalForSpecies = treeCount)

treesForSpeciesPercPlotWithTime_v2$percentSpeciesTotal <- (treesForSpeciesPercPlotWithTime_v2$numTypePerYearBin / treesForSpeciesPercPlotWithTime_v2$totalForSpecies) * 100

# stacked bar plot of changes through time (different bar segments) for each
# species (x categories)

speciesChangeTimeGraph <- ggplot(data = treesForSpeciesPercPlotWithTime_v2, mapping = aes(x = majorTreeType, y = percentSpeciesTotal, group = yearBin, fill = yearBin)) + 
  geom_col() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999")) +
  ylab("Percentage of each tree type") + xlab("Major tree types") + labs(fill = "Year range")

speciesChangeTimeGraph

# Calc percentage of each species in greenways
treesForSpeciesPercGreenways <- inputTrees_v2 %>% group_by(majorTreeType, inGreenwayBuffer) %>%
  summarize(numTrees = n())

treesForSpeciesPercGreenways_v2 <- dplyr::left_join(x = treesForSpeciesPercGreenways, y = treesForSpeciesPlot, by = "majorTreeType") %>%
  rename(totalForSpecies = treeCount)

treesForSpeciesPercGreenways_v2$percentSpeciesTotal <- (treesForSpeciesPercGreenways_v2$numTrees / treesForSpeciesPercGreenways_v2$totalForSpecies) * 100

speciesGreenwayGraph <- ggplot(data = treesForSpeciesPercGreenways_v2, mapping = aes(x = majorTreeType, y = percentSpeciesTotal, group = inGreenwayBuffer, fill = inGreenwayBuffer)) + 
  geom_col() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values = c("#4d9221", "#c51b7d")) + ylab("Percentage of each tree type") + 
  xlab("Major tree types") + labs(fill = "")
  

speciesGreenwayGraph


