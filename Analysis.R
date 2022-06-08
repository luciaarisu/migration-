#Clear all
rm(list=ls()) 
library(readr, rio)
library(plyr)
library (xts)
library(data.table, zoo)
library(scales)
library(ggplot2, xtable)
library(sandwich, lmtest)
library(igraph)
library(ggthemes, stargazer)

Paths = c("/Users/luciarisuenonavarro/Desktop/Migration project/R Project - lucia /Analysis.R")
names(Paths) = c("ts", "?")
setwd(Paths[Sys.info()[7]])

{
  if (Sys.info()[7] == "ts") {
    #import raw data
    # dat1 <- fread("/Users/kyrapauly/Library/CloudStorage/OneDrive-Personal/Uni/EMI Year 2/Period 5/EBC2109 - Network Economics/Migration Flow Project/migr_imm3ctb__custom_2617862_page_linear.csv.gz")
    # dat2 <- fread("/Users/kyrapauly/Library/CloudStorage/OneDrive-Personal/Uni/EMI Year 2/Period 5/EBC2109 - Network Economics/Migration Flow Project/migr_imm5prv__custom_2706304_page_linear.csv.gz")
    dat1 <- fread("migr_imm3ctb__custom_2617862_page_linear.csv.gz")
    dat2 <- fread("migr_imm5prv__custom_2706304_page_linear.csv.gz")
    
    #drop unnecessary column
    dat1_clean <- dat1[, DATAFLOW := NULL][, `LAST UPDATE`:= NULL][, freq := NULL][, OBS_FLAG := NULL][, unit := NULL][, agedef := NULL]
    dat1_clean <- dat1_clean[, .(ORIG = c_birth, DEST = geo, Year = TIME_PERIOD, NUMBER = OBS_VALUE, AGE = age, SEX = sex)]
    
    dat2_clean <- dat2[, DATAFLOW := NULL][, `LAST UPDATE`:= NULL][, freq := NULL][, OBS_FLAG := NULL][, unit := NULL][, agedef := NULL]
    dat2_clean <- dat2_clean[, .(ORIG = partner, DEST = geo, Year = TIME_PERIOD, FLOW = OBS_VALUE, AGE = age, SEX = sex)]
    
    yearlist <- unique(dat1_clean$Year)
    agelist <- unique(dat1_clean$AGE)
    sexlist <- unique(dat1_clean$SEX)
    destlist <- unique(dat1_clean$DEST)
    origlist <- unique(dat1_clean$ORIG)
    countrylist <- c(destlist, origlist)
    
    dat1premerge <- dat1_clean[DEST%in%countrylist & ORIG%in%countrylist,][DEST != ORIG,][Year%in%yearlist,][AGE%in%agelist][SEX%in%sexlist]
    dat2premerge <- dat2_clean[DEST%in%countrylist & ORIG%in%countrylist,][DEST != ORIG,][Year%in%yearlist,][AGE%in%agelist][SEX%in%sexlist]
    nrow(dat1premerge)
    nrow(dat2premerge)
    
    datMerge <- merge(dat1premerge, dat2premerge, all = T)
    datMerge[is.na(datMerge)] <- 0
    datClean <- datMerge[, NET_FLOW := FLOW - NUMBER]
    #reshape tall
    datCat <- melt(datClean, measure.vars = ("NUMBER"), variable.factor = T, value.name = "Number")[, variable := NULL]
    datCat <- datCat[DEST == "EL", DEST := "GR"][ORIG == "EL", ORIG := "GR"][ORIG != "AFR_OTH"] #rename Greece from EL to GR

    #levels(datCat$SEX) <- c(levels(datCat$SEX), "M")    # add new level
    datCat <- datCat[SEX == "T", SEX := "TOTAL"][SEX == "F", SEX := "FEMALE"]
    datCat <- datCat[AGE == "Y15-19", AGE := "15-19"][AGE == "Y15-64", AGE := "15-64"][AGE == "Y20-24", AGE := "20-24"][AGE == "Y25-29", AGE := "25-29"]
    setkey(datCat, DEST)
    #coerce factors
    cols <- c("SEX", "AGE")
    datCat[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
    datCopy <- datCat
    
    #rm junk
    rm(dat1,dat1_clean, dat1premerge, dat2, dat2_clean, dat2premerge, agelist, 
       countrylist, destlist, sexlist, yearlist, origlist, datMerge)
    #save
    save(datCat, file = "Data.Rdata")
  }
  
}

createNetwork <- function(dt, year, gender, age) {
  df <- as.data.frame(dt[Year == year][SEX==gender][AGE==age])
  net <- graph_from_data_frame(df, directed = T)
  E(net)$weight <-  E(net)$Number #make it weighted
  E(net)$label <- E(net)$Number #set edge labels (for graphing only)
  V(net)$size <- degree(net, mode = "all", normalized = T) 
  return(net)
}

getNetworkSummary <- function(net) {
  #get attributes
  nodes <- vcount(net)
  edges <- ecount(net)
  edge.dens <- edge_density(net)
  NetTrans <- transitivity(net, type = "global")
  diam <- diameter(net)
  res <- list("nodes" = nodes, "edges" = edges, "edge density" = edge.dens,"transitivity" = NetTrans, "diamter" = diam )
  
  return(res)
}


getNetworkAttributes <- function(net) {
  namelist <- V(net)$name
  
  strength.all <- strength(net, mode = "all")
  strength.in <- strength(net, mode = "in")
  strength.out <- strength(net, mode = "out")
  
      old.weight <- E(net)$weight
      netclone <- net
      E(netclone)$weight <- old.weight + 0.01 #needs to be >0
  between.ness <- betweenness(netclone, normalized = T)
  close.ness <- closeness(netclone, normalized = T)
  
  eigen.values <- eigen(as_adj(net, sparse=FALSE))$vectors[,1]
  #eigen_values <- eigen_centrality(net)$vector
  names(eigen.values) <- namelist

  result <- list("names" = namelist, "total strength" = strength.all, 
                 "strength outgoing edges" = strength.out,
                 "strength incoming edges" = strength.in,
                 "eigenvalues" = eigen.values, 
                 "betweenness" = between.ness, 
                 "closeness" = close.ness )
  return(result)
  
}

getNetworkMeansEntire <- function(dt, year, gender, age) {
  net <- createNetwork(dt, year, gender, age)
  #get attributes
  nodes <- vcount(net)
  edges <- ecount(net)
  edge.dens <- edge_density(net)
  NetTrans <- transitivity(net, type = "global")
  diam <- diameter(net)
  res <- list("nodes" = nodes, "edges" = edges, "edge density" = edge.dens,"transitivity" = NetTrans, "diamter" = diam )
  
  return(res)
}

getNetworkMeansInd <- function(dt, year, gender, age) {
  net <- createNetwork(dt, year, gender, age)
  namelist <- V(net)$name
  
  strength.all <- mean(strength(net, mode = "all"))
  strength.in <- mean(strength(net, mode = "in"))
  strength.out <- mean(strength(net, mode = "out"))
  
  old.weight <- E(net)$weight
  netclone <- net
  E(netclone)$weight <- old.weight + 0.01 #needs to be >0
  between.ness <- mean(betweenness(netclone, normalized = T))
  close.ness <- mean(closeness(netclone, normalized = T))
  
  eigen.values <- mean(eigen(as_adj(net, sparse=FALSE))$vectors[,1])
  #eigen_values <- eigen_centrality(net)$vector
  
  
  result <- list("names" = namelist, "mS" = strength.all, 
                 "mSo" = strength.out,
                 "mSi" = strength.in,
                 "mEC" = eigen.values, 
                 "mBtw" = between.ness, 
                 "mCl" = close.ness )
  return(result)
}

getNetworkVector <- function(dt, year, gender, age) {
  net <- createNetwork(dt, year, gender, age)
  ev <- eigen(as_adj(net, sparse=FALSE))$vectors[,1]
  sv <- strength(net, mode = "all")
    old.weight <- E(net)$weight
    netclone <- net
    E(netclone)$weight <- old.weight + 0.01 #needs to be >0
  bv <- betweenness(netclone, normalized = T)
  cv <- closeness(netclone, normalized = T)
  
  result <- list("strength" = sv, 
                 "eigenvalues" = ev, 
                 "betweenness" = bv, 
                 "closeness" = cv )
  return(result)
}

{
  plotNetwork <- function(net) {
    return(plot(net, edge.width = 1, edge.lty = 1,  
                edge.curved = F, edge.arrow.size = 0.6, 
                edge.arrow.width = 0.4, vertex.frame.color="transparent",
                vertex.label.cex = 0.7)) #, rescale=FALSE))
  }
  #create subsets
  countryList <- c("IT", "DE", "GR", "FR", "SE")
  europe <- datCat[DEST%in%countryList]
 }

#Age Levels: 15-19 15-64 20-24 25-29 TOTAL
#create 6 networks: 2020, 2015, 2010, for each: male and female
#make one big network
full <- createNetwork(datCat, 2020, "TOTAL", "TOTAL")

europe20_T <- createNetwork(europe, 2020, "TOTAL", "TOTAL")
europe20_F <- createNetwork(europe, 2020, "FEMALE", "TOTAL")

europe15_T <- createNetwork(europe, 2015, "TOTAL", "TOTAL")
europe15_F <- createNetwork(europe, 2015, "FEMALE", "TOTAL")

europe10_T <- createNetwork(europe, 2010, "TOTAL", "TOTAL")
europe10_F <- createNetwork(europe, 2010, "FEMALE", "TOTAL")

netlist <- list(europe20_T, europe20_F, europe15_T, europe15_F, europe10_T, europe10_F)
#make summary stats tables
#getNetworkSummary(full)


outmat2 <- matrix(nrow = 5, ncol = 6)
rownames(outmat2) <- c("Number of Nodes", "Number of Edges", "Transitivty", "Edge Density", "Diameter")
colnames(outmat2) <- c("Total, 2020", "Female, 2020", "Total, 2015", "Female, 2015", "Total, 2010", "Female, 2010")

#fill matrix
{
  outmat2[1,1] <-  getNetworkSummary(europe20_T)$nodes
  outmat2[2,1] <-  getNetworkSummary(europe20_T)$edges
  outmat2[3,1] <-  getNetworkSummary(europe20_T)$transitivity
  outmat2[4,1] <-  getNetworkSummary(europe20_T)$`edge density`
  outmat2[5,1] <-  getNetworkSummary(europe20_T)$diamter
  
  outmat2[1,2] <-  getNetworkSummary(europe20_F)$nodes
  outmat2[2,2] <-  getNetworkSummary(europe20_F)$edges
  outmat2[3,2] <-  getNetworkSummary(europe20_F)$transitivity
  outmat2[4,2] <-  getNetworkSummary(europe20_F)$`edge density`
  outmat2[5,2] <-  getNetworkSummary(europe20_F)$diamter
  
  outmat2[1,3] <-  getNetworkSummary(europe15_T)$nodes
  outmat2[2,3] <-  getNetworkSummary(europe15_T)$edges
  outmat2[3,3] <-  getNetworkSummary(europe15_T)$transitivity
  outmat2[4,3] <-  getNetworkSummary(europe15_T)$`edge density`
  outmat2[5,3] <-  getNetworkSummary(europe15_T)$diamter
  
  outmat2[1,4] <-  getNetworkSummary(europe15_F)$nodes
  outmat2[2,4] <-  getNetworkSummary(europe15_F)$edges
  outmat2[3,4] <-  getNetworkSummary(europe15_F)$transitivity
  outmat2[4,4] <-  getNetworkSummary(europe15_F)$`edge density`
  outmat2[5,4] <-  getNetworkSummary(europe15_F)$diamter
  
  outmat2[1,5] <-  getNetworkSummary(europe10_T)$nodes
  outmat2[2,5] <-  getNetworkSummary(europe10_T)$edges
  outmat2[3,5] <-  getNetworkSummary(europe10_T)$transitivity
  outmat2[4,5] <-  getNetworkSummary(europe10_T)$`edge density`
  outmat2[5,5] <-  getNetworkSummary(europe10_T)$diamter
  
  outmat2[1,6] <-  getNetworkSummary(europe10_F)$nodes
  outmat2[2,6] <-  getNetworkSummary(europe10_F)$edges
  outmat2[3,6] <-  getNetworkSummary(europe10_F)$transitivity
  outmat2[4,6] <-  getNetworkSummary(europe10_F)$`edge density`
  outmat2[5,6] <-  getNetworkSummary(europe10_F)$diamter
}
  
#getNetworkAttributes(europe20_T)

countryList
outmatbig <- matrix(ncol = 6, nrow = 30)
#colnames(outmatbig) <- c("Total, 2020", "Female, 2020", "Total, 2015", "Female, 2015", "Total, 2010", "Female, 2010")
colnames(outmatbig) <- c("2020 (T)", "2020 (F)", "2015 (T)", "2015 (F)", "2010 (T)", "2010 (F)")
rownames(outmatbig) <- c("Italy", "IT: Eigenv. Centrality", "IT: Strength (incoming)", "IT: Strength (total)", "IT: Betweenness", "IT: Closeness",
                       "Greece", "GR: Eigenv. Centrality", "GR: Strength (incoming)", "GR: Strength (total)", "GR: Betweenness", "GR: Closeness",
                       "Germany", "DE: Eigenv. Centrality", "DE: Strength (incoming)", "DE: Strength (total)", "DE: Betweenness", "DE: Closeness",
                       "France", "FR: Eigenv. Centrality", "FR: Strength (incoming)", "FR: Strength (total)", "FR: Betweenness", "FR: Closeness",
                       "Sweden", "SE: Eigenv. Centrality", "SE: Strength (incoming)", "SE: Strength (total)", "SE: Betweenness", "SE: Closeness")
#fill matrix column by column
{
  outmatbig[2,1] <- getNetworkAttributes(europe20_T)$eigenvalues["IT"]
  outmatbig[3,1] <- getNetworkAttributes(europe20_T)$`strength incoming edges`["IT"]
  outmatbig[4,1] <- getNetworkAttributes(europe20_T)$`total strength`["IT"]
  outmatbig[5,1] <- getNetworkAttributes(europe20_T)$betweenness["IT"]
  outmatbig[6,1] <- getNetworkAttributes(europe20_T)$closeness["IT"]
  
  outmatbig[8,1] <- getNetworkAttributes(europe20_T)$eigenvalues["GR"]
  outmatbig[9,1] <- getNetworkAttributes(europe20_T)$`strength incoming edges`["GR"]
  outmatbig[10,1] <- getNetworkAttributes(europe20_T)$`total strength`["GR"]
  outmatbig[11,1] <- getNetworkAttributes(europe20_T)$betweenness["GR"]
  outmatbig[12,1] <- getNetworkAttributes(europe20_T)$closeness["GR"]
  
  outmatbig[14,1] <- getNetworkAttributes(europe20_T)$eigenvalues["DE"]
  outmatbig[15,1] <- getNetworkAttributes(europe20_T)$`strength incoming edges`["DE"]
  outmatbig[16,1] <- getNetworkAttributes(europe20_T)$`total strength`["DE"]
  outmatbig[17,1] <- getNetworkAttributes(europe20_T)$betweenness["DE"]
  outmatbig[18,1] <- getNetworkAttributes(europe20_T)$closeness["DE"]
  
  outmatbig[20,1] <- getNetworkAttributes(europe20_T)$eigenvalues["FR"]
  outmatbig[21,1] <- getNetworkAttributes(europe20_T)$`strength incoming edges`["FR"]
  outmatbig[22,1] <- getNetworkAttributes(europe20_T)$`total strength`["FR"]
  outmatbig[23,1] <- getNetworkAttributes(europe20_T)$betweenness["FR"]
  outmatbig[24,1] <- getNetworkAttributes(europe20_T)$closeness["FR"]
  
  outmatbig[26,1] <- getNetworkAttributes(europe20_T)$eigenvalues["SE"]
  outmatbig[27,1] <- getNetworkAttributes(europe20_T)$`strength incoming edges`["SE"]
  outmatbig[28,1] <- getNetworkAttributes(europe20_T)$`total strength`["SE"]
  outmatbig[29,1] <- getNetworkAttributes(europe20_T)$betweenness["SE"]
  outmatbig[30,1] <- getNetworkAttributes(europe20_T)$closeness["SE"]
} #col 1
{
  outmatbig[2,2] <- getNetworkAttributes(europe20_F)$eigenvalues["IT"]
  outmatbig[3,2] <- getNetworkAttributes(europe20_F)$`strength incoming edges`["IT"]
  outmatbig[4,2] <- getNetworkAttributes(europe20_F)$`total strength`["IT"]
  outmatbig[5,2] <- getNetworkAttributes(europe20_F)$betweenness["IT"]
  outmatbig[6,2] <- getNetworkAttributes(europe20_F)$closeness["IT"]
  
  outmatbig[8,2] <- getNetworkAttributes(europe20_F)$eigenvalues["GR"]
  outmatbig[9,2] <- getNetworkAttributes(europe20_F)$`strength incoming edges`["GR"]
  outmatbig[10,2] <- getNetworkAttributes(europe20_F)$`total strength`["GR"]
  outmatbig[11,2] <- getNetworkAttributes(europe20_F)$betweenness["GR"]
  outmatbig[12,2] <- getNetworkAttributes(europe20_F)$closeness["GR"]
  
  outmatbig[14,2] <- getNetworkAttributes(europe20_F)$eigenvalues["DE"]
  outmatbig[15,2] <- getNetworkAttributes(europe20_F)$`strength incoming edges`["DE"]
  outmatbig[16,2] <- getNetworkAttributes(europe20_F)$`total strength`["DE"]
  outmatbig[17,2] <- getNetworkAttributes(europe20_F)$betweenness["DE"]
  outmatbig[18,2] <- getNetworkAttributes(europe20_F)$closeness["DE"]
  
  outmatbig[20,2] <- getNetworkAttributes(europe20_F)$eigenvalues["FR"]
  outmatbig[21,2] <- getNetworkAttributes(europe20_F)$`strength incoming edges`["FR"]
  outmatbig[22,2] <- getNetworkAttributes(europe20_F)$`total strength`["FR"]
  outmatbig[23,2] <- getNetworkAttributes(europe20_F)$betweenness["FR"]
  outmatbig[24,2] <- getNetworkAttributes(europe20_F)$closeness["FR"]
  
  outmatbig[26,2] <- getNetworkAttributes(europe20_F)$eigenvalues["SE"]
  outmatbig[27,2] <- getNetworkAttributes(europe20_F)$`strength incoming edges`["SE"]
  outmatbig[28,2] <- getNetworkAttributes(europe20_F)$`total strength`["SE"]
  outmatbig[29,2] <- getNetworkAttributes(europe20_F)$betweenness["SE"]
  outmatbig[30,2] <- getNetworkAttributes(europe20_F)$closeness["SE"]
} #col 2
{
  outmatbig[2,3] <- getNetworkAttributes(europe15_T)$eigenvalues["IT"]
  outmatbig[3,3] <- getNetworkAttributes(europe15_T)$`strength incoming edges`["IT"]
  outmatbig[4,3] <- getNetworkAttributes(europe15_T)$`total strength`["IT"]
  outmatbig[5,3] <- getNetworkAttributes(europe15_T)$betweenness["IT"]
  outmatbig[6,3] <- getNetworkAttributes(europe15_T)$closeness["IT"]
  
  outmatbig[8,3] <- getNetworkAttributes(europe15_T)$eigenvalues["GR"]
  outmatbig[9,3] <- getNetworkAttributes(europe15_T)$`strength incoming edges`["GR"]
  outmatbig[10,3] <- getNetworkAttributes(europe15_T)$`total strength`["GR"]
  outmatbig[11,3] <- getNetworkAttributes(europe15_T)$betweenness["GR"]
  outmatbig[12,3] <- getNetworkAttributes(europe15_T)$closeness["GR"]
  
  outmatbig[14,3] <- getNetworkAttributes(europe15_T)$eigenvalues["DE"]
  outmatbig[15,3] <- getNetworkAttributes(europe15_T)$`strength incoming edges`["DE"]
  outmatbig[16,3] <- getNetworkAttributes(europe15_T)$`total strength`["DE"]
  outmatbig[17,3] <- getNetworkAttributes(europe15_T)$betweenness["DE"]
  outmatbig[18,3] <- getNetworkAttributes(europe15_T)$closeness["DE"]
  
  outmatbig[20,3] <- getNetworkAttributes(europe15_T)$eigenvalues["FR"]
  outmatbig[21,3] <- getNetworkAttributes(europe15_T)$`strength incoming edges`["FR"]
  outmatbig[22,3] <- getNetworkAttributes(europe15_T)$`total strength`["FR"]
  outmatbig[23,3] <- getNetworkAttributes(europe15_T)$betweenness["FR"]
  outmatbig[24,3] <- getNetworkAttributes(europe15_T)$closeness["FR"]
  
  outmatbig[26,3] <- getNetworkAttributes(europe15_T)$eigenvalues["SE"]
  outmatbig[27,3] <- getNetworkAttributes(europe15_T)$`strength incoming edges`["SE"]
  outmatbig[28,3] <- getNetworkAttributes(europe15_T)$`total strength`["SE"]
  outmatbig[29,3] <- getNetworkAttributes(europe15_T)$betweenness["SE"]
  outmatbig[30,3] <- getNetworkAttributes(europe15_T)$closeness["SE"]
} #col 3
{
  outmatbig[2,4] <- getNetworkAttributes(europe15_F)$eigenvalues["IT"]
  outmatbig[3,4] <- getNetworkAttributes(europe15_F)$`strength incoming edges`["IT"]
  outmatbig[4,4] <- getNetworkAttributes(europe15_F)$`total strength`["IT"]
  outmatbig[5,4] <- getNetworkAttributes(europe15_F)$betweenness["IT"]
  outmatbig[6,4] <- getNetworkAttributes(europe15_F)$closeness["IT"]
  
  outmatbig[8,4] <- getNetworkAttributes(europe15_F)$eigenvalues["GR"]
  outmatbig[9,4] <- getNetworkAttributes(europe15_F)$`strength incoming edges`["GR"]
  outmatbig[10,4] <- getNetworkAttributes(europe15_F)$`total strength`["GR"]
  outmatbig[11,4] <- getNetworkAttributes(europe15_F)$betweenness["GR"]
  outmatbig[12,4] <- getNetworkAttributes(europe15_F)$closeness["GR"]
  
  outmatbig[14,4] <- getNetworkAttributes(europe15_F)$eigenvalues["DE"]
  outmatbig[15,4] <- getNetworkAttributes(europe15_F)$`strength incoming edges`["DE"]
  outmatbig[16,4] <- getNetworkAttributes(europe15_F)$`total strength`["DE"]
  outmatbig[17,4] <- getNetworkAttributes(europe15_F)$betweenness["DE"]
  outmatbig[18,4] <- getNetworkAttributes(europe15_F)$closeness["DE"]
  
  outmatbig[20,4] <- getNetworkAttributes(europe15_F)$eigenvalues["FR"]
  outmatbig[21,4] <- getNetworkAttributes(europe15_F)$`strength incoming edges`["FR"]
  outmatbig[22,4] <- getNetworkAttributes(europe15_F)$`total strength`["FR"]
  outmatbig[23,4] <- getNetworkAttributes(europe15_F)$betweenness["FR"]
  outmatbig[24,4] <- getNetworkAttributes(europe15_F)$closeness["FR"]
  
  outmatbig[26,4] <- getNetworkAttributes(europe15_F)$eigenvalues["SE"]
  outmatbig[27,4] <- getNetworkAttributes(europe15_F)$`strength incoming edges`["SE"]
  outmatbig[28,4] <- getNetworkAttributes(europe15_F)$`total strength`["SE"]
  outmatbig[29,4] <- getNetworkAttributes(europe15_F)$betweenness["SE"]
  outmatbig[30,4] <- getNetworkAttributes(europe15_F)$closeness["SE"]
} #col 4
{
  outmatbig[2,5] <- getNetworkAttributes(europe10_T)$eigenvalues["IT"]
  outmatbig[3,5] <- getNetworkAttributes(europe10_T)$`strength incoming edges`["IT"]
  outmatbig[4,5] <- getNetworkAttributes(europe10_T)$`total strength`["IT"]
  outmatbig[5,5] <- getNetworkAttributes(europe10_T)$betweenness["IT"]
  outmatbig[6,5] <- getNetworkAttributes(europe10_T)$closeness["IT"]
  
  outmatbig[8,5] <- getNetworkAttributes(europe10_T)$eigenvalues["GR"]
  outmatbig[9,5] <- getNetworkAttributes(europe10_T)$`strength incoming edges`["GR"]
  outmatbig[10,5] <- getNetworkAttributes(europe10_T)$`total strength`["GR"]
  outmatbig[11,5] <- getNetworkAttributes(europe10_T)$betweenness["GR"]
  outmatbig[12,5] <- getNetworkAttributes(europe10_T)$closeness["GR"]
  
  outmatbig[14,5] <- getNetworkAttributes(europe10_T)$eigenvalues["DE"]
  outmatbig[15,5] <- getNetworkAttributes(europe10_T)$`strength incoming edges`["DE"]
  outmatbig[16,5] <- getNetworkAttributes(europe10_T)$`total strength`["DE"]
  outmatbig[17,5] <- getNetworkAttributes(europe10_T)$betweenness["DE"]
  outmatbig[18,5] <- getNetworkAttributes(europe10_T)$closeness["DE"]
  
  outmatbig[20,5] <- getNetworkAttributes(europe10_T)$eigenvalues["FR"]
  outmatbig[21,5] <- getNetworkAttributes(europe10_T)$`strength incoming edges`["FR"]
  outmatbig[22,5] <- getNetworkAttributes(europe10_T)$`total strength`["FR"]
  outmatbig[23,5] <- getNetworkAttributes(europe10_T)$betweenness["FR"]
  outmatbig[24,5] <- getNetworkAttributes(europe10_T)$closeness["FR"]
  
  outmatbig[26,5] <- getNetworkAttributes(europe10_T)$eigenvalues["SE"]
  outmatbig[27,5] <- getNetworkAttributes(europe10_T)$`strength incoming edges`["SE"]
  outmatbig[28,5] <- getNetworkAttributes(europe10_T)$`total strength`["SE"]
  outmatbig[29,5] <- getNetworkAttributes(europe10_T)$betweenness["SE"]
  outmatbig[30,5] <- getNetworkAttributes(europe10_T)$closeness["SE"]
} #col 5
{
  outmatbig[2,6] <- getNetworkAttributes(europe10_F)$eigenvalues["IT"]
  outmatbig[3,6] <- getNetworkAttributes(europe10_F)$`strength incoming edges`["IT"]
  outmatbig[4,6] <- getNetworkAttributes(europe10_F)$`total strength`["IT"]
  outmatbig[5,6] <- getNetworkAttributes(europe10_F)$betweenness["IT"]
  outmatbig[6,6] <- getNetworkAttributes(europe10_F)$closeness["IT"]
  
  outmatbig[8,6] <- getNetworkAttributes(europe10_F)$eigenvalues["GR"]
  outmatbig[9,6] <- getNetworkAttributes(europe10_F)$`strength incoming edges`["GR"]
  outmatbig[10,6] <- getNetworkAttributes(europe10_F)$`total strength`["GR"]
  outmatbig[11,6] <- getNetworkAttributes(europe10_F)$betweenness["GR"]
  outmatbig[12,6] <- getNetworkAttributes(europe10_F)$closeness["GR"]
  
  outmatbig[14,6] <- getNetworkAttributes(europe10_F)$eigenvalues["DE"]
  outmatbig[15,6] <- getNetworkAttributes(europe10_F)$`strength incoming edges`["DE"]
  outmatbig[16,6] <- getNetworkAttributes(europe10_F)$`total strength`["DE"]
  outmatbig[17,6] <- getNetworkAttributes(europe10_F)$betweenness["DE"]
  outmatbig[18,6] <- getNetworkAttributes(europe10_F)$closeness["DE"]
  
  outmatbig[20,6] <- getNetworkAttributes(europe10_F)$eigenvalues["FR"]
  outmatbig[21,6] <- getNetworkAttributes(europe10_F)$`strength incoming edges`["FR"]
  outmatbig[22,6] <- getNetworkAttributes(europe10_F)$`total strength`["FR"]
  outmatbig[23,6] <- getNetworkAttributes(europe10_F)$betweenness["FR"]
  outmatbig[24,6] <- getNetworkAttributes(europe10_F)$closeness["FR"]
  
  outmatbig[26,6] <- getNetworkAttributes(europe10_F)$eigenvalues["SE"]
  outmatbig[27,6] <- getNetworkAttributes(europe10_F)$`strength incoming edges`["SE"]
  outmatbig[28,6] <- getNetworkAttributes(europe10_F)$`total strength`["SE"]
  outmatbig[29,6] <- getNetworkAttributes(europe10_F)$betweenness["SE"]
  outmatbig[30,6] <- getNetworkAttributes(europe10_F)$closeness["SE"]
} #col 6

#Age Levels: 15-19 15-64 20-24 25-29 TOTAL
#make matrix: create networks for each age level, compute sumstats and (mean) eigenvector centralities
#for cities of interest in each, cor them
agelevels <- c("15-19","15-64", "20-24", "25-29")

########################################################################
##############################NEW APPROACH#############################
########################################################################

#reshape wide
dc2 <- datCat
dc2 <- dc2[, NET_FLOW := NULL][, FLOW := NULL] #[, Year := NULL] #[Year==2020]
dc2$SEX <- as.factor(dc2$SEX)
dc2$AGE <- as.factor(dc2$AGE)
dc2$Year <- as.factor(dc2$Year)
setnames(dc2, 3, "YEAR")
#datWide <- dcast(dc2, DEST + ORIG +SEX +AGE ~ ...,  value.var = "Number", sep = " ")
datWide <- dcast(dc2, DEST + ORIG +AGE + YEAR ~ ...,  value.var = "Number", sep = " ")
datWide <- datWide[, MALE := TOTAL-FEMALE]

long <- melt(datWide, measure.vars = c("TOTAL", "FEMALE", "MALE"), variable.name = "SEX", variable.factor = T, value.name = "Number")
setcolorder(long, 2)
long[is.na(long)] <- 0
save(long, file = "Graph Data.RData")

g <- graph_from_data_frame(long)
E(g)$SEX <- as.factor(long$SEX)
E(g)$AGE <- as.factor(long$AGE)
E(g)$YEAR <- as.factor(long$YEAR)
E(g)$weight <- (long$Number+0.01)
E(g)$btw <- edge_betweenness(g)

getGraphVector <- function(net) {
  ev <- eigen(as_adj(net, sparse=FALSE))$vectors[,1]
  sv <- strength(net, mode = "all")
  old.weight <- E(net)$weight
  netclone <- net
  E(netclone)$weight <- old.weight + 0.01 #needs to be >0
  bv <- betweenness(netclone, normalized = T)
  cv <- closeness(netclone, normalized = T)
  
  result <- list("strength" = sv, 
                 "eigenvalues" = ev, 
                 "betweenness" = bv, 
                 "closeness" = cv )
  return(result)
}

getGraphSum <- function(net) {
  
  #get attributes
  nodes <- vcount(net)
  edges <- ecount(net)
  edge.dens <- edge_density(net)
  NetTrans <- transitivity(net, type = "global")
  diam <- diameter(net)
  avgdgin <- mean(degree(net, mode = "in"))
  avgdgout <- mean(degree(net, mode = "out"))
  avgdgall <- mean(degree(net, mode = "all"))
  res <- list("nodes" = nodes, "edges" = edges, "edge density" = edge.dens,
              "transitivity" = NetTrans, "diameter" = diam,
              "avg degree out" = avgdgout,
              "avg degree in" = avgdgin,
              "avg degree" = avgdgall)
  
  return(res)
}

#kruskal test matrix
{
  kmat <- matrix(nrow = 3, ncol = 3)
  rownames(kmat) <- c("Age", "Sex", "Year")
  colnames(kmat) <- c("# Immigrants", "Edge Betweenness", "Age")
  
  kmat[1,1] <- kruskal.test(E(g)$AGE,E(g)$Number)$p.value
  kmat[2,1] <- kruskal.test(E(g)$SEX,E(g)$Number)$p.value
  kmat[3,1] <- kruskal.test(E(g)$YEAR,E(g)$Number)$p.value
  
  kmat[1,2] <- kruskal.test(E(g)$AGE,E(g)$btw)$p.value
  kmat[2,2] <- kruskal.test(E(g)$SEX,E(g)$btw)$p.value
  kmat[3,2] <- kruskal.test(E(g)$YEAR,E(g)$btw)$p.value
  
  kmat[1,3] <- kruskal.test(E(g)$AGE,E(g)$AGE)$p.value
  kmat[2,3] <- kruskal.test(E(g)$SEX,E(g)$AGE)$p.value
  kmat[3,3] <- kruskal.test(E(g)$YEAR,E(g)$AGE)$p.value
}

{
  ##Correlation Matrix
  corG <- matrix(nrow = 4, ncol = 4)
  measures <- c("Strength", "EV Centr.", "Betweenness", "Closeness")
  rownames(corG) <- measures
  colnames(corG) <- measures
  for(i in 1:4) {
    for (j in 1:4) {
      corG[i,j] <- 1
    }
  }
  
  {
    corG[2,1] <- cor(as.numeric(getGraphVector(g)$eigenvalues), as.numeric(getGraphVector(g)$strength))
    corG[1,2] <- corG[2,1]
    
    corG[3,1] <- cor(as.numeric(getGraphVector(g)$betweenness), as.numeric(getGraphVector(g)$strength))
    corG[1,3] <- corG[3,1]
    
    corG[4,1] <- cor(as.numeric(getGraphVector(g)$closeness), as.numeric(getGraphVector(g)$strength))
    corG[1,4] <- corG[4,1]
    
    corG[3,2] <- cor(as.numeric(getGraphVector(g)$betweenness), as.numeric(getGraphVector(g)$eigenvalues))
    corG[2,3] <- corG[3,2]
    
    corG[4,2] <- cor(as.numeric(getGraphVector(g)$closeness), as.numeric(getGraphVector(g)$eigenvalues))
    corG[2,4] <- corG[4,2]
    
    corG[4,3] <- cor(as.numeric(getGraphVector(g)$closeness), as.numeric(getGraphVector(g)$betweenness))
    corG[3,4] <- corG[4,3]
  }
}

{
  sumMat <- matrix(nrow = 7, ncol=1)
  rownames(sumMat) <- c("Number of Nodes", "Number of Edges", "Transitivty", "Edge Density", 
                        "Diameter", "Average Degree (incoming)", "Average Degree")
  colnames(sumMat) <- c("Value")
  sumMat[1,1] <-  round(getGraphSum(g)$nodes, 0)
  sumMat[2,1] <-  round(getGraphSum(g)$edges, 0)
  sumMat[3,1] <-  getGraphSum(g)$transitivity
  sumMat[4,1] <-  getGraphSum(g)$`edge density`
  sumMat[5,1] <-  getGraphSum(g)$diameter
  sumMat[6,1] <-  getGraphSum(g)$`avg degree in`
  sumMat[7,1] <-  getGraphSum(g)$`avg degree`
}


{
  # par(bg="white")
  # set.seed(42069)
  # {
  #   plot(g,
  #        #layout=layout.graphopt(g),
  #        #layout = layout.sphere(g),
  #        #layout = layout_with_drl(g),
  #        layout = layout_with_lgl(g) ,
  #        # === vertex
  #        vertex.color = "lightskyblue",          # Node color
  #        vertex.frame.color = "transparent",                 # Node border color
  #        vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
  #        vertex.size=15,                               # Size of the node (default is 15)
  #        vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
  # 
  #        # # === vertex label
  #        vertex.label=V(g)$name,                   # Character vector used to label the nodes
  #        vertex.label.color="lightslategray",
  #        # vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
  #        # vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
  #        vertex.label.cex= 0.7,                           # Font size (multiplication factor, device-dependent)
  #        # vertex.label.dist=0,                          # Distance between the label and the vertex
  #        # vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
  # 
  #        # === Edge
  #        edge.color="lightcyan2",                           # Edge color
  #        edge.width=0.25,                                 # Edge width, defaults to 1
  #        edge.arrow.size=0.5,                            # Arrow size, defaults to 1
  #        edge.arrow.width=0.5,                           # Arrow width, defaults to 1
  #        edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
  #        edge.curved=0    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
  #   )
  # }
  # 
  # 
  # if (Sys.info()[7] == "ts") {
  #   setwd("/Users/ts/Dropbox/Apps/Overleaf/NetEc_Pauly")
  #  # savePlot(filename = "graph.png", device = dev.cur())
  #   
  #   setwd(Paths[Sys.info()[7]]) #back to normal home folder
  # }
}

setwd(Paths[Sys.info()[7]]) #back to normal home folder

#reshape copy wide
dc3 <- datCopy
#dc3 <- dc3[, NET_FLOW := NULL][, FLOW := NULL]
setnames(dc3, 3, "YEAR")

wide <- dcast(dc3, DEST + ORIG +AGE + YEAR ~ ...,  value.var = "Number", sep = " ")
wide <- wide[, MALE := TOTAL-FEMALE]

regdat <- melt(wide, measure.vars = c("TOTAL", "FEMALE", "MALE"), variable.name = "SEX",  value.name = "Number")
setcolorder(regdat, 2)
regdat[is.na(regdat)] <- 0

#throw out 2008 and 2009 (not in gdp data)
regdat <- regdat[YEAR > 2010,]

##merge in gdp data
gdpdat <- fread("/Users/luciarisuenonavarro/Desktop/Migration project/R Project - lucia /data files/GDP.csv")
gdpdat <- gdpdat[,.(DEST = geo, YEAR = TIME_PERIOD, GDPpcPPS = OBS_VALUE)][YEAR > 2010]
wip1 <- gdpdat[regdat, on=.(DEST=DEST, YEAR=YEAR)]

##merge in gini data
ginidat <- fread("/Users/luciarisuenonavarro/Desktop/Migration project/R Project - lucia /data files/GINI.csv")
ginidat <- ginidat[,.(DEST = geo, YEAR = TIME_PERIOD, GiniEDI = OBS_VALUE)][YEAR > 2010]

wip2 <- ginidat[wip1, on=.(DEST=DEST, YEAR=YEAR)]

##merge in unemployment
unempdat <- fread("/Users/luciarisuenonavarro/Desktop/Migration project/R Project - lucia /data files/UNEMPLOYMENT.csv")
unempdat <- unempdat[,.(DEST = geo, YEAR = TIME_PERIOD, UNEMP = OBS_VALUE)][YEAR > 2010]

##clean up final data, free space
data <- unempdat[wip2, on= .(DEST = DEST, YEAR = YEAR)] 
rm(gdpdat, unempdat, ginidat, wip1, wip2, dc3, wide)
setcolorder(data, c(6,1,2,7,8,9))

data[is.na(data)] <- 0
save(data, file = "Regression Data.RData")

h <- graph_from_data_frame(data)
E(h)$SEX <- as.factor(data$SEX)
E(h)$AGE <- as.factor(data$AGE)
E(h)$YEAR <- data$YEAR
E(h)$weight <- (data$Number+0.01)
E(h)$NUMBER <- data$Number
E(h)$btw <- edge_betweenness(h)
E(h)$GDP <- data$GDPpcPPS
E(h)$UNEMP <- data$UNEMP
E(h)$GINI <- data$GiniEDI

getGraphVectorByYear <- function(year) {
  gr <- graph_from_data_frame(data[YEAR == year])
  E(gr)$weight <- E(gr)$Number + 0.1 #must be >0
  ev <- eigen(as_adj(gr, sparse=FALSE))$vectors[,1]
  sv <- strength(gr, mode = "all")
  bv <- betweenness(gr, normalized = T)
  cv <- closeness(gr, normalized = T)
  names <- V(gr)$name
  YEAR <- rep(year, length(names))
  
  result <- list("YEAR" = YEAR,
                 "names" = names,
                 "strength" = sv, 
                 "eigenvalues" = ev, 
                 "betweenness" = bv, 
                 "closeness" = cv )
  return(result)
}



##2011, 2012, 2020 have 50 nodes
#the rest have 52

years1 <- c(2011, 2012, 2020)
years2 <- seq(2013, 2019)

nodedatByYear <- matrix(NA, ncol = 6, nrow = (50*3 +length(years2)*52))
colnames(nodedatByYear) <- c("NAME", "YEAR", "CLS", "BTW", "EV", "STR")


##extract node attributes by year
## for years1
j <- 1
for (i in seq.int(from = 1, by = 50, length.out = 3)) {
  
  year <- years1[j]
  
  yr <- getGraphVectorByYear(year)$YEAR
  names <- getGraphVectorByYear(year)$names
  cls <- getGraphVectorByYear(year)$closeness
  btw <- getGraphVectorByYear(year)$betweenness
  ev <-  getGraphVectorByYear(year)$eigenvalues
  s <- getGraphVectorByYear(year)$strength
  
  #bind
  nodedatByYear[i:(i+49),1:6] <- cbind(names, yr, cls, btw, ev, s)
  j <- j + 1
}

#for years2
j <- 1
for (i in seq.int(from = 151, by = 52, length.out = length(years2))) {
  year <- years2[j]
  
  yr <- getGraphVectorByYear(year)$YEAR
  names <- getGraphVectorByYear(year)$names
  cls <- getGraphVectorByYear(year)$closeness
  btw <- getGraphVectorByYear(year)$betweenness
  ev <- getGraphVectorByYear(year)$eigenvalues
  s <- getGraphVectorByYear(year)$strength
  
  #bind
  nodedatByYear[i:(i+51),1:6] <- cbind(names, yr, cls, btw, ev, s)
  j <- j + 1
}

nodes <- as.data.table(nodedatByYear)
nodes$YEAR <- as.numeric(nodes$YEAR)
nodes$CLS <- as.numeric(nodes$CLS)
nodes$BTW <- as.numeric(nodes$BTW)
nodes$EV <- as.numeric(nodes$EV)
nodes$STR <- as.numeric(nodes$STR)
nodes[is.na(nodes)] <-  0


full <- data
nodes <- nodes[full, on=.(NAME = DEST, YEAR=YEAR)]
full <- nodes[,.(ORIG, DEST = NAME, YEAR, AGE, SEX, Number, UNEMP, GiniEDI, GDPpcPPS, CLS, BTW, EV, STR)]
setkey(full, YEAR)
full <- full[, NumberL1:=c(NA, Number[-.N]), by=YEAR]
setcolorder(full, c(1,2,3,4,5,6,14,7,8,9,10,11,12,13))
full[is.na(full)] <- 0
save(full, file="Model.RData")

##make data time series
tsdat <- full
tsdat <- tsdat[, Y:= as.character(YEAR)]
tsdat <- tsdat[, Y := as.Date(Y, format = "%Y")] #[, Y := format(Y, format="%Y")]
setcolorder(tsdat, 15)
tsdat <- as.xts(tsdat)
colnames(tsdat) <- c("YEAR", "Number", "Lag_Number", "Unempl.", "Gini","GDP","Closeness", "Betweenness", "Eigenv_Centr.", "Strength")
save(tsdat, file = "Time Series Data for Regression.RData")
#this is what I did first (using igraph, which in hindsight is unnecessary and slow)
{
  # model <- graph_from_data_frame(full)
  # E(model)$SEX <- as.factor(full$SEX)
  # E(model)$AGE <- as.factor(full$AGE)
  # E(model)$YEAR <- full$YEAR
  # E(model)$weight <- (full$Number+0.01)
  # E(model)$NUMBER <- full$Number
  # E(model)$NUMBERL1 <- full$NumberL1
  # E(model)$btw <- edge_betweenness(model)
  # E(model)$GDP <- full$GDPpcPPS
  # E(model)$UNEMP <- full$UNEMP
  # E(model)$GINI <- full$GiniEDI
  # E(model)$CLS <- full$CLS
  # E(model)$BTW <- full$BTW
  # E(model)$EV <- full$EV
  # E(model)$STR <- full$STR
  # 
  # NUMBER <- E(model)$NUMBER
  # GDP <- E(model)$GDP
  # UNEMP <- E(model)$UNEMP
  # GINI <- E(model)$GINI
  # NUMBER_Lag1y <- E(model)$NUMBERL1
  # BETW <- E(model)$BTW <- full$BTW
  # EIGENV <- E(model)$EV <- full$EV
  # STRENGTH <- E(model)$STR <- full$STR
  # 
  # model1 <- lm(NUMBER ~ GDP + UNEMP + GINI)
  # se1 <- sqrt(diag(vcov(model1)))
  # 
  # model2 <- lm(NUMBER ~ GDP + UNEMP + GINI + NUMBER_Lag1y)
  # se2 <- sqrt(diag(vcov(model2)))
  # 
  # model3 <- lm(NUMBER ~ GDP + UNEMP + GINI + NUMBER_Lag1y + BETW + EIGENV + STRENGTH)
  # se3 <- sqrt(diag(vcov(model3)))
}


model1 <- lm(Number ~ GDP + Unempl. + Gini, data = tsdat)
se1 <- sqrt(diag(vcov(model1)))

model2 <- lm(Number ~ GDP + Unempl. + Gini + Lag_Number, data = tsdat)
se2 <- sqrt(diag(vcov(model2)))

model3 <- lm(Number ~ GDP + Unempl. + Gini + Lag_Number + 
               Betweenness + Eigenv_Centr. + Strength, data = tsdat)
se3 <- sqrt(diag(vcov(model3)))

###ROBUSTNESS CHECKS
#Check for Autocorrelation, Heteroscedasticity, Misspecification
#1
box1 <- Box.test(model1$residuals, lag = 5, type = "Ljung")
acf(resid(model1), plot=T, lag.max = 10)$acf[2]
bp1 <- bptest(model1) #so we have heteroscedasticity
#coeftest(model1, vcov = NeweyWest(model1))
reset1 <- resettest(model1)

# Adjust standard errors
cov1         <- NeweyWest(model1)
robust_se_1    <- sqrt(diag(cov1))

#2
box2 <- Box.test(model2$residuals, lag = 5, type = "Ljung")
acf(resid(model2), plot=T, lag.max = 10)$acf[2]
bp2 <- bptest(model2) #so we have heteroscedasticity
#coeftest(model2, vcov = NeweyWest(model2))
reset2 <- resettest(model2)

# Adjust standard errors
cov2         <- NeweyWest(model2)
robust_se_2    <- sqrt(diag(cov2))

#3
box3 <- Box.test(model3$residuals, lag = 5, type = "Ljung")
acf(resid(model3), plot=T, lag.max = 10)$acf[2]
bp3 <- bptest(model3) #so we have heteroscedasticity
#coeftest(model3, vcov = NeweyWest(mode32))
reset3 <- resettest(model3)

# Adjust standard errors
cov3         <- NeweyWest(model3)
robust_se_3    <- sqrt(diag(cov3))

robustmat <- matrix(nrow = 3, ncol = 3)
colnames(robustmat) <- c("Box–Pierce ", "Breusch-Pagan", "Ramsey's RESET")
rownames(robustmat) <- c("Model 1", "Model 2", "Model 3")
{
  robustmat[1,1] <- box1$p.value
  robustmat[1,2] <- bp1$p.value
  robustmat[1,3] <- reset1$p.value
  
  robustmat[2,1] <- box2$p.value
  robustmat[2,2] <- bp2$p.value
  robustmat[2,3] <- reset2$p.value
  
  robustmat[3,1] <- box3$p.value
  robustmat[3,2] <- bp3$p.value
  robustmat[3,3] <- reset3$p.value
  
}
########################PRINTING MATRICES TO LATEX###########


if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/NetEc_Pauly/Tables/")
  
  #Table 1
  numdig <- matrix(c(0,0,3, rep(3, 4)),ncol=2, nrow=7)
  print(xtable(sumMat, align = "l|l", caption = "Entire Network", digits = numdig, label = "full"), caption.placement = 'top', table.placement = "H",
        type = "latex", file = "gSummary")
  
  #Table 2
  numdig2 <- matrix(c(0,0, rep(3, 3)),ncol=7, nrow=nrow(outmat2))
  print(xtable(outmat2, align = "l|l|l|l|l|l|l", caption = "Summary Statistics of Individual (European) Networks", digits = numdig2, label = "full"), caption.placement = 'top', table.placement = "H",
        type = "latex", file = "indSummary")
  
  #Table 3
  print(xtable(outmatbig, align = "|l|l|l|l|l|l|l|", 
               caption = "Node Statistics of Individual Networks, (T) denotes both sexes, (F) only female", 
               digits = 3, label = "full"), 
        include.rownames= T ,
        caption.placement = 'top', 
        table.placement = "H", hline.after = c(-1, 0, 6,12,18,24, nrow(outmatbig)),
        type = "latex", 
        file = "nodeSum")
  
  #Correlation Matrix
  numdig3 <- matrix(c(0,0,4,4,4,0,4,0,4,4,0,4,4,0,4,0,4,4,4,0), nrow = 4, ncol = 5, byrow = T)
  print(xtable(corG, align = "l|llll", caption = "Correlation Matrix of Node Centrality Measures", 
               digits = numdig3, label = "corr"), include.rownames = T,
        caption.placement = 'top', table.placement = "H",
        type = "latex", file = "corr")
  
  #Kruskal-Wallis Tests
  print(xtable(kmat, align = "l|ccc", digits = 16,
               caption = "p–values of Kruskal-Wallis Rank-Sum Tests using Edge Attributes", 
               label = "kw"), include.rownames = T, 
        caption.placement = 'top', table.placement = "H",
        type = "latex", file = "kw")
  
  #Robustness
  print(xtable(robustmat, align = "l|ccc", digits = 8,
               caption = "p–values of Robustness Tests", 
               label = "robustness"), include.rownames = T, 
        caption.placement = 'top', table.placement = "H",
        type = "latex", file = "robustness")
  
  #Regression Models
  stargazer(model1, model2, model3, se = list(se1, se2, se3),
            out.header = F, 
            title = "Regression Models", 
            table.placement = "H", label = "Regs", 
            out = "regs",
            model.names = T,
            notes = c("Standard Errors reported in Parentheses"),
            notes.align = "r",
            star.cutoffs = c(0.05, 0.01, 0.001))
  
  stargazer(model1, model2, model3, se = list(robust_se_1, robust_se_2, robust_se_3),
            out.header = F, 
            title = "Regression Models Using Newey-West HAC Standard Errors", 
            table.placement = "H", label = "RegsRobust", 
            out = "regsrobust",
            model.names = T,
            star.cutoffs = c(0.05, 0.01, 0.001))
            # notes = c("Newey & West HAC SE reported in Parentheses"),
            # notes.align = "r",
            
  
}

resids <- as.data.frame(cbind(model1$residuals, model2$residuals, model3$residuals))
setDT(resids)
resids[V1 > 20000] <- 0
resids[V2 > 20000] <- 0
resids[V3 > 20000] <- 0

resids[V1 < -20000] <- 0
resids[V2 < -20000] <- 0
resids[V3 < -20000] <- 0

colnames(resids) <- c("Model 1", "Model 2", "Model 3")
residlong <- melt(resids, measure.vars = c("Model 1", "Model 2", "Model 3"), 
                  variable.factor = T, variable.name = "Model")


residplot <- ggplot(residlong, aes(x = value, fill = Model, color = Model)) +
  geom_histogram(alpha=0.5, position="identity", bins=60) + 
  theme_minimal() + ylab("Density") + xlab("Residuals") +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom")

#ggtitle("Histogram of Residuals") +

residplot


if (Sys.info()[7] == "ts") {
  setwd("/Users/ts/Dropbox/Apps/Overleaf/NetEc_Pauly")
  ggsave("residplot.png",  bg = "white", dpi = "retina", width = 15, height = 10, units = "cm")
  setwd(Paths[Sys.info()[7]]) #back to normal home folder
}


setwd(Paths[Sys.info()[7]]) #back to normal home folder

plot.igraph(g)
