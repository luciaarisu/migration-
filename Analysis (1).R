library(data.table, ggplot2)
library(xtable)
library(scales)
library(igraph)
library(ggthemes)
Paths = c("/Users/luciarisuenonavarro/Desktop/Migration project/")
names(Paths) = c("ts", "?")
setwd(Paths[Sys.info()[7]])

{
  if (Sys.info()[7] == "ts") {
    #import raw data
    # dat1 <- fread("/Users/luciarisuenonavarro/Desktop/Migration project/migration_country_birth.gz")
    # dat2 <- fread("/Users/luciarisuenonavarro/Desktop/Migration project/migration_country_previous_residence.gz")
    dat1 <- fread("migration_country_birth.gz")
    dat2 <- fread("migration_country_previous_residence.gz")
    
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
    datCat <- datCat[DEST == "EL", DEST := "GR"][ORIG == "EL", ORIG := "GR"] #rename Greece from EL to GR

    #levels(datCat$SEX) <- c(levels(datCat$SEX), "M")    # add new level
    datCat <- datCat[SEX == "T", SEX := "TOTAL"][SEX == "F", SEX := "FEMALE"]
    datCat <- datCat[AGE == "Y15-19", AGE := "15-19"][AGE == "Y15-64", AGE := "15-64"][AGE == "Y20-24", AGE := "20-24"][AGE == "Y25-29", AGE := "25-29"]
    setkey(datCat, DEST)
    #coerce factors
    cols <- c("SEX", "AGE")
    datCat[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
    
    
    #rm junk
    rm(dat1,dat1_clean, dat1premerge, dat2, dat2_clean, dat2premerge, agelist, 
       countrylist, destlist, sexlist, yearlist, origlist, datMerge)
    #save
    save(datCat, file = "/Users/ts/Downloads/BACKUP OF R PROJECT/Data.Rdata")
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

prettifyNetwork <- function(net) {
  labels <- E(net)$Number
  labels[labels==0] <- ""
  E(net)$label <- labels #set edge labels (for graphing only)
  
  return(net)
}

dropLabs <- function(net) {
  E(net)$label <- " " #set edge labels (for graphing only)
  
  return(net)
}

{
  
  
  
  
  plotNetwork <- function(net) {
    return(plot(net, edge.width = 1, edge.lty = 1,  
                edge.curved = F, edge.arrow.size = 0.6, 
                edge.arrow.width = 0.4, vertex.frame.color="transparent",
                vertex.label.cex = 0.7)) #, rescale=FALSE))
  }
  # create subsets
  countryList <- c("IT", "DE", "GR", "FR", "SE")
  europe <- datCat[DEST%in%countryList]
  
  
  
  plotNetwork(prettifyNetwork(europe16))
  plotNetwork(prettifyNetwork(full_2016))
  
  
  test <- dropLabs(europe16)
  l <- layout_on_sphere(test)
  plot(prettifyNetwork(europe16), vertex.frame.color="transparent",
       vertex.label.cex = 0.7, layout = l )
  
  #plot(test, layout=layout.fruchterman.reingold.grid(full_2016))
  #tkplot(prettifyNetwork(europe16))
  
  #reduce size: to make a subgraph, we need to get the vertex IDs
  countries <- c("IT", "DE", "GR", "FR", "SE", "AF", "AFR", "IR", 
                 "SYR", "NEU28_FOR", "AFR_N") #manually picked those w/ highest strength
  verts <- V(full_2016)$name #get names
  boolVec <- verts %in%countries #check 
  foo <- which(boolVec == T, arr.ind = T) # get which indices
  
  
  # sub <- subgraph(europe16, vids = c(1,  2,  5 ,15, 18 ,22, 27, 29, 38, 44))
  #sub2 <- induced_subgraph(europe16, c(11, 20, 14, 33, 34, 12))
  #plotNetwork(prettifyNetwork(sub))
  #plot(sub2, layout=layout.circle, main="FUCK THIS") 
 }

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
getNetworkSummary(full)

outmat <- matrix(nrow = 5, ncol=1)
rownames(outmat) <- c("Number of Nodes", "Number of Edges", "Transitivty", "Edge Density", "Diameter")
colnames(outmat) <- c("Value")
outmat[1,1] <-  getNetworkSummary(full)$nodes
outmat[2,1] <-  getNetworkSummary(full)$edges
outmat[3,1] <-  getNetworkSummary(full)$transitivity
outmat[4,1] <-  getNetworkSummary(full)$`edge density`
outmat[5,1] <-  getNetworkSummary(full)$diamter

print(xtable(outmat, align = "l|l", caption = "Entire Network, 2020", digits = 2, label = "full"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Research Question Kyra Network Econ/Tables/fullSummary")

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
  
print(xtable(outmat2, align = "l|l|l|l|l|l|l", caption = "Summary Statistics of Individual (European) Networks", digits = 2, label = "full"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Research Question Kyra Network Econ/Tables/indSummary")

getNetworkAttributes(europe20_T)

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

print(xtable(outmatbig, align = "|l|l|l|l|l|l|l|", 
             caption = "Node Statistics of Individual Networks, (T) denotes both sexes, (F) only female", 
             digits = 3, label = "full"), 
            include.rownames= T ,
            caption.placement = 'top', 
      table.placement = "H", hline.after = c(-1, 0, 6,12,18,24, nrow(outmatbig)),
      type = "latex", 
      file = "/Users/ts/Dropbox/Apps/Overleaf/Research Question Kyra Network Econ/Tables/nodeSum")




