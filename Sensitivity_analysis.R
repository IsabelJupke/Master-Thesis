# Master thesis Isabel Jupke 
# Plot results of scenarios
# Example: Secnario 4 (including all five FFGs)

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung")

# Set replicate number
n <- 10 

# Set input
fp.in <- "input"
fname.input<-"1spec_generalSettings_noFiltering.txt"
fname.input.out<-"1spec_generalSettings_noFiltering_out.txt" #general settings

# Define name: 
# Include name of parameter that is tested and value
name.vec <-  paste("Gatherer_Sensitivity_nInit0500_", append(paste(0,1:9,sep = ""),10:n), sep = "") 

# Start model runs
for (i in 1:n){
  namestr <- name.vec[i]   
  fname.taxpara <- "Gatherer_Sensitivity_"
  f<-readLines(file.path(fp.in,fname.input))
  outF<-file.path(fp.in,fname.input.out)
  outF<-file(outF,"w")
  writeLines(f[1:3],outF)
  writeLines(paste("fNameInputTaxParam:\t",fname.taxpara,sep=""),outF)
  writeLines(f[5:6],outF)
  writeLines(paste0("strSubOut:\t",namestr),outF)
  writeLines(f[8:length(f)],outF)
  flush(outF)
  close(outF)
  
  fTaxaParam <- strsplit(f[4],"\t")[[1]][2]
  param <- read.table(file.path(fp.in, fTaxaParam))
  
# Change parameters here:
  
  #nInit -50%
  param[2,2] <- 500
  #nInit +50%
  #param[2,2] <- 1500
  #fBasal -50%
  #param[3,2] <- 0.5
  #fBasal +50%
  #param[3,2] <- 1.5
  #kFood -50%
  #param[4,2] <- 0.5
  #kFood +50%
  #param[4,2] <- 1.5
  #fIngest -50%
  #param[5,2] <- 4
  #fIngest +50%
  #param[5,2] <- 12
  #pStarve -50%
  #param[6,2] <- 0.25
  #pStarve +50%
  #param[6,2] <- 0.75
  #kDens -50%
  #param[7,2] <- 12.5
  #kDens +50%
  #param[7,2] <- 37.5
  #fGrowthEgg -50%
  #param[8,2] <- 2
  #fGrowthEgg -50%
  #param[8,2] <- 6
  #emergence 1 
  #param[9,2] <- 1
  #fRepro -50%
  #param[10,2] <- 0.25
  #fRepro +50%
  #param[10,2] <- 0.75
  #reproStart -50% (+50% nicht möglich)
  #param[11,2] <- 182
  #clutchSize -50%
  #param[15,2] <- 50
  #clutchSize +50%
  #param[15,2] <- 150
  #nRepro -50%
  #param[17,2] <- 2
  #nRepro -50%
  #param[17,2] <- 6
  #BtwRepro -50%
  #param[19,2] <- 34
  #BtwRepro +50%
  #param[19,2] <- 102
  #eggM -50%
  #param[21,2] <- 0.000075
  #eggM +50%
  #param[21,2] <- 0.000225
  #hatchMRelSd -50%
  #param[22,2] <- 0.001
  #hatchMRelSd +50%
  #param[22,2] <- 0.003
  #adultM -50% 
  #param[23,2] <- 0.005
  #adultM +50% 
  #param[23,2] <- 0.015
  #facMaxM -50%
  #param[25,2] <- 0.5
  #facMaxM +50%
  #param[25,2] <- 1.5
  
  write.table(param, file = file.path(fp.in,fname.taxpara), sep = "\t", row.names = F, col.names = F, quote = F)
  rand.seed <- 123 + i
  command <- "java"
  args <- c("-Xmx1G","-jar","IBM-Bugs.jar",fname.input.out, rand.seed)
  system2(command = command,args = args)
  print(i) # Displays progress
}
beep(1)  # End signal

# Calculation of average deviation from standard

# Load runs with default parameter values
liste.tabellen <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/Gatherer_parms/out_Gatherer_Standard", 
                   append(paste(0,1:9,sep = ""),10:n), ".csv", sep= "") 
for (i in 1:n){
  liste.tabellen[[i]] <- read.csv(name.file[i])  
}

# Load runs with changed vaules +50% and -50%
liste.tabellen.parms <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/Gatherer_parms/out_Gatherer_nInit_1500_", 
                   append(paste(0,1:9,sep = ""),10:n), ".csv", sep= "") 
for (i in 1:n){
  liste.tabellen.parms[[i]] <- read.csv(name.file[i])  
}
liste.tabellen.parms2 <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/Gatherer_parms/out_Gatherer_nInit_500_", 
                   append(paste(0,1:9,sep = ""),10:n), ".csv", sep= "") 
for (i in 1:n){
  liste.tabellen.parms2[[i]] <- read.csv(name.file[i])  
}

# Calculate average deviation from standards
values.ave<-vector("list", 10)
for (i in 1:n){
  #+50%
  diff.tab<- liste.tabellen[[i]][5]-liste.tabellen.parms[[i]][5]
  sum.val<-sum(abs(diff.tab[,1]))
  ave.tab<- sum.val/2605
  values.ave[[i]][1]<- ave.tab
  #-50%
  diff.tab2<- liste.tabellen[[i]][5]-liste.tabellen.parms2[[i]][5]
  sum.val2<-sum(abs(diff.tab2[,1]))
  ave.tab2<- sum.val2/2605
  values.ave[[i]][2]<- ave.tab2
}

# Get vector with deviations for all replicates
all.val.ave<- unlist(values.ave)
# Get average
(ave<- sum(all.val.ave)/(n*2))

