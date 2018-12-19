# Master thesis Isabel Jupke 
# Latin hypercube sampling and starting the model
# Example: Secnario 4 (including all five FFGs)

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs")

# Load libraries, set options and get LHC functions
library(beepr)
library(lhs) 
options(scipen=999)
source(file.path(fp.r,"functions_LHC.R"))

# Set number of replications and datasets
n.sim<-100 # Datasets
n <- 50    # Replications

# Set minimum and maximum factor for LHC
min <- 0.5
max <- 2

# Set the file names
name.vec <-  paste("10_5spec_LHC_", append(paste(0,1:9,sep = ""),10:n.sim), sep = "") # for output
name.vec.out <- "10_5spec_LHC"                                                        # for parameter file
name.vec.out2 <- "10_5spec_LHC_Uebersicht"                                            # overview of all datasets

# Set input
fp.r<- getwd()
fp.in <- "input"
fp.out<-"output"
fname.input<-"5spec_generalSettings_noFiltering.txt"
fname.input.out<-"5spec_generalSettings_noFiltering_out.txt" 
f<-readLines(file.path(fp.in,fname.input))

# Preparation of parameters
fTaxaParam <- strsplit(f[4],"\t")[[1]][2]
param <- read.table(file.path(fp.in, fTaxaParam))
param[,3]<- param[,2]   
param[,2]<- "Delta"      # all parameters with attribute "Delta" will not be varied in LHC

# all parameters with attribute "change" will be varied in LHC 
# here all sensitive parameters are chosen
param[123,2]<- "change"  # fbasal
param[125,2]<- "change"  # fIngest
param[127,2]<- "change"  # kDens
param[128,2]<- "change"  # FGrowthEgg
param[130,2]<- "change"  # fRepro
param[135,2]<- "change"  # clutchSize
param[141,2]<- "change"  # eggM
param[143,2]<- "change"  # adultM
param[145,2]<- "change"  # fMaxM

prior.tax <- param[2:nrow(param),]   #exclude first row

# In case previous parametervalues should be included, load table: 
#Tabelle_new<- read.csv("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/input/09_5spec_LHC_Uebersicht", header=F, sep = "\t")
# And include column:
#prior.tax [,3] <- Tabelle_new[,59]# Übernahme von 59 (Spalte 57+2) aus 09_5spec als V3 
# In case parameter values need to be changed, replace value:
#prior.tax[121,3] <- "50"

# Start LHC 
all.samp <- generate.par.samp.matrix.lhs(n.samp=n.sim,par.prior=prior.tax,minfac=min,maxfac=max) 
temp1<-rbind(1:n.sim,all.samp[,])
rownames(temp1)[1] <- "iSim"
fname.taxpara <- name.vec.out
write.table(temp1, file = file.path(fp.in,fname.taxpara), sep = "\t", row.names = F, col.names = F, quote = F) #param

# Get overview for all datasets
temp2<-cbind(prior.tax[,2],all.samp)
fname.taxpara2 <- name.vec.out2
write.table(temp2,file.path(fp.in,fname.taxpara2),sep="\t",row.names=T,col.names=F, quote = F)

# Note start time for model runs
start<- Sys.time() 

# Start model runs
for (j in 1:n){
  rand.seed <- 123+j
  name.vec2 <- paste("_Run_", j, sep = "")
  
  for (i in 1:n.sim){
    namestr <- paste(name.vec[i], name.vec2, sep = "")   
    fname.out     <- paste("results_",namestr,".txt",sep="")
    fname.taxpara <- name.vec.out
    
    outF<-file.path(fp.in,fname.input.out)
    outF<-file(outF,"w")
    writeLines(f[1:3],outF)
    writeLines(paste("fNameInputTaxParam:\t",fname.taxpara,sep=""),outF)
    writeLines(f[5:6],outF)
    writeLines(paste0("strSubOut:\t",namestr),outF)
    writeLines(paste("nSim:\t",1,sep=""),outF)
    #writeLines(paste("nRuns:\t",nRun,sep=""),outF)
    writeLines(f[8:length(f)],outF)
    flush(outF)
    close(outF)
    
    command <- "java"
    args <- c("-Xmx1G","-jar","IBM-Bugs.jar",fname.input.out, rand.seed)
    
    output_col <- temp1[,i]
    write.table(x = output_col,file.path(fp.in,fname.taxpara),sep="\t",row.names=T,col.names=F, quote = F) 
    system2(command=command,args=args)
    
    print(i) # Progress for i
  }
  print(j) # Progress for j
}
beep(3)  # End signal

# Get time needed
end<- Sys.time() # End time
(time<- end-start) 
