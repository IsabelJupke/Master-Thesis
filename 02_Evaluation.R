# Master thesis Isabel Jupke 
# Evalutaion of model results
# Example: Secnario 4 (including all five FFGs)

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung")

# Load library
library(tidyverse)

# Set number of replications, datasets and species
n.sim<-100 # Datasets
n <- 50    # Replications
m <- n* n.sim
x <- 5     # Number of species

# Set name for final table (coexistence rates and corresponding parameters)
name.ueber <- "5spec_08_Koex"

# Load all files for Scenario 4 in empty list 
run.name <- paste("_Run_",1:n, sep = "")
liste.tabellen <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_10_5spec_LHC_", 
                   append(paste(0,rep(1:9,each = n),sep = ""),rep(10:n.sim, each = n)),run.name[rep(c(1:n), 
                  times = n)],".csv", sep= "") 
for (i in 1:m){
  liste.tabellen[[i]] <- read.csv(name.file[i])  
}


# Preprare empty tibbles
prey.tibble <- pred.tibble <- filt.tibble <- graz.tibble <- shred.tibble <-tibble()

# Evaluate every species (extinction)
# If the species survives the entire model run, a 1 is placed in tibble.
# If the species does not survive the model run, a 0 and the timestep of extinction is placed. 

# Gatherer
for(list.element in 1:length(liste.tabellen)){
  red.prey <- liste.tabellen[[list.element]][(liste.tabellen[[list.element]]$prey_.detectionLimTotal==0) & ( liste.tabellen[[list.element]]$timestep>1) ,]
  prey.tibble[list.element,"Functional Group"] = "Prey"
  if  (nrow(red.prey) == 0){
    prey.tibble[list.element,"Survival"] = 1  
    prey.tibble[list.element,"Timestep"] = NA
  }else{
    prey.tibble[list.element,"Survival"] = 0
    prey.tibble[list.element,"Timestep"] = red.prey[1,"timestep"]
  }
}

# Predator
for(list.element in 1:length(liste.tabellen)){
  red.pred <- liste.tabellen[[list.element]][(liste.tabellen[[list.element]]$predator_.detectionLimTotal==0) & ( liste.tabellen[[list.element]]$timestep>1186) ,]
  pred.tibble[list.element,"Functional Group"] = "Predator" 
  if  (nrow(red.pred) == 0){
    pred.tibble[list.element,"Survival"] = 1  
    pred.tibble[list.element,"Timestep"] = NA
  }else{
    pred.tibble[list.element,"Survival"] = 0
    pred.tibble[list.element,"Timestep"] = red.pred[1,"timestep"]
  }
}


# Filterer
for(list.element in 1:length(liste.tabellen)){
  red.filt <- liste.tabellen[[list.element]][(liste.tabellen[[list.element]]$filterer_.detectionLimTotal==0) & ( liste.tabellen[[list.element]]$timestep>1) ,]
  filt.tibble[list.element,"Functional Group"] = "Filterer" 
  if  (nrow(red.filt) == 0){
    filt.tibble[list.element,"Survival"] = 1  
    filt.tibble[list.element,"Timestep"] = NA
  }else{
    filt.tibble[list.element,"Survival"] = 0
    filt.tibble[list.element,"Timestep"] = red.filt[1,"timestep"]
  }
}

# Grazer
for(list.element in 1:length(liste.tabellen)){
  red.graz <- liste.tabellen[[list.element]][(liste.tabellen[[list.element]]$grazer_.detectionLimTotal==0) & ( liste.tabellen[[list.element]]$timestep>1) ,]
  graz.tibble[list.element,"Functional Group"] = "Grazer" 
  if  (nrow(red.graz) == 0){
    graz.tibble[list.element,"Survival"] = 1  
    graz.tibble[list.element,"Timestep"] = NA
  }else{
    graz.tibble[list.element,"Survival"] = 0
    graz.tibble[list.element,"Timestep"] = red.graz[1,"timestep"]
  }
}

# Shredder
for(list.element in 1:length(liste.tabellen)){
  red.shred <- liste.tabellen[[list.element]][(liste.tabellen[[list.element]]$shredderer_.detectionLimTotal==0) & ( liste.tabellen[[list.element]]$timestep>1) ,]
  shred.tibble[list.element,"Functional Group"] = "Shredder" 
  if  (nrow(red.shred) == 0){
    shred.tibble[list.element,"Survival"] = 1  
    shred.tibble[list.element,"Timestep"] = NA
  }else{
    
    shred.tibble[list.element,"Survival"] = 0
    shred.tibble[list.element,"Timestep"] = red.shred[1,"timestep"]
  }
}

# Include numbers of dataset and replicate (here: "run")
prey.tibble$DataSet <- pred.tibble$DataSet <- filt.tibble$DataSet <- graz.tibble$DataSet <- shred.tibble$DataSet  <- rep(1:n.sim, each = n) # Dataset
prey.tibble$Run <- pred.tibble$Run <- filt.tibble$Run <- graz.tibble$Run <- shred.tibble$Run  <-rep(1:n, times = n.sim)                     # Replications

# Combine all to get final table (tibble)
all.tibble <- prey.tibble %>% bind_rows(pred.tibble) %>% bind_rows(filt.tibble) %>% bind_rows(graz.tibble) %>% bind_rows(shred.tibble) %>% arrange(DataSet, Run) 

# Calculation of coexistence rates for every dataset
liste.surv <- list()
liste.wert <- data.frame()
for (i in 1:n.sim) {
  set <- filter(all.tibble,DataSet==i)
  for (j in 1:n) {
    liste.wert [j,1] <- j
    subset <- filter(set, Run == j)
    if (sum(subset$Survival) ==x) {  
      liste.wert[j,2] <- 1
    } else {
      liste.wert[j,2] <- 0
    }
  }
  liste.surv [[i]] <- liste.wert
}
liste.ueber<- c()
for (k in 1:n.sim) {
  set2 <- liste.surv[[k]][,2]
  liste.ueber [k] <- sum(set2)/n *100
} 

# Dataset(s) with highest coexistence rate
which(liste.ueber == max(liste.ueber)) 

# Overall coexistence rate
sum(liste.ueber)/n.sim


# Load coexistence rate in empty list 
liste.koex1<- list()
for (k in 1:n.sim) {
  set2 <- liste.surv[[k]][,2]
  liste.koex1 [k] <- sum(set2)/n *100
}

# Load corresponding overview table
Tabelle_ueber<- read.csv("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/input/10_5spec_LHC_Uebersicht", header=F, sep = "\t")

# Adjustments for new table and get final table
koex.komplett<-  c("Koexrate", "Delta", liste.koex1)
koex.tab<-data.frame(koex.komplett)
anzahl<- n.sim+2
name.vec.tab<- paste("V",1:anzahl, sep = "")
names(koex.tab)<- name.vec.tab
Tab_koex_fin<- rbind(koex.tab, Tabelle_ueber)


# Wirte final table in "Auswertung/ Daten"
fp.in <- "Daten"
write.table(Tab_koex_fin, file = file.path(fp.in,name.ueber), sep = "\t", 
            row.names = F, col.names = F, quote = F) 
# --> proceed table in next Script: Scatterplots

# Calculate extinction rates
all.data <- as.data.frame(all.tibble)
all.data[,6] <- rep(1:(n*n.sim), each = 5)

# All extinction happenings 
liste.b <- c()
for(i in 1:nrow(all.data)){
  if (all.data[i,2] == 0 ){       
    liste.b[i] <- all.data[i,6]
  } 
}
liste.pur<- na.omit(liste.b) 
(b <-length(liste.pur)) #4625

# All extinctions but multiple extinction for one run are excluded
doubbles.list <- unique(liste.pur)
(a <- length(doubbles.list)) #3761

# Calculate cases with multiple extinction happenings
(c<- b-a) #864 

# Calculate cases for shredder
liste5 <- c()
for(i in 1:nrow(all.data)){
  if ( all.data[i,1] == "Shredder" & all.data[i,2] == 0 ){      
    liste5[i] <- i
  }
}

liste.pur5<- na.omit(liste5)
(v <-length(liste.pur5))  #3356

# Calculate cases for grazer
liste4 <- c()
for(i in 1:nrow(all.data)){
  if ( all.data[i,1] == "Grazer" & all.data[i,2] == 0 ){      
    liste4[i] <- i
  }
}

liste.pur4<- na.omit(liste4)
(w <-length(liste.pur4)) # 1269

# Calculate cases for  filterer
liste3 <- c()
for(i in 1:nrow(all.data)){
  if ( all.data[i,1] == "Filterer" & all.data[i,2] == 0 ){      
    liste3[i] <- i
  }
}

liste.pur3<- na.omit(liste3)
(z <-length(liste.pur3)) #0 

# Calculate cases for predator
liste <- c()
for(i in 1:nrow(all.data)){
  if ( all.data[i,1] == "Predator" & all.data[i,2] == 0 ){      
    liste[i] <- all.data[i,6]
  }
}

liste.pur<- na.omit(liste)
(x <-length(liste.pur)) #0

# Calculate cases for gatherer
liste2 <- c()
for(i in 1:nrow(all.data)){
  if ( all.data[i,1] == "Prey" & all.data[i,2] == 0 ){      
    liste2[i] <- i
  }
}

liste.pur2<- na.omit(liste2)
(y <-length(liste.pur2)) #0
