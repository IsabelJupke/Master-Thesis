# Master thesis Isabel Jupke 
# Display of popualtion dynamics (abundances over time)
# for all four scenarios

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung")

# Load library
library(tidyverse)

# Number of replications
n <- 100

# Load initial and final parameters for filterer
liste.tabellen1 <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Filterer_einzel_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") 
for (i in 1:n){
  liste.tabellen1[[i]] <- read.csv(name.file[i])  
}
liste.tabellen2 <- list() 
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Filterer_einzel_fin_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") #gibt die Dateinamen (inkl. Pfad)
for (i in 1:n){
  liste.tabellen2[[i]] <- read.csv(name.file[i])  #läd die Datein in eine Liste (theoretisch reicht dieser Schritt, dann muss man aber immer über die Liste auf die Tabelle zugreifen)
}

# Plot Nr 1
plot(liste.tabellen1[[1]][,3],liste.tabellen1[[1]][,5], type = "l", xlab = "Time [d]", 
     ylab = "Abundances", col= '#5B1A18' , ylim = c(0,100000)) #, main = paste("Einzellauf Filterer")
title("Filterer", adj = 0)
for (i in 1:n) {
  lines(liste.tabellen1[[i+1]][,5], col= '#5B1A18') # dunkel = vorher
  lines(liste.tabellen2[[i]][,5], col= '#240A09AF')
}

# Load initial and final parameters for Grazer
liste.tabellen1 <- list() 
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Grazer_einzel_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") 
for (i in 1:n){
  liste.tabellen1[[i]] <- read.csv(name.file[i])  
}
liste.tabellen2 <- list() 
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Grazer_einzel_fin_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") 
for (i in 1:n){
  liste.tabellen2[[i]] <- read.csv(name.file[i])  
}

# Plot Nr 2
plot(liste.tabellen1[[1]][,3],liste.tabellen1[[1]][,5], type = "l", xlab = "Time [d]", 
     ylab = "Abundances", col= '#46ACC8' , ylim = c(0,150)) #, main = paste("Einzellauf Filterer")
title("Grazer", adj = 0)
for (i in 1:n) {
  lines(liste.tabellen1[[i+1]][,5], col= '#46ACC8')
  lines(liste.tabellen2[[i]][,5], col= "#B5DDE9") #nachher
}

# Plot initial and final parameters for shredder
liste.tabellen1 <- list() 
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Shredderer_einzel_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") #gibt die Dateinamen (inkl. Pfad)
for (i in 1:n){
  liste.tabellen1[[i]] <- read.csv(name.file[i])  #läd die Datein in eine Liste (theoretisch reicht dieser Schritt, dann muss man aber immer über die Liste auf die Tabelle zugreifen)
}
liste.tabellen2 <- list() #muss als Speicherort für den loop erstellt werden
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_Shredderer_einzel_fin_", 
                   append(paste(0,1:9,sep = ""),10:n),".csv", sep= "") #gibt die Dateinamen (inkl. Pfad)
for (i in 1:n){
  liste.tabellen2[[i]] <- read.csv(name.file[i])  #läd die Datein in eine Liste (theoretisch reicht dieser Schritt, dann muss man aber immer über die Liste auf die Tabelle zugreifen)
}

# Plot Nr 3
plot(liste.tabellen1[[1]][,3],liste.tabellen1[[1]][,5], type = "l", xlab = "Time [d]", 
     ylab = "Abundances", col= "#b4a800" , ylim = c(0,300)) #, main = paste("Einzellauf Filterer")
title("Shredder", adj = 0)
for (i in 1:n) {
  lines(liste.tabellen1[[i+1]][,5], col= "#b4a800" )
  lines(liste.tabellen2[[i]][,5], col= "#e7db32") #nachher
}

# Save plot
savePlot(filename = "C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung/R_plots/Vgl_alle_01", type = "jpeg")
dev.off()

# Create legend
windows()
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("topleft", legend = c("Filterer initial ", "Filterer final", "Grazer initial", "Grazer final", "Shredder initial", "Shredder final"), pch=16,pt.cex=3, cex=1.2, bty='n',
       col= c('#5B1A18','#240A09AF', '#46ACC8', "#B5DDE9", "#b4a800", "#e7db32"), title = "Species")

# Save legend
savePlot(filename = "C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung/fertige Plots/Vgl_alle_Legend", type = "jpeg")
dev.off()