# Master thesis Isabel Jupke 
# Display of popualtion dynamics (abundances over time)
# Example: Scenario 4 (progressive adding of species), includes all 5 FFGs

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung")

# Set number of replications and datasets
n <- 1       # Replications
n.sim <- 100 # Datasets
m <- n* n.sim

# Load files for Scenario 4 in empty list (only replication nr1 is loaded for every dataset)
liste.tabellen <- list()
name.file <- paste("C:/Users/isabe/Desktop/Masterarbeit/03 IBM Modell/03 IBM-Bugs/output/out_08_5spec_LHC_", 
                  append(paste(0,rep(1:9,each = n),sep = ""),rep(10:n.sim, each = n)),"_Run_1",".csv", sep= "") 
for (i in 1:m){
  liste.tabellen[[i]] <- read.csv(name.file[i])  
}

# Plot all five graphs in one window
windows(30,25)
par(mfrow = c(5, 1))  

# Graph for gatherer
par(mai=c(0.5,0.8,0.2,0.1))
plot(liste.tabellen[[1]][,3],liste.tabellen[[1]][,5], type = "l", xlab="",
     ylab = "Abundance", col= "#D6723678" , ylim = c(0,10000), 
     cex.main = 2, cex.axis=1.5, cex.lab= 1.5)  
title("Scenario 4", adj = 0)
for (i in 1:m) {
  lines(liste.tabellen[[i]][,5], col= "#D6723678")
}

# Graph for predator
par(mai=c(0.5,0.8,0.2,0.1))
plot(liste.tabellen[[1]][,3],liste.tabellen[[1]][,6], type = "l", xlab = "",
     ylab = "Abundance", col= "#F2AD0078" , ylim = c(0,500),
     cex.axis= 1.5, cex.lab= 1.5) 
for (i in 1:m) {
  lines(liste.tabellen[[i]][,6], col= "#F2AD0078")
}

# Graph for filterer
par(mai=c(0.5,0.8,0.2,0.1))
plot(liste.tabellen[[1]][,3],liste.tabellen[[1]][,7], type = "l", xlab = "",
     ylab = "Abundance", col= "#240A0978" , ylim = c(0,350000),
     cex.axis= 1.5, cex.lab= 1.5) 
for (i in 1:m) {
  lines(liste.tabellen[[i]][,7], col= "#240A0978")
}

# Graph for grazer
par(mai=c(0.5,0.8,0.2,0.1))
plot(liste.tabellen[[1]][,3],liste.tabellen[[1]][,8], type = "l", xlab = "", 
     ylab = "Abundance", col= "#46ACC878" , ylim = c(0,500),
     cex.axis= 1.5, cex.lab= 1.5) 
for (i in 1:m) {
  lines(liste.tabellen[[i]][,8], col= "#46ACC878")
}

# Graph for shredder
par(mai=c(0.55,0.8,0.2,0.1))
plot(liste.tabellen[[1]][,3],liste.tabellen[[1]][,9], type = "l", xlab = "Time [d]", 
     ylab = "Abundance", col= "#E2D200" , ylim = c(0,500),
     cex.axis= 1.5, cex.lab= 1.5) 
for (i in 1:m) {
  lines(liste.tabellen[[i]][,9], col= "#E2D200")
}

# Save plot
savePlot(filename = "C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung/fertige Plots/5spec_Popdyn_01", type = "jpeg")
dev.off()

# Create legend from empty plot
windows()
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("topleft", legend = c("Gatherer ", "Predator", "Filterer", "Grazer", "Shredder"), pch=16,pt.cex=3, cex=1.2, bty='n',
       col= c("#D67236","#F2AD00", "#5B1A18", "#46ACC878", "#E2D200"), title = "Species")
# Save legend
savePlot(filename = "C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung/fertige Plots/5spec_Legend", type = "jpeg")
dev.off()