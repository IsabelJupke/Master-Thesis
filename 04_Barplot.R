# Master thesis Isabel Jupke 
# Barplots for exticntion rates
# for all four scenarios

# Set working directory
setwd("C:/Users/isabe/Desktop/Masterarbeit/05 Auswertung")

# Load library 
library(ggplot2)
library(dplyr)
library(wesanderson)

# Create color palette
wpd1  <- wes_palette("Darjeeling1")
wpd2  <- wes_palette("Darjeeling2")
ff    <- wes_palette("FantasticFox1")
wpc   <- wes_palette("Chevalier1")
wpbg  <- wes_palette("GrandBudapest1")
colors <- c(wpd1[3],ff[3],ff[2], wpd2[4], wpd1[2], wpbg[2], wpc[4], wpbg[3]) %>% rev()

# Create table with obtained extinction rates for all four scenarios
Percentage<- c(67.21, 32.68, 0.11,92.79, 6.56,  0.65, 94.53, 5.45, 0.02, 66.26, 22.97, 10.77)
Species<- c( "Predator","Predator and Gatherer", "Gatherer", "Filterer", "Predator and Filterer","Predator","Grazer","all species", "Grazer and Predator", "Shredder", "Shredder and Grazer", "Grazer")
Scenario<- c(rep("Scenario 1", times= 3),rep("Scenario 2", times= 3), rep("Scenario 3", times= 3), rep("Scenario 4", times= 3) )
df<- cbind(Scenario, Species, Percentage)
df2<- as.data.frame(df)
df2[,3]<- as.numeric(as.character(df2[,3]))


# Order species (with extinction rates >1%)
order1 <- c("Predator", "Grazer", "Shredder", "Predator and Gatherer",
            "Shredder and Grazer", "all species", "Predator and Filterer", "Filterer")
order2 <- rev(order1)

# Create tags (for extinction rates >1%)
Percentage2<- c("67.21%", "32.68%","92.79%", "6.56%", "94.53%", "5.45%", "66.26%", "22.97%", "10.77%")
windows()

# Order Species and plot barplot
df2$Species <- factor(df2$Species, levels = order2)
df2 %>%
  filter(Percentage>=1) %>%
  ggplot( aes(x= Scenario, y= Percentage, fill= Species), theme_set(theme_bw())) +
  geom_bar(stat = "identity", color = "black", size = 0.7)  
  scale_fill_manual(values = colors) +
  geom_text(aes(label=Percentage2), size = 6,vjust = c(8,4,1.2,8,8,1.2,1.5,8,3), position ="stack") + 
  theme_classic()+
  theme(axis.title=element_text(size=14),axis.text=element_text(size=14)) 
  ylab("Extinction Rates [%]") + xlab("")

# Save plot
savePlot(filename = "/Users/isabe/Desktop/Masterarbeit/05 Auswertung/fertige Plots/Barplot", type = "jpeg")
dev.off()




