library(lattice)
library(ggpubr)
library(reshape2)
library(matrixStats)
library(readxl)
library(dplyr)
library(openxlsx)

time <- read_excel("RMSD_whole.xlsx", sheet = "SB-366791")[,c(1)]

rmsd1 <- read_excel("RMSD_whole.xlsx", sheet = "AMG-9810")[,c(2:5)]
rmsd_2 <- read_excel("RMSD_whole.xlsx", sheet = "CZP")[,c(2:5)]
rmsd2 <- rmsd_2[seq(1, nrow(rmsd_2), by = 2), ]
rmsd3 <- read_excel("RMSD_whole.xlsx", sheet = "CAP")[,c(2:5)]
rmsd4 <- read_excel("RMSD_whole.xlsx", sheet = "FAD")[,c(2:5)]
rmsd5 <- read_excel("RMSD_whole.xlsx", sheet = "EVP4593")[,c(2:5)]
rmsd6 <- read_excel("RMSD_whole.xlsx", sheet = "L-694247")[,c(2:5)]
rmsd7 <- read_excel("RMSD_whole.xlsx", sheet = "VER-155008")[,c(2:5)]
rmsd8 <- read_excel("RMSD_whole.xlsx", sheet = "PI-103")[,c(2:5)]
rmsd9 <- read_excel("RMSD_whole.xlsx", sheet = "CYM-5442")[,c(2:5)]
rmsd10 <- read_excel("RMSD_whole.xlsx", sheet = "Rociletinib")[,c(2:5)]
rmsd11 <- read_excel("RMSD_whole.xlsx", sheet = "LY225910")[,c(2:5)]
rmsd12 <- read_excel("RMSD_whole.xlsx", sheet = "SC-51089")[,c(2:5)]
rmsd13 <- read_excel("RMSD_whole.xlsx", sheet = "Lemborexant")[,c(2:5)]
rmsd14 <- read_excel("RMSD_whole.xlsx", sheet = "KI-8751")[,c(2:5)]
rmsd15 <- read_excel("RMSD_whole.xlsx", sheet = "Olmesartan-medoxomil")[,c(2:5)]
rmsd16 <- read_excel("RMSD_whole.xlsx", sheet = "Stemregenin-1")[,c(2:5)]
rmsd17 <- read_excel("RMSD_whole.xlsx", sheet = "Ro-5126766")[,c(2:5)]
rmsd18 <- read_excel("RMSD_whole.xlsx", sheet = "MLN1117")[,c(2:5)]
rmsd19 <- read_excel("RMSD_whole.xlsx", sheet = "KG-5")[,c(2:5)]
rmsd20 <- read_excel("RMSD_whole.xlsx", sheet = "GW-583340")[,c(2:5)]
rmsd21 <- read_excel("RMSD_whole.xlsx", sheet = "G007-LK")[,c(2:5)]
rmsd22 <- read_excel("RMSD_whole.xlsx", sheet = "Tesaglitazar")[,c(2:5)]
rmsd23 <- read_excel("RMSD_whole.xlsx", sheet = "Vancomycin")[,c(2)]

##################
rmsd_list1 <- data.frame(valors=unlist(rmsd1))
rmsd_list2 <- data.frame(valors=unlist(rmsd2))
rmsd_list3 <- data.frame(valors=unlist(rmsd3))
rmsd_list4 <- data.frame(valors=unlist(rmsd4))
rmsd_list5 <- data.frame(valors=unlist(rmsd5))
rmsd_list6 <- data.frame(valors=unlist(rmsd6))
rmsd_list7 <- data.frame(valors=unlist(rmsd7))
rmsd_list8 <- data.frame(valors=unlist(rmsd8))
rmsd_list9 <- data.frame(valors=unlist(rmsd9))
rmsd_list10 <- data.frame(valors=unlist(rmsd10))
rmsd_list11 <- data.frame(valors=unlist(rmsd11))
rmsd_list12 <- data.frame(valors=unlist(rmsd12))
rmsd_list13 <- data.frame(valors=unlist(rmsd13))
rmsd_list14 <- data.frame(valors=unlist(rmsd14))
rmsd_list15 <- data.frame(valors=unlist(rmsd15))
rmsd_list16 <- data.frame(valors=unlist(rmsd16))
rmsd_list17 <- data.frame(valors=unlist(rmsd17))
rmsd_list18 <- data.frame(valors=unlist(rmsd18))
rmsd_list19 <- data.frame(valors=unlist(rmsd19))
rmsd_list20 <- data.frame(valors=unlist(rmsd20))
rmsd_list21 <- data.frame(valors=unlist(rmsd21))
rmsd_list22 <- data.frame(valors=unlist(rmsd22))
rmsd_list23 <- data.frame(valors=unlist(rmsd23))

combined_df <- rbind(rmsd_list1, rmsd_list2, rmsd_list3, 
                     rmsd_list4, rmsd_list5, rmsd_list6, rmsd_list7,
                     rmsd_list8, rmsd_list9, rmsd_list10, rmsd_list11,
                     rmsd_list12, rmsd_list13, rmsd_list14, rmsd_list15,
                     rmsd_list16, rmsd_list17, rmsd_list18, rmsd_list19,
                     rmsd_list20, rmsd_list21, rmsd_list22, rmsd_list23)

#data <- as.data.frame(cbind(time, combined_df))

lig <- c(rep("AMG", 808), rep("CZP", 808), rep("CAP", 808), rep("CA1", 808),
         rep("CA2", 808), rep("CA3", 808), rep("CA4", 808), rep("CA5", 808), 
         rep("CA6", 808), rep("CA7", 808), rep("CA8", 808), rep("CA9", 808), 
         rep("CA10", 808), rep("CA11", 808), rep("CA12", 808), rep("CA13", 808), 
         rep("CA14", 808), rep("CA15", 808), rep("CA16", 808), rep("CA17", 808),
         rep("CA18", 808), rep("CA19", 808), rep("CA20", 202))

data_final <- cbind(combined_df, lig)
#colnames(data_final) <- c("Time", "RMSD", "Ligand")
colnames(data_final) <- c("RMSD", "Ligand")


#########################################################
data_final$Ligand <- factor(data_final$Ligand, levels = unique(data_final$Ligand))

plot2 <- ggboxplot(data_final, x = "Ligand", y = "RMSD", 
                   ylab = "RMSD",
                   xlab = "Candidates")+
  geom_boxplot(size = 0.3) +
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(color = "black", face = "bold", size = 16),
    axis.text.x = element_text(colour = "black", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks = element_line(colour = "black", size = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )+
  scale_y_continuous(name = "RMSD")
                     #limits = c(0, 10.5),
                     #breaks = seq(from = 0, to = 10.5, by = 1.5))  

plot2

ggsave(file = "RMSD_boxplot.png",
       plot = plot2 , width = 9, height = 6, dpi = 300)

################
#STATS

resultados <- data_final %>%
  group_by(Ligand) %>%
  dplyr::summarise(
    Cuartil_1 = quantile(`RMSD`, 0.25),
    Mediana = median(`RMSD`),
    Cuartil_3 = quantile(`RMSD`, 0.75),
    Promedio = mean(`RMSD`),
    Desviacion_Estandar = sd(`RMSD`)
    #Datos_Menores_a_0 = sum(`RMSD` < 0),
    #Datos_Mayores_a_0 = sum(`RMSD` >= 0),
  )

resultados

resultados_df <- as.data.frame(resultados)
stats <- "Stats_RMSD.xlsx"
write.xlsx(resultados_df, stats)

