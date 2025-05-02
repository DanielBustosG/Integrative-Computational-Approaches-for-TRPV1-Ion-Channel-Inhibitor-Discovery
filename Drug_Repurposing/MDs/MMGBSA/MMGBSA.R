library(lattice)
library(ggpubr)
library(reshape2)
library(matrixStats)
library(readxl)
library(dplyr)
library(openxlsx)

time <- read_excel("MMGBSA_whole.xlsx", sheet = "SB-366791")[,c(1)]

mmgbsa1 <- read_excel("MMGBSA_whole.xlsx", sheet = "AMG-9810")[,c(2:5)]
mmgbsa2 <- read_excel("MMGBSA_whole.xlsx", sheet = "CZP")[,c(2:5)]
mmgbsa_3 <- read_excel("MMGBSA_whole.xlsx", sheet = "CAP")[,c(2:5)]
mmgbsa3 <- mmgbsa_3[seq(1, nrow(mmgbsa_3), by = 2), ]
mmgbsa4 <- read_excel("MMGBSA_whole.xlsx", sheet = "FAD")[,c(2:5)]
mmgbsa5 <- read_excel("MMGBSA_whole.xlsx", sheet = "EVP4593")[,c(2:5)]
mmgbsa6 <- read_excel("MMGBSA_whole.xlsx", sheet = "L-694247")[,c(2:5)]
mmgbsa7 <- read_excel("MMGBSA_whole.xlsx", sheet = "VER-155008")[,c(2:5)]
mmgbsa8 <- read_excel("MMGBSA_whole.xlsx", sheet = "PI-103")[,c(2:5)]
mmgbsa9 <- read_excel("MMGBSA_whole.xlsx", sheet = "CYM-5442")[,c(2:5)]
mmgbsa10 <- read_excel("MMGBSA_whole.xlsx", sheet = "Rociletinib")[,c(2:5)]
mmgbsa11 <- read_excel("MMGBSA_whole.xlsx", sheet = "LY225910")[,c(2:5)]
mmgbsa12 <- read_excel("MMGBSA_whole.xlsx", sheet = "SC-51089")[,c(2:5)]
mmgbsa13 <- read_excel("MMGBSA_whole.xlsx", sheet = "Lemborexant")[,c(2:5)]
mmgbsa14 <- read_excel("MMGBSA_whole.xlsx", sheet = "KI-8751")[,c(2:5)]
mmgbsa15 <- read_excel("MMGBSA_whole.xlsx", sheet = "Olmesartan-medoxomil")[,c(2:5)]
mmgbsa16 <- read_excel("MMGBSA_whole.xlsx", sheet = "Stemregenin-1")[,c(2:5)]
mmgbsa17 <- read_excel("MMGBSA_whole.xlsx", sheet = "Ro-5126766")[,c(2:5)]
mmgbsa18 <- read_excel("MMGBSA_whole.xlsx", sheet = "MLN1117")[,c(2:5)]
mmgbsa19 <- read_excel("MMGBSA_whole.xlsx", sheet = "KG-5")[,c(2:5)]
mmgbsa20 <- read_excel("MMGBSA_whole.xlsx", sheet = "GW-583340")[,c(2:5)]
mmgbsa21 <- read_excel("MMGBSA_whole.xlsx", sheet = "G007-LK")[,c(2:5)]
mmgbsa22 <- read_excel("MMGBSA_whole.xlsx", sheet = "Tesaglitazar")[,c(2:5)]
mmgbsa23 <- read_excel("MMGBSA_whole.xlsx", sheet = "Vancomycin")[,c(2)]

##################
mmgbsa_list1 <- data.frame(valors=unlist(mmgbsa1))
mmgbsa_list2 <- data.frame(valors=unlist(mmgbsa2))
mmgbsa_list3 <- data.frame(valors=unlist(mmgbsa3))
mmgbsa_list4 <- data.frame(valors=unlist(mmgbsa4))
mmgbsa_list5 <- data.frame(valors=unlist(mmgbsa5))
mmgbsa_list6 <- data.frame(valors=unlist(mmgbsa6))
mmgbsa_list7 <- data.frame(valors=unlist(mmgbsa7))
mmgbsa_list8 <- data.frame(valors=unlist(mmgbsa8))
mmgbsa_list9 <- data.frame(valors=unlist(mmgbsa9))
mmgbsa_list10 <- data.frame(valors=unlist(mmgbsa10))
mmgbsa_list11 <- data.frame(valors=unlist(mmgbsa11))
mmgbsa_list12 <- data.frame(valors=unlist(mmgbsa12))
mmgbsa_list13 <- data.frame(valors=unlist(mmgbsa13))
mmgbsa_list14 <- data.frame(valors=unlist(mmgbsa14))
mmgbsa_list15 <- data.frame(valors=unlist(mmgbsa15))
mmgbsa_list16 <- data.frame(valors=unlist(mmgbsa16))
mmgbsa_list17 <- data.frame(valors=unlist(mmgbsa17))
mmgbsa_list18 <- data.frame(valors=unlist(mmgbsa18))
mmgbsa_list19 <- data.frame(valors=unlist(mmgbsa19))
mmgbsa_list20 <- data.frame(valors=unlist(mmgbsa20))
mmgbsa_list21 <- data.frame(valors=unlist(mmgbsa21))
mmgbsa_list22 <- data.frame(valors=unlist(mmgbsa22))
mmgbsa_list23 <- data.frame(valors=unlist(mmgbsa23))

combined_df <- rbind(mmgbsa_list1, mmgbsa_list2, mmgbsa_list3, mmgbsa_list4, 
                     mmgbsa_list5, mmgbsa_list6, mmgbsa_list7, mmgbsa_list8, 
                     mmgbsa_list9, mmgbsa_list10, mmgbsa_list11, mmgbsa_list12, 
                     mmgbsa_list13, mmgbsa_list14, mmgbsa_list15, mmgbsa_list16, 
                     mmgbsa_list17, mmgbsa_list18, mmgbsa_list19, mmgbsa_list20, 
                     mmgbsa_list21, mmgbsa_list22, mmgbsa_list23)

#data <- as.data.frame(cbind(time, combined_df))

lig <- c(rep("AMG", 400), rep("CZP", 400), rep("CAP", 400), 
         rep("CA1", 400), rep("CA2", 400), rep("CA3", 400), 
         rep("CA4", 400), rep("CA5", 400), rep("CA6", 400), rep("CA7", 400), 
         rep("CA8", 400), rep("CA9", 400), rep("CA10", 400), rep("CA11", 400), 
         rep("CA12", 400), rep("CA13", 400), rep("CA14", 400), rep("CA15", 400),
         rep("CA16", 400), rep("CA17", 400), rep("CA18", 400), rep("CA19", 400), 
         rep("CA20", 100))

data_final <- cbind(combined_df, lig)
#colnames(data_final) <- c("time", "MMGBSA", "Ligand")
colnames(data_final) <- c("MMGBSA", "Ligand")


#########################################################
data_final$Ligand <- factor(data_final$Ligand, levels = unique(data_final$Ligand))

plot2 <- ggboxplot(data_final, x = "Ligand", y = "MMGBSA", 
                   ylab = "MMGBSA",
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
  ) +
  scale_y_continuous(name = "MM-GBSA energy")
                     #limits = c(-100, -20),
                     #breaks = seq(from = -100, to = -20, by = 20))  

plot2

ggsave(file = "MMGBSA_boxplot.png",
       plot = plot2 , width = 9, height = 6, dpi = 300)

################
#STATS

resultados <- data_final %>%
  group_by(Ligand) %>%
  dplyr::summarise(
    Cuartil_1 = quantile(`MMGBSA`, 0.25),
    Mediana = median(`MMGBSA`),
    Cuartil_3 = quantile(`MMGBSA`, 0.75),
    Promedio = mean(`MMGBSA`),
    Desviacion_Estandar = sd(`MMGBSA`),
    #Datos_Menores_a_0 = sum(`MMGBSA` < 0),
    #Datos_Mayores_a_0 = sum(`MMGBSA` >= 0),
  )

resultados

resultados_df <- as.data.frame(resultados)
stats <- "Stats_MMGBSA.xlsx"
write.xlsx(resultados_df, stats)

