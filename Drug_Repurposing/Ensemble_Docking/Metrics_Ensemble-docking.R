library(tidyverse)
library(tools)
library(ggpubr)
library(scales)
library(ggplot2)


dockings_df <- read.csv("dockings.csv")[1:4]

count_na <- sum(is.na(dockings_df$Crystal))
cat("# of NAs in 'Crystal':", count_na, "\n")




# plotting docking energy in every frame
fra <- ggboxplot(dockings_df, x = "Crystal", y = "DockingScore", color = "Crystal", 
                 palette ="hue", 
                 xlab = "TRPV1 Crystallographic structures",
                 ylab = "Docking energy (kcal/mol)", )  +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 18),
    axis.title = element_text(color = "black", face = "bold", size = 18),
    axis.text.x = element_text(colour = "black", size = 11),
    axis.text.y = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black", size = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  ) 
fra

ggsave(file = "Boxplot_energy_frames.png",
       plot = fra, width = 10, height = 8, dpi = 300)

######################################################

library("dplyr")
library("psych")  

rank <- 1:562

##################################
# Geometric mean
geo_mean <- dockings_df %>% group_by(Molecule) %>%
  summarise(Geometric_mean = -exp(mean(log(abs(DockingScore))))) %>%
  arrange(Geometric_mean)

geo_mean$RankingGeometricMean <- rank

##################################
# Arithmetric mean
arit_mean <- dockings_df %>% group_by(Molecule) %>%
  summarise(Arithmetic_mean = mean(DockingScore)) %>%
  arrange(Arithmetic_mean)

arit_mean$RankingArithmeticMean <- rank

##################################
# Harmonic mean
harm_mean <- dockings_df %>% group_by(Molecule) %>%
  summarise(Harmonic_mean = harmonic.mean(DockingScore)) %>%
  arrange(Harmonic_mean)

harm_mean$RankingHarmonicMean <- rank

##################################
# Minimum value (best)
min <- dockings_df %>%                                    
  group_by(Molecule) %>%
  summarise(Minimum = min(DockingScore)) %>%
  arrange(Minimum)

min$RankingMinimumValue <- rank

##################################
# Maximum value (worst)
# max <- my_data_frame %>%                                    
#   group_by(ligand) %>%
#   summarise_at(vars(energy),
#                list(Maximum = max)) %>%
#   arrange(Maximum)

##################################
# median
med <- dockings_df %>% group_by(Molecule) %>%
  summarise(Median = median(DockingScore)) %>%
  arrange(Median)

med$RankingMedian <- rank

#################################
# Sort by ligand
g2 <- geo_mean %>% arrange(Molecule)
h2 <- harm_mean %>% arrange(Molecule)
a2 <- arit_mean %>% arrange(Molecule)
min2 <- min %>% arrange(Molecule)
med2 <- med %>% arrange(Molecule)

candidates <- cbind(g2, h2, a2, min2, med2)

candidates_rank <- subset(candidates, select = c(1, 3, 6, 9, 12, 15))

#####################################
modeFunction <- function(df) {
  library(dplyr)
  
  df %>% 
    mutate(Mode = apply(.[,2:6], 1, function(x) {
      tab <- table(x)
      if(length(tab) == 5) {
        max(x)
      } else {
        as.numeric(names(tab))[which.max(tab)]
      }
    }),
    FrequencyInRanking = apply(.[,2:6], 1, function(x) {
      tab <- table(x)
      if(length(tab) == 5) {
        paste0("1/", length(x))
      } else {
        paste0(max(tab), "/5")
      }
    }))
}

# Calling mode function
candidates_rank_modes <- modeFunction(candidates_rank)
candidates_rank_modes <- candidates_rank_modes %>%
  arrange(Mode)



########################################
# Saving the data in excel
library(openxlsx)
write.xlsx(candidates_rank_modes, 'candidates_rank_modes.xlsx')
write.xlsx(candidates, 'candidates_full_data.xlsx')
