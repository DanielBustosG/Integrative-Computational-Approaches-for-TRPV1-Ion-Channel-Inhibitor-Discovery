library(readxl)
library(ggplot2)


data <- readxl::read_excel("dockings_per_crystal.xlsx", sheet = "table")

total <- 3048
data$percentage <- (data$amount / total) * 100

gg <- ggplot(data, aes(x = crystal, y = amount)) +
  geom_bar(stat = "identity", fill = "salmon1", color = "black", width = 0.8, linewidth = 0.2) +
  #geom_text(aes(label = amount), vjust = -0.9, size = 4) +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.9, size = 4) +
  labs(title = "Amount of compounds at the vanilloide site",
       x = "TRPV1 PDB ids",
       y = "Amount of compounds") +
  theme_minimal()  +
  theme(axis.text.x = element_text(color = "black", size = 12), 
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(size = 16, face = "bold", color = "black"),
        axis.ticks.y = element_line(colour = "black", size = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 3500, by = 500), 
                     limits = c(0, 3500))

print(gg)

ggsave("Figura2.png", width = 9, height = 5, bg = "white")


