setwd("~/209_ITB/Thesis/Penelitian Mandiri III dan IV/Algoritma Grafik/Output")

library(dplyr)
library(ggforce)
library(reshape2)

rm(list=ls())

jadwal = readxl::read_excel("~/209_ITB/Thesis/Penelitian Mandiri III dan IV/Algoritma Grafik/Input/ppic.xlsx", 
                            sheet = "jadwal all")

plt = 
  jadwal %>% 
  ggplot(aes(x = minggu, label = label, fill = minggu)) +
  
  annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = 1, color = "lightblue") +
  annotate("segment", x = 1, xend = 2, y = 0, yend = 0, size = 1, color = "blue") +
  annotate("segment", x = 2, xend = 3, y = 0, yend = 0, size = 1, color = "darkblue") +
  annotate("segment", x = 3, xend = 4, y = 0, yend = 0, size = 1, color = "steelblue") +
  annotate("segment", x = 4, xend = 5, y = 0, yend = 0, size = 1, color = "green") +
  annotate("segment", x = 5, xend = 6, y = 0, yend = 0, size = 1, color = "black") +
  annotate("segment", x = 6, xend = 7, y = 0, yend = 0, size = 1, color = "darkred") +
  
  geom_point(aes(y = -.1), size = 2, shape = 21, stroke = 1.5, fill = "white") +
  
  geom_label(aes(y = 0, label = label_minggu),
             fill = "black",
             color = "white") +
  
  geom_mark_circle(aes(y = 0, label = NA, description = label, fill = event), 
                   con.cap = -1, fill = NA, color = NA, label.fontsize = 10, label.hjust = 0,
                   label.fill = NA,
                   con.size = .5, con.border = "one",
                   con.type = "elbow") +

  labs(title = "Timeline Raw Material Procurement",
       subtitle = "The horizontal line indicates the week.",
       caption = "Production starts in the third week.\nHowever, since the raw materials began to be shipped in the first week, we must consider the warehouse capacity.\nAs well as in the second week.\nTherefore, the first and the second weeks will be used as a parameter in the model.")+
  
  theme_void() +
  
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25, family = "Patua"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 22),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "black", family = "Lato")) +
  xlim(0,7)+ ylim(-130,130) 

png("timeline.png", width = 15, height = 9, units = "in",res = 900)
print(plt)
dev.off()