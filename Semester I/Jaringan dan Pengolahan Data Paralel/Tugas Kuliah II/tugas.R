rm(list=ls())
library(dplyr)
library(ggplot2)
library(readxl)

judul = "Rekap Perbandingan Runtime Antar Server dan Antar Metode MPI"
subtitle = "Dikerjakan oleh 20921004@mahasiswa.itb.ac.id"
caption = "Visualisasi dengan ggplot"

data = read_excel("run paralel.xlsx")
plt = 
  data %>% 
  mutate(label = round(waktu,2)) %>% 
  ggplot(aes(x = jenis,
             y = waktu,
             fill = proc)) +
  geom_col(width = .7,
           color = "black") +
  geom_label(aes(label = label),
             color = "white",
             size = 4) +
  facet_wrap(~proc,ncol = 2,nrow = 1) +
  theme_minimal() +
  labs(title = judul,
       subtitle = subtitle,
       caption = caption,
       y = "Waktu (dalam satuan detik)") +
  theme(plot.title = element_text(face = "bold",size = 22),
        plot.subtitle = element_text(face = "bold",size = 17),
        strip.text = element_text(size = 15,face = "bold"),
        strip.background = element_rect(size = .5),
        legend.position = "none",
        axis.text.y = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue","darkgreen"))

ggsave("jawab.png",plt,dpi = 450, width = 16, height = 9)
