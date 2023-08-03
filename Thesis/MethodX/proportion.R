rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggpattern)

df = data.frame(
  raw    = c(1:6),
  obj_1  = c(300340,383947,68515,155542,122533,108218),
  obj_2  = c(363486,532404,20402,60124,16888,145791),
  obj_3  = c(192649,543385,90742,56508,52958,202853),
  fix    = c(7956000,19944000,648000,2952000,288000,4212000)
)


price = c(16546,15504,18110,14717,19800,15193)

df %>% 
  mutate(f1 = obj_1 * price,
         f2 = obj_2 * price,
         f3 = obj_3 * price) %>% 
  summarise_all(sum) %>% 
  

df %>% 
  reshape2::melt(id.vars = "raw") %>% 
  group_by(variable) %>% 
  mutate(persen = value/sum(value)*100,
         persen = round(persen,1)) %>% 
  ungroup() %>% 
  mutate(label = paste0(persen,"%")) %>% 
  mutate(variable = case_when(
    variable == "obj_1" ~ "Obj Func 1",
    variable == "obj_2" ~ "Obj Func 2",
    variable == "obj_3" ~ "Obj Func 3",
    variable == "fix"   ~ "One-year min order"
  )) %>% 
  mutate(variable = factor(variable,levels = c("One-year min order",
                                               "Obj Func 1",
                                               "Obj Func 2",
                                               "Obj Func 3"))) %>% 
  ggplot(aes(x = variable,
             y = persen,
             fill = factor(raw))) +
  geom_bar(position="stack", stat="identity",color = "black") +
  ggrepel::geom_label_repel(aes(label = label),position = position_stack(vjust = 0.5),
                            size = 4) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Raw Material Proportion Comparation",
       subtitle = "between one-year min order and 3rd objective function result",
       y = "Percentage",
       fill = "Raw Material") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 17),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  ggsave("proportion.png",
         width = 14,
         height = 8,
         dpi = 550)
