rm(list=ls())

library(grid)
library(gridExtra)
library(readxl)
library(dplyr)

gridFtable <- function(d, pd = 5, fontsize = 11, fontfamily = "PT Mono") {
  
  ## set plot theme
  t1 <- ttheme_default(padding = unit(c(pd, pd), "mm"), 
                       base_size = fontsize, 
                       base_family = fontfamily)
  
  ## character table with added row and column names
  extended_matrix <- cbind(c("", rownames(d)), rbind(colnames(d), as.matrix(d)))
  
  ## get grob values
  g <- tableGrob(extended_matrix, theme = t1)
  
  ## convert widths from grobwidth to inch
  widthsIn <- lapply(g$widths, function(x) {convertUnit(x, unitTo = "inch", valueOnly = TRUE)})
  heigthsIn <- lapply(g$heights, function(x) {convertUnit(x, unitTo = "inch", valueOnly = TRUE)})
  
  ## calculate width and height of the table
  w <- sum(unlist(widthsIn)) - .6*convertUnit(unit(pd, "mm"), unitTo = "inch", valueOnly = TRUE)
  h <- sum(unlist(heigthsIn)) - .6*convertUnit(unit(pd, "mm"), unitTo = "inch", valueOnly = TRUE)
  
  return(list(grobData = g, data = d, width = w, heigth = h, theme = t1))
}

setwd("~/209_ITB/Thesis/MethodX/Manuscript")

df = data.frame(
  raw = c(1:6,"Total"),
  x1  = c("300,340","383,947","68,515","155,542","122,533","108,218","1,139,094"),
  x2  = c("363,486","532,404","20,402","60,124","16,888","145,791","1,139,094"),
  x3  = c("192,649","543,385","90,742","56,508","52,958","202,853","1,139,094")
)

colnames(df)[1]   = "Raw material"
colnames(df)[2:4] = paste0("Optimal Solution from\nobj function ",1:3)


df_n = df

saveTable <- gridFtable(df_n)

setwd("~/209_ITB/Thesis/MethodX")

png(file   = "./obj func.png", 
    width  = saveTable$width, 
    height = saveTable$heigth, 
    units  = "in", res = 100)
grid.newpage()
grid.table(saveTable$data, rows = NULL, theme = saveTable$theme)
dev.off()
getwd()
