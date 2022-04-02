rm(list=ls())

library(dplyr)

# ========================================================
# simmilarity dan dissimilarity
# contoh
df = data.frame(roll = 1:4,
                marks = c(96,87,83,96),
                grades = c("A","B","B","A"))

df

# untuk nominal
grades = df$grades
mat_jarak = matrix(nrow = nrow(df),
                   ncol = nrow(df))

for(i in 1:nrow(df)){
  for(j in 1:nrow(df)){
    jarak = sum(grades[i] != grades[j])
    mat_jarak[i,j] = jarak
  }
}
df
mat_jarak

# SMC - simple matching coefficient
  # smc = (number of matches) / (number of attributes)
smc = function(x,y){
  sama = sum(x == y)
  all = length(x)
  sama/all
}

# contoh data
x = c(1,0,0,0,0,0,0,0,0,0)
y = c(0,0,0,0,0,0,1,0,0,1)

# hitung SMC
smc(x,y)

# ========================================================
# euclidean distance
# contoh
df = data.frame(point = paste0("x",1:4),
                att1 = c(1,3,2,4),
                att2 = c(2,5,0,5))

mat_jarak = matrix(nrow = nrow(df),
                   ncol = nrow(df))

for(i in 1:nrow(df)){
  for(j in 1:nrow(df)){
    jarak_1 = abs(df$att1[i] - df$att1[j])
    jarak_2 = abs(df$att2[i] - df$att2[j])
    jarak = sqrt(jarak_1^2 + jarak_2^2)
    mat_jarak[i,j] = jarak
  }
}

mat_jarak

# ========================================================
# decision tree
  # algoritma induksi decision tree
rm(list=ls())
df = 
  mtcars %>% 
  select(am,gear,carb,cyl,mpg) %>% 
  mutate(am = ifelse(am == 1,"Y","N"),
         gear = case_when(
           gear == 3 ~ "low",
           gear == 4 ~ "medium",
           gear == 5 ~ "high"
         ))
rownames(df) = NULL

df

dec_tree = caret::train(am ~ ., data = df, method = "rpart")
summary(dec_tree)

plot(dec_tree$finalModel)























