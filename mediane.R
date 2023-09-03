#imputation par mediane : 
library(readxl)
EES_stage_imputation <- read_excel("EES_stage_imputation.xlsx")

subset_med <- data.frame(EES_stage_imputation[,8:29])
summary(subset_med)

#nombre des valeurs manquantes total (NA) : 
print(sum(is.na(subset_med)))

#imputation des valeurs manquantes du totale
 
#imputation de B11 / B12 / B13 : 

for ( i in 1:length(subset_med$b13)){
  if (
    (is.na(subset_med$  b13[i]) == TRUE) &
    (is.na(subset_med$ b12[i]) == FALSE) &
    (is.na(subset_med$ b11[i]) == FALSE)
  ){subset_med$  b13[i] = subset_med$ b12[i] + subset_med$ b11[i]
  }
  
  else if (
    
    (is.na(subset_med$  b13[i]) == TRUE) &
    (is.na(subset_med$ b12[i]) == TRUE) &
    (is.na(subset_med$ b11[i]) == FALSE))
    
  {
    subset_med$  b13[i] = median(subset_med$  b13, na.rm = TRUE)
    subset_med$ b12[i] = subset_med$b13[i] - subset_med$b11[i]
    
  }
  
  else if (
    (is.na(subset_med$  b13[i]) == TRUE) &
    (is.na(subset_med$ b12[i]) == FALSE) &
    (is.na(subset_med$ b11[i]) == TRUE)){
    subset_med$  b13[i] = median(subset_med$  b13, na.rm = TRUE)
    subset_med$ b11[i] = subset_med$b13[i] - subset_med$b12[i]
  }
  
  else if (
    (is.na(subset_med$  b13[i]) == FALSE) &
    (is.na(subset_med$ b12[i]) == FALSE) &
    (is.na(subset_med$ b11[i]) == TRUE)){
    subset_med$ b11[i] = subset_med$b13[i] - subset_med$b12[i]
  }
  
  else if (
    (is.na(subset_med$  b13[i]) == FALSE) &
    (is.na(subset_med$ b12[i]) == TRUE) &
    (is.na(subset_med$ b11[i]) == FALSE)){
    subset_med$ b12[i] = subset_med$b13[i] - subset_med$b11[i]
  }
  
  else if(
    (is.na(subset_med$  b13[i]) == TRUE) &
    (is.na(subset_med$ b12[i]) == TRUE) &
    (is.na(subset_med$ b11[i]) == TRUE))
  {
    subset_med$  b13[i] = median(subset_med$  b13, na.rm = TRUE)
    if (median(subset_med$  b13, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b12[i] = median(subset_med$b13, na.rm = TRUE) %/% 2
      subset_med$ b11[i] = median(subset_med$b13, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b12[i] = (median(subset_med$b13, na.rm = TRUE) - 1) %/% 2
      subset_med$ b11[i] = ((median(subset_med$b13, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  b13[i]) == FALSE) &
             (is.na(subset_med$ b12[i]) == TRUE) &
             (is.na(subset_med$ b11[i]) == TRUE)){
    if (median(subset_med$  b13, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b12[i] = median(subset_med$  b13, na.rm = TRUE) %/% 2
      subset_med$ b11[i] = median(subset_med$  b13, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b12[i] = (median(subset_med$  b13, na.rm = TRUE) - 1) %/% 2
      subset_med$ b11[i] = ((median(subset_med$  b13, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B14 / B15/ B16 :

for ( i in 1:length(subset_med$b16)){
  if (
    (is.na(subset_med$  b16[i]) == TRUE) &
    (is.na(subset_med$ b15[i]) == FALSE) &
    (is.na(subset_med$ b14[i]) == FALSE)
  ){subset_med$  b16[i] = subset_med$ b15[i] + subset_med$ b14[i]
  }
  
  else if (
    
    (is.na(subset_med$  b16[i]) == TRUE) &
    (is.na(subset_med$ b15[i]) == TRUE) &
    (is.na(subset_med$ b14[i]) == FALSE))
    
  {
    subset_med$  b16[i] = median(subset_med$  b16, na.rm = TRUE)
    subset_med$ b15[i] = subset_med$b16[i] - subset_med$b14[i]
    
  }
  
  else if (
    (is.na(subset_med$  b16[i]) == TRUE) &
    (is.na(subset_med$ b15[i]) == FALSE) &
    (is.na(subset_med$ b14[i]) == TRUE)){
    subset_med$  b16[i] = median(subset_med$  b16, na.rm = TRUE)
    subset_med$ b14[i] = subset_med$b16[i] - subset_med$b15[i]
  }
  
  else if (
    (is.na(subset_med$  b16[i]) == FALSE) &
    (is.na(subset_med$ b15[i]) == FALSE) &
    (is.na(subset_med$ b14[i]) == TRUE)){
    subset_med$ b14[i] = subset_med$b16[i] - subset_med$b15[i]
  }
  
  else if (
    (is.na(subset_med$  b16[i]) == FALSE) &
    (is.na(subset_med$ b15[i]) == TRUE) &
    (is.na(subset_med$ b14[i]) == FALSE)){
    subset_med$ b15[i] = subset_med$b16[i] - subset_med$b14[i]
  }
  
  else if(
    (is.na(subset_med$  b16[i]) == TRUE) &
    (is.na(subset_med$ b15[i]) == TRUE) &
    (is.na(subset_med$ b14[i]) == TRUE))
  {
    subset_med$  b16[i] = median(subset_med$  b16, na.rm = TRUE)
    if (median(subset_med$  b16, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b15[i] = median(subset_med$b16, na.rm = TRUE) %/% 2
      subset_med$ b14[i] = median(subset_med$b16, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b15[i] = (median(subset_med$b16, na.rm = TRUE) - 1) %/% 2
      subset_med$ b14[i] = ((median(subset_med$b16, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  b16[i]) == FALSE) &
             (is.na(subset_med$ b15[i]) == TRUE) &
             (is.na(subset_med$ b14[i]) == TRUE)){
    if (median(subset_med$  b16, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b15[i] = median(subset_med$  b16, na.rm = TRUE) %/% 2
      subset_med$ b14[i] = median(subset_med$  b16, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b15[i] = (median(subset_med$  b16, na.rm = TRUE) - 1) %/% 2
      subset_med$ b14[i] = ((median(subset_med$  b16, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B21 / B22 / B23 : 
for ( i in 1:length(subset_med$b23)){
  if (
    (is.na(subset_med$  b23[i]) == TRUE) &
    (is.na(subset_med$ b22[i]) == FALSE) &
    (is.na(subset_med$ b21[i]) == FALSE)
  ){subset_med$  b23[i] = subset_med$ b22[i] + subset_med$ b21[i]
  }
  
  else if (
    
    (is.na(subset_med$  b23[i]) == TRUE) &
    (is.na(subset_med$ b22[i]) == TRUE) &
    (is.na(subset_med$ b21[i]) == FALSE))
    
  {
    subset_med$  b23[i] = median(subset_med$  b23, na.rm = TRUE)
    subset_med$ b22[i] = subset_med$b23[i] - subset_med$b21[i]
    
  }
  
  else if (
    (is.na(subset_med$  b23[i]) == TRUE) &
    (is.na(subset_med$ b22[i]) == FALSE) &
    (is.na(subset_med$ b21[i]) == TRUE)){
    subset_med$  b23[i] = median(subset_med$  b23, na.rm = TRUE)
    subset_med$ b21[i] = subset_med$b23[i] - subset_med$b22[i]
  }
  
  else if (
    (is.na(subset_med$  b23[i]) == FALSE) &
    (is.na(subset_med$ b22[i]) == FALSE) &
    (is.na(subset_med$ b21[i]) == TRUE)){
    subset_med$ b21[i] = subset_med$b23[i] - subset_med$b22[i]
  }
  
  else if (
    (is.na(subset_med$  b23[i]) == FALSE) &
    (is.na(subset_med$ b22[i]) == TRUE) &
    (is.na(subset_med$ b21[i]) == FALSE)){
    subset_med$ b22[i] = subset_med$b23[i] - subset_med$b21[i]
  }
  
  else if(
    (is.na(subset_med$  b23[i]) == TRUE) &
    (is.na(subset_med$ b22[i]) == TRUE) &
    (is.na(subset_med$ b21[i]) == TRUE))
  {
    subset_med$  b23[i] = median(subset_med$  b23, na.rm = TRUE)
    if (median(subset_med$  b23, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b22[i] = median(subset_med$b23, na.rm = TRUE) %/% 2
      subset_med$ b21[i] = median(subset_med$b23, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b22[i] = (median(subset_med$b23, na.rm = TRUE) - 1) %/% 2
      subset_med$ b21[i] = ((median(subset_med$b23, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  b23[i]) == FALSE) &
             (is.na(subset_med$ b22[i]) == TRUE) &
             (is.na(subset_med$ b21[i]) == TRUE)){
    if (median(subset_med$  b23, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b22[i] = median(subset_med$  b23, na.rm = TRUE) %/% 2
      subset_med$ b21[i] = median(subset_med$  b23, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b22[i] = (median(subset_med$  b23, na.rm = TRUE) - 1) %/% 2
      subset_med$ b21[i] = ((median(subset_med$  b23, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B31 / B32 / B33 : 

for ( i in 1:length(subset_med$b33)){
  if (
    (is.na(subset_med$  b33[i]) == TRUE) &
    (is.na(subset_med$ b32[i]) == FALSE) &
    (is.na(subset_med$ b31[i]) == FALSE)
  ){subset_med$  b33[i] = subset_med$ b32[i] + subset_med$ b31[i]
  }
  
  else if (
    
    (is.na(subset_med$  b33[i]) == TRUE) &
    (is.na(subset_med$ b32[i]) == TRUE) &
    (is.na(subset_med$ b31[i]) == FALSE))
    
  {
    subset_med$  b33[i] = median(subset_med$  b33, na.rm = TRUE)
    subset_med$ b32[i] = subset_med$b33[i] - subset_med$b31[i]
    
  }
  
  else if (
    (is.na(subset_med$  b33[i]) == TRUE) &
    (is.na(subset_med$ b32[i]) == FALSE) &
    (is.na(subset_med$ b31[i]) == TRUE)){
    subset_med$  b33[i] = median(subset_med$  b33, na.rm = TRUE)
    subset_med$ b31[i] = subset_med$b33[i] - subset_med$b32[i]
  }
  
  else if (
    (is.na(subset_med$  b33[i]) == FALSE) &
    (is.na(subset_med$ b32[i]) == FALSE) &
    (is.na(subset_med$ b31[i]) == TRUE)){
    subset_med$ b31[i] = subset_med$b33[i] - subset_med$b32[i]
  }
  
  else if (
    (is.na(subset_med$  b33[i]) == FALSE) &
    (is.na(subset_med$ b32[i]) == TRUE) &
    (is.na(subset_med$ b31[i]) == FALSE)){
    subset_med$ b32[i] = subset_med$b33[i] - subset_med$b31[i]
  }
  
  else if(
    (is.na(subset_med$  b33[i]) == TRUE) &
    (is.na(subset_med$ b32[i]) == TRUE) &
    (is.na(subset_med$ b31[i]) == TRUE))
  {
    subset_med$  b33[i] = median(subset_med$  b33, na.rm = TRUE)
    if (median(subset_med$  b33, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b32[i] = median(subset_med$b33, na.rm = TRUE) %/% 2
      subset_med$ b31[i] = median(subset_med$b33, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b32[i] = (median(subset_med$b33, na.rm = TRUE) - 1) %/% 2
      subset_med$ b31[i] = ((median(subset_med$b33, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  b33[i]) == FALSE) &
             (is.na(subset_med$ b32[i]) == TRUE) &
             (is.na(subset_med$ b31[i]) == TRUE)){
    if (median(subset_med$  b33, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b32[i] = median(subset_med$  b33, na.rm = TRUE) %/% 2
      subset_med$ b31[i] = median(subset_med$  b33, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b32[i] = (median(subset_med$  b33, na.rm = TRUE) - 1) %/% 2
      subset_med$ b31[i] = ((median(subset_med$  b33, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B34 / B35 / B36 : 

for ( i in 1:length(subset_med$b36)){
  if (
    (is.na(subset_med$  b36[i]) == TRUE) &
    (is.na(subset_med$ b35[i]) == FALSE) &
    (is.na(subset_med$ b34[i]) == FALSE)
  ){subset_med$  b36[i] = subset_med$ b35[i] + subset_med$ b34[i]
  }
  
  else if (
    
    (is.na(subset_med$  b36[i]) == TRUE) &
    (is.na(subset_med$ b35[i]) == TRUE) &
    (is.na(subset_med$ b34[i]) == FALSE))
    
  {
    subset_med$  b36[i] = median(subset_med$  b36, na.rm = TRUE)
    subset_med$ b35[i] = subset_med$b36[i] - subset_med$b34[i]
    
  }
  
  else if (
    (is.na(subset_med$  b36[i]) == TRUE) &
    (is.na(subset_med$ b35[i]) == FALSE) &
    (is.na(subset_med$ b34[i]) == TRUE)){
    subset_med$  b36[i] = median(subset_med$  b36, na.rm = TRUE)
    subset_med$ b34[i] = subset_med$b36[i] - subset_med$b35[i]
  }
  
  else if (
    (is.na(subset_med$  b36[i]) == FALSE) &
    (is.na(subset_med$ b35[i]) == FALSE) &
    (is.na(subset_med$ b34[i]) == TRUE)){
    subset_med$ b34[i] = subset_med$b36[i] - subset_med$b35[i]
  }
  
  else if (
    (is.na(subset_med$  b36[i]) == FALSE) &
    (is.na(subset_med$ b35[i]) == TRUE) &
    (is.na(subset_med$ b34[i]) == FALSE)){
    subset_med$ b35[i] = subset_med$b36[i] - subset_med$b34[i]
  }
  
  else if(
    (is.na(subset_med$  b36[i]) == TRUE) &
    (is.na(subset_med$ b35[i]) == TRUE) &
    (is.na(subset_med$ b34[i]) == TRUE))
  {
    subset_med$  b36[i] = median(subset_med$  b36, na.rm = TRUE)
    if (median(subset_med$  b36, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b35[i] = median(subset_med$b36, na.rm = TRUE) %/% 2
      subset_med$ b34[i] = median(subset_med$b36, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b35[i] = (median(subset_med$b36, na.rm = TRUE) - 1) %/% 2
      subset_med$ b34[i] = ((median(subset_med$b36, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  b36[i]) == FALSE) &
             (is.na(subset_med$ b35[i]) == TRUE) &
             (is.na(subset_med$ b34[i]) == TRUE)){
    if (median(subset_med$  b36, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ b35[i] = median(subset_med$  b36, na.rm = TRUE) %/% 2
      subset_med$ b34[i] = median(subset_med$  b36, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ b35[i] = (median(subset_med$  b36, na.rm = TRUE) - 1) %/% 2
      subset_med$ b34[i] = ((median(subset_med$  b36, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de c11 / c12 / c13 : 

for ( i in 1:length(subset_med$c13)){
  if (
    (is.na(subset_med$  c13[i]) == TRUE) &
    (is.na(subset_med$ c12[i]) == FALSE) &
    (is.na(subset_med$ c11[i]) == FALSE)
  ){subset_med$  c13[i] = subset_med$ c12[i] + subset_med$ c11[i]
  }
  
  else if (
    
    (is.na(subset_med$  c13[i]) == TRUE) &
    (is.na(subset_med$ c12[i]) == TRUE) &
    (is.na(subset_med$ c11[i]) == FALSE))
    
  {
    subset_med$  c13[i] = median(subset_med$  c13, na.rm = TRUE)
    subset_med$ c12[i] = subset_med$c13[i] - subset_med$c11[i]
    
  }
  
  else if (
    (is.na(subset_med$  c13[i]) == TRUE) &
    (is.na(subset_med$ c12[i]) == FALSE) &
    (is.na(subset_med$ c11[i]) == TRUE)){
    subset_med$  c13[i] = median(subset_med$  c13, na.rm = TRUE)
    subset_med$ c11[i] = subset_med$c13[i] - subset_med$c12[i]
  }
  
  else if (
    (is.na(subset_med$  c13[i]) == FALSE) &
    (is.na(subset_med$ c12[i]) == FALSE) &
    (is.na(subset_med$ c11[i]) == TRUE)){
    subset_med$ c11[i] = subset_med$c13[i] - subset_med$c12[i]
  }
  
  else if (
    (is.na(subset_med$  c13[i]) == FALSE) &
    (is.na(subset_med$ c12[i]) == TRUE) &
    (is.na(subset_med$ c11[i]) == FALSE)){
    subset_med$ c12[i] = subset_med$c13[i] - subset_med$c11[i]
  }
  
  else if(
    (is.na(subset_med$  c13[i]) == TRUE) &
    (is.na(subset_med$ c12[i]) == TRUE) &
    (is.na(subset_med$ c11[i]) == TRUE))
  {
    subset_med$  c13[i] = median(subset_med$  c13, na.rm = TRUE)
    if (median(subset_med$  c13, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ c12[i] = median(subset_med$c13, na.rm = TRUE) %/% 2
      subset_med$ c11[i] = median(subset_med$c13, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ c12[i] = (median(subset_med$c13, na.rm = TRUE) - 1) %/% 2
      subset_med$ c11[i] = ((median(subset_med$c13, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }else if ( (is.na(subset_med$  c13[i]) == FALSE) &
             (is.na(subset_med$ c12[i]) == TRUE) &
             (is.na(subset_med$ c11[i]) == TRUE)){
    if (median(subset_med$  c13, na.rm = TRUE) %% 2 == 0)
    {
      subset_med$ c12[i] = median(subset_med$  c13, na.rm = TRUE) %/% 2
      subset_med$ c11[i] = median(subset_med$  c13, na.rm = TRUE) %/% 2
    }else
    {
      subset_med$ c12[i] = (median(subset_med$  c13, na.rm = TRUE) - 1) %/% 2
      subset_med$ c11[i] = ((median(subset_med$  c13, na.rm = TRUE) - 1) %/% 2) + 1
      
    }
  }
}

#imputation du reste du données : 

for (i in colnames(subset_med)) {
  
  col <- subset_med[[i]]
  col_med <- median(col, na.rm = TRUE)
  col[is.na(col)] <- col_med
  subset_med[[i]] <- col
  
}

#nombre des valeurs manquantes total (NA) aprés imputation : 
print(sum(is.na(subset_med)))
summary(subset_med)
