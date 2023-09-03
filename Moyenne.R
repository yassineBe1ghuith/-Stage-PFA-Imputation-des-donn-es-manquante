# Imputation par la moyenne :
library(readxl)

EES_stage_imputation <- read_excel("EES_stage_imputation.xlsx")

subset_moy <- data.frame(EES_stage_imputation[, 8:29])
summary(subset_moy)

#Nombre total des valeurs manquantes (NA) :

print(sum(is.na(subset_moy)))

#imputation des valeurs manquantes de total

#imputation de B11 / B12 / B13 :

for (i in 1:length(subset_moy$b13)) {
  if ((is.na(subset_moy$b13[i]) == TRUE) &
      (is.na(subset_moy$b12[i]) == FALSE) &
      (is.na(subset_moy$b11[i]) == FALSE)) {
    subset_moy$b13[i] = subset_moy$b12[i] + subset_moy$b11[i]
  }
  
  else if ((is.na(subset_moy$b13[i]) == TRUE) &
           (is.na(subset_moy$b12[i]) == TRUE) &
           (is.na(subset_moy$b11[i]) == FALSE))
    
  {
    subset_moy$b13[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE)
    subset_moy$b12[i] = subset_moy$b13[i] - subset_moy$b11[i]
    
  }
  
  else if ((is.na(subset_moy$b13[i]) == TRUE) &
           (is.na(subset_moy$b12[i]) == FALSE) &
           (is.na(subset_moy$b11[i]) == TRUE)) {
    subset_moy$b13[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE)
    subset_moy$b11[i] = subset_moy$b13[i] - subset_moy$b12[i]
  }
  
  else if ((is.na(subset_moy$b13[i]) == FALSE) &
           (is.na(subset_moy$b12[i]) == FALSE) &
           (is.na(subset_moy$b11[i]) == TRUE)) {
    subset_moy$b11[i] = subset_moy$b13[i] - subset_moy$b12[i]
  }
  
  else if ((is.na(subset_moy$b13[i]) == FALSE) &
           (is.na(subset_moy$b12[i]) == TRUE) &
           (is.na(subset_moy$b11[i]) == FALSE)) {
    subset_moy$b12[i] = subset_moy$b13[i] - subset_moy$b11[i]
  }
  
  else if ((is.na(subset_moy$b13[i]) == TRUE) &
           (is.na(subset_moy$b12[i]) == TRUE) &
           (is.na(subset_moy$b11[i]) == TRUE))
  {
    subset_moy$b13[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE)
    if (mean(subset_moy$b13, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b12[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b11[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b12[i] = (mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b11[i] = ((mean(
        subset_moy$b13, trim = 0.5, na.rm = TRUE
      ) - 1) %% 2) + 1
      
    }
  } else if ((is.na(subset_moy$b13[i]) == FALSE) &
             (is.na(subset_moy$b12[i]) == TRUE) &
             (is.na(subset_moy$b11[i]) == TRUE)) {
    if (mean(subset_moy$b13, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b12[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b11[i] = mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b12[i] = (mean(subset_moy$b13, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b11[i] = ((mean(
        subset_moy$b13, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B14 / B15/ B16 :

for (i in 1:length(subset_moy$b16)) {
  if ((is.na(subset_moy$b16[i]) == TRUE) &
      (is.na(subset_moy$b15[i]) == FALSE) &
      (is.na(subset_moy$b14[i]) == FALSE)) {
    subset_moy$b16[i] = subset_moy$b15[i] + subset_moy$b14[i]
  }
  
  else if ((is.na(subset_moy$b16[i]) == TRUE) &
           (is.na(subset_moy$b15[i]) == TRUE) &
           (is.na(subset_moy$b14[i]) == FALSE))
    
  {
    subset_moy$b16[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE)
    subset_moy$b15[i] = subset_moy$b16[i] - subset_moy$b14[i]
    
  }
  
  else if ((is.na(subset_moy$b16[i]) == TRUE) &
           (is.na(subset_moy$b15[i]) == FALSE) &
           (is.na(subset_moy$b14[i]) == TRUE)) {
    subset_moy$b16[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE)
    subset_moy$b14[i] = subset_moy$b16[i] - subset_moy$b15[i]
  }
  
  else if ((is.na(subset_moy$b16[i]) == FALSE) &
           (is.na(subset_moy$b15[i]) == FALSE) &
           (is.na(subset_moy$b14[i]) == TRUE)) {
    subset_moy$b14[i] = subset_moy$b16[i] - subset_moy$b15[i]
  }
  
  else if ((is.na(subset_moy$b16[i]) == FALSE) &
           (is.na(subset_moy$b15[i]) == TRUE) &
           (is.na(subset_moy$b14[i]) == FALSE)) {
    subset_moy$b15[i] = subset_moy$b16[i] - subset_moy$b14[i]
  }
  
  else if ((is.na(subset_moy$b16[i]) == TRUE) &
           (is.na(subset_moy$b15[i]) == TRUE) &
           (is.na(subset_moy$b14[i]) == TRUE))
  {
    subset_moy$b16[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE)
    if (mean(subset_moy$b16, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b15[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b14[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b15[i] = (mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b14[i] = ((mean(
        subset_moy$b16, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_moy$b16[i]) == FALSE) &
             (is.na(subset_moy$b15[i]) == TRUE) &
             (is.na(subset_moy$b14[i]) == TRUE)) {
    if (mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b15[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b14[i] = mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b15[i] = (mean(subset_moy$b16, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b14[i] = ((mean(
        subset_moy$b16, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B21 / B22 / B23 :
for (i in 1:length(subset_moy$b23)) {
  if ((is.na(subset_moy$b23[i]) == TRUE) &
      (is.na(subset_moy$b22[i]) == FALSE) &
      (is.na(subset_moy$b21[i]) == FALSE)) {
    subset_moy$b23[i] = subset_moy$b22[i] + subset_moy$b21[i]
  }
  
  else if ((is.na(subset_moy$b23[i]) == TRUE) &
           (is.na(subset_moy$b22[i]) == TRUE) &
           (is.na(subset_moy$b21[i]) == FALSE))
    
  {
    subset_moy$b23[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE)
    subset_moy$b22[i] = subset_moy$b23[i] - subset_moy$b21[i]
    
  }
  
  else if ((is.na(subset_moy$b23[i]) == TRUE) &
           (is.na(subset_moy$b22[i]) == FALSE) &
           (is.na(subset_moy$b21[i]) == TRUE)) {
    subset_moy$b23[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE)
    subset_moy$b21[i] = subset_moy$b23[i] - subset_moy$b22[i]
  }
  
  else if ((is.na(subset_moy$b23[i]) == FALSE) &
           (is.na(subset_moy$b22[i]) == FALSE) &
           (is.na(subset_moy$b21[i]) == TRUE)) {
    subset_moy$b21[i] = subset_moy$b23[i] - subset_moy$b22[i]
  }
  
  else if ((is.na(subset_moy$b23[i]) == FALSE) &
           (is.na(subset_moy$b22[i]) == TRUE) &
           (is.na(subset_moy$b21[i]) == FALSE)) {
    subset_moy$b22[i] = subset_moy$b23[i] - subset_moy$b21[i]
  }
  
  else if ((is.na(subset_moy$b23[i]) == TRUE) &
           (is.na(subset_moy$b22[i]) == TRUE) &
           (is.na(subset_moy$b21[i]) == TRUE))
  {
    subset_moy$b23[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE)
    if (mean(subset_moy$b23, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b22[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b21[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b22[i] = (mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b21[i] = ((mean(
        subset_moy$b23, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_moy$b23[i]) == FALSE) &
             (is.na(subset_moy$b22[i]) == TRUE) &
             (is.na(subset_moy$b21[i]) == TRUE)) {
    if (mean(subset_moy$b23, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b22[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b21[i] = mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b22[i] = (mean(subset_moy$b23, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b21[i] = ((mean(
        subset_moy$b23, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B31 / B32 / B33 :

for (i in 1:length(subset_moy$b33)) {
  if ((is.na(subset_moy$b33[i]) == TRUE) &
      (is.na(subset_moy$b32[i]) == FALSE) &
      (is.na(subset_moy$b31[i]) == FALSE)) {
    subset_moy$b33[i] = subset_moy$b32[i] + subset_moy$b31[i]
  }
  
  else if ((is.na(subset_moy$b33[i]) == TRUE) &
           (is.na(subset_moy$b32[i]) == TRUE) &
           (is.na(subset_moy$b31[i]) == FALSE))
    
  {
    subset_moy$b33[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE)
    subset_moy$b32[i] = subset_moy$b33[i] - subset_moy$b31[i]
    
  }
  
  else if ((is.na(subset_moy$b33[i]) == TRUE) &
           (is.na(subset_moy$b32[i]) == FALSE) &
           (is.na(subset_moy$b31[i]) == TRUE)) {
    subset_moy$b33[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE)
    subset_moy$b31[i] = subset_moy$b33[i] - subset_moy$b32[i]
  }
  
  else if ((is.na(subset_moy$b33[i]) == FALSE) &
           (is.na(subset_moy$b32[i]) == FALSE) &
           (is.na(subset_moy$b31[i]) == TRUE)) {
    subset_moy$b31[i] = subset_moy$b33[i] - subset_moy$b32[i]
  }
  
  else if ((is.na(subset_moy$b33[i]) == FALSE) &
           (is.na(subset_moy$b32[i]) == TRUE) &
           (is.na(subset_moy$b31[i]) == FALSE)) {
    subset_moy$b32[i] = subset_moy$b33[i] - subset_moy$b31[i]
  }
  
  else if ((is.na(subset_moy$b33[i]) == TRUE) &
           (is.na(subset_moy$b32[i]) == TRUE) &
           (is.na(subset_moy$b31[i]) == TRUE))
  {
    subset_moy$b33[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE)
    if (mean(subset_moy$b33, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b32[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b31[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b32[i] = (mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b31[i] = ((mean(
        subset_moy$b33, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_moy$b33[i]) == FALSE) &
             (is.na(subset_moy$b32[i]) == TRUE) &
             (is.na(subset_moy$b31[i]) == TRUE)) {
    if (mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b32[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b31[i] = mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b32[i] = (mean(subset_moy$b33, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b31[i] = ((mean(
        subset_moy$b33, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B34 / B35 / B36 :

for (i in 1:length(subset_moy$b36)) {
  if ((is.na(subset_moy$b36[i]) == TRUE) &
      (is.na(subset_moy$b35[i]) == FALSE) &
      (is.na(subset_moy$b34[i]) == FALSE)) {
    subset_moy$b36[i] = subset_moy$b35[i] + subset_moy$b34[i]
  }
  
  else if ((is.na(subset_moy$b36[i]) == TRUE) &
           (is.na(subset_moy$b35[i]) == TRUE) &
           (is.na(subset_moy$b34[i]) == FALSE))
    
  {
    subset_moy$b36[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE)
    subset_moy$b35[i] = subset_moy$b36[i] - subset_moy$b34[i]
    
  }
  
  else if ((is.na(subset_moy$b36[i]) == TRUE) &
           (is.na(subset_moy$b35[i]) == FALSE) &
           (is.na(subset_moy$b34[i]) == TRUE)) {
    subset_moy$b36[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE)
    subset_moy$b34[i] = subset_moy$b36[i] - subset_moy$b35[i]
  }
  
  else if ((is.na(subset_moy$b36[i]) == FALSE) &
           (is.na(subset_moy$b35[i]) == FALSE) &
           (is.na(subset_moy$b34[i]) == TRUE)) {
    subset_moy$b34[i] = subset_moy$b36[i] - subset_moy$b35[i]
  }
  
  else if ((is.na(subset_moy$b36[i]) == FALSE) &
           (is.na(subset_moy$b35[i]) == TRUE) &
           (is.na(subset_moy$b34[i]) == FALSE)) {
    subset_moy$b35[i] = subset_moy$b36[i] - subset_moy$b34[i]
  }
  
  else if ((is.na(subset_moy$b36[i]) == TRUE) &
           (is.na(subset_moy$b35[i]) == TRUE) &
           (is.na(subset_moy$b34[i]) == TRUE))
  {
    subset_moy$b36[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE)
    if (mean(subset_moy$b36, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$b35[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b34[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b35[i] = (mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b34[i] = ((mean(
        subset_moy$b36, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_moy$b36[i]) == FALSE) &
             (is.na(subset_moy$b35[i]) == TRUE) &
             (is.na(subset_moy$b34[i]) == TRUE)) {
    if (mean(subset_moy$b36, na.rm = TRUE) %/% 2 == 0)
    {
      subset_moy$b35[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$b34[i] = mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$b35[i] = (mean(subset_moy$b36, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$b34[i] = ((mean(
        subset_moy$b36, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de c11 / c12 / c13 :

for (i in 1:length(subset_moy$c13)) {
  if ((is.na(subset_moy$c13[i]) == TRUE) &
      (is.na(subset_moy$c12[i]) == FALSE) &
      (is.na(subset_moy$c11[i]) == FALSE)) {
    subset_moy$c13[i] = subset_moy$c12[i] + subset_moy$c11[i]
  }
  
  else if ((is.na(subset_moy$c13[i]) == TRUE) &
           (is.na(subset_moy$c12[i]) == TRUE) &
           (is.na(subset_moy$c11[i]) == FALSE))
    
  {
    subset_moy$c13[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE)
    subset_moy$c12[i] = subset_moy$c13[i] - subset_moy$c11[i]
    
  }
  
  else if ((is.na(subset_moy$c13[i]) == TRUE) &
           (is.na(subset_moy$c12[i]) == FALSE) &
           (is.na(subset_moy$c11[i]) == TRUE)) {
    subset_moy$c13[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE)
    subset_moy$c11[i] = subset_moy$c13[i] - subset_moy$c12[i]
  }
  
  else if ((is.na(subset_moy$c13[i]) == FALSE) &
           (is.na(subset_moy$c12[i]) == FALSE) &
           (is.na(subset_moy$c11[i]) == TRUE)) {
    subset_moy$c11[i] = subset_moy$c13[i] - subset_moy$c12[i]
  }
  
  else if ((is.na(subset_moy$c13[i]) == FALSE) &
           (is.na(subset_moy$c12[i]) == TRUE) &
           (is.na(subset_moy$c11[i]) == FALSE)) {
    subset_moy$c12[i] = subset_moy$c13[i] - subset_moy$c11[i]
  }
  
  else if ((is.na(subset_moy$c13[i]) == TRUE) &
           (is.na(subset_moy$c12[i]) == TRUE) &
           (is.na(subset_moy$c11[i]) == TRUE))
  {
    subset_moy$c13[i] = mean(subset_moy$c13, na.rm = TRUE)
    if (mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$c12[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$c11[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$c12[i] = (mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$c11[i] = ((mean(
        subset_moy$c13, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_moy$c13[i]) == FALSE) &
             (is.na(subset_moy$c12[i]) == TRUE) &
             (is.na(subset_moy$c11[i]) == TRUE)) {
    if (mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %% 2 == 0)
    {
      subset_moy$c12[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %/% 2
      subset_moy$c11[i] = mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) %/% 2
    } else
    {
      subset_moy$c12[i] = (mean(subset_moy$c13, trim = 0.5, na.rm = TRUE) - 1) %/% 2
      subset_moy$c11[i] = ((mean(
        subset_moy$c13, trim = 0.5, na.rm = TRUE
      ) - 1) %/% 2) + 1
      
    }
  }
}

#imputation du reste du valeurs manquantes
for (i in colnames(subset_moy)) {
  col <- subset_moy[[i]]
  col_mean <- mean(col, trim = 0.5, na.rm = TRUE)
  col[is.na(col)] <- col_mean
  subset_moy[[i]] <- col
  
}

#Nombre total des valeurs manquantes (NA) aprés imputation :
print(sum(is.na(subset_moy)))
summary(subset_moy)
