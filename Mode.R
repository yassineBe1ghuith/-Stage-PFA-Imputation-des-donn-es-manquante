# Imputation par la valeur la plus fréquante :
library(readxl)

EES_stage_imputation <- read_excel("EES_stage_imputation.xlsx")

subset_mode <- data.frame(EES_stage_imputation[, 8:29])

#Nombre total des valeurs manquantes (NA) :
summary(subset_mode)
print(sum(is.na(subset_mode)))

#fonction pour calculer le mode :
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#imputation des valeurs manquantes de total

#imputation de B11 / B12 / B13 :

for (i in 1:length(subset_mode$b13)) {
  if ((is.na(subset_mode$b13[i]) == TRUE) &
      (is.na(subset_mode$b12[i]) == FALSE) &
      (is.na(subset_mode$b11[i]) == FALSE)) {
    subset_mode$b13[i] = subset_mode$b12[i] + subset_mode$b11[i]
  }
  
  else if ((is.na(subset_mode$b13[i]) == TRUE) &
           (is.na(subset_mode$b12[i]) == TRUE) &
           (is.na(subset_mode$b11[i]) == FALSE))
    
  {
    subset_mode$b13[i] = getmode(subset_mode$b13)
    subset_mode$b12[i] = subset_mode$b13[i] - subset_mode$b11[i]
    
  }
  
  else if ((is.na(subset_mode$b13[i]) == TRUE) &
           (is.na(subset_mode$b12[i]) == FALSE) &
           (is.na(subset_mode$b11[i]) == TRUE)) {
    subset_mode$b13[i] = getmode(subset_mode$b13)
    subset_mode$b11[i] = subset_mode$b13[i] - subset_mode$b12[i]
  }
  
  else if ((is.na(subset_mode$b13[i]) == FALSE) &
           (is.na(subset_mode$b12[i]) == FALSE) &
           (is.na(subset_mode$b11[i]) == TRUE)) {
    subset_mode$b11[i] = subset_mode$b13[i] - subset_mode$b12[i]
  }
  
  else if ((is.na(subset_mode$b13[i]) == FALSE) &
           (is.na(subset_mode$b12[i]) == TRUE) &
           (is.na(subset_mode$b11[i]) == FALSE)) {
    subset_mode$b12[i] = subset_mode$b13[i] - subset_mode$b11[i]
  }
  
  else if ((is.na(subset_mode$b13[i]) == TRUE) &
           (is.na(subset_mode$b12[i]) == TRUE) &
           (is.na(subset_mode$b11[i]) == TRUE))
  {
    subset_mode$b13[i] = getmode(subset_mode$b13)
    if (getmode(subset_mode$b13) %% 2 == 0)
    {
      subset_mode$b12[i] = getmode(subset_mode$b13) %/% 2
      subset_mode$b11[i] = getmode(subset_mode$b13) %/% 2
    } else
    {
      subset_mode$b12[i] = (getmode(subset_mode$b13) - 1) %/% 2
      subset_mode$b11[i] = ((getmode(subset_mode$b13) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$b13[i]) == FALSE) &
             (is.na(subset_mode$b12[i]) == TRUE) &
             (is.na(subset_mode$b11[i]) == TRUE)) {
    if (getmode(subset_mode$b13) %% 2 == 0)
    {
      subset_mode$b12[i] = getmode(subset_mode$b13) %/% 2
      subset_mode$b11[i] = getmode(subset_mode$b13) %/% 2
    } else
    {
      subset_mode$b12[i] = (getmode(subset_mode$b13) - 1) %/% 2
      subset_mode$b11[i] = ((getmode(subset_mode$b13) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B14 / B15/ B16 :

for (i in 1:length(subset_mode$b16)) {
  if ((is.na(subset_mode$b16[i]) == TRUE) &
      (is.na(subset_mode$b15[i]) == FALSE) &
      (is.na(subset_mode$b14[i]) == FALSE)) {
    subset_mode$b16[i] = subset_mode$b15[i] + subset_mode$b14[i]
  }
  
  else if ((is.na(subset_mode$b16[i]) == TRUE) &
           (is.na(subset_mode$b15[i]) == TRUE) &
           (is.na(subset_mode$b14[i]) == FALSE))
    
  {
    subset_mode$b16[i] = getmode(subset_mode$b16)
    subset_mode$b15[i] = subset_mode$b16[i] - subset_mode$b14[i]
    
  }
  
  else if ((is.na(subset_mode$b16[i]) == TRUE) &
           (is.na(subset_mode$b15[i]) == FALSE) &
           (is.na(subset_mode$b14[i]) == TRUE)) {
    subset_mode$b16[i] = getmode(subset_mode$b16)
    subset_mode$b14[i] = subset_mode$b16[i] - subset_mode$b15[i]
  }
  
  else if ((is.na(subset_mode$b16[i]) == FALSE) &
           (is.na(subset_mode$b15[i]) == FALSE) &
           (is.na(subset_mode$b14[i]) == TRUE)) {
    subset_mode$b14[i] = subset_mode$b16[i] - subset_mode$b15[i]
  }
  
  else if ((is.na(subset_mode$b16[i]) == FALSE) &
           (is.na(subset_mode$b15[i]) == TRUE) &
           (is.na(subset_mode$b14[i]) == FALSE)) {
    subset_mode$b15[i] = subset_mode$b16[i] - subset_mode$b14[i]
  }
  
  else if ((is.na(subset_mode$b16[i]) == TRUE) &
           (is.na(subset_mode$b15[i]) == TRUE) &
           (is.na(subset_mode$b14[i]) == TRUE))
  {
    subset_mode$b16[i] = getmode(subset_mode$b16)
    if (getmode(subset_mode$b16) %% 2 == 0)
    {
      subset_mode$b15[i] = getmode(subset_mode$b16) %/% 2
      subset_mode$b14[i] = getmode(subset_mode$b16) %/% 2
    } else
    {
      subset_mode$b15[i] = (getmode(subset_mode$b16) - 1) %/% 2
      subset_mode$b14[i] = ((getmode(subset_mode$b16) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$b16[i]) == FALSE) &
             (is.na(subset_mode$b15[i]) == TRUE) &
             (is.na(subset_mode$b14[i]) == TRUE)) {
    if (getmode(subset_mode$b16) %% 2 == 0)
    {
      subset_mode$b15[i] = getmode(subset_mode$b16) %/% 2
      subset_mode$b14[i] = getmode(subset_mode$b16) %/% 2
    } else
    {
      subset_mode$b15[i] = (getmode(subset_mode$b16) - 1) %/% 2
      subset_mode$b14[i] = ((getmode(subset_mode$b16) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B21 / B22 / B23 :
for (i in 1:length(subset_mode$b23)) {
  if ((is.na(subset_mode$b23[i]) == TRUE) &
      (is.na(subset_mode$b22[i]) == FALSE) &
      (is.na(subset_mode$b21[i]) == FALSE)) {
    subset_mode$b23[i] = subset_mode$b22[i] + subset_mode$b21[i]
  }
  
  else if ((is.na(subset_mode$b23[i]) == TRUE) &
           (is.na(subset_mode$b22[i]) == TRUE) &
           (is.na(subset_mode$b21[i]) == FALSE))
    
  {
    subset_mode$b23[i] = getmode(subset_mode$b23)
    subset_mode$b22[i] = subset_mode$b23[i] - subset_mode$b21[i]
    
  }
  
  else if ((is.na(subset_mode$b23[i]) == TRUE) &
           (is.na(subset_mode$b22[i]) == FALSE) &
           (is.na(subset_mode$b21[i]) == TRUE)) {
    subset_mode$b23[i] = getmode(subset_mode$b23)
    subset_mode$b21[i] = subset_mode$b23[i] - subset_mode$b22[i]
  }
  
  else if ((is.na(subset_mode$b23[i]) == FALSE) &
           (is.na(subset_mode$b22[i]) == FALSE) &
           (is.na(subset_mode$b21[i]) == TRUE)) {
    subset_mode$b21[i] = subset_mode$b23[i] - subset_mode$b22[i]
  }
  
  else if ((is.na(subset_mode$b23[i]) == FALSE) &
           (is.na(subset_mode$b22[i]) == TRUE) &
           (is.na(subset_mode$b21[i]) == FALSE)) {
    subset_mode$b22[i] = subset_mode$b23[i] - subset_mode$b21[i]
  }
  
  else if ((is.na(subset_mode$b23[i]) == TRUE) &
           (is.na(subset_mode$b22[i]) == TRUE) &
           (is.na(subset_mode$b21[i]) == TRUE))
  {
    subset_mode$b23[i] = getmode(subset_mode$b23)
    if (getmode(subset_mode$b23) %% 2 == 0)
    {
      subset_mode$b22[i] = getmode(subset_mode$b23) %/% 2
      subset_mode$b21[i] = getmode(subset_mode$b23) %/% 2
    } else
    {
      subset_mode$b22[i] = (getmode(subset_mode$b23) - 1) %/% 2
      subset_mode$b21[i] = ((getmode(subset_mode$b23) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$b23[i]) == FALSE) &
             (is.na(subset_mode$b22[i]) == TRUE) &
             (is.na(subset_mode$b21[i]) == TRUE)) {
    if (getmode(subset_mode$b23) %% 2 == 0)
    {
      subset_mode$b22[i] = getmode(subset_mode$b23) %/% 2
      subset_mode$b21[i] = getmode(subset_mode$b23) %/% 2
    } else
    {
      subset_mode$b22[i] = (getmode(subset_mode$b23) - 1) %/% 2
      subset_mode$b21[i] = ((getmode(subset_mode$b23) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B31 / B32 / B33 :

for (i in 1:length(subset_mode$b33)) {
  if ((is.na(subset_mode$b33[i]) == TRUE) &
      (is.na(subset_mode$b32[i]) == FALSE) &
      (is.na(subset_mode$b31[i]) == FALSE)) {
    subset_mode$b33[i] = subset_mode$b32[i] + subset_mode$b31[i]
  }
  
  else if ((is.na(subset_mode$b33[i]) == TRUE) &
           (is.na(subset_mode$b32[i]) == TRUE) &
           (is.na(subset_mode$b31[i]) == FALSE))
    
  {
    subset_mode$b33[i] = getmode(subset_mode$b33)
    subset_mode$b32[i] = subset_mode$b33[i] - subset_mode$b31[i]
    
  }
  
  else if ((is.na(subset_mode$b33[i]) == TRUE) &
           (is.na(subset_mode$b32[i]) == FALSE) &
           (is.na(subset_mode$b31[i]) == TRUE)) {
    subset_mode$b33[i] = getmode(subset_mode$b33)
    subset_mode$b31[i] = subset_mode$b33[i] - subset_mode$b32[i]
  }
  
  else if ((is.na(subset_mode$b33[i]) == FALSE) &
           (is.na(subset_mode$b32[i]) == FALSE) &
           (is.na(subset_mode$b31[i]) == TRUE)) {
    subset_mode$b31[i] = subset_mode$b33[i] - subset_mode$b32[i]
  }
  
  else if ((is.na(subset_mode$b33[i]) == FALSE) &
           (is.na(subset_mode$b32[i]) == TRUE) &
           (is.na(subset_mode$b31[i]) == FALSE)) {
    subset_mode$b32[i] = subset_mode$b33[i] - subset_mode$b31[i]
  }
  
  else if ((is.na(subset_mode$b33[i]) == TRUE) &
           (is.na(subset_mode$b32[i]) == TRUE) &
           (is.na(subset_mode$b31[i]) == TRUE))
  {
    subset_mode$b33[i] = getmode(subset_mode$b33)
    if (getmode(subset_mode$b33) %% 2 == 0)
    {
      subset_mode$b32[i] = getmode(subset_mode$b33) %/% 2
      subset_mode$b31[i] = getmode(subset_mode$b33) %/% 2
    } else
    {
      subset_mode$b32[i] = (getmode(subset_mode$b33) - 1) %/% 2
      subset_mode$b31[i] = ((getmode(subset_mode$b33) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$b33[i]) == FALSE) &
             (is.na(subset_mode$b32[i]) == TRUE) &
             (is.na(subset_mode$b31[i]) == TRUE)) {
    if (getmode(subset_mode$b33) %% 2 == 0)
    {
      subset_mode$b32[i] = getmode(subset_mode$b33) %/% 2
      subset_mode$b31[i] = getmode(subset_mode$b33) %/% 2
    } else
    {
      subset_mode$b32[i] = (getmode(subset_mode$b33) - 1) %/% 2
      subset_mode$b31[i] = ((getmode(subset_mode$b33) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de B34 / B35 / B36 :

for (i in 1:length(subset_mode$b36)) {
  if ((is.na(subset_mode$b36[i]) == TRUE) &
      (is.na(subset_mode$b35[i]) == FALSE) &
      (is.na(subset_mode$b34[i]) == FALSE)) {
    subset_mode$b36[i] = subset_mode$b35[i] + subset_mode$b34[i]
  }
  
  else if ((is.na(subset_mode$b36[i]) == TRUE) &
           (is.na(subset_mode$b35[i]) == TRUE) &
           (is.na(subset_mode$b34[i]) == FALSE))
    
  {
    subset_mode$b36[i] = getmode(subset_mode$b36)
    subset_mode$b35[i] = subset_mode$b36[i] - subset_mode$b34[i]
    
  }
  
  else if ((is.na(subset_mode$b36[i]) == TRUE) &
           (is.na(subset_mode$b35[i]) == FALSE) &
           (is.na(subset_mode$b34[i]) == TRUE)) {
    subset_mode$b36[i] = getmode(subset_mode$b36)
    subset_mode$b34[i] = subset_mode$b36[i] - subset_mode$b35[i]
  }
  
  else if ((is.na(subset_mode$b36[i]) == FALSE) &
           (is.na(subset_mode$b35[i]) == FALSE) &
           (is.na(subset_mode$b34[i]) == TRUE)) {
    subset_mode$b34[i] = subset_mode$b36[i] - subset_mode$b35[i]
  }
  
  else if ((is.na(subset_mode$b36[i]) == FALSE) &
           (is.na(subset_mode$b35[i]) == TRUE) &
           (is.na(subset_mode$b34[i]) == FALSE)) {
    subset_mode$b35[i] = subset_mode$b36[i] - subset_mode$b34[i]
  }
  
  else if ((is.na(subset_mode$b36[i]) == TRUE) &
           (is.na(subset_mode$b35[i]) == TRUE) &
           (is.na(subset_mode$b34[i]) == TRUE))
  {
    subset_mode$b36[i] = getmode(subset_mode$b36)
    if (getmode(subset_mode$b36) %% 2 == 0)
    {
      subset_mode$b35[i] = getmode(subset_mode$b36) %/% 2
      subset_mode$b34[i] = getmode(subset_mode$b36) %/% 2
    } else
    {
      subset_mode$b35[i] = (getmode(subset_mode$b36) - 1) %/% 2
      subset_mode$b34[i] = ((getmode(subset_mode$b36) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$b36[i]) == FALSE) &
             (is.na(subset_mode$b35[i]) == TRUE) &
             (is.na(subset_mode$b34[i]) == TRUE)) {
    if (getmode(subset_mode$b36) %% 2 == 0)
    {
      subset_mode$b35[i] = getmode(subset_mode$b36) %/% 2
      subset_mode$b34[i] = getmode(subset_mode$b36) %/% 2
    } else
    {
      subset_mode$b35[i] = (getmode(subset_mode$b36) - 1) %/% 2
      subset_mode$b34[i] = ((getmode(subset_mode$b36) - 1) %/% 2) + 1
      
    }
  }
}

#imputation de c11 / c12 / c13 :

for (i in 1:length(subset_mode$c13)) {
  if ((is.na(subset_mode$c13[i]) == TRUE) &
      (is.na(subset_mode$c12[i]) == FALSE) &
      (is.na(subset_mode$c11[i]) == FALSE)) {
    subset_mode$c13[i] = subset_mode$c12[i] + subset_mode$c11[i]
  }
  
  else if ((is.na(subset_mode$c13[i]) == TRUE) &
           (is.na(subset_mode$c12[i]) == TRUE) &
           (is.na(subset_mode$c11[i]) == FALSE))
    
  {
    subset_mode$c13[i] = getmode(subset_mode$c13)
    subset_mode$c12[i] = subset_mode$c13[i] - subset_mode$c11[i]
    
  }
  
  else if ((is.na(subset_mode$c13[i]) == TRUE) &
           (is.na(subset_mode$c12[i]) == FALSE) &
           (is.na(subset_mode$c11[i]) == TRUE)) {
    subset_mode$c13[i] = getmode(subset_mode$c13)
    subset_mode$c11[i] = subset_mode$c13[i] - subset_mode$c12[i]
  }
  
  else if ((is.na(subset_mode$c13[i]) == FALSE) &
           (is.na(subset_mode$c12[i]) == FALSE) &
           (is.na(subset_mode$c11[i]) == TRUE)) {
    subset_mode$c11[i] = subset_mode$c13[i] - subset_mode$c12[i]
  }
  
  else if ((is.na(subset_mode$c13[i]) == FALSE) &
           (is.na(subset_mode$c12[i]) == TRUE) &
           (is.na(subset_mode$c11[i]) == FALSE)) {
    subset_mode$c12[i] = subset_mode$c13[i] - subset_mode$c11[i]
  }
  
  else if ((is.na(subset_mode$c13[i]) == TRUE) &
           (is.na(subset_mode$c12[i]) == TRUE) &
           (is.na(subset_mode$c11[i]) == TRUE))
  {
    subset_mode$c13[i] = getmode(subset_mode$c13)
    if (getmode(subset_mode$c13) %% 2 == 0)
    {
      subset_mode$c12[i] = getmode(subset_mode$c13) %/% 2
      subset_mode$c11[i] = getmode(subset_mode$c13) %/% 2
    } else
    {
      subset_mode$c12[i] = (getmode(subset_mode$c13) - 1) %/% 2
      subset_mode$c11[i] = ((getmode(subset_mode$c13) - 1) %/% 2) + 1
      
    }
  } else if ((is.na(subset_mode$c13[i]) == FALSE) &
             (is.na(subset_mode$c12[i]) == TRUE) &
             (is.na(subset_mode$c11[i]) == TRUE)) {
    if (getmode(subset_mode$c13) %% 2 == 0)
    {
      subset_mode$c12[i] = getmode(subset_mode$c13) %/% 2
      subset_mode$c11[i] = getmode(subset_mode$c13) %/% 2
    } else
    {
      subset_mode$c12[i] = (getmode(subset_mode$c13) - 1) %/% 2
      subset_mode$c11[i] = ((getmode(subset_mode$c13) - 1) %/% 2) + 1
      
    }
  }
}

#imputation du reste du données : 
for (i in colnames(subset_mode)) {
  col <- subset_mode[[i]]
  col_mode <- getmode(col)
  col[is.na(col)] <- col_mode
  subset_mode[[i]] <- col
  
}
View(subset_mode)