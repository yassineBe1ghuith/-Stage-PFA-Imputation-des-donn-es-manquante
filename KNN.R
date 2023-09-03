#imputation avec Knn :
library(readxl)

EES_stage_imputation <- read_excel("EES_stage_imputation.xlsx")

library(VIM)

df <- data.frame(EES_stage_imputation[8:29])

print(sum(is.na(df)))

#imputation Simple 
ImpSimple = function(x){
  #b13
  for (i in 1:length(df$b13)){
    if((is.na(df$b13[i]) == TRUE) &
       (is.na(df$b12[i]) == FALSE) &
       (is.na(df$b11[i]) == FALSE)
    ) {df$b13[i] = df$b12[i] + df$b11[i]}
  }
  #b12
  for (i in 1:length(df$b12)){
    if((is.na(df$b13[i]) == FALSE)&
       (is.na(df$b12[i]) == TRUE)&
       (is.na(df$b11[i]) == FALSE)
    ) {df$b12[i] = df$b13[i] - df$b11[i]}
  }
  #b11
  for (i in 1:length(df$b11)){
    if((is.na(df$b13[i]) == FALSE)&
       (is.na(df$b12[i]) == FALSE)&
       (is.na(df$b11[i]) == TRUE)
    ) {df$b11[i] = df$b13[i] - df$b12[i]}
  }
  #b11 et b12
  for (i in 1:length(df$b13)){
    if((is.na(df$b13[i]) == FALSE) &
       (is.na(df$b12[i]) == TRUE) &
       (is.na(df$b11[i]) == TRUE)
    ) {if (df$b13[i] %% 2 == 0 ){
      
        df$b11[i] = df$b13[i] %/% 2
        df$b12[i] = df$b13[i] %/% 2
        
    }else{
      
      df$b11[i] = ((df$b13[i] + 1) %/% 2)
      df$b12[i] = (df$b13[i] + 1) %/% 2 - 1
    
      }
    }
  }
  #--------------------------------
  #b16
  for (i in 1:length(df$b16)){
    if((is.na(df$b16[i]) == TRUE)&
       (is.na(df$b15[i]) == FALSE)&
       (is.na(df$b14[i]) == FALSE)
    ) {df$b16[i] = df$b15[i] + df$b14[i]}
  }
  #b15
  for (i in 1:length(df$b15)){
    if((is.na(df$b16[i]) == FALSE)&
       (is.na(df$b15[i]) == TRUE)&
       (is.na(df$b14[i]) == FALSE)
    ) {df$b15[i] = df$b16[i] - df$b14[i]}
  }
  #b14
  for (i in 1:length(df$b14)){
    if((is.na(df$b16[i]) == FALSE)&
       (is.na(df$b15[i]) == FALSE)&
       (is.na(df$b14[i]) == TRUE)
    ) {df$b14[i] = df$b16[i] - df$b15[i]}
  }
  
  #b14 et b15
  for (i in 1:length(df$b16)){
    if((is.na(df$b16[i]) == FALSE) &
       (is.na(df$b15[i]) == TRUE) &
       (is.na(df$b14[i]) == TRUE)
    ) {if (df$b16[i] %% 2 == 0 ){
      
      df$b14[i] = df$b16[i] %/% 2
      df$b15[i] = df$b16[i] %/% 2
      
    }else{
      
      df$b14[i] = ((df$b16[i] + 1) %/% 2)
      df$b15[i] = (df$b16[i] + 1) %/% 2 - 1
      
     }
    }
  }
  #--------------------------------
  #b23
  for (i in 1:length(df$b23)){
    if((is.na(df$b23[i]) == TRUE)&
       (is.na(df$b22[i]) == FALSE)&
       (is.na(df$b21[i]) == FALSE)
    ) {df$b23[i] = df$b22[i] + df$b21[i]}
  }
  #b22
  for (i in 1:length(df$b22)){
    if((is.na(df$b23[i]) == FALSE)&
       (is.na(df$b22[i]) == TRUE)&
       (is.na(df$b21[i]) == FALSE)
    ) {df$b22[i] = df$b23[i] - df$b21[i]}
  }
  #b21
  for (i in 1:length(df$b21)){
    if((is.na(df$b23[i]) == FALSE)&
       (is.na(df$b22[i]) == FALSE)&
       (is.na(df$b21[i]) == TRUE)
    ) {df$b21[i] = df$b23[i] - df$b22[i]}
  }
  #b21 et b22
  for (i in 1:length(df$b23)){
    if((is.na(df$b23[i]) == FALSE) &
       (is.na(df$b22[i]) == TRUE) &
       (is.na(df$b21[i]) == TRUE)
    ) {if (df$b23[i] %% 2 == 0 ){
      
      df$b21[i] = df$b23[i] %/% 2
      df$b22[i] = df$b23[i] %/% 2
      
    }else{
      
      df$b21[i] = ((df$b23[i] + 1) %/% 2)
      df$b22[i] = (df$b23[i] + 1) %/% 2 - 1
      
      }
    }
  }
  #--------------------------------
  #b33
  for (i in 1:length(df$b33)){
    if((is.na(df$b33[i]) == TRUE)&
       (is.na(df$b32[i]) == FALSE)&
       (is.na(df$b31[i]) == FALSE)
    ) {df$b33[i] = df$b32[i] + df$b31[i]}
  }
  #b32
  for (i in 1:length(df$b32)){
    if((is.na(df$b33[i]) == FALSE)&
       (is.na(df$b32[i]) == TRUE)&
       (is.na(df$b31[i]) == FALSE)
    ) {df$b32[i] = df$b33[i] - df$b31[i]}
  }
  #b31
  for (i in 1:length(df$b31)){
    if((is.na(df$b33[i]) == FALSE)&
       (is.na(df$b32[i]) == FALSE)&
       (is.na(df$b31[i]) == TRUE)
    ) {df$b31[i] = df$b33[i] - df$b32[i]}
  }
  #b31 et b32
  for (i in 1:length(df$b33)){
    if((is.na(df$b33[i]) == FALSE) &
       (is.na(df$b32[i]) == TRUE) &
       (is.na(df$b31[i]) == TRUE)
    ) {if (df$b33[i] %% 2 == 0 ){
      
      df$b31[i] = df$b33[i] %/% 2
      df$b32[i] = df$b33[i] %/% 2
      
    }else{
      
      df$b31[i] = ((df$b33[i] + 1) %/% 2)
      df$b32[i] = (df$b33[i] + 1) %/% 2 - 1
      
      }
    }
  }
  #--------------------------------------
  #b36
  for (i in 1:length(df$b36)){
    if((is.na(df$b36[i]) == TRUE)&
       (is.na(df$b35[i]) == FALSE)&
       (is.na(df$b34[i]) == FALSE)
    ) {df$b36[i] = df$b35[i] + df$b34[i]}
  }
  #b35
  for (i in 1:length(df$b35)){
    if((is.na(df$b36[i]) == FALSE)&
       (is.na(df$b35[i]) == TRUE)&
       (is.na(df$b34[i]) == FALSE)
    ) {df$b35[i] = df$b36[i] - df$b34[i]}
  }
  #b34
  for (i in 1:length(df$b34)){
    if((is.na(df$b36[i]) == FALSE)&
       (is.na(df$b35[i]) == FALSE)&
       (is.na(df$b34[i]) == TRUE)
    ) {df$b34[i] = df$b36[i] - df$b35[i]}
  }
  
  #b34 et b35
  for (i in 1:length(df$b36)){
    if((is.na(df$b36[i]) == FALSE) &
       (is.na(df$b35[i]) == TRUE) &
       (is.na(df$b34[i]) == TRUE)
    ) {if (df$b36[i] %% 2 == 0 ){
      
      df$b34[i] = df$b36[i] %/% 2
      df$b35[i] = df$b36[i] %/% 2
      
    }else{
      
      df$b34[i] = ((df$b36[i] + 1) %/% 2)
      df$b35[i] = (df$b36[i] + 1) %/% 2 - 1
      
      }
    }
  }
  #----------------------------------------
  #c13
  for (i in 1:length(df$c13)){
    if((is.na(df$c13[i]) == TRUE)&
       (is.na(df$c12[i]) == FALSE)&
       (is.na(df$c11[i]) == FALSE)
    ) {df$c13[i] = df$c12[i] + df$c11[i]}
  }
  #c12
  for (i in 1:length(df$c12)){
    if((is.na(df$c13[i]) == FALSE)&
       (is.na(df$c12[i]) == TRUE)&
       (is.na(df$c11[i]) == FALSE)
    ) {df$c12[i] = df$c13[i] - df$c11[i]}
  }
  #c11
  for (i in 1:length(df$c11)){
    if((is.na(df$c13[i]) == FALSE)&
       (is.na(df$c12[i]) == FALSE)&
       (is.na(df$c11[i]) == TRUE)
    ) {df$c11[i] = df$c13[i] - df$c12[i]}
  }
  
  #c11 et c12
  for (i in 1:length(df$c13)){
    if((is.na(df$c13[i]) == FALSE) &
       (is.na(df$c12[i]) == TRUE) &
       (is.na(df$c11[i]) == TRUE)
    ) {if (df$c13[i] %% 2 == 0 ){
      
      df$c11[i] = df$c13[i] %/% 2
      df$c12[i] = df$c13[i] %/% 2
      
    }else{
      
      df$c11[i] = ((df$c13[i] + 1) %/% 2)
      df$c12[i] = (df$c13[i] + 1) %/% 2 - 1
      
      }
    }
  }
  #--------------------------------------------
  return(df)
}

df  = ImpSimple(df)
print(sum(is.na(df)))

#creation du matrice des valeurs totaux 
df_tot = df[,c('b13','b16','b23','b33','b36','c13')]

#imputation des valeurs totaux avec kNN : 

df_tot_imp = kNN(df_tot , variable = colnames(df_tot) , imp_var = FALSE , k = 10)
  
#remplacement des colonnes imputées : 

df$b13 = df_tot_imp$b13
df$b16 = df_tot_imp$b16
df$b23 = df_tot_imp$b23
df$b33 = df_tot_imp$b33
df$b36 = df_tot_imp$b36
df$c13 = df_tot_imp$c13

#imputation finale : 

df = ImpSimple(df)
df = kNN(df , variable = colnames(df) , imp_var = FALSE , k = 10)
print(sum(is.na(df)))
