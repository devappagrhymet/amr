# install.packages("openxlsx")
# library(vtable)
# install.packages("corrplot")

library(shiny)
library(haven)
# install.packages("haven")

library(ggcorrplot)
# install.packages("ggcorrplot")

# library(flextable)
# install.packages("flextable")

library(kableExtra)
# install.packages("kableExtra")

library(psych)
# install.packages("psych")

library(knitr)
# install.packages("knitr")

library(tidyverse)
# install.packages("tidyverse")

library(sjlabelled)
# install.packages("sjlabelled")

library(rstatix)
# install.packages("rstatix")

library(DEoptimR)
# install.packages("DEoptimR")

library(questionr)
# install.packages("questionr")

library(lavaan)
# install.packages("lavaan")

# library(grid)
# install.packages("grid")

library(gridExtra)
# install.packages("gridExtra")

library(ggplot2)
# install.packages("ggplot2")

library(ggsignif)
# install.packages("ggsignif")

library(ggdendro)
# install.packages("ggdendro")

library(maps)
# install.packages("maps")

library(mapproj)
# install.packages("mapproj")

library(RColorBrewer)
# install.packages("RColorBrewer")

library(GGally)
# install.packages("GGally")

library(patchwork)
# install.packages("patchwork")

library(plotly)
# install.packages("plotly")

library(palmerpenguins)
# install.packages("palmerpenguins")

library(DT)
# install.packages("DT")

library(fmsb)
# install.packages("fmsb")

# library(plyr)

# library(xlsx)
# install.packages("xlsx")

library(openxlsx)
# install.packages("openxlsx")

library(vtable)
# install.packages("vtable")

library(data.table)
# install.packages("data.table")

library(highcharter)
# install.packages("highcharter")

library(readr)
# install.packages("readr")

library(readxl)
# install.packages("readxl")

library(lmtest)
# install.packages("lmtest")

library(car)
# install.packages("car")

library(glue)
# install.packages("glue")


#### fonctions helpers ####

# fonction permettant de détecter les colonnes non numériques
colonnes_non_numeriques <- function(donnees){
  colonnes <- names(donnees)
  colonnes_non_numeriques <- c()
  donnees <- donnees %>% mutate_if(is.character, as.numeric)
  for (i in 1:length(colonnes)){
    df_c <- na.omit(donnees[,i])
    if (length(df_c) == 0){
      colonnes_non_numeriques <- colonnes[i]
    }
  }
  if (length(colonnes_non_numeriques) >= 1){
    return(colonnes_non_numeriques)
  }else{
    return(NULL)
  }
}


# sauvegarde de la base de données 
exporter <- function(extension_fichier, donnees){
    downloadHandler(
      filename = function(){
        paste('bd_',Sys.Date(), extension_fichier)
      },
      content <- function(file){
        if (extension_fichier == ".csv"){ # format csv
          write.csv(donnees, file, row.names = FALSE)
        } else if (extension_fichier == ".xlsx") { # format xlsx
          write.xlsx(donnees, file)
        } else if (extension_fichier == ".dta") { # format stata
          sjlabelled::write_stata(donnees, file)
        } else if (extension_fichier == ".sav") { # format spss
          sjlabelled::write_spss(donnees, file)
        }else if (extension_fichier == ".sas7bdat") { # format sas
          sjlabelled::write_sas(donnees, file)
        }
      }
    ) 
}

# sauvegarde une image
exporter_image <- function(extension_fichier, imageplot){
  downloadHandler(
    filename = function(){
      paste('img',Sys.Date(), extension_fichier)
    },
    content <- function(file){
      if (extension_fichier == ".png"){ # format csv
        png()
      } else if (extension_fichier == ".pdf") { # format xlsx
        pdf()
      } else if (extension_fichier == ".jpg") { # format stata
        jpeg()
      } 
      imageplot
      dev.off()
    }
  ) 
}

colonnes_double <- function(df){
  col_df <- names(df)
  if (length(col_df) == length(unique(col_df))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

outlier <- function(donnees, i){
  donnees%>%
    mutate(lower_bound=quantile(donnees[,i],0.25),
           upper_bound=quantile(donnees[,i],0.75))%>%
    mutate(is_outlier=if_else(donnees[,i]<lower_bound|donnees[,i]>upper_bound,TRUE,FALSE))%>%
    summarise(outliers=sum(is_outlier))
}

statdesc <- function(dataset){
  nrow = ncol(dataset)
  data_summary = data.frame(matrix(nrow = nrow, ncol = 9))
  rownames(data_summary) = colnames(dataset)
  colnames(data_summary) = c("min", "StdDev","first_quart","median","mean","third_quart","max","na", "outliers")
  for(i in 1:ncol(dataset)){ 
    data_summary$min[i] = min(dataset[,i],na.rm = T)
    data_summary$StdDev[i] = sd(dataset[,i],na.rm = T)
    data_summary$first_quart[i] = as.numeric(summary(dataset[,i])[2])
    data_summary$median[i] = median(dataset[,i],na.rm = T)
    data_summary$mean[i] = mean(dataset[,i],na.rm = T)
    data_summary$third_quart[i] = as.numeric(summary(dataset [,i])[5])
    data_summary$max[i] = max(dataset[,i],na.rm = T)
    data_summary$na[i] = length(which(is.na(dataset[,i])))
    data_summary$outliers[i] = outlier(dataset,i)
  }
  return(data_summary)
}

#Création d'une fonction permettant de faire un tableau croisé
cross_tab <- function(var_ligne, var_col, poids){
  df <- tibble(var_ligne, var_col, poids)
  tab1 <- df %>% 
    select(var_col, poids) %>% 
    sjlabelled::as_label() %>% 
    group_by(var_col) %>% 
    dplyr::summarise(effectif = sum(poids), .groups = 'drop') %>% 
    dplyr::mutate(var_ligne = as_factor("Ensemble"), .before = 1)
  
  tab <- df %>% 
    select(var_ligne, var_col, poids) %>% 
    sjlabelled::as_label() %>%
    group_by(var_ligne, var_col) %>% 
    dplyr::summarise(effectif = sum(poids), .groups = 'drop') %>% 
    bind_rows(tab1) %>% 
    group_by(var_ligne) %>% 
    dplyr::mutate(pct = round(100 * effectif / sum(effectif),1)) %>% 
    select(var_ligne, var_col, pct) 
  
  return(tab)
}

cross_tab2 <- function(var_ligne, var_col, poids){
  df <- tibble(var_ligne, var_col, poids)
  
  tab <- df %>% 
    select(var_ligne, var_col, poids) %>% 
    sjlabelled::as_label() %>%
    group_by(var_ligne, var_col) %>% 
    dplyr::summarise(effectif = sum(poids), .groups = 'drop') %>% 
    group_by(var_ligne) %>% 
    dplyr::mutate(pct = round(100 * effectif / sum(effectif),1)) %>% 
    select(var_ligne, var_col, pct) 
  
  return(tab)
}

#Fonction permettant la mise en forme d'un tableau rectangulaire
mise_en_forme <- function(tableau, nom_tableau, digits = 1,
                          ...){
  #Nombre de lignes du tableau
  mod <- nrow(tableau)
  tableau %>% 
    kable(format = "html", booktabs=T,  digits = digits,
          format.args = list(decimal.mark = ",", big.mark = " "),
          caption = nom_tableau, ...)%>%
    kable_styling(latex_options = c("striped", "HOLD_position",
                                    "condensed"),
                  full_width = F, position = "center", font_size = 9) %>% 
    row_spec(mod, bold = T) %>% 
    column_spec(1, bold = T)
}


format_tab <- function(var_ligne, var_col, poids, nom_tableau){
  
  #Obtenir le tableau de contingence
  tab <- cross_tab2(var_ligne, var_col, poids)
  
  #Agréger suivant la variable en ligne  
  tab <- tab %>% 
    pivot_wider(names_from = var_col, 
                values_from = pct, values_fill = 0) 
  
  #Sortie
  # mise_en_forme(tab, nom_tableau)
  
}


#Création d'une fonction ggplot2 pour faire les graphiques

graph_tab <- function(var_ligne, var_col, poids, fil){
  
  
  
  df <- cross_tab(var_ligne, var_col, poids)
  
  library(khroma)
  ggplot(df) +
    aes(x = var_ligne,  y = pct, fill = var_col) +
    geom_col(position = "fill", width = .8) +
    geom_text(
      aes(y = pct, label = pct), position = position_fill(.5),
      colour = "white", fontface = "bold", size = 3.5, hjust = .5
    ) +
    scale_x_discrete(labels = scales::label_wrap(50)) +
    scale_y_continuous( expand = c(0, 0)) +
    scale_fill_bright() + #package khroma
    labs(x = "", y = "", fill = fil) +
    #guides(x = guide_axis(angle = 90))+ #Mettre le label des bar en vertical
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = .5,
                                margin = margin(t = 5, b = 15)), 
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = -90, colour = "black", 
                                 vjust = 1, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(size = 9, face = "italic", hjust = 0,
                                  margin = margin(t = 15)),
      plot.margin = margin(2, 2, 2, 2),
      plot.background = element_rect(colour = "black", size = 1),
      legend.text = element_text(size = 10, colour = "black"),
      legend.title = element_text(size = 10, face = "bold", 
                                  colour = "black"),
      panel.background = element_rect(colour = "black", size = 0.5)) 
  
}

graph_tab2 <- function(var_ligne, var_col, poids){
  
  df <- cross_tab(var_ligne, var_col, poids)
  
  library(khroma)
  ggplot(df) +
    aes(x = var_ligne,  y = pct, fill = var_col) +
    geom_col(position = "fill", width = .8) +
    geom_text(
      aes(y = pct, label = pct), position = position_fill(.5),
      colour = "white", fontface = "bold", size = 3.5, hjust = .5
    ) +
    scale_x_discrete(labels = scales::label_wrap(50)) +
    scale_y_continuous( expand = c(0, 0)) +
    scale_fill_bright() + #package khroma
    labs(x = "", y = "") +
    #guides(x = guide_axis(angle = 90))+ #Mettre le label des bar en vertical
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = .5,
                                margin = margin(t = 5, b = 15)), 
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = -90, colour = "black", 
                                 vjust = 1, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(size = 9, face = "italic", hjust = 0,
                                  margin = margin(t = 15)),
      plot.margin = margin(2, 2, 2, 2),
      plot.background = element_rect(colour = "black", size = 1),
      legend.text = element_text(size = 10, colour = "black"),
      legend.title = element_text(size = 10, face = "bold", 
                                  colour = "black"),
      panel.background = element_rect(colour = "black", size = 0.5)) 
  
}

#Création d'une fonction permettant de donner les statistiques descriptives d'une variable quantitative.

univarie <- function(nom_var_quanti, nom_var_quali, nom_weight, 
                     nom_tableau, ...) { 
  
  df <- tibble(nom_var_quanti, nom_var_quali, nom_weight)
  
  
  #Application du test pour comparer les distributions
  
  res.test <- kruskal.test(df$nom_var_quanti ~ df$nom_var_quali)
  
  #Test utilisé pour comparer les distributions
  cat("On a utilisé le test suivant : ", res.test$method, "\n")
  
  #Statistique de test
  cat("La statistique de test vaut : ", round(res.test$statistic, 5), "\n")
  
  #p-value associée
  cat("La p-value associée vaut : ", res.test$p.value, "\n")
  
  #Conclusion
  if(res.test$p.value < 0.005){
    cat("On en déduit donc que les moyennes sont significativement différentes", "\n")
  } else {
    cat("On en déduit donc que les moyennes ne sont significativement différentes", "\n")
  }
  df <- as_label(df)
  res <- df %>%  
    group_by(nom_var_quali) %>%
    mutate(avg = weighted.mean(nom_var_quanti, nom_weight,
                               na.rm = TRUE)) %>%  
    summarise(n = n(),
              n_outlier = sum(is_outlier(nom_var_quanti, coef = 3), 
                              na.rm = T),
              moyenne = weighted.mean(nom_var_quanti, nom_weight, na.rm = TRUE),
              mediane = median(nom_var_quanti, na.rm = T),
              ecart.type = sqrt(
                sum(nom_weight * (nom_var_quanti - avg) ^ 2, na.rm = T )
                /sum(nom_weight, na.rm = T)),
              minimum = min(nom_var_quanti, na.rm = T),
              maximum = max(nom_var_quanti, na.rm = T))
  
  #Constitution d'une base pour les besoins du boxplot
  dta <- res
  
  #Calcul des bornes de l'intervalle de confiance
  dta$IC_inf <- with(dta, moyenne - 2 * ecart.type)
  dta$IC_sup <- with(dta, moyenne + 2 * ecart.type)
  
  # Réalisation du boxplot vertical
  boxplot(df$nom_var_quanti ~ df$nom_var_quali, ...)
  
  #Ajout du point rouge correspondant à la moyenne
  stripchart(moyenne ~ nom_var_quali,
             data = dta,
             method = "overplot",
             pch = 19,
             col = "red",
             vertical = TRUE,
             add = TRUE)
  
  #Ajout du trait de la borne inf de l'intervalle
  stripchart(IC_inf ~ nom_var_quali,
             data = dta,
             method = "stack",
             pch = 95,
             col = "red",
             vertical = TRUE,
             add = TRUE)
  
  #Ajout du trait de la borne sup de l'intervalle
  stripchart(IC_sup ~ nom_var_quali,
             data = dta,
             method = "stack",
             pch = 95,
             col = "red",
             vertical = TRUE,
             add = TRUE)
  
  # sortie
  mise_en_forme(res, nom_tableau)
  
}


#Création d'une fonction permettant d'inclure dans le rapport le résumé stats d'une variable quantitative.
stat_desc <- function(var_ligne, var_col, poids, nom_tableau){
  df <- tibble(var_ligne, var_col, poids)
  
  
  #Statistiques descriptives sur l'ensemble de la base
  
  tab <- df %>% select(var_col, poids) %>% 
    mutate(avg = weighted.mean(x = var_col, w = poids, na.rm = TRUE)) %>%  
    summarise(n = n(),
              moyenne = weighted.mean(x = var_col, w = poids, na.rm = TRUE),
              mediane = median(var_col, na.rm = T),
              ecart.type = sqrt(sum(poids * (var_col - avg) ^ 2, na.rm = T )
                                /sum(poids, na.rm = T)),
              minimum = min(var_col, na.rm = T),
              maximum = max(var_col, na.rm = T)) %>% 
    mutate(var_ligne = as_factor("Ensemble"), .before = 1)
  
  #Statistiques descriptives par catégories
  res <- df %>% as_label() %>% 
    group_by(var_ligne) %>%
    mutate(avg = weighted.mean(x = var_col, w = poids, na.rm = TRUE)) %>%  
    summarise(n = n(),
              moyenne = weighted.mean(x = var_col, w = poids, na.rm = TRUE),
              mediane = median(var_col, na.rm = T),
              ecart.type = sqrt(sum(poids * (var_col - avg) ^ 2, na.rm = T )
                                /sum(poids, na.rm = T)),
              minimum = min(var_col, na.rm = T),
              maximum = max(var_col, na.rm = T)) %>%
    bind_rows(tab) 
  
  
  
  #Noms des variables du tableau
  colnames(res) <- c(get_label(df$var_ligne), names(res)[2:7])
  
  mise_en_forme(res, nom_tableau)
}



score_calcul=function(grp_var, ...){
  ### Initial factor analysis
  M1 = fa(grp_var,  rotate =  "none", nfactors = length(grp_var), ...)
  p1_= as.matrix(M1$loadings)
  P2_ = as.vector(colSums(p1_^2))
  p3_ = P2_/length(grp_var)
  Pov_ = p3_/sum(p3_)
  cum_pov_ = cumsum(Pov_)
  numf= min(which(cum_pov_ > 0.55))
  
  ### Final estimation
  M_<-fa(grp_var, rotate="none", nfactors = numf, scores="Bartlett", fm="pa",SMC = FALSE)
  
  # ## calculate weight
  M1_= as.matrix(M_$loadings)
  M2_ = as.vector(colSums(M1_^2))
  M3_ = M2_/length(grp_var)
  s_Pov_ = M3_/sum(M3_)
  s_cum_pov_ = cumsum(s_Pov_)
  
  weight_ =s_Pov_
  weight_ = head(weight_, numf) 
  weight_score = sweep(M_$scores, MARGIN=2, weight_, `*`)
  score.sum = apply(weight_score,1,sum)
  
  #Examination de la qualité des analyses factorielles (KMO)
  kmo_stat <- KMO(grp_var)
  
  #Examination de la convergence (test de bartlett)
  bar_test <- cortest.bartlett(M_$residual, n=M_$n.obs, diag=TRUE)
  
  
  ## generate index pillar
  score = score.sum/sum(weight_)
  
  resultats = list(kmo_stat = kmo_stat, bar_test = bar_test, score = score)
  return(resultats)
}

#Sélection de la liste des biens
#Création d'une fonction permettant de calculer le taux de possession d'un bien
owner <- function(x){
  ind  <-  ifelse(is.na(x) | x ==0, 0, 1)
  pct  <-  100*sum(ind) / length(x)
  
  return(pct)
}

#Fonction permettant de normaliser une variable
min_max <- function(x,...) {
  res <- (x - min(x, na.rm = T)) / diff(range(x))
  return(res)
}


#Fonction permettant de corriger les valeurs aberantes
corr_outliers <- function(x){
  
  #Calcul des valeurs de remplacement en excluant les outliers
  med <- median(x[!is_outlier(x, coef = 3)], na.rm = T)
  
  #Imputation des valeurs
  x[is_outlier(x, coef = 3)] <- med
  
  return(x)
}

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}




