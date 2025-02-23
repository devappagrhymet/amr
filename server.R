options(shiny.maxRequestSize=1000*1024^2)

function(input, output, session) {
  
  values <- reactiveValues(
    base_sum = NULL,
    col_base_sum = NULL,
    abs_col_base_sum = NULL,
    adap_col_base_sum = NULL,
    trans_col_base_sum = NULL,
    fs_col_base_sum = NULL,
    
    ext = NULL,
    bd_va = NULL,
    
    aa_axe_analyse = NULL,
    statdes = NULL,
    
    t_abs = NULL,
    t_adap = NULL,
    t_trans = NULL,
    
    cols_non_numeric_cab = NULL,
    cols_non_numeric_cad = NULL,
    cols_non_numeric_ctr = NULL,
    cols_non_numeric_fs = NULL,
    cols_non_numeric_pond = NULL,
  )
  
  # récupération des données chargées
  bd <- reactive({
    req(input$fichier_var_interm) # vérifier si le fichier de BD est chargé
    # Vérification du format de fichier
    ext <- tools::file_ext(input$fichier_var_interm$name)
    validate(need(ext %in% c("csv", "sav", "dta", "xlsx", "sas7bdat"), 
                  "fichier invalide. Charger un fichier au format .csv, .dta, .sav, .xlsx, .sas7bdat")
    )
    if (ext == "csv"){ # vérifier si c'est avec séparateur , ou point-virgule
      dataset <- fread(input$fichier_var_interm$datapath, encoding=input$encodage)
    } else if (ext == "sav") {
      dataset <- sjlabelled::read_spss(input$fichier_var_interm$datapath)
    } else if (ext == "dta") {
      dataset <- sjlabelled::read_stata(input$fichier_var_interm$datapath)
    }else if (ext == "xlsx") {
      dataset <- read_excel(input$fichier_var_interm$datapath)
    } else if (ext == "sas7bdat") {
      dataset <- sjlabelled::read_sas(input$fichier_var_interm$datapath)
    }
    
    values$ext <- ext # garder l'extension du fichier chargé dans une variable globale
    
    if (!colonnes_double(dataset)){
      return(dataset) # valeur de retour de la BD
    } else{
        return(NULL)
    }
  })

  # Code permettant de savoir si le fichier est chargé
  output$fileUploaded <- reactive({
    return(!is.null(bd()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  # Affichage des données dès que la base de données est chargée
  observeEvent(input$fichier_var_interm, {
    
    # req(bd()) # Vérifier si la base de données est chargée
    
    # Affichage des données du fichier chargé
    if (!colonnes_double(bd())){
      
      output$bd_var_interm <- DT::renderDataTable(
        bd(),
        options = list(scrollX = TRUE),
        rownames = FALSE
      ) # Fin Affichage des données du fichier chargé
      
      # sauvegarde de la base de données 
      output$save_bd_var_interm_csv <- exporter(".csv", bd()) # au format CSV
      output$save_bd_var_interm_xlsx <- exporter(".xlsx", bd()) # au format XLSX
      output$save_bd_var_interm_spss <- exporter(".sav", bd()) # au format STATA
      output$save_bd_var_interm_dta <- exporter(".dta", bd()) # au format STATA
      output$save_bd_var_interm_sas <- exporter(".sas7bdat", bd()) # au format STATA
      
      # après le téléchargement de la bd, mettre à jour les listes de choix pour les varaibles
      updateSelectInput(session, "var_absorption", choices = colnames(bd()))
      updateSelectInput(session, "var_adaptation", choices = colnames(bd()))
      updateSelectInput(session, "var_transformation", choices = colnames(bd()))
      updateSelectInput(session, "var_food_sec", choices = colnames(bd()))
      updateSelectInput(session, "axe_analyse", choices = colnames(bd()))
      updateSelectInput(session, "var_ponderation", choices = colnames(bd()))
    }
    
  })
  
  observeEvent({input$var_absorption}, {
    req(bd())
    # récupération des noms des colonnes sélectionnées
    values$abs_col_base_sum <- input$var_absorption
    t_abs <- length(values$abs_col_base_sum)
    if (t_abs < 3){
      output$txt_absorption <- renderText({"Vous devez choisir au moins 3 variables !"})
    }else{
      output$txt_absorption <- renderText("")
    }
    values$t_abs <- t_abs
  })
  
  observeEvent({input$var_adaptation}, {
    req(bd())
    # récupération des noms des colonnes sélectionnées
    values$adap_col_base_sum <- input$var_adaptation
    t_adap <- length(values$adap_col_base_sum)
    if (t_adap < 3){
      output$txt_adaptation <- renderText({"Vous devez choisir au moins 3 variables !"})
    }else{
      output$txt_adaptation <- renderText("")
    }
    values$t_adap <- t_adap
  })
  
  observeEvent({input$var_transformation}, {
    req(bd())
    # récupération des noms des colonnes sélectionnées
    values$trans_col_base_sum <- input$var_transformation
    t_trans <- length(values$trans_col_base_sum)
    if (t_trans < 3){
      output$txt_transformation <- renderText({"Vous devez choisir au moins 3 variables !"})
    }else{
      output$txt_transformation <- renderText("")
    }
    values$t_trans <- t_trans
  })
  
  observeEvent({input$var_food_sec}, {
    req(bd())
    # récupération des noms des colonnes sélectionnées
    values$fs_col_base_sum <- input$var_food_sec
    t_foodsec <- length(values$fs_col_base_sum)
    if (t_foodsec < 2){
      output$txt_foodsec <- renderText({"Vous devez choisir les 2 variables !"})
    }else{
      output$txt_foodsec <- renderText("")
    }
  })
  
  observeEvent({input$axe_analyse}, {
    # récupération des noms des colonnes sélectionnées
    values$aa_axe_analyse <- input$axe_analyse
  })
  
  observeEvent({input$var_ponderation}, {
    # récupération des noms des colonnes sélectionnées
    values$ponderation <- input$var_ponderation
    t_pond <- length(values$ponderation)
    if (t_pond >= 1){
      output$txt_ponderation <- renderText({""})
    }else{
      output$txt_ponderation <- renderText("Vous devez choisir la variable de ponderation !")
    }
  })
  
  observeEvent(input$CalculerCapacites, {
    
    req(input$var_absorption)
    req(input$var_adaptation)
    req(input$var_transformation)
    req(input$var_food_sec)
    req(input$var_ponderation)
    
    # Vérifier si les colonnes sélectionnées sont des numériques
    is_cols_nums_cab <- FALSE
    dbcab <- bd() %>% 
      select(values$abs_col_base_sum)
    dbcab <- as.data.frame(dbcab)
    cols_non_numeric_cab <- colonnes_non_numeriques(dbcab)
    values$cols_non_numeric_cab <- cols_non_numeric_cab
    if (is.null(cols_non_numeric_cab)){
      is_cols_nums_cab <- TRUE
    }
    
    is_cols_nums_cad <- FALSE
    dbcad <- bd() %>% 
      select(values$adap_col_base_sum)
    dbcad <- as.data.frame(dbcad)
    cols_non_numeric_cad <- colonnes_non_numeriques(dbcad)
    values$cols_non_numeric_cad <- cols_non_numeric_cad
    if (is.null(cols_non_numeric_cad)){
      is_cols_nums_cad <- TRUE
    }
    
    is_cols_nums_ctr <- FALSE
    dbctr <- bd() %>% 
      select(values$trans_col_base_sum)
    dbctr <- as.data.frame(dbctr)
    cols_non_numeric_ctr <- colonnes_non_numeriques(dbctr)
    values$cols_non_numeric_ctr <- cols_non_numeric_ctr
    if (is.null(cols_non_numeric_ctr)){
      is_cols_nums_ctr <- TRUE
    }
    
    is_cols_nums_fs <- FALSE
    dbfs <- bd() %>% 
      select(values$fs_col_base_sum)
    dbfs <- as.data.frame(dbfs)
    cols_non_numeric_fs <- colonnes_non_numeriques(dbfs)
    values$cols_non_numeric_fs <- cols_non_numeric_fs
    if (is.null(cols_non_numeric_fs)){
      is_cols_nums_fs <- TRUE
    }
    
    is_cols_nums_pond <- FALSE
    dbpond <- bd() %>% 
      select(values$ponderation)
    dbpond <- as.data.frame(dbpond)
    cols_non_numeric_pond <- colonnes_non_numeriques(dbpond)
    values$cols_non_numeric_pond <- cols_non_numeric_pond
    if (is.null(cols_non_numeric_pond)){
      is_cols_nums_pond <- TRUE
    }
    
    is_cols_nums <- is_cols_nums_cab && is_cols_nums_cad && is_cols_nums_ctr && is_cols_nums_fs && is_cols_nums_pond
    
    if ((values$t_abs >= 3) && (values$t_adap >= 3) && (values$t_trans >= 3) && is_cols_nums){
      
      # convertir les colonnes choisies en numéric
      values$col_base_sum <- c(values$abs_col_base_sum, values$adap_col_base_sum, values$trans_col_base_sum, values$fs_col_base_sum, values$ponderation)
      bd_va <- bd() %>%
        mutate_at(vars(values$col_base_sum), list(as.numeric))
      # bd_va$poids <- as.numeric(as.character(bd_va$poids))
      bd_va <- as.data.frame(bd_va)

      # Analyse descriptive des variables choisies
      base_temp <- bd_va %>% 
        select(values$col_base_sum)
      values$base_sum <- base_temp # sauvegarde de la base des variables choisies dans une variable globale
      bd_sum1 <- statdesc(base_temp)%>%
        select(outliers, na, median)
      bd_sum2 <- st(values$base_sum, out='return')
      colnames(bd_sum2) <- c("Variable","N", "Moyenne", "Ecart_type", "Minimum", "Quantile1", "Quantile3", "Maximum")
      bd_sum <-cbind(bd_sum1, bd_sum2)%>%
        select(N, outliers, na, Moyenne, Ecart_type, Minimum, Quantile1, Quantile3, Maximum)
      output$description_var <- DT::renderDataTable(
        bd_sum,
        options = list(scrollX = TRUE),
        rownames = TRUE
      ) # Fin Analyse descriptive des variables choisies
      
      # sauvegarde des données d'analyse descriptive 
      output$save_description_var_csv <- exporter(".csv", bd_sum) # au format CSV
      output$save_description_var_xlsx <- exporter(".xlsx", bd_sum) # au format XLSX
      
      # création du graphique de coorélation entre les variables de la capacité d'absorption
      db_cab <-bd_va %>% 
        select(values$abs_col_base_sum)
      theme_set(theme_bw())
      corr <- round(cor(db_cab), 2)
      graphique_corr_plot_cab <- ggcorrplot(corr=corr,
                                            hc.order=TRUE,
                                            lab=TRUE,
                                            lab_size=3,
                                            colors=c("red", "white", "blue"),
                                            title="Corrélation entre les variables du pilier CAB",
                                            ggtheme=theme_bw)
      output$corr_plot_abs <- renderPlot(graphique_corr_plot_cab)
      
      # création du graphique de coorélation entre les variables de la capacité d'adaptation
      db_cad <- bd_va %>% 
        select(values$adap_col_base_sum)
      theme_set(theme_bw())
      corr <- round(cor(db_cad), 2)
      graphique_corr_plot_cad <- ggcorrplot(corr=corr,
                                            hc.order=TRUE,
                                            lab=TRUE,
                                            lab_size=3,
                                            colors=c("red", "white", "blue"),
                                            title="Corrélation entre les variables du pilier CAD",
                                            ggtheme=theme_bw)
      output$corr_plot_adap <- renderPlot(graphique_corr_plot_cad)
      
      # création du graphique de coorélation entre les variables de la capacité de transformation
      db_ctr <- bd_va %>% 
        select(values$trans_col_base_sum)
      theme_set(theme_bw())
      corr <- round(cor(db_ctr), 2)
      graphique_corr_plot_trans <- ggcorrplot(corr=corr,
                                              hc.order=TRUE,
                                              lab=TRUE,
                                              lab_size=3,
                                              colors=c("red", "white", "blue"),
                                              title="Corrélation entre les variables du pilier CTR",
                                              ggtheme=theme_bw)
      output$corr_plot_trans <- renderPlot(graphique_corr_plot_trans)
      
      ### Capacité d'absorption (CAB) ############################################"
      ############################################################################"
      
      # récupération des noms des colonnes sélectionnées
      bd_cab <- bd_va %>% 
        select(values$abs_col_base_sum)
      noms_colonnes_cab <- names(bd_cab)
      
      estimations <- score_calcul(bd_cab)
      
      #Examination de la convergence (test de bartlett)
      bar_test <- estimations$bar_test
      
      #Examination de la qualité des analyses factorielles (KMO)
      kmo_stat <- estimations$kmo_stat
      
      #Matrice de comparaison des piliers
      KMO_mat <- tibble(pilier = "CAB", bartlett_stat = bar_test$chisq,
                        bartlett_p.value = bar_test$p.value,
                        KMO = kmo_stat$MSA)
      
      #Indice de possession de biens durables
      bd_va <- bd_va %>%
        dplyr::mutate(cab = estimations$score,
                      cab = corr_outliers(cab),
                      cab = 100*min_max(cab), .before = 1)
      
      #Contribution des variables choisies au pilier
      contrib_matrix_cab <- bd_va %>% 
        dplyr::summarise(across(all_of(noms_colonnes_cab), ~ cor(.x, cab))) %>% 
        dplyr::mutate_all(abs) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(sum_cor = sum(c_across(all_of(noms_colonnes_cab)))) %>% 
        dplyr::mutate_all(~(100 * .x / sum_cor)) %>% select(-sum_cor) %>% 
        pivot_longer(everything(), names_to = "variable",
                     values_to = "contribution") %>% 
        dplyr::mutate(pilier = "CAB",
                      KMO = kmo_stat$MSAi)
      
      contrib_matrix_cab <- contrib_matrix_cab %>% filter(pilier == "CAB") %>% 
        dplyr::mutate(description = noms_colonnes_cab,
                      .after = variable )
      
      contrib_matrix_cab2 <- contrib_matrix_cab %>% filter(pilier == "CAB") %>% 
        select(-pilier, -KMO) %>% 
        dplyr::mutate(description = noms_colonnes_cab,
                      .after = variable )
      
      # transformation des données pour dessiner le radar chart
      contrib_matrix_cab_wider <- contrib_matrix_cab %>%
        select(pilier, variable, contribution) %>%
        pivot_wider(names_from = variable, values_from = contribution)
      
      contrib_matrix_cab_wider <- contrib_matrix_cab_wider %>%
        select(-pilier)
      contrib_matrix_cab_wider <- as.data.frame(contrib_matrix_cab_wider)
      rownames(contrib_matrix_cab_wider) <- c("CAB")
      
      # Définition des plages de valeurs pour les variables : maximum and minimum
      max_min <- data.frame()
      for(i in 1:length(noms_colonnes_cab)){
        if (i==1){
          max_min <- data.frame(c(100, 0))
        }else{
          max_min <- cbind(max_min, c(100, 0))
        }
      }
      rownames(max_min) <- c("Max", "Min")
      colnames(max_min) <- names(contrib_matrix_cab_wider)
      
      # Bind the variable ranges to the data
      contrib_matrix_cab_wider_radar <- rbind(max_min, contrib_matrix_cab_wider)
      
      # afficher la contribution des variables à la capacité d'absorption
      output$contrib_var_absorption <- DT::renderDataTable(contrib_matrix_cab2,
                                                           options = list(scrollX = TRUE),
                                                           rownames = FALSE)
      # sauvegarde de df au format CSV
      output$save_contrib_var_absorption_csv <- exporter(".csv", contrib_matrix_cab2) # au format CSV
      output$save_contrib_var_absorption_xlsx <- exporter(".xlsx", contrib_matrix_cab2) # au format XLSX
      # dessiner le graphique pour les corrélation (radarplot)
      output$contrib_var_absorption_graph <- renderPlot(
        create_beautiful_radarchart(contrib_matrix_cab_wider_radar, caxislabels = c(0, 25, 50, 75, 100)),
      )
      
      ### Capacité d'adaptation (CAD) ############################################"
      ############################################################################"
      
      # récupération des noms des colonnes sélectionnées
      bd_cad <- bd_va %>% 
        select(values$adap_col_base_sum)
      noms_colonnes_cad <- names(bd_cad) 
      
      estimations <- score_calcul(bd_cad)
      
      #Examination de la convergence (test de bartlett)
      bar_test <- estimations$bar_test
      
      #Examination de la qualité des analyses factorielles (KMO)
      kmo_stat <- estimations$kmo_stat
      
      #Matrice de comparaison des piliers
      KMO_mat <- bind_rows(KMO_mat,
                           tibble(pilier = "CAD", 
                                  bartlett_stat = bar_test$chisq,
                                  bartlett_p.value = bar_test$p.value,
                                  KMO = kmo_stat$MSA))
      
      #Indice de possession de biens durables
      bd_va <- bd_va %>%
        dplyr::mutate(cad = estimations$score,
                      cad = corr_outliers(cad),
                      cad = 100*min_max(cad), .before = 2)
      
      #Contribution des variables au pilier
      contrib_matrix_cad <- bd_va %>% 
        dplyr::summarise(across(all_of(noms_colonnes_cad), ~ cor(.x, cab))) %>% 
        dplyr::mutate_all(abs) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(sum_cor = sum(c_across(all_of(noms_colonnes_cad)))) %>% 
        dplyr::mutate_all(~(100 * .x / sum_cor)) %>% 
        select(-sum_cor) %>% 
        pivot_longer(everything(), names_to = "variable",
                     values_to = "contribution") %>% 
        dplyr::mutate(pilier = "CAD", KMO = kmo_stat$MSAi)
      
      #Afficher la liste des variables
      contrib_matrix_cad <- contrib_matrix_cad %>% filter(pilier == "CAD") %>% 
        dplyr::mutate(description = noms_colonnes_cad,
                      .after = variable )
      
      contrib_matrix_cad2 <- contrib_matrix_cad %>% filter(pilier == "CAD") %>% 
        select(-pilier, -KMO) %>% 
        dplyr::mutate(description = noms_colonnes_cad,
                      .after = variable )
      
      # transformation des données pour dessiner le radar chart
      contrib_matrix_cad_wider <- contrib_matrix_cad %>%
        select(pilier, variable, contribution) %>%
        pivot_wider(names_from = variable, values_from = contribution)
      contrib_matrix_cad_wider <- contrib_matrix_cad_wider %>%
        select(-pilier)
      contrib_matrix_cad_wider <- as.data.frame(contrib_matrix_cad_wider)
      rownames(contrib_matrix_cad_wider) <- c("CAD")
      
      # Define the variable ranges: maximum and minimum
      max_min <- data.frame()
      for(i in 1:length(noms_colonnes_cad)){
        if (i==1){
          max_min <- data.frame(c(100, 0))
        }else{
          max_min <- cbind(max_min, c(100, 0))
        }
      }
      rownames(max_min) <- c("Max", "Min")
      colnames(max_min) <- names(contrib_matrix_cad_wider)
      
      # Bind the variable ranges to the data
      contrib_matrix_cad_wider_radar <- rbind(max_min, contrib_matrix_cad_wider)
      
      # afficher la contribution des variables à la capacité d'adaptation
      output$contrib_var_adaptation <- DT::renderDataTable(contrib_matrix_cad2,
                                                           options = list(scrollX = TRUE),
                                                           rownames = FALSE)
      # sauvegarde de df au format CSV
      output$save_contrib_var_adaptation_csv <- exporter(".csv", contrib_matrix_cad2) # au format CSV
      output$save_contrib_var_adaptation_xlsx <- exporter(".xlsx", contrib_matrix_cad2) # au format XLSX
      # dessiner le graphique pour les corrélation (radarplot)
      output$contrib_var_adaptation_graph <- renderPlot(
        create_beautiful_radarchart(contrib_matrix_cad_wider_radar, caxislabels = c(0, 25, 50, 75, 100)
        )
      )
      
      ### Capacité de transformation (CTR) ############################################"
      ############################################################################"
      
      # récupération des noms des colonnes sélectionnées
      bd_ctr <- bd_va %>% 
        select(values$trans_col_base_sum)
      noms_colonnes_ctr <- names(bd_ctr) 
      
      estimations <- score_calcul(bd_ctr)
      
      #Examination de la convergence (test de bartlett)
      bar_test <- estimations$bar_test
      
      #Examination de la qualité des analyses factorielles (KMO)
      kmo_stat <- estimations$kmo_stat
      
      #Matrice de comparaison des piliers
      KMO_mat <- bind_rows(KMO_mat,
                           tibble(pilier = "CTR", 
                                  bartlett_stat = bar_test$chisq,
                                  bartlett_p.value = bar_test$p.value,
                                  KMO = kmo_stat$MSA))
      
      #Indice CTR
      bd_va <- bd_va %>%
        dplyr::mutate(ctr = estimations$score,
                      ctr = corr_outliers(ctr),
                      ctr = 100*min_max(ctr), .before = 3)
      
      #Contribution des variables au pilier
      contrib_matrix_ctr <- bd_va %>% 
        dplyr::summarise(across(all_of(noms_colonnes_ctr), ~ cor(.x, cab))) %>%
        dplyr::mutate_all(abs) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(sum_cor = sum(c_across(all_of(noms_colonnes_ctr)))) %>% 
        dplyr::mutate_all(~(100 * .x / sum_cor)) %>% 
        select(-sum_cor) %>% 
        pivot_longer(everything(), names_to = "variable",
                     values_to = "contribution") %>% 
        dplyr::mutate(pilier = "CTR",
                      KMO = kmo_stat$MSAi)
      
      #Afficher la liste des variables
      contrib_matrix_ctr <- contrib_matrix_ctr %>% 
        filter(pilier == "CTR") %>% 
        dplyr::mutate(description = noms_colonnes_ctr,
                      .after = variable )
      
      contrib_matrix_ctr2 <- contrib_matrix_ctr %>% filter(pilier == "CTR") %>% 
        select(-pilier, -KMO) %>% 
        dplyr::mutate(description = noms_colonnes_ctr,
                      .after = variable )
      
      # transformation des données pour dessiner le radar chart
      contrib_matrix_ctr_wider <- contrib_matrix_ctr %>%
        select(pilier, variable, contribution) %>%
        pivot_wider(names_from = variable, values_from = contribution)
      contrib_matrix_ctr_wider <- contrib_matrix_ctr_wider %>%
        select(-pilier)
      contrib_matrix_ctr_wider <- as.data.frame(contrib_matrix_ctr_wider)
      rownames(contrib_matrix_ctr_wider) <- c("CTR")
      
      # Define the variable ranges: maximum and minimum
      max_min <- data.frame()
      for(i in 1:length(noms_colonnes_ctr)){
        if (i==1){
          max_min <- data.frame(c(100, 0))
        }else{
          max_min <- cbind(max_min, c(100, 0))
        }
      }
      rownames(max_min) <- c("Max", "Min")
      colnames(max_min) <- names(contrib_matrix_ctr_wider)
      
      # Bind the variable ranges to the data
      contrib_matrix_ctr_wider_radar <- rbind(max_min, contrib_matrix_ctr_wider)
      
      # afficher la contribution des variables à la capacité de transformation
      output$contrib_var_transformation <- DT::renderDataTable(contrib_matrix_ctr2,
                                                               options = list(scrollX = TRUE),
                                                               rownames = FALSE)
      # sauvegarde 
      output$save_contrib_var_transformation_csv <- exporter(".csv", contrib_matrix_ctr2) # au format CSV
      output$save_contrib_var_transformation_xlsx <- exporter(".xlsx", contrib_matrix_ctr2) # au format XLSX
      # dessiner le graphique pour les corrélation (radarplot)
      output$contrib_var_transformation_graph <- renderPlot(
        create_beautiful_radarchart(contrib_matrix_ctr_wider_radar, caxislabels = c(0, 25, 50, 75, 100)
        )
      )
      
      # Afficher la matrice de comparaison des capacités
      output$matrice_comparaison_capacites <- DT::renderDataTable(KMO_mat,
                                                                  options = list(scrollX = TRUE),
                                                                  rownames = FALSE)
      output$save_matrice_comparaison_capacites_csv <- exporter(".csv", KMO_mat) # au format CSV
      output$save_matrice_comparaison_capacites_xlsx <- exporter(".xlsx", KMO_mat) # au format XLSX
      
      ### Construction du modèle #######################################"
      # input$var_food_sec
      ##############################################################################"
      
      vars_food_sec <- values$fs_col_base_sum
      variablesFoodSec <- paste(vars_food_sec, collapse = " + ")
      
      ri_model <- paste("\n",
                        "#Relation avec les indicateurs secal\n", 
                        "RI =~ ", variablesFoodSec, "\n",
                        "#Relation avec les piliers\n", 
                        "RI ~ cab + cad + ctr\n")
      
      ri_model_estim <- sem(ri_model, data = bd_va)
      
      #Description du modèle
      summary(ri_model_estim, fit.measures = T)
      
      #Valeur de l'indice
      bd_va <- bd_va %>% 
        dplyr::mutate(RI = lavaan::lavPredict(ri_model_estim),
                      RI = 100 * min_max(RI), .before = 4)
      
      # classification des ménages
      bd_va <- bd_va %>%
        dplyr::mutate(categorie_resilience = case_when(
          RI >= 50 ~ "Ménages résilients",
          RI >= 30 & RI < 50 ~ "Ménages peu résilients",
          TRUE ~ "Non résilients"
        ), .before = 5)
      
      ####Test de Durbin-Watson
      # bptest(RI ~ cab + cad + ctr, data=bd_va)
      # resid <- residuals(ri_model_estim)
      # residModel <- lm(resid ~ cab + cad + ctr, data=bd_va)
      # dwTest <- durbinWatsonTest(residModel)
      # print(dwTest)
      
      # Creation de la distribution de "RI"
      distribution_ri <- ggplot(bd_va, aes(x = RI)) +
        geom_density(color = "blue", fill = "blue", alpha = 0.5) +  # Plot de la distribution de "RI"
        stat_function(fun = dnorm, args = list(mean = mean(bd_va$RI), sd = sd(bd_va$RI)), color = "red") +  # Plot de la courbe de la distribution normale 
        xlab("RI") +  # Label for x-axis
        ylab("Density") +  # Label for y-axis
        ggtitle("Distribustion de la loi normale et celle de RI")
      
      # Affichage de la Matrice de structure de résilience 
      output$distribution_ri_graph <- renderPlot(
        distribution_ri
      )
      
      ###diagramme QQ-plot
      diagramme_qqplot <- ggplot(bd_va, aes(sample = RI)) +
        stat_qq() + 
        stat_qq_line() +  
        xlab("Quantiles théoriques") + 
        ylab("Quantiles de l'échantillon") +
        ggtitle("Courbe QQplot de RI vs. Distribution normale")
      
      # Affichage du diagramme QQ-plot 
      output$diagramme_qqplot_graph <- renderPlot(
        diagramme_qqplot
      )
      
      
      values$bd_va <- bd_va
      
      ### ANALYSE DE L'INDICE #########################################################################"
      #################################################################################################"
      
      capacite_ri <- bd_va
      
      # DEBUT affichage des capacités calculées
      output$capacites_ri <- DT::renderDataTable(
        capacite_ri,
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
      # sauvegarde des capacités calculées
      output$save_capacites_ri_csv <- exporter(".csv", capacite_ri) # au format CSV
      output$save_capacites_ri_xlsx <- exporter(".xlsx", capacite_ri) # au format XLSX
      output$save_capacites_ri_spss <- exporter(".sav", capacite_ri) # au format STATA
      output$save_capacites_ri_dta <- exporter(".dta", capacite_ri) # au format STATA
      
      # CALCUL DE L'INDICE MOYEN GOLBAL
      statdes_global <- bd_va %>%
        dplyr::mutate_if(is.numeric, round, digits = 2) %>%
        dplyr::summarise(
          Moyenne = mean(RI, na.rm = TRUE),
          Ecart_type = sd(RI, na.rm = TRUE),
          Minimum = min(RI, na.rm = TRUE),
          Maximum = max(RI, na.rm = TRUE),
          Médiane = median(RI, na.rm = TRUE)
        ) 
      
      # Affichage de L'INDICE MOYEN GOLBAL
      output$irm <- renderText({
        statdes_global$Moyenne
      })
      
      # construction de la matrice de structure de la résilience
      rsm_cilss_ensemble <- bd_va %>%
        dplyr::summarise(across(c(cab, cad, ctr), ~ cor(.x, RI))) %>%
        dplyr::mutate_all(abs) %>% 
        dplyr::mutate(sum_cor = sum(cab, cad, ctr)) %>% 
        dplyr::mutate_all(~(100 * .x / sum_cor)) %>% select(-sum_cor) %>% 
        dplyr::mutate(niveau = as_factor("Ensemble"), .before = 1) %>% 
        dplyr::mutate(Regroupement = as_factor("global"))
      
      rsm_cilss_ensemble_longer <- rsm_cilss_ensemble %>%
        select(-Regroupement, -niveau) %>%
        pivot_longer(cols=c('cab', 'cad', 'ctr'),
                     names_to='capacites',
                     values_to='contribution') %>%
        dplyr::mutate_if(is.numeric, round, digits = 2)
      
      stucture_matrice_resilience <- ggplot(data=rsm_cilss_ensemble_longer, aes(x=capacites, y=contribution, fill=capacites)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=contribution), color="white", hjust = 1, fontface = "bold", size=5)+
        labs(x = "Capacités",
             y = "Contribution des capacités à l'indice de résilience ") +
        scale_fill_manual(
          values = c(
            "#d4ac0d",
            "#000080",
            "#008000"
          )
        )+
        theme(axis.title = element_text(size = 18, color = "#873600", face = "bold")) +
        theme(axis.text = element_text(size = 14, color = "red")) +
        coord_flip()
      
      # Affichage de la Matrice de structure de résilience 
      output$structure_resilience <- renderPlot(
        stucture_matrice_resilience
      )
    } else {
      # is_cols_nums <- is_cols_nums_cab && is_cols_nums_cad && is_cols_nums_ctr
      if (!is_cols_nums_cab){
        output$txt_absorption <- renderText({paste("Colonne(s) non numérique(s) : ", values$cols_non_numeric_cab)})
      }
      if (!is_cols_nums_cad){
        # output$txt_adaptation <- renderText({"Vous devez choisir uniquement des variables numériques !"})
        output$txt_adaptation <- renderText({paste("Colonne(s) non numérique(s) : ", values$cols_non_numeric_cad)})
      }
      if (!is_cols_nums_ctr){
        # output$txt_transformation <- renderText({"Vous devez choisir uniquement des variables numériques !"})
        output$txt_transformation <- renderText({paste("Colonne(s) non numérique(s) : ", values$cols_non_numeric_ctr)})
      }
      if (!is_cols_nums_fs){
        output$txt_foodsec <- renderText({"Vous devez choisir uniquement des variables numériques !"})
        output$txt_foodsec <- renderText({paste("Colonne(s) non numérique(s) : ", values$cols_non_numeric_fs)})
      }
      if (!is_cols_nums_pond){
        output$txt_ponderation <- renderText({"Vous devez choisir uniquement des variables numériques !"})
      }
    }
    
  })
  
  observeEvent(input$lancerAnalyse, {
    
    ### ANALYSE PAR ZONE ########################################################3###################"
    #################################################################################################"
    
    if (!is.null(values$bd_va)){
      bd_va <- values$bd_va
      
      
      #### statistiques descriptives sur la variable RI selon l'axe d'analyse
      # voir si l'axe est une variable labelisée
      statdes <- bd_va %>% as_label() %>% 
        dplyr::group_by(.[[values$aa_axe_analyse]]) %>%
        dplyr::summarise(
          Moyenne = mean(RI, na.rm = TRUE),
          Ecart_type = sd(RI, na.rm = TRUE),
          Minimum = min(RI, na.rm = TRUE),
          Maximum = max(RI, na.rm = TRUE),
          Médiane = median(RI, na.rm = TRUE)
        ) %>%
        dplyr::mutate_if(is.numeric, round, digits = 2) %>%
        ungroup()
      colnames(statdes) <- c("Axe", "Moyenne", "Ecart-type", "Minimum", "Maximum", "Médiane")
      
      # afficher les informations descriptives sur l'indice de résilience selon l'axe d'analyse
      output$stats_zone <- DT::renderDataTable(statdes,
                                               options = list(scrollX = TRUE),
                                               rownames = FALSE)
      #sauvegarde 
      output$save_stats_zone_csv <- exporter(".csv", statdes) # au format CSV
      output$save_stats_zone_xlsx <- exporter(".xlsx", statdes) # au format XLSX
      
      # graphique sur la moyenne de l'indice de résilience selon l'axe d'analyse
      barplot_indice_Moy_region <- ggplot(data=statdes, aes(x=reorder(Axe, Moyenne), y=Moyenne)) +
        # geom_bar(stat="identity", fill="steelblue", position=position_dodge())+
        # geom_col(width = 0.5, fill="DodgerBlue") +
        geom_col(width = 0.5, fill="#008000") +
        geom_text(aes(label=Moyenne), color="white", hjust = 1, fontface = "bold", size=5)+
        labs(x = "Axe d'analyse",
             y = "Moyenne de l'indice de résilience",
        ) +
        #scale_fill_brewer(palette="Paired")+
        theme(axis.text.x = element_text(size = 12, angle = 90)) +
        theme(axis.title = element_text(size = 18, color = "#873600", face = "bold")) +
        # theme(axis.text.x = element_text(size = 6, angle = 90), axis.text.y = element_text(size = 7))
        # theme_minimal()
        coord_flip()
      
      # affichage du graphique sur la moyenne de l'indice de résilience selon l'axe d'analyse
      output$barplot_indiceMoy_region <- renderPlot(
        barplot_indice_Moy_region
      )
      
      #Contribution des capacités à l'indice Suivant l'axe d'analyse
      ##########################################################################"
      contibution_cpacite_ri <- bd_va %>% as_label() %>% 
        dplyr::group_by(.[[values$aa_axe_analyse]]) %>% 
        dplyr::summarise(across(c(cab, cad, ctr), ~ cor(.x, RI))) %>%
        dplyr::mutate_at(vars(cab, cad, ctr), abs) %>% rowwise() %>%
        dplyr::mutate(sum_cor = sum(cab, cad, ctr)) %>%  
        dplyr::mutate_at(vars(cab, cad, ctr), ~(100 * .x / sum_cor)) %>%
        select(-sum_cor) %>% ungroup() %>%
        dplyr::mutate_if(is.numeric, round, digits = 2)
      
      colnames(contibution_cpacite_ri) <- c("Axe", "cab", "cad", "ctr")
      
      contibution_cpacite_ri_longer <- contibution_cpacite_ri %>%
        pivot_longer(cols=c('cab', 'cad', 'ctr'),
                     names_to='capacites',
                     values_to='contribution')
      
      # afficher la contribution des capacités à l'indice de résilience : 
      ##########################################################################"
      output$contribution_capacites_ri <- DT::renderDataTable(contibution_cpacite_ri,
                                                              options = list(scrollX = TRUE),
                                                              rownames = FALSE)
      # sauvegarde
      output$save_contribution_capacites_ri_csv <- exporter(".csv", contibution_cpacite_ri) # au format CSV
      output$save_contribution_capacites_ri_xlsx <- exporter(".xlsx", contibution_cpacite_ri) # au format XLSX
      
      # Création du graphique cumulé sur la contribution des capacités à l'indice de résilience
      barplot_contibution_cpacite_ri_cumule <- ggplot(data=contibution_cpacite_ri_longer, aes(x=Axe, y=contribution, fill=capacites)) +
        geom_col()+
        geom_text(aes(label=contribution), position="stack", vjust=1.6, 
                  color="white", size=3.5) +
        scale_fill_manual(
          values = c(
            "#d4ac0d",
            "#000080",
            "#008000"
          )
        )+
        theme_minimal() 
      
      #brplot <- graph_tab(bd_classes_resilience$Axe, bd_classes_resilience$Categorie, bd_classes_resilience$poids, "Classes de résilience")
      
      # affichage du graphique diagramme à barre cumulé sur la contribution des capacités à l'indice de résilience
      output$barplot_contribution_capacites_ri_cumule <- renderPlot(
        barplot_contibution_cpacite_ri_cumule
        #brplot
      )
      
      # Classe de résilience par axe : values$ponderation
      les_champs <- c(values$aa_axe_analyse, "categorie_resilience", values$ponderation)
      bd_classes_resilience <- bd_va %>% as_label() %>%
        select(all_of(les_champs))
      colnames(bd_classes_resilience) = c("Axe", "Categorie", "poids")
      
      #cl_resili <- bd_classes_resilience %>%
      #  group_by(Axe) %>%
      #  add_count(Categorie, name = "cl") %>%
      #  distinct() %>%
      #  pivot_wider(names_from = c(Categorie),
      #              values_from = c(cl)
      #  )
      # print(bd_classes_resilience)
      
      # axe_analyse <- paste('bd_va$', values$aa_axe_analyse, sep="")
      classes_resilience <- format_tab(bd_classes_resilience$Axe, bd_classes_resilience$Categorie, 
                                       bd_classes_resilience$poids, "Classe de résilience")
      
      # afficher classes de rsilience
      output$classe_resilience <- DT::renderDataTable(classes_resilience,
                                                      options = list(scrollX = TRUE),
                                                      rownames = FALSE)
      #sauvegarde 
      output$save_classe_resilience_csv <- exporter(".csv", classes_resilience) # au format CSV
      output$save_classe_resilience_xlsx <- exporter(".xlsx", classes_resilience) # au format XLSX
      
    }
    
  })
  
}