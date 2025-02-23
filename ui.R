fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # Titre de l'application :AMR
  # titlePanel("Analyse et Mesure de la Résilience (AMR)"),
  fluidRow(
    column(width = 1, 
           img(id = "LogoCILSS", src = "Logo CILSS HAUTE RESOLUTION.jpg", style = "width: 100%; padding: 0;"),
    ),
    column(width = 9, 
           titlePanel("Analyse et Mesure de la Résilience (AMR)"),
    ),
    column(width = 2, 
           img(id = "LogoCILSS", src = "FRSP-COULEUR_normal-1024x463.jpg", style = "width: 100%; padding: 0;"),
    ),
    
  ),
  # template avec 2 parties : menu vertical et fenêtre principale
  sidebarLayout(
    
    # Barre latérale gauche
    sidebarPanel(
      # importation de la base de données
      fileInput("fichier_var_interm", 
                "Charger votre fichier (.sav, .dta, .xlsx, .csv, .sas7bdat)",
                buttonLabel = "Charger ...",
                accept = c(".sav", ".dta", ".xlsx", ".csv", ".sas7bdat")
      ), # Fin fileInput
      
      hr(),
      
      #conditionalPanel(
      #  condition = "output.fileUploaded", # test pour voir si le fichier est chargé pour afficher le bouton
        selectInput("var_absorption", 
                    "Variables pour le calcul de la capacité d'absorption", 
                    choices = NULL,
                    multiple = TRUE),
        conditionalPanel(
          condition = "length(input$var_absorption) < 3",
          span(textOutput("txt_absorption"), style="color:red; font-style:italic")
        ),
        
        selectInput("var_adaptation", 
                    "Variables pour le calcul de la capacité d'adaptation", 
                    choices = NULL,
                    multiple = TRUE),
        conditionalPanel(
          condition = "length(input$var_adaptation) < 3",
          span(textOutput("txt_adaptation"), style="color:red; font-style:italic")
        ),
        
        selectInput("var_transformation", 
                    "Variables pour le calcul de la capacité de transformation", 
                    choices = NULL,
                    multiple = TRUE),
        conditionalPanel(
          condition = "length(input$var_transformation) < 3",
          span(textOutput("txt_transformation"), style="color:red; font-style:italic")
        ),
        
        selectInput("var_food_sec", 
                    "Variables de sécurité alimentaire", 
                    choices = NULL,
                    multiple = TRUE),
        span(textOutput("txt_foodsec"), style="color:red; font-style:italic"),
        
        selectInput("var_ponderation", 
                    "Variable de Pondération", 
                    choices = NULL,
                    multiple = FALSE),
        span(textOutput("txt_ponderation"), style="color:red; font-style:italic"),
        
        actionButton("CalculerCapacites", "Valider")
      #),
      
    ), # Fin sidebarPanel, Barre latérale gauche
    
    
    # Fenêtre principale
    mainPanel(
      
      # Bloc des onglets
      tabsetPanel(
        
        # A propos
        tabPanel(
          "A propos ...",
          br(),br(),
          
          tags$div(
            tags$p(
              h2("Introduction"),
              hr(),
            ),
            tags$p(
              "L’indice de résilience (RI) développé avec l’approche d’Analyse et de Mesure de la Résilience (AMR) 
              est le fruit de la collaboration entre le CILSS et la FAO, en réponse aux besoins exprimés par les pays 
              de la région à recevoir un appui dans l'analyse et l’évaluation de la résilience. Il intègre une logique 
              de création d’une cohérence avec les autres processus d’analyse de la sécurité alimentaire tel que le Cadre 
              Harmonisé d’identification et d’analyse des zones à risque et des populations vulnérables au Sahel et en Afrique 
              de l’Ouest." 
            ),
            tags$p(
              "Cet outils, adapté au contexte de la région Sahel et Afrique de l’Ouest, a été conçu pour servir 
              d’outil permettant d’informer les gouvernements sur la situation de la résilience des populations 
              et sur les facteurs qui contribuent à la résilience des ménages dans l’espace CILSS-CEDEAO." 
            ),
            tags$p(
              "L’indice de résilience AMR est issu d’un long processus inclusif entre le CILSS et les pays identifiés pour 
              participer aux analyses tests. Il est obtenu d’un modèle des équations structurelles à variables latentes." 
            )
          ),
          br(),
          br(),
          
          tags$div(
            tags$p(
              h2("Aperçu général sur l’indice de résilience avec l’approche AMR"),
              hr(),
              "La résilience face à l’insécurité alimentaire désigne la capacité d’une population, d’un ménage ou d’un individu à anticiper, 
              absorber, s’adapter et se rétablir face aux perturbations liées à l’alimentation, tout en préservant ou en rétablissant de manière 
              durable leurs moyens de subsistance, qu’ils soient agricoles ou non agricoles (FAO). Cette définition souligne le caractère inobservable 
              et multidimensionnel de la résilience. Plusieurs approches sont utilisées pour estimer la résilience face à l’insécurité alimentaire. 
              L’approche de l’Analyse et Mesure de la Résilience (AMR) du CILSS consiste à simuler un modèle économétrique pour obtenir un indice 
              de résilience. Dans cette section, nous présenterons la justification du choix du modèle économétrique, sa spécification et les données 
              nécessaires à sa mise en œuvre.   " 
            ),
            br(),br(),
            tags$p(
              h2("Choix du modèle "),
              hr(),
              "Le modèle économétrique choisi pour calculer l’indice de capacité de résilience avec l’approche AMR appartient à la famille 
              des modèles d’équations structurelles à variables latentes. Ces modèles sont reconnus dans la recherche scientifique pour leur 
              capacité à analyser les relations causales entre de nombreuses variables, qu’elles soient observables ou non. Ils permettent 
              une modélisation complexe et une analyse approfondie des relations, en estimant simultanément toutes les relations du modèle. 
              Cette approche garantit des estimations cohérentes et des tests de significativité robustes." ,
              "Il existe trois grandes catégories de modèles d’équations structurelles à variables latentes :"
            ),
            
            tags$ul(
              tags$li(
                "Modèles à relations réflexives : la variable latente influence les variables manifestes, qui sont les conséquences indirectes 
                de cette variable latente."
              ),
              tags$li(
                "Modèles à relations formatives : pour ce type de modèle, la variable latente est construite à partir des variables manifestes, 
                découlant ainsi de ces dernières."
              ),
              tags$li(
                "Modèles MIMIC (Multiple Input Multiple Indicators Causes) : Ces modèles combinent des relations formatives et réflexives. 
                C’est cette catégorie de modèle qui est retenu pour estimer l’indice de résilience selon l’approche AMR."
              ),
            ),
            
            tags$p(
              "En effet, selon l’approche AMR, la résilience repose sur trois capacités qui sont résultantes de plusieurs autres variables 
              observables et l’indice inobservable de résilience est obtenu à partir de ces variables manifestes. Il s’agit de la capacité 
              d’absorption, de la capacité d’adaptation et de la capacité de transformation."
            ),
            
            br(),br(),
            
            tags$p(
              h2("Spécification du modele AMR"),
              hr(),
              "Le modèle AMR est de type MIMIC contraint. Ce modèle garantit non seulement la convergence, mais également la cohérence externe 
              avec les indicateurs de sécurité alimentaire sélectionnés comme variables de contrôle. Dans le contexte de l’analyse de la sécurité
              alimentaire, les indicateurs les plus couramment utilisés comme variables de contrôle sont le score de consommation alimentaire 
              et l’indice des stratégies d’adaptation basées sur les moyens d’existence.",
            ),
            tags$p(
              "Le schéma conceptuel du modèle AMR, qui repose sur les trois capacités (absorption, adaptation et transformation), et contraint 
              par le score de consommation alimentaire et l’indice des stratégies d’adaptation basées sur les moyens d’existence (ISAME) 
              se présente comme suit :",
            ),
            
            tags$p(
              tags$img(height = 250, width = 500,src = "schema_conceptuel_amr.png")
            ),
            br(),
            tags$p(
              "Mathématiquement, l’équation du modèle AMR est spécifié comme suit : ",
            ),
            tags$p(
              tags$img(height = 100, width = 350,src = "equation_amr.png")
            ),
            
            br(),
            tags$p(
              tags$b("Avec : ")
            ),
            tags$p(
              tags$b("R", tags$sub("i")), " : indice de capacité de résilience"
            ),
            tags$p(
              tags$b("CAB", tags$sub("i")), " : capacité d'absorption"
            ),
            tags$p(
              tags$b("CAD", tags$sub("i")), " : capacité d'adaptation"
            ),
            tags$p(
              tags$b("CTR", tags$sub("i")), " : capacité de transformation"
            ),
            tags$p(
              tags$b("FS", tags$sub("i")), " : vecteur d'indicateurs de sécurité alimentaire de contrôle et de l'indice des stratégies 
              d'adaptation basées sur les moyens d'existence (ISAME)"
            ),
            tags$p(
              tags$b("X", tags$sub("i")), " : le vecteur des autres variables exogènes (non expliquées par les données), ces varaibles 
              peuvent être des facteurs géo-climatiques et environnementales, non observables à travers les données d'enquêtes"
            ),
            tags$p(
              tags$b("ε"), ": le terme d'erreur associé à l'estimatin des paramètres"
            ),
            tags$p(
              tags$b("α, β , γ , µ , ε "), ": les vecteurs de régression (coefficients) entre les variables exogènes auxquelles ils sont 
              associés et le vecteur des indicateurs de sécurité alimentaire retenus (SCA et ISAME)"
            ),
            
            br(),br(),
            
            tags$p(
              h2("Données nécessaires et leurs sources "),
              hr(),
              "Le modèle d’analyse et de mesure de la résilience est implémenté sur les données issues des enquêtes sur la sécurité alimentaire. Il peut également être mis en œuvre à partir des données des enquêtes sur les conditions de vie des ménages et l’agriculture. Toutes ces enquêtes sont conduites à priori et cela dans plusieurs pays par les Instituts Nationaux des Statistiques en collaboration avec les structures nationales en charge du suivi de la sécurité alimentaire et de la résilience notamment les services de coordination du système d’alerte précoce. ",
            ),
            
            br(),br(),
            
            tags$p(
              h2("Construction des capacités"),
              hr(),
              "Le modèle AMR repose sur trois capacités à savoir : la capacité d’absorption, la capacité d’adaptation et la capacité de transformation. La capacité d’absorption est la capacité d’un ménage à atténuer les effets des chocs sur ses moyens de subsistance et ses besoins fondamentaux. S’agissant de la capacité d’adaptation, c’est l’aptitude d’un ménage à s’adapter aux impacts des chocs et stress, d’atténuer les dommages potentiels et de tirer parti des opportunités pouvant émerger du changement. Quant à la capacité de transformation, elle désigne l’aptitude d’un ménage de parvenir à un nouvel état grâce à une combinaison d’innovations technologiques, de réformes institutionnelles, de changements culturels et de comportement, entre autres.",
            ),
            tags$p(
              "Toutes ces différentes capacités sont également des variables latentes, obtenues à partir des variables observables identifiées. La méthode de calcul de ces capacités est une analyse factorielle exploratoire qui permet d’explorer la structure sous-jacente des données. Son objectif est de construire des facteurs latents à partir de variables mesurées, sans hypothèses préalables. C’est une méthode qui est mieux adaptée quand on a un mélange de variables continues, catégorielles ordinales ou binaires.",
            ),
            tags$p(
              "Pour évaluer la qualité d’ajustement du modèle après une analyse factorielle exploratoire, on utilise généralement l’indice KMO (Kaiser-Meyer-Olkin). Il mesure la corrélation moyenne entre les variables observées et fournit une indication de la faisabilité de la factorisation. Une valeur KMO proche de 1 indique une bonne adéquation des données au modèle factoriel. En revanche, si l’indice KMO est faible (inférieur à 0,6), cela suggère que les variables sont insuffisamment corrélées pour justifier une analyse factorielle.",
            ),
            tags$p(
              "Les variables sélectionnées pour construire chaque capacité ont été choisies en fonction de leur corrélation avec la capacité en question et de leur pertinence pour la formulation des recommandations pour action dans le domaine de la sécurité alimentaire et nutritionnelle.",
            ),
            
            tags$p(
              tags$b("Pour la construction de la capacité d'absorption, les variables suivants sont retenues : "),
              "Unités de bétail tropical, Indice d’équipement agricole, Indice de richesse, Stocks alimentaires, Terres cultivées (superficies),
              Proportion des dépenses alimentaires par rapport aux dépenses totales (en valeur inverse), Ratio de dépendance, Revenus agricoles" 
            ),
            tags$p(
              tags$b("Pour la construction de la capacité d'adaptation, les variables suivants sont retenues : "),
              "L’indice de diversification des cultures, Indice de diversification des activités génératrices de revenus, Score d'assistances formelles reçues,
              Score d'assistances informelles reçues, Nombre d’années d’éducation du chef de ménage ou des membres âgés de 15 à 60 ans, Nombre de repas pris par les enfants" 
            ),
            tags$p(
              tags$b("Pour la construction de la capacité de transforamtion, les variables suivants sont retenues : "),
              "Utilisation de toilettes améliorées, Adoption de bonnes pratiques d’hygiène et d’assainissement, Accès à l’électricité ou au solaire, 
              Accès à l’eau potable, Indice d’accès aux infrastructures communautaires (en valeurs inverses des variables de distances), Adoption de techniques améliorées de production,
              Adoption de techniques améliorées de transformation et de conservation" 
            ),
            
          ),
        ), # 
        
        # Debut onglet guide
        tabPanel(
          "Guide",
          br(),br(),
          h2("Comment utiliser l'outil AMR ?"),
          hr(),
          tags$ul(
            tags$li("Assurez-vous que votre ensemble de données est nettoyé (pas de NA ni de valeurs aberrantes) et organisé avec chaque variable dans une colonne nommée distincte."),
            br(),br(),
            tags$li("Les variables permettant le calcul de la capacité d’absorption doivent être préfixées par", tags$b("«abs_ »"),"."),
            br(),br(),
            tags$li("Les variables permettant le calcul de la capacité d’adaptation doivent être préfixées par", tags$b("«adap_ »"), "."),
            br(),br(),
            tags$li("Les variables permettant le calcul de la capacité de transformation doivent être préfixées par ", tags$b("«trans_ »"), "."),
            br(),br(),
            tags$li("Les variables de la sécurité alimentaire doivent être préfixées par ", tags$b("«fs_ »"), "."),
            br(),br(),
            tags$li("Les axes d’analyses (Régions, Milieux de résidence, Zones de moyen d’existence, …) doivent être précédés par ", tags$b("«aa_ »"), "."),
            br(),br(),
            tags$li("Téléchargez les données en cliquant sur ", tags$b("Charger"), " pour sélectionner le fichier de données. 
                    Si les données se charge correctement, un tableau d'aperçu apparaît sous l'onglet ", tags$b("Base de données"), "."),
            br(),br(),
            tags$li("Sélectionner les variables : Choisissez au moins trois variables pour chacun des trois capacités : 
                    absorption, adaptation et transformation, Choisissez également les variables de sécurité alimentaire : 
                    FCS (Score de consommation alimentaire) et HDDS (Score de diversité alimentaire) et Cliquez sur le 
                    bouton ", tags$b("Valider"), "."),
            br(),br(),
            tags$li("Naviguez dans les différents onglets pour voir les résultats.")
          )
        ), # Debut onglet guide
        
        # Onglet pour l'affichage de la base de données
        tabPanel(
          "Base de données",
          br(),br(),
          conditionalPanel(
            condition = "output.fileUploaded", # test pour voir si le fichier est chargé pour afficher le bouton
            fluidRow(
              column(width = 12, 
                     DT::dataTableOutput("bd_var_interm"),
              )
            ), # fluidRow, Base de données
            br(), 
            fluidRow(
              column(width = 12, 
                     downloadButton('save_bd_var_interm_csv', 'Save to csv'),
                     downloadButton('save_bd_var_interm_xlsx', 'Save to xlsx'),
                     downloadButton('save_bd_var_interm_spss', 'Save to sav (SPSS)'),
                     downloadButton('save_bd_var_interm_dta', 'Save to dta (Stata)'),
                     downloadButton('save_bd_var_interm_sas', 'Save to sas7bdat (SAS)'),
                     radioButtons(inputId = 'encodage', label = 'Encodage', choices = c(UTF8='UTF-8', Latin1='Latin-1')),
              ),
            ), # fluidRow, Base de données
            
          ),
          br(), br(),
          
        ), # Fin tabPanel, Onglet pour l'affichage de la base de données
        
        
        # Onglet pour l'affichage du résultat des calculs
        tabPanel(
          "Capacités/RI",
          conditionalPanel(
            condition = "input.CalculerCapacites >= 1", 
            br(),
            h2("Résultat du calcul des capacités et de l'indice de résilience "),
            hr(),
            fluidRow(
              column(width = 12, 
                     DT::dataTableOutput("capacites_ri"),
              )
            ), # fluidRow, capacite indices
            br(),
            fluidRow(
              column(width = 12, 
                     downloadButton('save_capacites_ri_csv', 'Save to csv'),
                     downloadButton('save_capacites_ri_xlsx', 'Save to xlsx'),
                     downloadButton('save_capacites_ri_spss', 'Save to sav (SPSS)'),
                     downloadButton('save_capacites_ri_dta', 'Save to dta (Stata)'),
                     
              ),
            ), # fluidRow, capacite indices
            
            
            ########################################################################################"
            br(),
            h2("Analyse descriptive des variables choisies "),
            hr(),
            fluidRow(
              column(width = 12, 
                     DT::dataTableOutput("description_var"),
              )
            ), # fluidRow, l'affichage des corrélations entre les variables
            br(),
            fluidRow(
              column(width = 12, 
                       downloadButton('save_description_var_csv', 'Save to csv'),
                       downloadButton('save_description_var_xlsx', 'Save to xlsx'),
              ),
            ), # fluidRow, capacité indices
            br(),
            h2("Corrélation entre les variables choisies "),
            hr(),
            
            # Affichage des graphiques de corrélation
            fluidRow(
              column(width = 6, 
                     #conditionalPanel(
                     #  condition = "output.fileUploaded",
                       plotOutput("corr_plot_abs")
                     #)
              ),
              column(width = 6, 
                     #conditionalPanel(
                     # condition = "output.fileUploaded",
                       plotOutput("corr_plot_adap")
                     #)
              ),
            ), # fluidRow, Affichage des graphiques de corrélation
            br(),
            fluidRow(
              column(width = 6, 
                     #conditionalPanel(
                     #  condition = "output.fileUploaded",
                       plotOutput("corr_plot_trans")
                     #)
              ),
            ),# fluidRow, Affichage des graphiques de corrélation
            
            #######################################################################################"
            
            
          ), # fin conditionalPanel
        ), # # Fin tabPanel, Onglet pour l'affichage du résultat des calculs
        
        # Onglet Validité du modèle
        tabPanel(
          "Contrôle Validité",
          br(),
          conditionalPanel(
            condition = "input.CalculerCapacites >= 1", # test pour voir si le fichier est chargé pour afficher le bouton
            # Affichage des indicateurs de validité du modèle
            fluidRow(
              column(width = 12, 
                     h2("Indicateurs de validité du modèle")
              )
            ),
            br(),
            fluidRow(
              column(width = 12, 
                     DT::dataTableOutput("matrice_comparaison_capacites"),
                     downloadButton('save_matrice_comparaison_capacites_csv', 'Save to CSV'),
                     downloadButton('save_matrice_comparaison_capacites_xlsx', 'Save to xlsx'),
              )
            ),
            br(), br(), 
            tags$div(
              tags$p(
                "Pour évaluer la qualité d’ajustement du modèle après une analyse factorielle exploratoire, on utilise généralement 
                l’indice KMO (Kaiser-Meyer-Olkin). Il mesure la corrélation moyenne entre les variables observées et fournit une indication 
                de la faisabilité de la factorisation. Une variable KMO :" 
              ),
              tags$ul(
                tags$li("inférieure à 0.4 indique que les variables sont insuffisamment corrélées pour justifier 
                        une analyse factorielle ;"),
                tags$li("supérieure ou égale à 0.4 et inférieure à 0.6 est tolérable ;"),
                tags$li("supérieure ou égale à 0.6 indique une bonne adéquation des données au modèle factoriel."),
              ),
            ),
            
            hr(),
            fluidRow(
              column(width = 12, 
                     h2("Vérification de l'hypothèse de la normalité des résidus")
              )
            ),
            hr(), 
            fluidRow(height="auto", align="center",
                     column(width = 6, 
                            plotOutput("distribution_ri_graph")
                     ),
                     column(width = 6, 
                            plotOutput("diagramme_qqplot_graph")
                     )
            ),
            
            br(), br(), 
            tags$div(
              tags$p(
                "Dans le premier graphique, la distribution de RI est représentée en bleue et la distribution de la loi normale en rouge.
                Dans le deuxième graphique, la distribution de la loi normale est représentée par la ligne droite." 
              ),
              tags$p(
                "Si dans les 2 graphiques, les distributions de RI sont proches de la loi normale, cela permet de valider l'hypothèse 
                d'indépendance des résidus de l'estimation du modèle." 
              ),
            ),
            
          ),
          br(), br(),
        ), # Onglet Validité du modèle
        
        # Onglet pour l'analyse de l'indice
        tabPanel(
          "Analyse de l'indice",
          br(),
          
          conditionalPanel(
            condition = "input.CalculerCapacites >= 1", # test pour voir si le fichier est chargé pour afficher le bouton
            fluidRow(
              column(width = 12, 
                     h2("Indice de résilience moyen")
              )
            ),
            
            fluidRow(
              column(width = 12, 
                     h1(textOutput("irm"))
              )
            ),
            hr(),
            
            fluidRow(
              column(width = 12, 
                     h2("Matrice de structure de la résilience")
              )
            ),
            fluidRow(
              column(width = 12, 
                     plotOutput("structure_resilience")
              )
            ),
            
            br(),br(),
            
            fluidRow(
              column(width = 12, 
                     h2("Corrélation entre les indices de capacité et les variables qui les composent")
              )
            ),
            hr(),
            fluidRow(
              column(width = 12, 
                     h3("Capacité d'absorption")
              )
            ),
            hr(),
            fluidRow(column(width = 6, 
                            DT::dataTableOutput("contrib_var_absorption"),
                            downloadButton('save_contrib_var_absorption_csv', 'Save to CSV'),
                            downloadButton('save_contrib_var_absorption_xlsx', 'Save to xlsx'),
                    ),
                    column(width = 6,
                           plotOutput("contrib_var_absorption_graph", width = "100%")
                    )
            ),
            br(),
            fluidRow(
              column(width = 12, 
                     h3("Capacité d'adaptation")
              )
            ),
            hr(),
            fluidRow(column(width = 6, 
                            DT::dataTableOutput("contrib_var_adaptation"),
                            downloadButton('save_contrib_var_adaptation_csv', 'Save to CSV'),
                            downloadButton('save_contrib_var_adaptation_xlsx', 'Save to xlsx'),
                    ),
                    column(width = 6,
                           plotOutput("contrib_var_adaptation_graph")
                    )
            ),
            fluidRow(
              column(width = 12, 
                     h3("Capacité de transformation")
              )
            ),
            hr(),
            fluidRow(column(width = 6, 
                            DT::dataTableOutput("contrib_var_transformation"),
                            downloadButton('save_contrib_var_transformation_csv', 'Save to CSV'),
                            downloadButton('save_contrib_var_transformation_xlsx', 'Save to xlsx'),
            ),
            column(width = 6,
                   plotOutput("contrib_var_transformation_graph")
            )
            ),
            br(),
            br(),
          
          )
        ), # FIN Onglet pour l'analyse de l'indice
        
        # Onglet pour l'analyse de l'indice par zone
        tabPanel(
          "Analyse par axe",
          br(),
          conditionalPanel(
            condition = "input.CalculerCapacites >= 1", # test pour voir si le fichier est chargé pour afficher le bouton
            
            fluidRow(
              column(width = 12, 
                     selectInput("axe_analyse", 
                                 "Choississez l'axe d'analyse", 
                                 choices = NULL,
                                 multiple = FALSE),
                     actionButton("lancerAnalyse", "Exécuter")
              ),
            ),
            hr(),
            fluidRow(
              column(width = 12, 
                     h2("Statistiques descriptives de l'indice de résilience")
              )
            ),
            fluidRow(
              column(width = 12, 
                     DT::dataTableOutput("stats_zone"),
                     downloadButton('save_stats_zone_csv', 'Save to CSV'),
                     downloadButton('save_stats_zone_xlsx', 'Save to XLSX')
              )         
            ),
            br(),
            fluidRow(height="auto", align="center",
                     column(width = 12, 
                            plotOutput("barplot_indiceMoy_region")
                     )
            ),
            br(), br(),
            fluidRow(
              column(width = 12, 
                     h2("Contribution des capacités à l'indice de résilience")
              )
            ),
            fluidRow(column(width = 12, 
                            DT::dataTableOutput("contribution_capacites_ri"),
                            downloadButton('save_contribution_capacites_ri_csv', 'Save to CSV'),
                            downloadButton('save_contribution_capacites_ri_xlsx', 'Save to XLSX')
                    )         
            ),
            br(), 
            fluidRow(height="auto", align="center",
                     column(width = 12, 
                            plotOutput("barplot_contribution_capacites_ri_cumule")
                     )
            ),
            br(), 
            
            br(), br(),
            fluidRow(
              column(width = 12, 
                     h2("Classes de résilience ")
              )
            ),
            fluidRow(column(width = 12, 
                            DT::dataTableOutput("classe_resilience"),
                            downloadButton('save_classe_resilience_csv', 'Save to CSV'),
                            downloadButton('save_classe_resilience_xlsx', 'Save to XLSX')
            )         
            ),
            br(), br(),
          ),
        ), # FIN Onglet pour l'analyse de l'indice par zone
        
        
    
      ), # Fin tabsetPanel, Bloc des onglets
    ), # Fin mainPanel, Fenêtre principale
    
    
    
    
  ), # Fin sidebarLayout
  
)