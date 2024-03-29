#ui

#install.packages("shinydashboard")
#install.packages("DT")

library(shinydashboard)
library(shiny)
library(DT)

ingredients <- c("Pâtes", "Saumon", "Oeufs", "Crème", "Parmesan", "Bouillon poulet", 
                 "Sauce soja", "Algue nori", "Oignon vert", "Poulet", "Carotte", 
                 "Oignon", "Sauce yakisoba", "Sauce pesto", "Tomate cerise", 
                 "Poivre noir", "Crevette", "Germe de soja", "Sauce Pad ThaÏ", 
                 "Huile d'olive", "Basilic", "Ail", "Piment rouge", "Persil", 
                 "Jus de citron", "Romarin", "Epices", "Sauce teriyaki", 
                 "Graines de sésame", "Marinade de gingembre", "Laitue", 
                 "Croutons", "Vinaigrette césar", "Poivron", "Cumin", 
                 "Marinade au yaourt", "Sauce Alfredo", "Champignons", 
                 "Sauce barbecue", "Paprika", "Miel", "Yaourt nature", 
                 "Tomates", "Mozzarella", "Pain", "Aubergine", "Courgette", 
                 "Concombre", "Viande hachée", "Riz", "Herbes", "Feta", 
                 "Olive noire", "Sel", "Carotte", "Pomme de terre", 
                 "Bouillon de légumes", "Poisson", "Farine", "Sucre", 
                 "Celeri", "Noix", "Poireau", "Lardon", "Vin blanc", 
                 "Jambon", "Levure", "Fromage", "Creme fraiche", 
                 "Pate feuilletée", "Jambon", "Champignon", "Epinard", 
                 "Vanille", "Pois chiche", "Lait de coco", "Tahini", 
                 "Coriandre")


recettes <- read.csv("recettes.csv", sep=";")


allergenes <- c("Saumon", "Crevette", "Oeuf", "Lait", "Fromage", "Crème", "Bouillon poulet", 
                "Graines de sésame", "Laitue", "Jambon", "Lardon", "Pâtes", "Pain", "Farine", 
                "Levure")
regimes <- c("Tous", "Végétarien", "Végétalien", "Sans gluten", "Cétogène")
types_plat <- c("Tous", "Entrée", "Plat principal", "Dessert")
durees <- c("Tous", "Express (<20 min)", "Rapide (<30 min)", "Normale (<1 heure)", "Longue (<2 heures)")
origine <- c("Tous", "International", "Italie", "Japon", "Thaïlande", "Origine méditerranéenne", 
             "Amérique", "Moyen-Orient", 
             "France", "Espagne",  "Grèce", "USA", "Inde", "Asie", 
             "Maghreb", "Liban", "Méditerrannée", "Moyen-Orien")

ui <- dashboardPage(
  dashboardHeader(title = "NutriPlaisirs"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      # Onglets dans le menu
      menuItem("Profil Nutritionnel", tabName = "user", icon = icon("user")), 
      menuItem("Choix de Recettes", tabName = "filter_recipes", icon = icon("cutlery")),
      menuItem("Recommandations IMC", tabName = "aides", icon = icon("search")),
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  #style de l'interface
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* Styles du logo dans len-tête */
                                .skin-blue .main-header .logo {
                                  background-color: #354B61; /* Couleur de fond du logo */
                                    color: #ECF0F5; /* Couleur du texte des autres liens dans le menu latéral */
                                }
                              
                              /* Styles du logo lorsquil est survolé */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #2E4154; /* Couleur de fond du logo lorsquil est survolé */
                                                              }
                              
                              /* Styles de la barre de navigation */
                                .skin-blue .main-header .navbar {
                                  background-color: #496785; /* Couleur de fond de la barre de navigation */
                                }
                              
                              /* Styles de la barre latérale principale */
                                .skin-blue .main-sidebar {
                                  background-color: #354B61; /* Couleur de fond de la barre latérale principale */
                                }
                              
                              /* Styles de longlet sélectionné dans le menu latéral */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                                  background-color: #1D2A36; /* Couleur de fond de longlet sélectionné dans le menu latéral */
                                }
                              
                              /* Styles des autres liens dans le menu latéral */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                                  background-color: #263545; /* Couleur de fond des autres liens dans le menu latéral */
                                    color: #FFFFFF; /* Couleur du texte des autres liens dans le menu latéral */
                                }
                              
                              /* Styles des autres liens dans le menu latéral lorsquils sont survolés */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                                  background-color: #1D2A36; /* Couleur de fond des autres liens dans le menu latéral lorsquils sont survolés */
                                }
                              
                              /* Styles du bouton de basculement lorsquil est survolé */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                  background-color: #3E5770; /* Couleur de fond du bouton de basculement lorsquil est survolé */
                                }
                              
                              /* Styles de la zone de contenu */
                                .content-wrapper, .right-side {
                                  background-color: #ECF0F5; /* Couleur de fond de la zone de contenu */
                                }
                              
                              /* Styles des éléments de menu dans la barre latérale */
                                .treeview-menu > li > a {
                                  width: 240px; /* Définir une largeur fixe pour les éléments de menu */
                                }
                              
                              /* Styles de la boîte personnalisée */
                                .custom-box {
                                  background-color: #E1E9F5; /* Couleur de fond de la boîte personnalisée */
                                }
                                
                                /* Styles de la boîte personnalisée */
                                .custom-box1 {
                                  background-color: #CED5E0; /* Couleur de fond de la boîte petit déjeuner */
                                }
                                
                                /* Styles de la boîte personnalisée */
                                .custom-box2 {
                                  background-color: #CED5E0; /* Couleur de fond de la boîte déjeuner */
                                }
                                
                                /* Styles de la boîte personnalisée */
                                .custom-box3 {
                                  background-color: #CED5E0; /* Couleur de fond de la boîte diner */
                                }
                                
                                /* Styles de la boîte personnalisée */
                                .custom-box4 {
                                  background-color: #CED5E0; /* Couleur de fond des boîtes résultats IMC et kcal */
                                }
                              
                              /* Styles du popup */
                                .modal-content {
                                  background-color: #E1E9F5; /* Couleur de fond du popup */
                                }
                              
                              /* Styles de len-tête du popup */
                                .modal-header {
                                  background-color: #3C8DBC; /* Couleur de fond de len-tête du popup */
                                color: #fff; /* Couleur du texte de len-tête du popup */
                                }
                                
                                /* Styles du bouton Calculer */
                                #calculate_button {
                                  background-color: #428BCA; /* Couleur de fond du bouton Calculer */
                                  border-color: #428BCA; /* Couleur de bordure du bouton Calculer */
                                  color: #fff; /* Couleur du texte */
                                }
                                
                                /* Ajoutez la classe personnalisée pour centrer le texte dans le wellPanel */
                                .center-text {
                                  text-align: center;
                                }
                                
                                .center-button {
                                  margin: 0 auto;
                                  display: block;
                                  background-color: #BEC4CF; /* Couleur de fond du bouton */
                                }
                            
                                .center-button:hover {
                                  background-color: #B7BDC7; /* Couleur de fond au survol */
                                }

                              '))),
    
    # Définir la largeur du corps du tableau de bord
    width = "100%",
    tabItems(
      # 2ème onglet Choix de Recettes
      tabItem(
        tabName = "filter_recipes",
        h2("Choix de Recettes"),
        fluidRow(
          box(
            width = 12, # Utiliser la largeur complète de la rangée
            title = "Filtrer les Recettes",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(12, selectizeInput("ingredient", "Ingrédients", choices = ingredients, multiple = TRUE, options = list(
                placeholder = "Sélectionnez les ingrédients...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("allergene", "Allergènes", choices = allergenes, multiple = TRUE, options = list(
                placeholder = "Sélectionnez les allergènes...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("regime", "Diète", choices = setNames(regimes, regimes), options = list(
                placeholder = "Sélectionnez la diète...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("type_recette", "Type de plat", choices = types_plat, options = list(
                placeholder = "Sélectionnez le type de plat...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("duree", "Durée de préparation", choices = setNames(durees, durees), options = list(
                placeholder = "Sélectionnez la durée de préparation...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("origine", "Origine", choices = origine, options = list(
                placeholder = "Sélectionnez le type d'origine...",
                plugins = list('remove_button')
              ))),
              column(2, numericInput("max_kcal", "Max des Kcal:", value = 700)),
            ),
            class = "custom-box" 
          )
        ),
        fluidRow(
          box(
            width = 12, # Utiliser la largeur complète de la rangée
            title = "Recettes",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("recettes_table"),
            class = "custom-box" # Appliquer couleur du fond de box
          )
        )
      ),
      
      # 4ème onglet About l'appli
      tabItem(
        tabName = "about",
        h2("À propos de NutriPlaisirs"),
        h3("NutriPlaisirs : Des Recettes à Votre Goût"),
        p("Cette application a été développée pour vous aider à trouver des recettes délicieuses en fonction de vos préférences et de vos objectifs."),
        p("Elle vous permet de filtrer les recettes en fonction de divers critères tels que les ingrédients, les allergènes, le type de plat, la durée de préparation et l'origine. Elle vous permet aussi, par la même occasion, de calculer votre besoin énergétique journalier."),
        p("Profitez de NutriPlaisirs pour découvrir de nouvelles idées culinaires et atteindre vos objectifs !"),
        h3("Auteurs"),
        p("L'application a été conçue dans le cadre du module OPEN par des étudiants de l'Isara."),
        p("Les auteurs de cette application sont :"),
        p("- Cédric Octave ADECHINA"),
        p("- Zineb BELMADAN"),
        p("- Maxime DE BONI"),
        p("- Diamande DELANNAY"),
        p("- Antônio Gabriel ELEUTERIO VIANA"),
        p("- Thomas FERNANDEZ-SANTONNAT"),
        h3("Informations des calculs nutritionnels"),
        p("Les valeurs de kcal/j sont basées sur des recommandations générales et peuvent nécessiter des ajustements en fonction de facteurs individuels tels que la composition corporelle et l'état de santé. Ils fournissent donc une estimation générale des besoins caloriques quotidiens et de la répartition des calories entre les repas. Il est toujours recommandé de consulter un professionnel pour obtenir des conseils nutritionnels personnalisés."),
        p("Voici comment les différents calculs sont effectués :"),
        h4("1. Calcul de l'IMC (Indice de Masse Corporelle) :"),
        p("L'IMC est calculé en divisant le poids (en kg) par le carré de la taille (en m). Cet indice est utilisé pour évaluer le statut pondéral d'une personne."),
        h4("2. Calcul des besoins caloriques quotidiens :"), 
        p("Les besoins caloriques quotidiens sont calculés à partir de la formule du BMR (Basal Metabolic Rate), qui est une estimation des calories que votre corps brûle au repos. Cette estimation tient compte de facteurs tels que votre poids, votre taille, votre âge et votre sexe."), 
        p("Ensuite, en fonction de votre niveau d'activité physique, cette estimation est ajustée. Les personnes plus actives ont besoin de plus de calories pour soutenir leurs activités quotidiennes et leurs exercices physiques."),
        p("Puis les objectifs sont pris en compte à partir de la moyenne de ces recommandations :"),
        p("    - Si l'objectif est de garder le même poids, il faut un apport calorique quotidien égal."),
        p("    - Si l'objectif est de prendre du poids, il faut augmenter légèrement l'apport calorique quotidien (de 200 à 500 kcal)."),
        p("    - Si l'objectif est de prendre de la masse musculaire, il faut augmenter légèrement l'apport calorique quotidien (de 300 kcal)."),
        p("    - Si l'objectif est de perdre du poids, il faut diminuer progressivement l'apport calorique quotidien (de 300 à 700 kcal)."),
        h4("3. Répartition des calories entre les repas :"), 
        p("Les calories sont réparties entre les repas (petit-déjeuner, déjeuner, dîner) en fonction de proportions standard. En moyenne, le petit-déjeuner représente environ 20-25% des calories totales, le déjeuner et le dîner environ 30-35% chacun.")
      ),
      
      # 1er onglet Informations utilisateur
      tabItem(
        tabName = "user",
        h2("Profil Nutritionnel et Besoins Caloriques"),
        h3("Calculateur de besoins caloriques journaliers"),
        sidebarLayout(
          sidebarPanel(
            width = 6,
            numericInput("weight", "Entrez votre poids (en kg) :", value = 70),
            sliderInput("height", "Choisissez votre taille (en m) :", min = 0.5, max = 2.5, value = 1.7, step = 0.01),
            numericInput("age", "Entrez votre âge :", value = 30),
            radioButtons("gender", "Genre :",
                         c("Homme", "Femme")),
            radioButtons("activity_level", "Niveau d'activité physique :",
                         c("Sédentaire" = 1, "Modéré" = 2, "Actif" = 3)),
            selectInput("goal", "Objectif :",
                        c("Maintenir le poids", "Prendre du poids", "Prendre de la masse musculaire", "Perdre du poids")),
            actionButton("calculate_button", "Calculer"),
            class = "custom-box" # Appliquer couleur du fond de box
          ),
          mainPanel(
            width = 5,
            h3("Comment utiliser le calculateur de besoins caloriques journaliers ? "),
            wellPanel(
              p("Entrez vos informations personnelles dans le panneau de gauche :"),
              p("- Renseignez votre âge, votre sexe, votre poids et votre taille"), 
              p("- Sélectionnez votre niveau d'activité physique et votre objectif parmi les options proposées"),
              p("- Cliquez sur le bouton : Calculer pour obtenir une estimation des calories à consommer"),
              class = "custom-box" # Appliquer couleur du fond de box
            ),
            conditionalPanel(
              condition = "input.calculate_button > 0",
              h3("Résultats IMC"),
              wellPanel(
                textOutput("bmi_result"),
                class = "custom-box4" 
              ),
              actionButton("bouton2", "Cliquez pour découvrir des informations liées à votre IMC", class = "center-button"),
              h3("Besoins caloriques quotidiens"),
              wellPanel(
                textOutput("calories_result"),
                class = "custom-box4" 
              ),
              h3("Proposition de calories par repas"),
              fluidRow(
                column(4, 
                       wellPanel(
                         h4("Petit-déjeuner"),
                         textOutput("breakfast_calories"),
                         class = "custom-box1 center-text" 
                       )
                ),
                column(4, 
                       wellPanel(
                         h4("Déjeuner"),
                         textOutput("lunch_calories"),
                         class = "custom-box2 center-text" 
                       )
                ),
                column(4, 
                       wellPanel(
                         h4("Dîner"),
                         textOutput("dinner_calories"),
                         class = "custom-box3 center-text"
                       )
                )
              ),
              actionButton("bouton", "Cliquez pour découvrir nos recettes", class = "center-button"),
            )
          )
        )
      ),
      
      # 3ème onglet Recommandations
      tabItem(
        tabName = "aides",
        h2("Recommandations basées sur l'IMC"),
        conditionalPanel(
          condition = "input.calculate_button == 0",
          mainPanel(
            width = 7,
            wellPanel(
              class = "custom-box",
              p("Pour faire apparaître les conseils sur votre IMC, il est impératif de calculer votre IMC en rentrant vos informations dans l'onglet «Profil Nutritionnel».")))),
        conditionalPanel(
          condition = "input.calculate_button > 0",
          mainPanel(
            width = 11,
            wellPanel(
              class = "custom-box",
              p("Vous avez la liberté de choisir vos recettes et votre apport calorique quotidien en fonction de vos préférences et de vos objectifs. Cependant, en ce qui concerne votre santé liée à l'Indice de Masse Corporelle (IMC), nous aimerions vous fournir quelques conseils recommandés."),
              p("Dans le graphique ci-dessous, plusieurs plages de l'IMC sont représentées, avec votre IMC marqué spécifiquement. Cela vous permet de visualiser facilement où vous vous situez par rapport aux différentes catégories de poids.")),
            wellPanel(
              class = "custom-box",
              title = "Graphique IMC",
              plotOutput("imc_plot")),
            wellPanel(
              class = "custom-box",
              textOutput("imc_recommandations"),
              uiOutput("physique_image"))
          )
        )
      )
    )
  )
)
