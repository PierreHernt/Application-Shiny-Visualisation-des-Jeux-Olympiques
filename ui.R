# Définition de l'interface utilisateur
ui <- fluidPage(
  # Ajout d'un thème visuel à l'application Shiny
  theme = shinytheme("sandstone"),
  
  # Barre de navigation principale
  navbarPage(
    id = 'tabset', # ID pour identifier la barre de navigation
    title = div(img(src = "JO.png", height = "40px"), style = "padding-left:50px;"), # Logo des Jeux Olympiques
    header = tagList(
      useShinydashboard() # Inclusion du package Shinydashboard pour améliorer l'apparence
    ),
    
    # --- Onglet Accueil ---
    tabPanel(
      title = "Accueil", # Nom de l'onglet
      br(), br(), br(), # Espacement vertical
      
      # Titre principal centré
      h1(
        'Tout savoir sur les jeux Olympiques',
        style = "color: black; text-align: center; font-size: 50px; font-weight: bold;"
      ),
      br(),
      
      # Sous-titre principal centré
      h2(
        "Cette application permet de retracer l'histoire des jeux olympiques de 1896 à aujourd'hui...",
        style = "color: black; text-align: center; font-size: 25px;"
      ),
      br(), br(),
      
      # Première rangée de boutons d'action
      fluidRow(
        actionLink( 
          inputId = "link2", # ID du bouton
          label = HTML(as.character(valueBoxOutput("bbox"))) # Contenu affiché dans le bouton
        ),
        actionLink(
          inputId = "link1",
          label = HTML(as.character(valueBoxOutput("abox")))
        ),
        actionLink(
          inputId = "link3",
          label = HTML(as.character(valueBoxOutput("cbox")))
        )
      ),
      
      # Deuxième rangée de boutons d'action
      fluidRow(
        actionLink(
          inputId = "link2b",
          label = HTML(as.character(valueBoxOutput("bsous_box_a")))
        ),
        actionLink(
          inputId = "link1b",
          label = HTML(as.character(valueBoxOutput("asous_box_a")))
        )
      ),
      
      # Troisième rangée de boutons d'action
      fluidRow(
        actionLink(
          inputId = "link2c",
          label = HTML(as.character(valueBoxOutput("bsous_box_b")))
        ),
        actionLink(
          inputId = "link1c",
          label = HTML(as.character(valueBoxOutput("asous_box_b")))
        )
      ),
      
      # Quatrième rangée de boutons d'action
      fluidRow(
        actionLink(
          inputId = "link1d",
          label = HTML(as.character(valueBoxOutput("bsous_box_c")))
        )
      ),
      
      # Image centrale
      fluidRow(
        column(
          12, align = "center",
          div(style = "display: inline-block;", img(src = "JO.png", width = 1000)) # Logo des JO en large
        )
      )
    ),
                           
# Résultats - Menu principal pour afficher différentes analyses et classements
navbarMenu(
  "Résultats",
  
  # --- Classement par édition ---
  tabPanel(
    'Edition',
    value = 'tab1',
    fluidPage(
      # Titre principal
      h1(
        "Classement des athlètes par rapport au nombre de médailles remportées en fonction de l'édition", 
        align = "center",  
        style = "color: #B88F44CC;"
      ),
      br(), br(),
      
      # Rangée pour les filtres et graphiques
      fluidRow(
        column(
          3,
          wellPanel(
            # Sélection du nombre d'athlètes à afficher
            sliderInput(
              inputId = "bins_athlete1", 
              label = "Choisir un nombre d'athlètes à visualiser", 
              min = 1, 
              max = 30,
              value = 10, 
              step = 1
            ),
            # Sélection de l'édition
            selectInput(
              inputId = "choix_edition",
              label = "Choisir l'édition",
              choices = unique(classement_edition$edition),
              selected = " "
            )
          )
        ),
        # Graphique du classement
        column(9, plotlyOutput("graph_classement_edition"))
      ),
      br(), br(),
      
      # Tableau des résultats
      column(12, reactableOutput("datatable_classement_edition"))
    )
  ),
  
  # --- Classement par pays ---
  tabPanel(
    'Pays',
    value = 'tab1b',
    tabsetPanel(
      
      # Nombre d'athlètes par pays
      tabPanel(
        "Nombre d'athlètes",
        fluidPage(
          fluidRow(
            # Titre principal
            h1(
              "Classement des pays par rapport au nombre d'athlètes sur tous les Jeux Olympiques", 
              align = "center",
              style = "color: #B88F44CC;"
            ),
            h4("entre 1896 et 2022", align = "center"),
            
            # Filtre pour sélectionner le nombre de pays
            column(
              3,
              wellPanel(
                sliderInput(
                  inputId = "bins_athetes_par_pays", 
                  label = "Choisir un nombre de pays à visualiser", 
                  min = 1, 
                  max = 30,
                  value = 10, 
                  step = 1
                )
              )
            ),
            
            # Graphique des pays
            column(7, plotlyOutput("graph_athletes_pays")),
            
            # Texte explicatif
            column(2, textOutput("commentaire_nb_athlete_pays"))
          ),
          
          br(), br(),
          
          # Evolution du nombre d'athlètes par pays
          fluidRow(
            h1(
              "Evolution du nombre d'athlètes en fonction des pays sur tous les Jeux Olympiques", 
              align = "center", 
              style = "color: #B88F44CC;"
            )
          ),
          br(), br(),
          
          # Filtres pour sélectionner deux pays
          fluidRow(
            column(
              3,
              wellPanel(
                selectInput(
                  inputId = "pays_selection1",
                  label = "Choisir le premier pays",
                  choices = unique(nb_athlete_edition$country),
                  selected = "France"
                ),
                selectInput(
                  inputId = "pays_selection2",
                  label = "Choisir le deuxième pays",
                  choices = unique(nb_athlete_edition$country),
                  selected = "United States"
                ),
                actionButton(
                  inputId = "idActionButton", 
                  label = "Click !"
                )
              )
            ),
            
            # Graphiques des évolutions et commentaires
            column(
              7,
              plotlyOutput("line_nb_athlete_edition"),
              br(), br(),
              textOutput("commentaire_graph_nb_athlete_pays")
            )
          ),
          br(), br()
        )
      ),
      
      # Nombre de médailles par pays
      tabPanel(
        "Nombre de médailles",
        h1(
          "Classement des pays ayant remporté le plus de médailles toutes éditions confondues", 
          align = "center",
          style = "color: #B88F44CC;"
        ),
        br(), br(),
        
        # Filtres pour afficher le classement
        wellPanel(
          fluidRow(
            column(
              3,
              sliderInput(
                inputId = "bins_medaille_par_pays", 
                label = "Choisir le nombre de pays à visualiser", 
                min = 1, 
                max = 30,
                value = 10, 
                step = 1
              )
            ),
            column(
              3,
              selectInput(
                inputId = "choix_edition_medaille_pays",
                label = "Choisir l'édition",
                choices = unique(top_pays_medailles_edition_pivot$edition),
                selected = "2022 Winter"
              )
            ),
            column(
              3,
              actionButton(
                inputId = "idActionButton1", 
                label = "Click !"
              )
            )
          )
        ),
        
        # Graphiques des résultats
        fluidRow(
          column(
            6,
            plotlyOutput("graph_medaille_pays"),
            br(),
            textOutput("commentaire_medaille")
          ),
          column(
            6,
            plotlyOutput("graph_medaille_pays_edition"),
            br(),
            textOutput("commentaire_medaille_edition")
          )
        )
      ),
      
      # Classement des pays hôtes
      tabPanel(
        "Nombre de fois pays hôte",
        fluidRow(
          h1(
            "Classement des pays qui ont organisé le plus de fois les Jeux Olympiques", 
            align = "center",
            style = "color: #B88F44CC;"
          ),
          br(), br(),
          
          # Filtre pour le nombre de pays
          column(
            3,
            wellPanel(
              sliderInput(
                inputId = "bins_pays_hote", 
                label = "Choisir le nombre de pays à visualiser", 
                min = 1, 
                max = 30,
                value = 10, 
                step = 1
              )
            )
          ),
          
          # Graphique et tableau
          column(6, plotlyOutput("graph_pays_hote")),
          br(),
          column(12, reactableOutput("datatable_hote"))
        )
      )
    )
  )
),

# Menu "Transformations des JO"
navbarMenu(
  "Transformations des JO",
  
  # --- Évolution du nombre d'athlètes ---
  tabPanel(
    "Nombre d'athlètes",
    value = "tab2",
    br(),
    
    # Titre principal
    fluidRow(
      h1("Evolution du nombre d'athlètes", style = "color: #B88F44CC; text-align: center")
    ),
    br(),
    
    # Filtres pour saison et édition
    fluidRow(
      column(
        width = 7,
        selectInput(
          inputId = "choix_saison_a",
          label = "Choisir la saison",
          choices = unique(evol_nb_athlete$season)
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = "choix_edition_a",
          label = "Choisir l'édition",
          choices = unique(evol_nb_athlete$edition)
        )
      )
    ),
    br(),
    
    # Graphique et logo
    fluidRow(
      column(width = 7, plotlyOutput("evol_athletes")),
      column(width = 4, htmlOutput('logo_ed'), offset = 1)
    ),
    br(), br(),
    
    # Indicateurs du nombre d'hommes et de femmes
    fluidRow(
      column(
        width = 4,
        tags$img(
          src = "fleche.png",
          height = "100px",
          width = "100px"
        )
      ),
      valueBox(
        tagList(textOutput("nb_femmes"), tags$sup(style = "font-size: 40px")),
        "Nombre de femmes", 
        icon = icon("venus"), 
        color = "fuchsia"
      ),
      valueBox(
        tagList(textOutput("nb_hommes"), tags$sup(style = "font-size: 40px")),
        "Nombre d'hommes", 
        icon = icon("mars"), 
        color = "blue"
      )
    )
  ),
  
  # --- Évolution du nombre de disciplines ---
  tabPanel(
    "Nombre de disciplines",
    value = 'tab2b',
    br(),
    
    # Titre principal
    h1("Evolution du nombre de disciplines", style = "color: #B88F44CC; text-align: center"),
    br(), br(),
    
    # Section pour les Jeux Olympiques d'été
    h2("Jeux Olympiques d'été", style = "color: #B88F44CC; text-align: center"),
    br(),
    fluidRow(
      column(
        width = 5,
        selectInput(
          inputId = "choix_edition_ete",
          label = "Choisir l'édition",
          choices = unique(disciplines_ete$edition)
        )
      )
    ),
    fluidRow(
      column(width = 6, plotlyOutput("evol_discipline_a")),
      column(width = 6, DT::dataTableOutput('events_ete'))
    ),
    
    # Section pour les Jeux Olympiques d'hiver
    h2("Jeux Olympiques d'hiver", style = "color: #B88F44CC; text-align: center"),
    br(),
    fluidRow(
      column(
        width = 5,
        selectInput(
          inputId = "choix_edition_hiver",
          label = "Choisir l'édition",
          choices = unique(disciplines_hiver$edition)
        )
      )
    ),
    fluidRow(
      column(width = 6, plotlyOutput("evol_discipline_b")),
      column(width = 6, DT::dataTableOutput('events_hiver'))
    )
  ),
  
  # --- Caractéristiques physiques ---
  tabPanel(
    "Caractéristiques physiques",
    value = 'tab2c',
    br(),
    
    # Titre principal
    h1("Evolution des caractéristiques physiques", style = "color: #B88F44CC; text-align: center"), 
    br(), br(),
    
    # Jeux Olympiques d'été
    h2("Jeux Olympiques d'été", style = "color: #B88F44CC; text-align: center"), 
    br(),
    tabsetPanel(
      # Évolution de la taille
      tabPanel(
        "Evolution de la taille",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_a",
              label = "Choisir le sport",
              choices = c('ALL', unique(evol_phys_ete$sport)),
              selected = 'Athletics'
            ),
            selectInput(
              inputId = 'choix_event_a',
              label = 'Choisir la discipline',
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_taille_ete"))
        )
      ),
      
      # Évolution du poids
      tabPanel(
        "Evolution du poids",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_b",
              label = "Choisir le sport",
              choices = c('All', unique(evol_phys_ete$sport)),
              selected = 'Athletics'
            ),
            selectInput(
              inputId = 'choix_event_b',
              label = 'Choisir la discipline',
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_poids_ete"))
        )
      ),
      
      # Évolution de l'âge
      tabPanel(
        "Evolution de l'âge",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_c",
              label = "Choisir le sport",
              choices = c('All', unique(evol_phys_ete$sport)),
              selected = 'Athletics'
            ),
            selectInput(
              inputId = "choix_event_c",
              label = 'Choisir la discipline',
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_age_ete"))
        )
      )
    ),
    
    # Jeux Olympiques d'hiver
    h2("Jeux Olympiques d'hiver", style = "color: #B88F44CC; text-align: center"), 
    br(),
    tabsetPanel(
      # Évolution de la taille
      tabPanel(
        "Evolution de la taille",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_d",
              label = "Choisir le sport",
              choices = c('All', unique(evol_phys_hiver$sport)),
              selected = 'Ice Hockey'
            ),
            selectInput(
              inputId = "choix_event_d",
              label = "Choisir la discipline",
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_taille_hiver"))
        )
      ),
      
      # Évolution du poids
      tabPanel(
        "Evolution du poids",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_e",
              label = "Choisir le sport",
              choices = unique(evol_phys_hiver$sport)
            ),
            selectInput(
              inputId = "choix_event_e",
              label = "Choisir la discipline",
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_poids_hiver"))
        )
      ),
      
      # Évolution de l'âge
      tabPanel(
        "Evolution de l'âge",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "choix_sport_f",
              label = "Choisir le sport",
              choices = unique(evol_phys_hiver$sport)
            ),
            selectInput(
              inputId = 'choix_event_f',
              label = "Choisir la discipline",
              choices = NULL
            )
          ),
          mainPanel(plotlyOutput("evol_age_hiver"))
        )
      )
    )
  )
),

                           
# Onglet "À propos de nous"
tabPanel(
  'A propos de nous',
  value = 'tab3',
  
  # Titre principal
  fluidRow(
    h1(
      'Qui sommes-nous ?', 
      style = "color: black; text-align: center; font-size: 50px; font-weight: bold;"
    )
  ),
  br(),
  
  # Section de présentation textuelle
  fluidRow(
    column(
      6,
      textOutput('presentation'), # Texte dynamique pour la présentation
      style = "color: black; text-align: center; font-size: 20px;",
      offset = 3 # Centrer la colonne
    )
  ),
  br(),
  
  # Image illustrant la section "À propos de nous"
  fluidRow(
    column(
      12,
      align = "center",
      div(
        style = "display: inline-block;", 
        img(src = "propos.png", width = 800)
      )
    )
  ),
  br(),
  
  # Liens externes avec icônes
  fluidRow(
    # Lien vers le site de Digisport
    tags$a(
      href = "https://digisport.univ-rennes.fr/",
      tags$img(
        src = "liens_digi.png",
        title = "Digisport",
        width = 250,
        height = 200
      )
    ),
    
    # Lien vers le site des Jeux Olympiques
    tags$a(
      href = "https://olympics.com/fr/",
      tags$img(
        src = "JO.png",
        title = "JO",
        width = 200,
        height = 150
      )
    ),
    
    # Lien vers le profil LinkedIn de Pierre
    tags$a(
      href = "https://www.linkedin.com/in/pierrehernot/",
      tags$img(
        src = "Pierrelink.png",
        title = "Pierre Hernot",
        width = 200,
        height = 200,
        align = 'right'
      )
    ),
    
    # Lien vers le profil LinkedIn d'Anthony
    tags$a(
      href = "https://www.linkedin.com/in/anthony-place-528091193/",
      tags$img(
        src = "Antholink.png",
        title = "Anthony Place",
        width = 200,
        height = 200,
        align = 'right'
      )
    )
  )
)
))

