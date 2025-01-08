server <- function(input, output,session) {
  
  
# Page accueil
  output$abox <- renderValueBox({
    valueBox(
      value = h4("Resultats", style = "text-align:left;font-size: 30px"),
      subtitle = "de 1896 à 2022",
      color = "light-blue",
      icon = icon("medal",verify_fa = FALSE),
      width = NULL
    )
  })
  
  observeEvent(input$link1, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab1")
  })
  
  
  output$asous_box_a <- renderValueBox({
    valueBox(
      value = h4("1. Classement par édition", style = "text-align:left;font-size: 20px"),
      subtitle = "",
      color = "light-blue",
    )
  })
  
  observeEvent(input$link1b, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab1")
  })
  
  output$asous_box_b <- renderValueBox({
    valueBox(
      value = h4("2. Classement par pays", style = "text-align:left;font-size: 20px"),
      subtitle = "",
      color = "light-blue",
    )
  })
  
  observeEvent(input$link1c, {
    updateTabsetPanel(inputId = "tabset", selected = "tab1b")
  })
  
# Transformation des JO
  
  output$bbox <- renderValueBox({
    valueBox(
      value = h4("Transformations des JO", style = "text-align:left;font-size: 30px"),
      subtitle = "de 1896 à 2022",
      color = "olive",
      icon = icon("arrow-trend-up",verify_fa = FALSE)
    )
  })
  
  observeEvent(input$link2, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab2")
  })
  
# Nombre d'athlètes
  
  output$bsous_box_a <- renderValueBox({
    valueBox(
      value = h4("1. Nombre d'athlètes", style = "text-align:left;font-size: 20px"),
      subtitle = "",
      color = "olive"
    )
  })
  observeEvent(input$link2b, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab2")
  })
  
# Nombre de disciplines
  output$bsous_box_b <- renderValueBox({
    valueBox(
      value = h4("2. Nombre de disciplines", style = "text-align:left;font-size: 20px"),
      subtitle = "",
      color = "olive"
    )
  })
  observeEvent(input$link2c, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab2b")
  })
  
# Caractéristiques physiques
  output$bsous_box_c <- renderValueBox({
    valueBox(
      value = h4("3. Caractéristiques physiques", style = "text-align:left;font-size: 20px"),
      subtitle = "",
      color = "olive"
    )
  })
  observeEvent(input$link1d, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab2c")
  })
  
  
  
# A propos de nous
  output$cbox <- renderValueBox({
    valueBox(
      value = h4("A propos de nous", style = "text-align:left;font-size: 30px"),
      subtitle = "Anthony PLACE et Pierre HERNOT",
      color = "yellow",
      icon = icon("fa-sharp fa-solid fa-user-group",verify_fa = FALSE)
    )
  })
  
  observeEvent(input$link3, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab3")
  })
  
  
  
  
  #TRANSFORMATIONS DES JO
  
  #Nombre d'athlètes
  
  fill_evol_athlete <- reactive({
    subset(evol_nb_athlete, season == input$choix_saison_a)
  })  
  
  output$evol_athletes <- renderPlotly({
    ggplotly(ggplot(fill_evol_athlete(), aes(x=year, y=Athletes, group=sex,color=sex)) +
               geom_point(size=2) +
               geom_line()  +
               scale_color_manual(values=c("black","red")) +
               theme(plot.title = element_text(hjust = 0.5))+
               theme(panel.background = element_rect(fill = "#EDE4CA", colour = "#917D3D",
                                                     size = 2, linetype = "solid")))
  }) 
  
  table_athlete <- reactive({
    subset(evol_nb_athlete,edition == input$choix_edition_a)
  })
  
  output$nb_femmes <- renderText({
    (table_athlete()$Athletes[1])
  })
  
  output$nb_hommes <- renderText({
    (table_athlete()$Athletes[2])        
  })
  
  output$logo_ed = renderUI({
    tags$img(src = table_athlete()$logo[1], width="250px", height="400px",align = "center")  
    
  })
  
  
  
  # Nombre de disciplines
  
  
  
  output$evol_discipline_a <- renderPlotly({
    evol_discipline_ete
  })
  output$evol_discipline_b <- renderPlotly({
    evol_discipline_hiver
  }) 
  
  table_discipline_ete <- reactive({
    subset(disciplines_ete,edition == input$choix_edition_ete)
  })
  
  output$events_ete <- DT::renderDataTable({
    DT::datatable(table_discipline_ete(), options = list(width = 250,height = 250,scrollY=200, scroller = TRUE,bPaginate = FALSE))
  })
  
  table_discipline_hiver <- reactive({
    subset(disciplines_hiver,edition == input$choix_edition_hiver)
  })
  
  output$events_hiver <- DT::renderDataTable({
    DT::datatable(table_discipline_hiver(), options = list(width = 250,height = 250,scrollY=200, scroller = TRUE,bPaginate = FALSE))
  })
  
  #Caractéristiques physiques 
  
  #JO été
  
  #Taille JO été
  
  observeEvent(input$choix_sport_a, {
    if (input$choix_sport_a != "All"){
      updateSelectInput(session,"choix_event_a", "Choix discipline", choices= c('All', unique(evol_phys_ete$event[evol_phys_ete$sport == input$choix_sport_a])))
    } else { 
      updateSelectInput(session, "choix_event_a", "Choix discipline", choices =  c('All', unique(evol_phys_ete$event)))
    }
  })
  
  table_taille_ete <- reactive ({
    taille_a <- evol_phys_ete
    if(input$choix_sport_a != 'All'){
      taille_a <- taille_a[taille_a$sport == input$choix_sport_a, ]
    }
    if (input$choix_event_a != 'All'){
      taille_a <- taille_a[taille_a$event == input$choix_event_a, ]
    }
    taille_a
  })
  
  output$evol_taille_ete <- renderPlotly({
    ggplotly(ggplot(table_taille_ete(), aes(x=year, y=height,fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
  })
  
  #Poids JO été 
  
  observeEvent(input$choix_sport_b, {
    if (input$choix_sport_b != "All"){
      updateSelectInput(session,"choix_event_b", "Choix discipline", choices= c('All', unique(evol_phys_ete$event[evol_phys_ete$sport == input$choix_sport_b])))
    } else { 
      updateSelectInput(session, "choix_event_b", "Choix discipline", choices =  c('All', unique(evol_phys_ete$event)))
    }
  })
  
  table_poids_ete <- reactive ({
    poids_a <- evol_phys_ete
    if(input$choix_sport_b != 'All'){
      poids_a <- poids_a[poids_a$sport == input$choix_sport_b, ]
    }
    if (input$choix_event_b != 'All'){
      poids_a <- poids_a[poids_a$event == input$choix_event_b, ]
    }
    poids_a
  })
  
  output$evol_poids_ete <- renderPlotly({
    ggplotly(ggplot(table_poids_ete(), aes(x=year, y=weight, fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
    
  })
  
  # Âge JO été
  
  
  observeEvent(input$choix_sport_c, {
    if (input$choix_sport_c != "All"){
      updateSelectInput(session,"choix_event_c", "Choix discipline", choices= c('All', unique(evol_phys_ete$event[evol_phys_ete$sport == input$choix_sport_c])))
    } else { 
      updateSelectInput(session, "choix_event_c", "Choix discipline", choices =  c('All', unique(evol_phys_ete$event)))
    }
  })
  
  table_age_ete <- reactive ({
    age_a <- evol_phys_ete
    if(input$choix_sport_c != 'All'){
      age_a <- age_a[age_a$sport == input$choix_sport_c, ]
    }
    if (input$choix_event_c != 'All'){
      age_a <- age_a[age_a$event == input$choix_event_c, ]
    }
    age_a
  })
  
  output$evol_age_ete <- renderPlotly({
    ggplotly(ggplot(table_age_ete(), aes(x=year, y=weight, fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
    
  })
  
  
  
  # JEUX HIVER
  
  #Taille JO Hiver
  
  observeEvent(input$choix_sport_d, {
    if (input$choix_sport_d != "All"){
      updateSelectInput(session,"choix_event_d", "Choix discipline", choices= c('All', unique(evol_phys_hiver$event[evol_phys_hiver$sport == input$choix_sport_d])))
    } else { 
      updateSelectInput(session, "choix_event_d", "Choix discipline", choices =  c('All', unique(evol_phys_hiver$event)))
    }
  })
  
  table_taille_hiver <- reactive ({
    taille_b <- evol_phys_hiver
    if(input$choix_sport_d != 'All'){
      taille_b <- taille_b[taille_b$sport == input$choix_sport_d, ]
    }
    if (input$choix_event_d != 'All'){
      taille_b <- taille_b[taille_b$event == input$choix_event_d, ]
    }
    taille_b
  })
  
  output$evol_taille_hiver <- renderPlotly({
    ggplotly(ggplot(table_taille_hiver(), aes(x=year, y=height,fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
  })
  
  #Poids JO Hiver
  
  
  observeEvent(input$choix_sport_e, {
    if (input$choix_sport_e != "All"){
      updateSelectInput(session,"choix_event_e", "Choix discipline", choices= c('All', unique(evol_phys_hiver$event[evol_phys_hiver$sport == input$choix_sport_e])))
    } else { 
      updateSelectInput(session, "choix_event_e", "Choix discipline", choices =  c('All', unique(evol_phys_hiver$event)))
    }
  })
  
  table_poids_hiver <- reactive ({
    poids_b <- evol_phys_hiver
    if(input$choix_sport_e != 'All'){
      poids_b <- poids_b[poids_b$sport == input$choix_sport_e, ]
    }
    if (input$choix_event_e != 'All'){
      poids_b <- poids_b[poids_b$event == input$choix_event_e, ]
    }
    poids_b
  })
  
  output$evol_poids_hiver <- renderPlotly({
    ggplotly(ggplot(table_poids_hiver(), aes(x=year, y=height,fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
  })
  
  
  # Âge JO Hiver
  
  observeEvent(input$choix_sport_f, {
    if (input$choix_sport_f != "All"){
      updateSelectInput(session,"choix_event_f", "Choix discipline", choices= c('All', unique(evol_phys_hiver$event[evol_phys_hiver$sport == input$choix_sport_f])))
    } else { 
      updateSelectInput(session, "choix_event_f", "Choix discipline", choices =  c('All', unique(evol_phys_hiver$event)))
    }
  })
  
  table_age_hiver <- reactive ({
    age_b <- evol_phys_hiver
    if(input$choix_sport_f != 'All'){
      age_b <- age_b[age_b$sport == input$choix_sport_f, ]
    }
    if (input$choix_event_f != 'All'){
      age_b <- age_b[age_b$event == input$choix_event_f, ]
    }
    age_b
  })
  
  output$evol_age_hiver <- renderPlotly({
    ggplotly(ggplot(table_age_hiver(), aes(x=year, y=height,fill=sex))+geom_boxplot()+scale_x_discrete(breaks=c("1896","1912", "1936","1968","1992","2004", "2020")))
  })
  
  #Résultats
  
  
  # Edition
  
  
  # Graphique édition
  
  output$graph_classement_edition<-renderPlotly({
    ggplotly(ggplot(choix_classement_edition()[1:(input$bins_athlete1),],aes(x=reorder(athlete,Total),y=Total,fill=athlete))+
               geom_bar(stat="identity")+
               coord_flip()+
               labs(title = '',
                    subtitle = '',
                    x=" ",
                    y="nombre de médailles")+
               theme(plot.title = element_text(hjust = 0.5),
                     legend.position = "none"))
  })
  
  #classement par année
  choix_classement_edition <- reactive({
    subset(classement_edition, edition == input$choix_edition)
  })  
  
  #Tableau
  output$datatable_classement_edition<-({
    renderReactable(reactable(choix_classement_edition(),
                              columns = list(
                                edition= colDef(name = "Edition",align="center"),
                                athlete = colDef(name = "Participant", align="center"),
                                country_noc = colDef(name="Equipe", align = "center"),
                                sport=colDef(name="Sport",align = "center"),
                                Gold=colDef(align = "center"),
                                Silver=colDef(align = "center"),
                                Bronze=colDef(align = "center"),
                                Total=colDef(align = "center")),
                              defaultSorted = list(Total = "desc"),
                              showSortIcon = FALSE,
                              searchable = TRUE,
                              showPageSizeOptions = TRUE,
                              pageSizeOptions = c(5, 10, 15,20),
                              defaultPageSize = 10,
                              striped = TRUE, highlight = TRUE,
                              theme = reactableTheme(
                                headerStyle = list(
                                  "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                                  "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                                  borderColor = "#555")))
    )})
  
  #Pays
  #Nombre d'athlètes
  output$graph_athletes_pays <- renderPlotly({
    ggplotly(ggplot(nb_athlete_pays[1:(input$bins_athetes_par_pays),], aes(x=reorder(country,nombre), y=nombre,fill=country)) +
               geom_col() +
               coord_flip() +
               ggtitle("") +
               labs(subtitle = '',
                    x=" ",
                    y="Nombre d'athlètes")+
               theme(plot.title = element_text(hjust = 0.5),
                     legend.position = "none"))
  })
  
  output$commentaire_nb_athlete_pays<-renderText({
    "Nous pouvons voir que les Etats-Unis, La France et le Royaume-Uni sont les pays les plus qui déclarent
    le plus d'athlètes toutes éditions et saisons confondues."})

  dataset <- reactive({
    subset(nb_athlete_edition, country== input$pays_selection1)
  })
  
  dataset1 <- reactive({
    subset(nb_athlete_edition, country == input$pays_selection2)
  })
  output$line_nb_athlete_edition<-renderPlotly({
    input$idActionButton
    isolate({
      ggplotly(ggplot(dataset(),mapping=aes(x=year,y=participants,fill=country,color=country))+
                 geom_line()+
                 geom_line(dataset1(),mapping=aes(x=year,y=participants,color=country))+
                 theme_bw()+
                 labs(title = "",
                      subtitle = '',
                      x="",
                      y="Nombre de participant")+
                 facet_wrap(~season,ncol=1))
    })})
  
  output$commentaire_graph_nb_athlete_pays<-renderText({
    "Le graphique nous permet de voir qu'à certaines périodes il y a un pic de nombre d'athlètes pour certains pays.
    En réalité, ce pic peut s'expliquer par le fait qu'à ce moment-là le pays est organisateur des Jeux olympiques.
    Ensuite, entre les Jeux olympiques d'été et d'hiver, on voit une grande différence dans le nombre d'athlètes. La plupart
    du temps, on peut voir un plus grand nombre d'athlètes aux Jeux olympiques d'été, excepté quelques pays
    nordique comme la Norvège où la balance s'équilibre.
    Enfin, on peut voir que les Jeux Olympiques d'hiver sont apparus bien après les Jeux Olympiques d'été."})
  
  output$graph_medaille_pays <- renderPlotly({
    input$idActionButton1
    isolate({
      ggplotly(ggplot(top_pays_medailles[1:(input$bins_medaille_par_pays*3),], aes(x=reorder(country,total), y=nombre, fill=medaille)) +
                 geom_col() +
                 coord_flip() +
                 scale_fill_manual(values=c("gold4","gray70","gold1")) +
                 ggtitle("Toutes éditions confondues") +
                 labs(subtitle = '',
                      x=" ",
                      y="Nombre de médailles")+
                 theme(plot.title = element_text(hjust = 0.5)))
    })})
  output$commentaire_medaille<-renderText({
    "On peut voir que les Etats-Unis sont dominateurs au classement des médailles. Ensuite,
    malgré l'ancienneté de l'Union Soviétique, elle se place tout de même en deuxième position."})
  
  top_pays_medailles_edition_pivot_reactive<- reactive({
    subset(top_pays_medailles_edition_pivot, edition == input$choix_edition_medaille_pays)
  })
  
  output$graph_medaille_pays_edition <- renderPlotly({
    input$idActionButton1
    isolate({
      ggplotly(ggplot(top_pays_medailles_edition_pivot_reactive()[1:(input$bins_medaille_par_pays*3),], mapping=aes(x=reorder(country,nombre), y=nombre, fill=medaille)) +
                 geom_col() +
                 ggtitle("En fonction d'une édition") +
                 labs(subtitle = '',
                      x=" ",
                      y="Nombre de médailles")+
                 theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))+
                 scale_fill_manual(values=c("gold4","gray70","gold1")))
    })})
  
  output$commentaire_medaille_edition<-renderText({
    "Sur la plupart des éditions d'été, les Etats-Unis se classent premier, avec, en deuxièmes et troisièmes position
    la Chine et la Russie qui s'échange en fonction des années leurs places. Toutefois, sur les éditions d'hiver, le classement est
    différent. En effet, la Chine apparaît très loin dans le classement est la première place est plus partagée
    avec des pays comme notamment la Norvège ou le Canada qui apparaissent parfois dans le top 3, avec encore une fois les 
    Etats-Unis."})
  
  output$graph_pays_hote<- renderPlotly({
    ggplotly(ggplot(nb_pays_hote[1:input$bins_pays_hote,], aes(x=reorder(country,nombre), y=nombre,fill=country)) +
               geom_col() +
               coord_flip() +
               ggtitle("") +
               labs(subtitle = 'JO entre 1896 et 2022',
                    x=" ",
                    y="Nombre de fois hôte")+
               theme(plot.title = element_text(hjust = 0.5),
                     legend.position = "none"))
  })
  
  output$datatable_hote<-({
    renderReactable(
      reactable(
        hote_games,
        columns = list(
          edition= colDef(name = "Edition",align="center"),
          city = colDef(name = "City", align="center"),
          country_noc = colDef(name="Code Pays", align = "center"),
          country=colDef(name="Country",align = "center")),
        showSortIcon = FALSE,
        searchable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 15,20),
        defaultPageSize = 10,
        striped = TRUE, highlight = TRUE,
        theme = reactableTheme(
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
            borderColor = "#555"))))
  })
  
  # A propos de nous 
  output$presentation <- renderText ({
    'Nous sommes deux étudiants en 1er de master ‘Sciences du sport et du numérique’ à l’EUR Digisport. Impatients de suivre les Jeux olympiques de Paris en 2024, nous avons décidé de faire un bilan de cette compétition depuis sa création.' 
  })
}