

options(dplyr.summarise.inform = FALSE)



# Chargement des données
athlete <- readRDS("data/athlete")
resultat <- readRDS("data/resultat")
JO <- readRDS("data/JO")
pays <- readRDS("data/pays")
medaille <- readRDS("data/medaille")
logo_edition <- readRDS("data/logo_edition")

#Filtrage des données
resultat<- resultat |> filter(!grepl("Intercalated", edition)) |> filter(!grepl("Equestrian", edition))
JO<- JO|> filter(!grepl("2024", edition)) |>
  filter(!grepl("2026", edition)) |>
  filter(!grepl("2028", edition)) |> 
  filter(!grepl("2032", edition)) |> 
  filter(!grepl("1940", edition)) |> 
  filter(!grepl("1944", edition)) |>
  filter(!grepl("1916", edition))
#Je rajoute le logo des éditions que j'ai récupérer sur wikipedia
JO <- JO |> mutate(logo = logo_edition$V1)

#Onglet n°4 Transformation des JO 
#Evolution du nb d'athlètes
resultat_athlete <- inner_join(resultat,athlete, by = c("country_noc", "athlete_id"))
resultat_JO <- inner_join(resultat_athlete,JO,by="edition")
evol_nb_athlete <- resultat_JO |>  select(edition,sex,year,athlete_id,season,logo)|> group_by(edition,season,year,sex,logo)|> summarize(Athletes = length(unique(athlete_id)))


#on calcul le nb de disciplines par edition
nb_discipline<-resultat|>
  group_by(edition)|>
  summarise(Total_disciplines=n_distinct(event))
#on divise la colonne edition et année en deux colonnes disctinte
nb_discipline <- inner_join(nb_discipline,JO,by = 'edition') |> select(annee='year',Total_disciplines,season)
nb_discipline_ete <- nb_discipline |> filter(!grepl("Winter", season)) 
nb_discipline_hiver <- nb_discipline |> filter(!grepl("Summer", season)) 

evol_discipline_ete <- ggplotly(ggplot(nb_discipline_ete, aes(x=annee, y=Total_disciplines)) +
                                  geom_point(size=2,color='red') +
                                  geom_line(color='red')  +
                                  labs(title = "Nombre de disciplines") +
                                  theme(plot.title = element_text(hjust = 0.5)) + 
                                  theme(panel.background = element_rect(fill = "#EDE4CA", colour = "#917D3D",
                                                                        size = 2, linetype = "solid")))

evol_discipline_hiver <- ggplotly(ggplot(nb_discipline_hiver, aes(x=annee, y=Total_disciplines)) +
                                    geom_point(size=2,color='blue') +
                                    geom_line(color='blue')  +
                                    labs(title = "Nombre de disciplines") +
                                    theme(plot.title = element_text(hjust = 0.5)) + 
                                    theme(panel.background = element_rect(fill = "#EDE4CA", colour = "#917D3D",
                                                                          size = 2, linetype = "solid")))

# Listes des sports et disciplines associées selon les éditions 

disciplines_edition <- inner_join(resultat,JO,by = 'edition') |> select(edition,sport,event,year) 
disciplines_edition <- distinct(disciplines_edition)

disciplines_ete <- disciplines_edition |> filter(!grepl("Winter", edition)) |> arrange(year)
disciplines_ete <- disciplines_ete |> select(edition,sport,event)

disciplines_hiver <- disciplines_edition |> filter(!grepl("Summer", edition)) |> arrange(year)
disciplines_hiver <- disciplines_hiver |> select(edition,sport,event)

# Evolution caractéristiques physiques
# Calcul de l'âge

evol_phys <- inner_join(resultat_athlete,JO,by='edition_id')
evol_phys$born <- as.Date(evol_phys$born)
evol_phys$competition_end_date <- as.Date(evol_phys$competition_end_date)
evol_phys$age <- as.numeric((evol_phys$competition_end_date-evol_phys$born)/365.25)
evol_phys <- evol_phys |> select(season,year,age,sport,sex,height,weight,athlete_id,event) |> distinct()
suppressWarnings(evol_phys$height <- as.integer(evol_phys$height))
suppressWarnings(evol_phys$weight <- as.integer(evol_phys$weight))
evol_phys <- evol_phys |> drop_na(height)
evol_phys <- evol_phys |> drop_na(weight)
evol_phys$age <- round(evol_phys$age,0)
evol_phys$year <- as.factor(evol_phys$year)

evol_phys_ete <- evol_phys |> filter(!grepl("Winter", season))
evol_phys_hiver <- evol_phys |> filter(!grepl("Summer", season))



# Code antho



JO_trie<-JO|>select(edition,city)
classement_edition<-inner_join(JO_trie,resultat,by="edition")
classement_edition$edition<-gsub("Olympics","",as.character(classement_edition$edition))
classement_edition$edition<-paste0(classement_edition$edition,classement_edition$city)
classement_edition<-classement_edition|>group_by(edition,athlete_id)|>select(athlete,country_noc,sport,medal,edition,athlete_id)

#On récupère le nb de médailles par athlètes

total_medaille_edition<-classement_edition|>
  group_by(edition,athlete_id,athlete,sport)|>
  filter(medal!="na")|>
  summarise(Total=n())

#On récupère le nb de médaille d'or par athlètes
Gold_edition<-classement_edition|>
  group_by(edition,athlete_id,athlete,sport)|>
  filter(medal=="Gold")|>
  summarise(Gold=n())

#On récupère le nb de médaille d'argent par athlète
Silver_edition<-classement_edition|>
  group_by(edition,athlete_id,athlete,sport)|>
  filter(medal=="Silver")|>
  summarise(Silver=n())

#On récupère le nb de médaille de bronze par athlète
Bronze_edition<-classement_edition|>
  group_by(edition,athlete_id,athlete,sport)|>
  filter(medal=="Bronze")|>
  summarise(Bronze=n())
#on récupère ceux qui n'ont pas eu de médaille
pas_medaille_edition<-classement_edition|>
  group_by(edition,athlete_id,athlete,sport)|>
  filter(medal=="na")|>
  summarise(rien=n())

#On joint les tableaux
Gold_Silver_edition<-full_join(Gold_edition,Silver_edition,by = c("edition", "athlete_id", "athlete", "sport"))
Bronze_total_medaille_edition<-full_join(Bronze_edition,total_medaille_edition,by = c("edition", "athlete_id", "athlete", "sport"))

classement_edition<-full_join(Gold_Silver_edition,Bronze_total_medaille_edition,by = c("edition", "athlete_id", "athlete", "sport"))


#On remplace les na par des 0
classement_edition[is.na(classement_edition)] <- 0

#il faut maintenant récupérer le country noc associé à l'athlète
#pas besoin de sélectionner tout le tableau, on récupère uniquement 
athlete_id_noc<-athlete|>select(athlete_id,country_noc)

#on joint les tab
classement_edition<-inner_join(classement_edition,athlete_id_noc,by = "athlete_id")

#on enlève la colonne id
classement_edition<-classement_edition[,-2]
classement_edition<-classement_edition|>relocate(country_noc,.before=sport)
classement_edition<-classement_edition|>arrange(edition,(desc(Total)))

graph_classement_edition_total<-ggplotly(ggplot(classement_edition[1:10,],(aes(x=reorder(athlete,Total),y=Total,fill=athlete)))+
                                           geom_bar(stat="identity")+
                                           coord_flip()+
                                           labs(title = 'TOP 20',
                                                subtitle = 'JO entre 1896 et 2022',
                                                x=" ",
                                                y="nombre de médailles")+
                                           theme(plot.title = element_text(hjust = 0.5),
                                                 legend.position = "none"))

classement_edition_pivot<-classement_edition|>pivot_longer(Gold:Bronze, names_to = "medaille", values_to = "nombre")
classement_edition_pivot<-classement_edition_pivot|>arrange(desc(Total))
classement_edition_pivot$medaille <- factor(classement_edition_pivot$medaille, levels = c("Bronze", "Silver", "Gold"))

classement_edition_graph<-ggplot(classement_edition_pivot[1:63,],(aes(x=reorder(athlete,Total),y=nombre,fill=medaille)))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title = 'Classement',
       subtitle = 'JO entre 1896 et 2022',
       x=" ",
       y="nombre de médailles")+
  scale_fill_manual(values=c("gold4","gray70","gold1"))

classement_edition_interactif<-reactable(
  classement_edition,
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
  pageSizeOptions = c(10, 20, 30),
  defaultPageSize = 10,
  striped = TRUE, highlight = TRUE,
  theme = reactableTheme(
    headerStyle = list(
      "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
      borderColor = "#555")))

nb_athlete_pays <- athlete |> select(country_noc) |> group_by(country_noc)|> summarise(nombre=n())
nb_athlete_pays<-inner_join(nb_athlete_pays,pays,by="country_noc")



#on classe dans l'ordre décroissant (mais cela ne prend pas en compte mes opérations précédentes)
nb_athlete_pays<-nb_athlete_pays|> arrange(desc(nombre))

top_pays_athlete <- ggplotly(ggplot(nb_athlete_pays, aes(x=reorder(country,nombre), y=nombre,fill=country)) +
                               geom_col() +
                               coord_flip() +
                               ggtitle("Classement des pays avec le plus d'athlètes") +
                               labs(subtitle = 'JO entre 1896 et 2022',
                                    x=" ",
                                    y="Nombre d'athlètes")+
                               theme(plot.title = element_text(hjust = 0.5),
                                     legend.position = "none"))

#tableau
JO_trie1<-JO|>select(edition,city,year,season)
nb_athlete_edition<-inner_join(JO_trie1,resultat,by="edition","year")
nb_athlete_edition$edition<-gsub("Olympics","",as.character(nb_athlete_edition$edition))
nb_athlete_edition$edition<-paste0(nb_athlete_edition$edition,nb_athlete_edition$city)
nb_athlete_edition<-nb_athlete_edition|>select(athlete_id,edition,country_noc,year,season)|>distinct()

nb_athlete_edition <- nb_athlete_edition|>group_by(country_noc,year,edition,season)|> summarise(participants=n())

nb_athlete_edition<-inner_join(nb_athlete_edition,pays,by="country_noc")

graph_nb_athlete_edition<-ggplot(nb_athlete_edition,(aes(x=year,y=participants,fill=country,color=country)))+
  geom_line()+
  theme_bw()+
  labs(title = "Evolution du nombre d'athletes par edition",
       subtitle = 'entre 1896 et 2022',
       x="pays",
       y="Nombre de participant")+
  facet_wrap(~season,ncol=1)

top_pays_medailles <- medaille |> select(country,total,silver,gold,bronze) |> pivot_longer(silver:bronze, names_to = "medaille", values_to = "nombre")
top_pays_medailles<-top_pays_medailles[complete.cases(top_pays_medailles[,2]),]
top_pays_medailles <- top_pays_medailles |> group_by(country,medaille) |> summarise(total = sum(total),nombre = sum(nombre)) |> arrange(desc(total))
top_pays_medailles$medaille <- factor(top_pays_medailles$medaille, levels = c("bronze", "silver", "gold"))

graph_top_pays_medailles <- ggplotly(ggplot(top_pays_medailles[1:90,], (aes(x=reorder(country,total), y=nombre, fill=medaille))) +
                                       geom_col() +
                                       coord_flip() +
                                       scale_fill_manual(values=c("gold4","gray70","gold1")) +
                                       ggtitle("Top des pays les plus médaillés") +
                                       labs(subtitle = 'JO entre 1896 et 2022',
                                            x=" ",
                                            y="Nombre de médailles")+
                                       theme(plot.title = element_text(hjust = 0.5)))

top_pays_medailles_edition <- medaille |> select(edition,country,total,silver,gold,bronze)
top_pays_medailles_edition$edition<-gsub("Olympics","",as.character(top_pays_medailles_edition$edition))

top_pays_medailles_edition_pivot<-top_pays_medailles_edition|>pivot_longer(silver:bronze, names_to = "medaille", values_to = "nombre")

top_pays_medailles_edition_pivot$medaille <- factor(top_pays_medailles_edition_pivot$medaille, levels = c("bronze", "silver", "gold","total"))


#graph
graph_pays_medailles_edition<-ggplotly(ggplot(top_pays_medailles_edition_pivot[1:50,], (aes(x=country, y=nombre, fill=medaille))) +
                                         geom_col() +
                                         ggtitle("Top des pays les plus médaillés") +
                                         labs(subtitle = 'JO entre 1896 et 2022',
                                              x=" ",
                                              y="Nombre de médailles")+
                                         theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))+
                                         scale_fill_manual(values=c("gold4","gray70","gold1")))


OG_bis <- inner_join(JO,pays, by = c("country_noc"))
nb_pays_hote <- OG_bis |> select(country) |> group_by(country)|> summarise(nombre=n()) |> arrange(desc(nombre))


top_pays_hote <- ggplotly(ggplot(nb_pays_hote[1:30,], aes(x=reorder(country,nombre), y=nombre,fill=country)) +
                            geom_col() +
                            coord_flip() +
                            ggtitle("Top 10 des pays ayant été le plus de fois hôte") +
                            labs(subtitle = 'JO entre 1896 et 2022',
                                 x=" ",
                                 y="Nombre de fois hôte")+
                            theme(plot.title = element_text(hjust = 0.5),
                                  legend.position = "none"))

hote_games<-JO|>select(edition,city,country_noc)
hote_games$edition<-gsub("Olympics","",as.character(hote_games$edition))
hote_games<-inner_join(hote_games,pays,by="country_noc")

hote_interactif<-reactable(
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
      borderColor = "#555")))
