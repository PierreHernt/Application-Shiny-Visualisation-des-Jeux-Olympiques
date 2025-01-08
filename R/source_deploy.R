options(dplyr.summarise.inform = FALSE)

# Chargement des données
athlete <- readRDS("data/athlete")
resultat <- readRDS("data/resultat")
JO <- readRDS("data/JO")
pays <- readRDS("data/pays")
medaille <- readRDS("data/medaille")
logo_edition <- readRDS("data/logo_edition")

# Filtrage des données
resultat <- resultat |> filter(!grepl("Intercalated|Equestrian", edition))
JO <- JO |> filter(!edition %in% c("2024", "2026", "2028", "2032", "1940", "1944", "1916")) |> 
  mutate(logo = logo_edition$V1)

# Transformation des JO : Évolution du nombre d'athlètes
evol_nb_athlete <- resultat |> 
  inner_join(athlete, by = c("country_noc", "athlete_id")) |> 
  inner_join(JO, by = "edition") |> 
  group_by(edition, season, year, sex, logo) |> 
  summarise(Athletes = n_distinct(athlete_id), .groups = "drop")

# Nombre de disciplines par édition
nb_discipline <- resultat |> 
  group_by(edition) |> 
  summarise(Total_disciplines = n_distinct(event), .groups = "drop") |> 
  inner_join(JO, by = "edition") |> 
  select(annee = year, Total_disciplines, season)

nb_discipline_ete <- nb_discipline |> filter(season == "Summer")
nb_discipline_hiver <- nb_discipline |> filter(season == "Winter")

# Fonction pour créer des graphiques
evol_discipline_plot <- function(data, color) {
  ggplotly(
    ggplot(data, aes(x = annee, y = Total_disciplines)) +
      geom_point(size = 2, color = color) +
      geom_line(color = color) +
      labs(title = "Nombre de disciplines") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#EDE4CA", colour = "#917D3D", size = 2, linetype = "solid")
      )
  )
}

evol_discipline_ete <- evol_discipline_plot(nb_discipline_ete, "red")
evol_discipline_hiver <- evol_discipline_plot(nb_discipline_hiver, "blue")

# Liste des disciplines par édition
create_disciplines <- function(data, season_filter) {
  data |> 
    filter(season == season_filter) |> 
    arrange(year) |> 
    select(edition, sport, event)
}

disciplines_ete <- create_disciplines(JO |> inner_join(resultat, by = "edition"), "Summer")
disciplines_hiver <- create_disciplines(JO |> inner_join(resultat, by = "edition"), "Winter")

# Caractéristiques physiques
evol_phys <- resultat |> 
  inner_join(athlete, by = c("country_noc", "athlete_id")) |> 
  inner_join(JO, by = "edition") |> 
  mutate(
    born = as.Date(born),
    competition_end_date = as.Date(competition_end_date),
    age = round(as.numeric((competition_end_date - born) / 365.25), 0),
    height = as.integer(height),
    weight = as.integer(weight)
  ) |> 
  drop_na(height, weight) |> 
  select(season, year, age, sport, sex, height, weight, athlete_id, event) |> 
  distinct()

evol_phys_ete <- evol_phys |> filter(season == "Summer")
evol_phys_hiver <- evol_phys |> filter(season == "Winter")

# Classement par édition
classement_edition <- JO |> 
  select(edition, city) |> 
  inner_join(resultat, by = "edition") |> 
  mutate(
    edition = gsub("Olympics", "", edition),
    edition = paste0(edition, city)
  ) |> 
  group_by(edition, athlete_id, athlete, country_noc, sport, medal) |> 
  summarise(Total = n(), .groups = "drop") |> 
  replace_na(list(Gold = 0, Silver = 0, Bronze = 0))


