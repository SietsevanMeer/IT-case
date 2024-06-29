#IT-casus data engineer



#CASUS 1: R deel 1

#Je kunt deze lijnen gebruiken als je een library mist (verwijder de # aan het begin van de lijn. 
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("tidyr")
#install.packages("ggthemes")
#install.packages("DBI") #Postgres verbinding libraries
#install.packages("RPostgres") 
#install.packages("cbsodataR")
#install.packages("ggsave") #Opslaan van ggplots


#Libraries
library(cbsodataR) #API van CVS Open data Statline.
library(ggplot2) #Gebruikt om data te visualizeren.
library(dplyr) # Gebruikt om de dataset te filteren.
library(lubridate) #Voor mutate gebruikt om de dataset uit te breiden met kwartaal statistieken.
library(tidyr) #Voor long-format, nodig om twee lijnen te plotten.
library(ggthemes) #Voor economist theme
library(DBI)
library(RPostgres)


#PostgreSQL verbinding

#PostgreSQL verbinding maken met local database (localhost), database naam cbs_data, en gebruiker CBS Data engineer.
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "cbs_data ", 
                 host = "localhost", 
                 port = 5432, 
                 user = "CBS Data engineer", 
                 password = "CBSData1234")



#Hulpfuncties

#Functie om met verschillende data formats te werken: DD-MM-YYYY, YYYY-MM-DD, MM-YYYY, Q*YYYY    * = 1,2,3,4
Data_input <- function(date_str) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
    date <- as.Date(date_str, format = "%Y-%m-%d")
    if (is.na(date)) stop("Ongeldige datum ingevoerd.")
    return(date)
  } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_str)) {
    date <- as.Date(date_str, format = "%d-%m-%Y")
    if (is.na(date)) stop("Ongeldige datum ingevoerd.")
    return(date)
  } else if (grepl("^\\d{2}-\\d{4}$", date_str)) {
    date <- as.Date(paste0("01-", date_str), format = "%d-%m-%Y")
    if (is.na(date)) stop("Ongeldige datum ingevoerd.")
    return(date)
  } else if (grepl("^\\d{4}-\\d{2}$", date_str)) {
    date <- as.Date(paste0(date_str, "-01"), format = "%Y-%m-%d")
    if (is.na(date)) stop("Ongeldige datum ingevoerd.")
    return(date)
  } else if (grepl("^Q[1-4]\\d{4}$", date_str)) {
    year <- as.integer(substr(date_str, 3, 6))
    quarter <- as.integer(substr(date_str, 2, 2))
    month <- (quarter - 1) * 3 + 1
    date <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"), format = "%Y-%m-%d")
    if (is.na(date)) stop("Ongeldige datum ingevoerd.")
    return(date)
  } else {
    stop("Ongeldige startdatum ingevoerd. Controleer of de startdatum geldig is, of dat u een ondersteund datumformaat gebruikt: DD-MM-YYYY, YYYY-MM-DD, MM-YYYY, Q*YYYY. *=1,2,3,4")
  }
}

# Functie om de start van het kwartaal te krijgen
start_of_quarter <- function(date) {
  quarter_start <- floor_date(date, "quarter")
  return(quarter_start)
}

# Functie om het einde van het kwartaal te krijgen
end_of_quarter <- function(date) {
  quarter_end <- ceiling_date(date, "quarter") - days(1) #Net een dag voor de volgende maand
  return(quarter_end)
}

# Functie die een R plot saved naar een png bestand via library ggsave.
save_plot_to_file <- function(plot, filename) {
  ggsave(filename, plot = plot, device = "png")
}

# Functie om png bestand op te slaan in database.
save_image_to_db <- function(con, table_name, file_path, description) {
  # Het bestand lezen
  img_data <- readBin(file_path, "raw", file.info(file_path)$size)
  
  # Het dataframe maken dat je terug vindt in de SQL database
  img_df <- data.frame(description = description, img = I(list(img_data)))
  
  # Het bestand opslaan
  dbWriteTable(con, table_name, img_df, append = TRUE, row.names = FALSE)
}





  
#Snelle methode: Het laden van de data met API kost het meeste tijd. 
#Als je meerdere aanvragen wilt doen, is een tweestapsmethode meer efficient.

#Stap 1: Laad de data (neemt het meeste tijd in beslag)
Consumentenprijsdata <- function() {
  consumentenprijs_data <- cbs_get_data("83131NED")
  consumentenprijs_data %>%
    select(Bestedingscategorieen, Perioden, CPI_1, CPIAfgeleid_2) %>%
    mutate(Perioden = as.Date(paste0(sub("MM", "", Perioden), "01"), format = "%Y%m%d")) %>%
    filter(!is.na(Perioden))
}
consumentenprijs_data1 <- Consumentenprijsdata()


#Stap 2: Kies "CPI012220" of "Frisdranken", "CPI073320" of "Internationale vluchten" om 
#meer meer effiecente en snelle data aanvragen te doen. 
Snelle_kwartaalmutatie_functie <- function(category, start_period, end_period) {
  category <- tolower(category)
  if (category %in% c("cpi012220", "Frisdranken", "frisdranken", "Frisdrank", "frisdrank")) {
    category <- "CPI012220"
  } else if (category %in% c("cpi073320", "Internationale vluchten", "internationale vluchten")) {
    category <- "CPI073320"
  } else {
    stop("Ongeldige categorie ingevoerd. Gebruik 'Frisdranken' of 'CPI012220', 'Internationale vluchten' of 'CPI073320'.")
  }
  
  start_date <- tryCatch({
    Data_input(start_period)
  }, error = function(e) {
    stop("Ongeldige startdatum ingevoerd. Controleer of de startdatum geldig is, of dat u een ondersteund datumformaat gebruikt: DD-MM-YYYY, YYYY-MM-DD, MM-YYYY, Q*YYYY. *=1,2,3,4")
  })
  
  end_date <- tryCatch({
    Data_input(end_period)
  }, error = function(e) {
    stop("Ongeldige einddatum ingevoerd. Controleer of de einddatum geldig is, of dat u een ondersteund datumformaat gebruikt: DD-MM-YYYY, YYYY-MM-DD, MM-YYYY, Q*YYYY. *=1,2,3,4")
  })
  
  dataset_start_date <- as.Date("1996-01-01")
  dataset_end_date <- Sys.Date()  # Neem aan dat de data maandelijks wordt bijgewerkt
  
  if (start_date < dataset_start_date) {
    warning("Uw gekozen startdatum valt buiten het bereik van de beschikbare data. Data is beschikbaar vanaf 1 januari 1996 en wordt maandelijks bijgewerkt.")
    start_date <- dataset_start_date
  }
  
  if (end_date > dataset_end_date) {
    warning("Uw gekozen einddatum valt buiten het bereik van de beschikbare data. Data is beschikbaar vanaf 1 januari 1996 en wordt maandelijks bijgewerkt.")
    end_date <- dataset_end_date
  }
  
  #Hier wederom zorgen we ervoor dat data ingedeeld worden in het juiste kwartiel.
  adjusted_start_date <- floor_date(start_date, "quarter")
  adjusted_end_date <- ceiling_date(end_date, "quarter") - days(1)
  previous_start_date <- adjusted_start_date %m-% months(3)
  
  #Zet de juiste tijdperiode en houd 
  filtered_data <- consumentenprijs_data1 %>%
    filter(Bestedingscategorieen == category & Perioden >= previous_start_date & Perioden <= adjusted_end_date)
  
  #Get the mutation: Mutation = ((New CPI/old CPI) - 1) * 100
  filtered_data <- filtered_data %>%
    mutate(Kwartaal = paste0(year(Perioden), " Q", quarter(Perioden))) %>%
    group_by(Bestedingscategorieen, Kwartaal) %>%
    summarise(
      CPI_1_avg = mean(CPI_1, na.rm = TRUE),
      CPIAfgeleid_2_avg = mean(CPIAfgeleid_2, na.rm = TRUE)
    ) %>%
    mutate(
      `Kwartaalmutatie CPI (%)` = (CPI_1_avg / lag(CPI_1_avg) - 1) * 100,
      `Kwartaalmutatie CPI, afgeleid (%)` = (CPIAfgeleid_2_avg / lag(CPIAfgeleid_2_avg) - 1) * 100
    ) %>%
    filter(Kwartaal >= paste0(year(adjusted_start_date), " Q", quarter(adjusted_start_date)) &
             Kwartaal <= paste0(year(adjusted_end_date), " Q", quarter(adjusted_end_date))) %>%
    select(Bestedingscategorieen, Kwartaal, `Kwartaalmutatie CPI (%)`, `Kwartaalmutatie CPI, afgeleid (%)`)
  
  return(filtered_data)
}





# Voorbeeld voor het gebruiken van de code, note we kunnen zowel namen gebruiken als codes. 
#Dus "CPI012220" / "Frisdranken" of "CPI073320" / "International flights"
Snelle_kwartaalmutatie_functie("Internationale vluchten", "2023-08-21", "2024-12-24")


--------------

  
  
  
#CASUS 1: R deel 3: Maak een aantal grafieken
  
  
  
  
  
#Grafiek 1: CPI vs Afgeleide CPI Frisdranken. U moet eerst de eerdere functies en hulpfuncties uitvoeren om deze code
#te kunnen laden.
# Laad data
consumentenprijs_data1 <- cbs_get_data("83131NED")

# Format de perioden naar een datum format. 
consumentenprijs_data <- consumentenprijs_data1 %>%
  mutate(Perioden = as.Date(paste0(sub("MM", "", Perioden), "01"), format = "%Y%m%d"))

# Verwijder NA rijen om de jaar rijen te verwijderen (we willen alleen maanden). 
consumentenprijs_data <- consumentenprijs_data %>%
  filter(!is.na(Perioden))


# Functie die de CPI per kwartaal berekent voor Frisdranken
get_quarterly_data_frisdranken <- function(start_period, end_period) {
  start_date <- Data_input(start_period)
  end_date <- Data_input(end_period)
  
  # Pas de start en einddate in een kwartiel.
  adjusted_start_date <- start_of_quarter(start_date)
  adjusted_end_date <- end_of_quarter(end_date)
  
  # Filter Frisdrank data
  frisdrank_data <- consumentenprijs_data %>%
    filter(Bestedingscategorieen == "CPI012220" & Perioden >= adjusted_start_date & Perioden <= adjusted_end_date)
  
  # Voeg een kwartiel kolom toe
  frisdrank_data <- frisdrank_data %>%
    mutate(Kwartaal = paste0(year(Perioden), " Q", quarter(Perioden)))
  
  # Bereken het kwartaal gemiddelde door per het gemiddelde CPI per drie maanden te berekenen.
  quarterly_data <- frisdrank_data %>%
    group_by(Kwartaal) %>%
    summarise(
      `Kwartaal CPI` = mean(CPI_1, na.rm = TRUE),
      `Kwartaal CPI afgeleid` = mean(CPIAfgeleid_2, na.rm = TRUE)
    )
  
  return(quarterly_data)
}

# Kwartaal data frisdranken gegeven een period (Deze functie is flexibel en lijkt op de functie die we eerder 
#hebben gemaakt voor de kwartaal mutatie)
kwartaal_data_frisdranken <- get_quarterly_data_frisdranken("2018-01-01", "2024-02-13")

# Reshape the data for plotting. Ggplot werkt makkelijker met long data, dus we transposen de CPI en CPI afgeleid kolommen.
kwartaal_data_frisdranken_long <- kwartaal_data_frisdranken %>%
  pivot_longer(cols = c(`Kwartaal CPI`, `Kwartaal CPI afgeleid`), names_to = "Indicator", values_to = "Value")

# Maak de x-as tekst kleiner (de kwartalen) als we ons datum bereik groot maken (leest makkelijker). 
x_text_size <- ifelse(length(unique(kwartaal_data_frisdranken_long$Kwartaal)) > 35, 8, 10)

# x-labels functie zorgt ervoor dat als we een groot datum bereik hebben dat we alleen de jaren op de x-as zetten.
#Wederom een aanpassing voor betere leesbaarheid.
x_labels <- function(x) {
  if (length(unique(x)) > 35) {
    sapply(seq_along(x), function(i) {
      if (i %% 4 == 1) {
        substr(x[i], 1, 4)
      } else {
        ""
      }
    })
  } else {
    sapply(seq_along(x), function(i) {
      if (i == 1 || grepl("Q1", x[i])) {
        x[i]
      } else {
        sub("^\\d{4} ", "", x[i])
      }
    })
  }
}

#De plot gemaakt met ggplot en ggthemes.
CPI_vs_CPIAfgeleid <- ggplot(kwartaal_data_frisdranken_long, 
                             aes(x = Kwartaal, y = Value, color = Indicator, shape = Indicator, group = Indicator)) +
  geom_line(linewidth = 1) +  
  geom_point(data = subset(kwartaal_data_frisdranken_long, Indicator == "Kwartaal CPI"), size = 3) +  #We gebruiken iets grotere puntjes dan driehoeken om eventuele overlappingen nog steeds duidelijk weer te geven.
  geom_point(data = subset(kwartaal_data_frisdranken_long, Indicator == "Kwartaal CPI afgeleid"), size = 2.5) +  
  labs(title = "CPI en CPI afgeleid per kwartaal voor Frisdranken",
       subtitle = "Ontwikkeling prijspeil frisdranken: Kwartaal CPI versus kwartaal CPI afgeleid.",
       x = "Kwartalen",
       y = "CPI waarde") +
  scale_color_manual(values = c("Kwartaal CPI" = "#1f77b4", "Kwartaal CPI afgeleid" = "#ff7f0e")) +
  scale_shape_manual(values = c("Kwartaal CPI" = 16, "Kwartaal CPI afgeleid" = 17)) +  #16 is een circle, 17 een driehoek. Zo houden we de lijnen nog beter uit elkaar. 
  scale_x_discrete(labels = x_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size, vjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, margin = margin(r = 20)), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
    panel.grid.minor = element_blank(),  
  )

print(CPI_vs_CPIAfgeleid)
# Sla het plaatje op als een png bestand.
#ggsave("CPI_vs_CPIAfgeleid.png", plot = CPI_vs_CPIAfgeleid, width = 10, height = 6)

# Sla het png bestand op in de database.
#save_image_to_db(con, "plots", "CPI_vs_CPIAfgeleid.png", "CPI and CPI afgeleid per kwartaal for Frisdranken")









#Grafiek 2: CPI mutatie per kwartaal voor Frisdranken. U eerst moet de Snelle_kwartaalmutatie_functie
#en alle hulpfuncties uitvoeren. 
mutatie_data_frisdranken <- Snelle_kwartaalmutatie_functie("Frisdranken","2015-01-01", "2023-12-31")

# Data 'long' maken
mutatie_data_frisdranken_long <- mutatie_data_frisdranken %>%
  pivot_longer(cols = c(`Kwartaalmutatie CPI (%)`), names_to = "Indicator", values_to = "Value")

# Vergelijkbare x_labels en x_text_size functie als eerder, maar met kleine aanpassingen
x_text_size <- ifelse(length(unique(mutatie_data_frisdranken_long$Kwartaal)) > 20, 8, 10)

x_labels <- function(x) {
  if (length(unique(x)) > 35) {
    sapply(seq_along(x), function(i) {
      if (i %% 4 == 1) {
        substr(x[i], 1, 4)
      } else {
        ""
      }
    })
  } else {
    sapply(seq_along(x), function(i) {
      if (i == 1 || grepl("Q1", x[i])) {
        x[i]
      } else {
        sub("^\\d{4} ", "", x[i])
      }
    })
  }
}

# Plot the CPI mutatie per kwartaal voor Frisdranken
CPI_kwartaalmutatie_frisdrank <- ggplot(mutatie_data_frisdranken_long, 
       aes(x = Kwartaal, y = Value, color = Indicator, shape = Indicator, group = Indicator)) +
  geom_line(size = 1) + 
  geom_point(data = subset(mutatie_data_frisdranken_long, Indicator == "Kwartaalmutatie CPI (%)"), size = 2) + 
  labs(title = "Mutatie CPI per kwartaal voor Frisdranken",
       subtitle = "Kwartaalmutatie CPI voor Frisdranken.",
       x = "Kwartalen",
       y = "Mutatie CPI (%)",
       color = "Indicator",
       shape = "Indicator") +
  scale_color_manual(values = c("Kwartaalmutatie CPI (%)" = "#1f77b4")) +
  scale_shape_manual(values = c("Kwartaalmutatie CPI (%)" = 16)) +  
  scale_x_discrete(labels = x_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size, vjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, margin = margin(r = 20)),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
    panel.grid.minor = element_blank(),  
  )

print(CPI_kwartaalmutatie_frisdrank)

# Sla het plaatje op in de PostgreSQL database
#ggsave("CPI_kwartaalmutatie_frisdrank.png", plot = CPI_kwartaalmutatie_frisdrank, width = 10, height = 6)
#save_image_to_db(con, "plots", "CPI_kwartaalmutatie_frisdrank.png", "Kwartaal CPI mutatie voor Frisdranken")






#Grafiek 3: Mutatie Internationale vluchten.
# De code volgt dezelfde stappen als grafiek 2.
mutatie_data_vluchten <- Snelle_kwartaalmutatie_functie("Internationale vluchten", "2015-01-01", "2023-12-31")

mutatie_data_vluchten_long <- mutatie_data_vluchten %>%
  pivot_longer(cols = c(`Kwartaalmutatie CPI (%)`), names_to = "Indicator", values_to = "Value")

x_text_size <- ifelse(length(unique(mutatie_data_vluchten_long$Kwartaal)) > 20, 8, 10)

x_labels <- function(x) {
  if (length(unique(x)) > 35) {
    sapply(seq_along(x), function(i) {
      if (i %% 4 == 1) {
        substr(x[i], 1, 4)
      } else {
        ""
      }
    })
  } else {
    sapply(seq_along(x), function(i) {
      if (i == 1 || grepl("Q1", x[i])) {
        x[i]
      } else {
        sub("^\\d{4} ", "", x[i])
      }
    })
  }
}

CPI_kwartaalmutatie_internationale_vluchten <- ggplot(mutatie_data_vluchten_long, 
       aes(x = Kwartaal, y = Value, color = Indicator, shape = Indicator, group = Indicator)) +
  geom_line(size = 1) + 
  geom_point(data = subset(mutatie_data_vluchten_long, Indicator == "Kwartaalmutatie CPI (%)"), size = 2) +  
  labs(title = "Mutatie CPI per kwartaal voor Internationale vluchten",
       subtitle = "Kwartaalmutatie CPI voor Internationale vluchten.",
       x = "Kwartalen",
       y = "Mutatie (%)",
       color = "Indicator",
       shape = "Indicator") +
  scale_color_manual(values = c("Kwartaalmutatie CPI (%)" = "#1f77b4")) +
  scale_shape_manual(values = c("Kwartaalmutatie CPI (%)" = 16)) + 
  scale_x_discrete(labels = x_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size, vjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, margin = margin(r = 20)),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
    panel.grid.minor = element_blank(),  
  )

print(CPI_kwartaalmutatie_internationale_vluchten)

#Om plaatje te saven naar database
#ggsave("CPI_kwartaalmutatie_internationale_vluchten.png", plot = CPI_kwartaalmutatie_internationale_vluchten, width = 10, height = 6)
#save_image_to_db(con, "plots", "CPI_kwartaalmutatie_internationale_vluchten.png", "Kwartaal CPI mutatie voor internationale vluchten")







#Grafiek 4: CPI Frisdranken vs CPI internationale vluchten
# Functie om data per kwartiel te krijgen. Volgt dezelfde structuur als de mutatie functies eerder.
kwartaal_data_algemeen <- function(start_period, end_period, category) {
  start_date <- Data_input(start_period)
  end_date <- Data_input(end_period)
  
  # Adjust start and end dates to quarters
  adjusted_start_date <- start_of_quarter(start_date)
  adjusted_end_date <- end_of_quarter(end_date)
  
  # Filter data for the given category
  category_data <- consumentenprijs_data %>%
    filter(Bestedingscategorieen == category & Perioden >= adjusted_start_date & Perioden <= adjusted_end_date)
  
  # Add a quarter column
  category_data <- category_data %>%
    mutate(Kwartaal = paste0(year(Perioden), " Q", quarter(Perioden)))
  
  # Calculate quarterly averages
  quarterly_data <- category_data %>%
    group_by(Kwartaal) %>%
    summarise(
      `Kwartaal CPI` = mean(CPI_1, na.rm = TRUE),
      Bestedingscategorieen = first(Bestedingscategorieen)
    )
  
  return(quarterly_data)
}

# Hier pakken we data voor frisdranken en vluchten voor een periode, gegeven als input. (We gebruiken de engelse naam om
#te differentieren. We hebben kwartaal_data_frisdranken al eerder gebruik bijvoorbeeld)
kwartaal_data_frisdranken1 <- kwartaal_data_algemeen("2012-01-01", "2023-12-31", "CPI012220")
kwartaal_data_vluchten1 <- kwartaal_data_algemeen("2012-01-01", "2023-12-31", "CPI073320")

# Combineer de datasets om beide producten tegelijk te kunnen plotten.
kwartaal_data_gecombineerd <- bind_rows(kwartaal_data_frisdranken1, kwartaal_data_vluchten1)

# We geven de code de bijpassende naam, dat leest fijner in de plot.
kwartaal_data_gecombineerd$Bestedingscategorieen <- recode(kwartaal_data_gecombineerd$Bestedingscategorieen,
                                                        "CPI012220" = "Frisdranken",
                                                        "CPI073320" = "Internationale vluchten")

# Data weer long maken.
kwartaal_data_gecombineerd_long <- kwartaal_data_gecombineerd %>%
  pivot_longer(cols = c(`Kwartaal CPI`), names_to = "Indicator", values_to = "Value")


x_text_size <- ifelse(length(unique(kwartaal_data_gecombineerd_long$Kwartaal)) > 20, 8, 10)

x_labels <- function(x) {
  if (length(unique(x)) > 35) {
    sapply(seq_along(x), function(i) {
      if (i %% 4 == 1) {
        substr(x[i], 1, 4)
      } else {
        ""
      }
    })
  } else {
    sapply(seq_along(x), function(i) {
      if (i == 1 || grepl("Q1", x[i])) {
        x[i]
      } else {
        sub("^\\d{4} ", "", x[i])
      }
    })
  }
}

# Plot Kwartaal CPI voor Frisdranken tegenover internationale vluchten. 
#Merk het verschil op tussen de twee producten. De een (frisdrank), een alledaags product, is stabiel en licht stijgend.
#Internationale vluchten, echter, bevatten veel volatiliteit.

CPI_frisdranken_vs_internationale_vluchten <- ggplot(kwartaal_data_gecombineerd_long, 
       aes(x = Kwartaal, y = Value, color = Bestedingscategorieen, shape = Bestedingscategorieen, group = Bestedingscategorieen)) +
  geom_line(size = 1) + 
  geom_point(data = subset(kwartaal_data_gecombineerd_long, Bestedingscategorieen == "Frisdranken"), size = 3) + 
  geom_point(data = subset(kwartaal_data_gecombineerd_long, Bestedingscategorieen == "Internationale vluchten"), size = 2.5) +  
  labs(title = "CPI per kwartaal voor Frisdranken en Internationale Vluchten",
       subtitle = "Ontwikkeling prijspeil: Kwartaal CPI",
       x = "Kwartalen",
       y = "CPI waarde",
       color = "CPI per kwartaal",
       shape = "CPI per kwartaal") +
  scale_color_manual(values = c("Frisdranken" = "#1f77b4", "Internationale vluchten" = "#ff7f0e")) +
  scale_shape_manual(values = c("Frisdranken" = 16, "Internationale vluchten" = 17)) +  
  scale_x_discrete(labels = x_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size, vjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, margin = margin(r = 20)),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
    panel.grid.minor = element_blank(), 
  )


print(CPI_frisdranken_vs_internationale_vluchten)

#ggsave("CPI_frisdranken_vs_internationale_vluchten.png", plot = CPI_frisdranken_vs_internationale_vluchten, width = 10, height = 6)
#save_image_to_db(con, "plots", "CPI_frisdranken_vs_internationale_vluchten.png", "CPI_frisdranken_vs_internationale_vluchten")



--------------

#Appendix
#Haal een plaatje uit de SQL database
# Functie om een afbeelding uit de database te halen en op te slaan
fetch_image_from_db <- function(con, table_name, description, output_path) {
  query <- sprintf("SELECT img FROM %s WHERE description = '%s'", table_name, description)
  result <- dbGetQuery(con, query)
  if (nrow(result) > 0) {
    img_data <- result$img[[1]]
    writeBin(img_data, output_path)
    cat("Afbeelding opgeslagen naar:", output_path, "\n")
  } else {
    cat("Geen afbeelding gevonden met de gegeven beschrijving.\n")
  }
}


# Voorbeeld gebruik
fetch_image_from_db(con, "public.plots", "CPI and CPI afgeleid per kwartaal for Frisdranken", "output_image.png")
