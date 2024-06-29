# IT-casus voor CBS data engineer macro-economie
Gemaakt door: Sietse van Meer

# R proces automatisering om de Consumenten Prijs Index (CPI) mutatie per kwartaal te berekenen, gegeven een verslagperiode. 
Deze repository bevat de R-code voor een data-engineering casus., waarmee we Consumenten Prijs Index (CPI) mutaties per kwartaal kunnen berekenen. 
Daarnaast maken we ook een aantal bijbehorende grafieken en is er code om verbinding te maken met een SQL database. 

## Bestanden in repository `IT-case'
- **IT-casus data engineer2.R**, het hoofdbestand voor de berekeningen en het maken van de grafieken. De grafieken zijn ook toegevoegd in de repository, maar kunnen door het runnen van de code ook opnieuw worden gemaakt.
- **CPI_vs_CPIAfgeleid.png**, grafiek 1 in `IT-casus data engineer2.R'
- **CPI_kwartaalmutatie_frisdrank.png**, grafiek 2 in `IT-casus data engineer2.R'
- **CPI_kwartaalmutatie_internationale_vluchten.png**, grafiek 3 in `IT-casus data engineer2.R'
- **CPI_frisdranken_vs_internationale_vluchten.png**, grafiek 4 in `IT-casus data engineer2.R' 

## Algemene functies van het script: `IT-casus data engineer2.R'
- **Gegevens Ophalen**: Haal gegevens rechtstreeks op van het CBS (Centraal Bureau voor de Statistiek) met behulp van het pakket `cbsodataR`.
- **Berekening van Kwartaalmutaties**: Bereken de kwartaalveranderingen in de CPI voor frisdranken en internationale vluchten.
- **Data Visualisatie**: Genereer plots om trends in de CPI van frisdranken en internationale vluchten te ontdekken.
- **Database-integratie**: Sla afbeeldingen van de plots op in een PostgreSQL-database en haal deze op.
- **Foutafhandeling**: Robuuste foutcontrole voor datuminput en duidelijke visualisaties. 

## Installation

### Vereisten software
- R (version 4.0.0 or later)
- RStudio (recommended for ease of use)
- SQL database (Ik gebruik combinatie van PostgreSQL en PgAdmin4)

## Vereisten libraries

Om de R-scripts uit te voeren, moet je de volgende pakketten geïnstalleerd hebben:

- `ggplot2`
- `dplyr`
- `lubridate`
- `tidyr`
- `ggthemes`
- `DBI`
- `RPostgres`
- `cbsodataR`
- `ggsave`

### Database Setup
Als je de PostgreSQL connectie wilt leggen, dan moet je deze software hebben gedownload. In R is de connectie gelegd met de code: 

```R
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "cbs_data", 
                 host = "localhost", 
                 port = 5432, 
                 user = "CBS Data engineer", 
                 password = "CBSData1234")
```

## Gebruiksaanwijzing "IT-casus data engineer.R"

## 1. **Voorbereiding**
Laad de libraries en installeer eventuele libraries die ontbreken (code hiervoor staat gecomment)

## 2. **Databaseconnectie**
Indien gewenst (optioneel), laad de PostreSQL verbinding om plaatjes naar te exporteren. Merk op dat je hier je eigen dbname, host, port, user, en password nodig hebt en dat het om een lokale opslagmethode gaat. Stap 2 is optioneel.

## 3. **Hulpfuncties**
Voer de hulpfuncties uit. Deze functies zijn nodige in de rest van de code. Ze zorgen ervoor dat we met dataformaten om kunnen gaan, dat we kwartalen kunnen identificeren en dat we plaatjes kunnen opslaan.

## 4. **Automatiseren van het proces**
Voer de snelle methode uit in twee stappen. In de eerste stap gebruiken we API. Dit kost het meeste tijd, daarom proberen we deze `call' zo min mogelijk te maken. In stap 2 volgt de functie en de automatisering van het proces. De onderstaande code is het eindresultaat en levert de CPI kwartaalmutatie op.

### Input
De input voor de snelle methode omvat de keuze van het product ("CPI012220" / "Frisdranken" of "CPI073320" / "Internationale vluchten") en de verslagperiode. Datumformaten die ondersteund worden zijn: DD-MM-YYYY, YYYY-MM-DD, MM-YYYY, Q*YYYY (* = 1,2,3,4).

### Throughput
De functie `Snelle_kwartaalmutatie_functie` wordt gebruikt om de maandelijkse data om te zetten in kwartaaldata. Deze kwartaaldata worden vervolgens gemiddeld om een CPI per kwartaal te berekenen. De mutatie van het CPI per kwartaal wordt berekend door de formule: ((Nieuw CPI / Oud CPI) - 1) * 100, om een percentage te krijgen, vergelijkbaar met jaarmutaties.

### Output
De output van de functie is een tabel met de volgende kolommen: Bestedingscategorieën, Kwartaal, Mutatie CPI (%) en Mutatie CPI Afgeleid (%).

```{r}
Snelle_kwartaalmutatie_functie("Internationale vluchten", "2023-08-21", "2024-12-24")

#Extra voorbeelden die niet in het R document staan
Snelle_kwartaalmutatie_functie("Frisdranken", "Q12019", "Q42023")
Snelle_kwartaalmutatie_functie("CPI012220", "01-2014", "07-2019")
```

| Bestedingscategorieen | Kwartaal | Kwartaalmutatie CPI (%) | Kwartaalmutatie CPI, afgeleid (%) |
|-----------------------|----------|-------------------------|-----------------------------------|
| CPI073320             | 2023 Q3  | -0.910                  | -0.961                            |
| CPI073320             | 2023 Q4  | -17.2                   | -18.2                             |
| CPI073320             | 2024 Q1  | -6.87                   | -8.21                             |
| CPI073320             | 2024 Q2  | 24.4                    | 24.6                              |



## 5. Visualisatie

Na het uitvoeren van de voorgaande stappen, kun je beginnen met het maken van grafieken. Voor grafieken 1 en 4 heb je stappen 1 tot 3 nodig en voor grafieken 2 en 3 heb je stappen 1 tot 4 nodig. 
- **Grafiek 1**: CPI (%) vs CPI Afgeleide (%) voor Frisdranken
- **Grafiek 2**: Kwartaalmutatie CPI voor Frisdranken
- **Grafiek 3**: Kwartaalmutatie CPI voor Internationale Vluchten
- **Grafiek 4**: CPI Frisdranken versus Internationale Vluchten

De uiteindelijke plaatjes zijn ook toegevoegd aan de github maar kunnen ook bekeken worden als je de code runt.

## 6. Visualisatie - Appendix
Optioneel is de appendix, waarin een functie staat om plaatjes ook weer uit de SQL database te halen.


## Contactinformatie

Voor vragen of opmerkingen over dit project kunt u contact opnemen via:

- **E-mail**: s.l.j.vanmeer@tilburguniversity.edu
