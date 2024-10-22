# African bat database

Database, scripts for area of occurence and code for Shinyapp associated with the publication:

African bat database: a curated database of occurrence records, distribution models and EOO and AOO conservation metrics for the bats of Sub-Saharan Africa
Ara Monadjem, Cecilia Montauban, Paul Webala, Theresa Laverty, Eric Bakwo-Fils, Laura Torrent, Iroro Tanshi, Adam Kane, David Waldien and Peter Taylor

## Database 
Metadata for the database: 
* Family: family of bat record; 
* Genus: genus of bat record; 
* Species: species of bat record; 
* Museum_number: the museum accession number of the bat record; 
* Date: the date on which the bat was recorded; 
* Year: the year in which the bat was recorded; 
* Country: the country in which the bat was recorded; 
* Location: the name of the location or locality that the bat was recorded; 
* Latitude: the latitude in decimal degrees of the record; 
* Longitude: the longitude in decimal degrees of the record; 
* Reference: the source of the record; 
* Holotype: whether the record is a type specimen; 
* Checked: whether the bat specimen was examined by the authors.

## Scripts  
* 1_african bats_maxent models_all_species.R provides code to run maxent models for each African bat species
* 2_aoo-eoo.R script to calculate EOO (extent of occurrence) and AOO (area of occupancy) for African bats
* Bat_dist_shiny.R script to generate shinyapp displaying searchable map of Africa for species occurrence records in the database 

## Shinyapp
Shinyapp is available at https://adam-kane.shinyapps.io/african_bat_dist/
