## ---------------------------
##
## Script name: Functions_for_Palmer_Penguins.r
##
## Purpose of script: 
##      Functions for cleaning and filtering data for analysis
##
## Author: Anon
##
## Date Created: 2023-11-11
##
##----------------------------


## A function to make sure the column names are cleaned up

clean_column_names <- function(penguins_data) {

  penguins_data %>%
    clean_names()
}

## A function to make sure the species names are shortened

shorten_species <- function(penguins_data) {
  
  penguins_data %>%
    
    #Thes lines will alter he species names to a specific name when it recognizes the name we call after "~"
    mutate(species = case_when( 
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

## A function to remove any empty columns or rows

remove_empty_columns_rows <- function(penguins_data) {
  
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}

## Function to remove individuals with NA sex

Remove_NAsex <- function(penguins_data) {
  
  penguins_data %>%
    filter(!is.na(sex))
}

## A function to subset the penguins data set to only Gentoo data (or whichever species is called in "species_selected")

filter_SelectedSpecies_only <- function(penguins_data, species_selected) {
  
  penguins_data %>%
    filter(species == species_selected)
}



