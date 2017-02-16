
# Packages ----------------------------------------------------------------
library(shiny)
library(drc)
library(ggplot2)
library(shinyjs)
library(esmisc)
library(plotly)


# drc models --------------------------------------------------------------
mods <- getMeanFunctions(display = FALSE)
select_mod <- sapply(mods, '[', 1)
names(select_mod) <- paste0(sapply(mods, '[', 1), 
                            ': ',
                            sapply(mods, '[', 2))



# parse textInput ---------------------------------------------------------
numextractall <- function(string) { 
  # http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
  as.numeric(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)), 
                    use.names = FALSE))
} 
