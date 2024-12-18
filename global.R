
# Carga de librerías ------------------------------------------------------
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(openxlsx)
library(janitor)
library(scales)
library(summaryBox)
library(shinycssloaders)
library(lubridate)
library(ggthemes)
library(leaflet)
library(viridis)
library(DT)
library(tidyverse)
library(ggrepel)
library(stringi)
library(sf)
library(flextable)
library(forcats)
library(shinyBS)

# Fecha --------------------------------------------------------------------
fecha = year(Sys.Date())

# Lectura de datos ---------------------------------------------------------
# load("www/Colombia_M.RData")
# Data_Primer_Curso <- arrow::read_parquet("www/Data_Primer_Curso.parquet")
Data_Primer_Curso <- read.xlsx("Data_Primiparos_GRAFICOS - PROGRAMAS.xlsx", sheet = "Sheet 1")


#Depuración ----------------------------------------------------------------

Data_Primer_Curso <- Data_Primer_Curso %>% 
  mutate(METODOLOGIA = ifelse(is.na(METODOLOGIA), MODALIDAD, METODOLOGIA)) %>%
  mutate(METODOLOGIA = toupper(METODOLOGIA)) %>% 
  mutate(METODOLOGIA = ifelse(METODOLOGIA == "VIRTUAL-A DISTANCIA", 
                              "VIRTUAL", METODOLOGIA)) %>% 
  mutate(METODOLOGIA = ifelse(METODOLOGIA == "DUAL", 
                              "PRESENCIAL", METODOLOGIA))

# Tabla de modalidades pregrado ---------------------------------------------
MODALIDADES_TABLA <- Data_Primer_Curso %>% 
  filter(ANO >= fecha - 5) %>%
  filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
  # filter(NIVEL_ACADEMICO == "PREGRADO") %>%
  group_by(ANO, METODOLOGIA, NIVEL_ACADEMICO) %>% 
  summarise(Nacional = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)) %>% 
  left_join(
    Data_Primer_Curso %>%
      filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
      filter(ANO >= fecha - 5) %>%
      # filter(NIVEL_ACADEMICO == "PREGRADO") %>%
      filter(MUNICIPIO_DE_OFERTA_DEL_PROGRAMA == "BOGOTA, D.C.") %>% 
      group_by(ANO, METODOLOGIA, NIVEL_ACADEMICO) %>%
      summarise(Bogotá = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
    by = c("ANO", "METODOLOGIA", "NIVEL_ACADEMICO")
  ) %>% 
  left_join(
    Data_Primer_Curso %>%
      filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
      filter(ANO >= fecha - 5) %>%
      # filter(NIVEL_ACADEMICO == "PREGRADO") %>%
      filter(INSTITUCION_DE_EDUCACION_SUPERIOR_IES == "UNIVERSIDAD PEDAGOGICA NACIONAL") %>%
      group_by(ANO, METODOLOGIA, NIVEL_ACADEMICO) %>%
      summarise(UPN = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
    by = c("ANO", "METODOLOGIA" , "NIVEL_ACADEMICO")
  ) %>%
  mutate(MODALIDAD = ifelse( METODOLOGIA == "DUAL" | METODOLOGIA == "PRESENCIAL", "PRESENCIAL", "OTRAS MODALIDADES")) %>%
  pivot_longer(cols = c("Nacional", "Bogotá", "UPN"),
               names_to = "CATEGORIA",
               values_to = "n")


