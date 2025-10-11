###########################################################################
#                        PROBLEM SET 1
#                            Equipo 3       
#  Autores:   Maria Paula Basto - Lucas Daniel Carrillo Aguirre 
#            Catalina Leal      -  Lucas Eduardo Vera Costa
#                     Punto 2 - Web Scrapping                                 
############################################################################

rm(list = ls()) # Vacía environment


# directorio de trabajo

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 

# Paquetes -----------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  rvest,
  httr,
  fs,
  janitor,
  readr,
  haven,
  readxl,
  glue,
  chromote,
  xml2,
  tibble,
  purrr,
  VIM,
  gtsummary,
  gt,
  scales
)

## 1) Parámetros--------------------------------------------------------

#Se carga la base original y se enumeran, del 1 al 10, las páginas subsiguientes para cada chunk

url_or   <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/"
paginas <- tibble::tibble(
  chunk = 1:10,
  url   = paste0(url_or, "geih_page_", chunk, ".html")
)

## 2) Se extraen las tablas de formato HTML -----------------------

funciontabla <- function(url_tabla, chunk){
  pg   <- read_html(url_tabla)
  nodo <- html_node(pg, "table")
  if (is.null(nodo)) return(tibble())
  
  tb <- html_table(nodo, fill = TRUE)
  
  tb <- tibble::as_tibble(tb, .name_repair = "unique") %>%
    janitor::clean_names()
  
  tb %>%
    dplyr::mutate(origen = url_tabla, chunk = chunk) %>%
    dplyr::relocate(chunk, origen)
}

## 3) Aplicar a todas las páginas ----------------------------------
data_final <- purrr::map2_dfr(paginas$url, paginas$chunk, funciontabla)


# 4) Guardar --------------------------------------------------------

write_csv(data_final, "stores/datos_extraidos.csv")
