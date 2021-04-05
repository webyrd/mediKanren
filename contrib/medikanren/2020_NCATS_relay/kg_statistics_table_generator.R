### LIBRARIES
library(gt) 
library(gtsummary)
library(lubridate)
library(tidyverse)
library(stringr)
library(readr)

co_occur_kg <- read_tsv(
  # tsv file-path-here
)

co_occur_kg <- co_occur_kg %>%
  arrange(desc(`edge count`)) %>%
  mutate(`Subject CURIE Prefix` = `subject CURIE prefix`, 
         `Object CURIE Prefix` = `object CURIE prefix`, 
         `Predicate` = predicate, 
         `Edge Count` = `edge count`) %>% 
  select(`Subject CURIE Prefix`:`Edge Count`)
  

co_occur_kg %>%
  gt() %>%
  tab_header(title = "Textmining Provider Co-occurrence Knowledge Graph Statistics") %>%
  gtsave(
    # file-path-here, extension .pdf
  )



