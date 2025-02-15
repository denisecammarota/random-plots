library(tidyverse)
library(ggplot2)
library(readxl)
library(geobr)
library(ggthemes)
library(viridis)

setwd('C:/Users/denis/Documents/random-stuff')
df <- read_excel('table.xlsx')
colnames(df) <- c('ID_MUNICIP', 'p')
df <- df %>% mutate(ID_MUNICIP = tolower(ID_MUNICIP))

map <- read_municipality(year = 2020)
map <- map %>% mutate(name_muni_2 = paste0(name_muni, ' (', abbrev_state, ')'))
map <- map %>% mutate(name_muni_2 = tolower(name_muni_2))
df <- df %>% mutate(ID_MUNICIP = ifelse(ID_MUNICIP == 'santo antônio de leverger (mt)',
                                        'santo antônio do leverger (mt)', ID_MUNICIP))

map_2 <- map %>% left_join(df, by = join_by(name_muni_2 == ID_MUNICIP))
map_2 <- map_2 %>% mutate(p = as.numeric(p))
map_2[is.na(map_2)] <- 0
map_2 <- map_2 %>% mutate(p_fact = factor(case_when(
  p < 0.25 ~ '< 0.25',
  p < 0.5 ~ '< 0.5',
  p < 0.75 ~ '< 0.75',
  p < 1.0 ~ '< 1.0',
  p < 1.25 ~ '< 1.25',
  p < 1.5 ~ '< 1.5',
  p >= 1.5 ~ '> 1.5'
  )
))

rainbow_palette <- rainbow(7)

ggplot(map_2) + geom_sf(aes(fill = p_fact), color = NA) + theme_map() + labs(fill = '') +
 labs(title ='Percentual de domicílios com responsável \n e cônjuge de mesmo sexo',
      caption = 'Fonte: IBGE (Censo 2022)') + 
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = rainbow_palette)
  
