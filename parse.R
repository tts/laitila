library(readxl)
library(tidyverse)
library(pxweb)
library(geofi)
library(sf)
library(patchwork)
library(ggsankey)

#-----------------------------------------------------------
# Number of people by municipality in 2023, born in Laitila
# 
# Data licensed from Tilastokeskus 
#-----------------------------------------------------------

data <- read_excel("24PIENEP_418.xlsx", skip = 2)
laitila <- data %>% 
  filter(Kuntakoodi != "SSS") %>% 
  rename(laitilasta = `2023`)

#------ Open data on municipalities, population 2023

d <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")
data_all <- d$data

data_all <- data_all %>% 
  rename(Kuntanimi = Alue,
         kuntalaisia = `Väestö 31.12.`) %>% 
  mutate(Kuntanimi = ifelse(Kuntanimi == "Maarianhamina - Mariehamn", "Maarianhamina", Kuntanimi)) %>% 
  select(Kuntanimi, kuntalaisia)

saveRDS(data_all, "vaesto_kunnittain_2023.RDS")

data_all_l <- left_join(data_all, laitila)

share <- data_all_l %>% 
  mutate(laitilasta = as.numeric(as.character(laitilasta)),
         kuntalaisia = as.numeric(as.character(kuntalaisia)),
         laitilalaistenosuus = round(100 * (laitilasta / kuntalaisia), digits = 2),
         Kuntakoodi = gsub("^0+", "", Kuntakoodi)) %>% 
  filter(Kuntanimi != "KOKO MAA")

#------ Municipalities by geofi

d1 <- get_municipalities(year = 2023)
saveRDS(d1, "kunnat.RDS")

kunnat_geom <- d1 %>% 
  select(kunta, nimi, geom) %>% 
  rename(Kuntakoodi = kunta) %>% 
  mutate(Kuntakoodi = as.character(Kuntakoodi))

share_geom <- kunnat_geom %>%
  right_join(share) %>% 
  filter(!is.na(laitilasta))

share_geom$Osuus <- cut(
  share_geom$laitilalaistenosuus,
  breaks = c(0.0, 0.50, 1, 10, 60), 
  include.lowest = TRUE,
  dig.lab=4
)
share_geom$Lkm <- cut(
  share_geom$laitilasta,
  breaks = c(1, 10, 50, 500, 1000, 5000),
  include.lowest = TRUE,
  dig.lab = 4
)

saveRDS(share_geom, "laitila_muualla.RDS")

do_plot <- function(fill, title) {
  ggplot() +
    geom_sf(data = d1) +
    geom_sf(data = share_geom, 
            aes(fill = !!sym(fill))) +
    scale_fill_viridis_d(option = "inferno", direction = -1) +
    guides(fill = guide_legend(title = title)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank())
}

m1 <- do_plot("Lkm", "Lukumäärä")
m2 <- do_plot("Osuus", "Osuus väestöstä %")

(m1 | m2) + 
  plot_layout(nrow = 1) +
  plot_annotation(title = "Laitilassa syntyneiden asuinkunta vuonna 2023",
                  caption = "Tilastokeskus | geofi: Access Finnish Geospatial Data | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5)))

ggsave("Laitilassa_syntyneet.png", dpi = 600, height = 12, width = 20, units = "cm")

#---------------------------------------
# Intermunicipal migration 1990-2023
#---------------------------------------

#----- by area of departure

migr_d <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")

migr_d_data <- migr_d$data

migr_d_data <- migr_d_data %>% 
  filter(`Kuntien välinen muutto` != 0) %>% 
  mutate(Lähtöalue = "Laitila",
         Tuloalue = gsub("Tulo - ", "", Tuloalue))

saveRDS(migr_d_data, "lahteneet_laitilasta.RDS")

#------ by area of arrival 

migr_a <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")

migr_a_data <- migr_a$data

migr_a_data <- migr_a_data %>% 
  filter(`Kuntien välinen muutto` != 0,
         Lähtöalue != "KOKO MAA") %>% 
  mutate(Tuloalue = "Laitila",
         Lähtöalue = gsub("Lähtö - ", "", Lähtöalue))

saveRDS(migr_a_data, "saapuneet_laitilaan.RDS")


#------ Add maakunta

migr_d_m_data <- left_join(migr_d_data, d1, by = c("Tuloalue" = "nimi"))
migr_a_m_data <- left_join(migr_a_data, d1, by = c("Lähtöalue" = "nimi"))

migr_d_m_data <- migr_d_m_data %>% 
  select(Tuloalue, Lähtöalue, Sukupuoli, Vuosi, `Kuntien välinen muutto`, maakunta_name_fi) %>% 
  rename(Maakunta = maakunta_name_fi, Kunta = Tuloalue)
migr_a_m_data <- migr_a_m_data %>% 
  select(Tuloalue, Lähtöalue, Sukupuoli, Vuosi, `Kuntien välinen muutto`, maakunta_name_fi) %>% 
  rename(Maakunta = maakunta_name_fi, Kunta = Lähtöalue)

#---- Departure

before2000 <- migr_d_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi < 2000) 

before2010 <- migr_d_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi >= 2000 & Vuosi < 2010) 

after2010 <- migr_d_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi >= 2010) 

#----- Arrival

before2000_a <- migr_a_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi < 2000) 

before2010_a <- migr_a_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi >= 2000 & Vuosi < 2010) 

after2010_a <- migr_a_m_data %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(`Kuntien välinen muutto` >= 10,
         Vuosi >= 2010) 

#---- Plot departure

do_sankey <- function(df) {
  df_long <- df %>% 
    make_long(Vuosi, Maakunta, Kunta) 
  
  s <- ggplot(df_long, 
              aes(x = x, next_x = next_x, 
                  node = node, next_node = next_node, 
                  fill = factor(node), label = node)) +
    geom_sankey(flow.alpha = .6,
                node.color = "gray30") +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    scale_fill_viridis_d(drop = FALSE) +
    theme_void(base_size = 30) +
    labs(x = NULL) +
    theme(legend.position = "none",
          plot.caption = element_text(vjust = .4, size = 3)) 

  return(s)
}

s1 <- do_sankey(before2000)
s2 <- do_sankey(before2010)
s3 <- do_sankey(after2010)

(s1 | s2 | s3) + 
  plot_layout(nrow = 1) +
  plot_annotation(title = "Laitilasta muualle muuttaneet",
                  subtitle = "Vähintään 10 henkilöä",
                  caption = "Tilastokeskus | rOpenGov/pxweb | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5),
                                plot.subtitle = element_text(size = 10, hjust = .5)))

ggsave("Laitilasta_muuttaneet.png", dpi = 600, height = 12, width = 20, units = "cm")

#---- Plot arrival

do_sankey_a <- function(df) {
  df_long <- df %>% 
    make_long(Kunta, Maakunta, Vuosi) 
  
  s <- ggplot(df_long, 
              aes(x = x, next_x = next_x, 
                  node = node, next_node = next_node, 
                  fill = factor(node), label = node)) +
    geom_sankey(flow.alpha = .6,
                node.color = "gray30") +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    scale_fill_viridis_d(drop = FALSE) +
    theme_void(base_size = 30) +
    labs(x = NULL) +
    theme(legend.position = "none",
          plot.caption = element_text(vjust = .4, size = 3)) 
  
  return(s)
}

s4 <- do_sankey_a(before2000_a)
s5 <- do_sankey_a(before2010_a)
s6 <- do_sankey_a(after2010_a)

(s4 | s5 | s6) + 
  plot_layout(nrow = 1) +
  plot_annotation(title = "Laitilaan muualta muuttaneet",
                  subtitle = "Vähintään 10 henkilöä",
                  caption = "Tilastokeskus | rOpenGov/pxweb | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5),
                                plot.subtitle = element_text(size = 10, hjust = .5)))


ggsave("Laitilaan_muuttaneet.png", dpi = 600, height = 12, width = 20, units = "cm")

