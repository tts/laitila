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


#------ Add maakunta (although not used here)

migr_d_m_data <- left_join(migr_d_data, d1, by = c("Tuloalue" = "nimi"))
migr_a_m_data <- left_join(migr_a_data, d1, by = c("Lähtöalue" = "nimi"))

migr_d_m_data <- migr_d_m_data %>% 
  select(Tuloalue, Lähtöalue, Sukupuoli, Vuosi, `Kuntien välinen muutto`, maakunta_name_fi) %>% 
  rename(Maakunta = maakunta_name_fi, Kunta = Tuloalue)
migr_a_m_data <- migr_a_m_data %>% 
  select(Tuloalue, Lähtöalue, Sukupuoli, Vuosi, `Kuntien välinen muutto`, maakunta_name_fi) %>% 
  rename(Maakunta = maakunta_name_fi, Kunta = Lähtöalue)


#---- Maps of arrival

do_filter <- function(df, filter) {
  f <- df %>% 
    mutate(Vuosi = as.integer(Vuosi)) %>% 
    filter(eval(rlang::parse_expr(filter))) %>% 
    group_by(Kunta) %>% 
    summarise(n = n())
}

add_geom <- function(df) {
  dfg <- kunnat_geom %>%
    right_join(df, join_by(nimi == Kunta))
  
  dfg$Lkm <- cut(
    dfg$n,
    breaks = c(1, 10, 30),
    include.lowest = TRUE,
    dig.lab = 4
  )
  
  return(dfg)
}

do_plot <- function(df, title) {
  ggplot() +
    geom_sf(data = d1) +
    geom_sf(data = df, 
            aes(fill = Lkm)) +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title = title)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank())
}

before2000_a <- do_filter(migr_a_m_data, "Vuosi < 2000")
before2010_a <- do_filter(migr_a_m_data, "Vuosi >= 2000 & Vuosi < 2010")
after2010_a <- do_filter(migr_a_m_data, "Vuosi >= 2010 & Vuosi < 2020")
after2020_a <- do_filter(migr_a_m_data, "Vuosi >= 2020")

share_geom_2000 <- add_geom(before2000_a)
share_geom_2010 <- add_geom(before2010_a)
share_geom_2020 <- add_geom(after2010_a)
share_geom_2023 <- add_geom(after2020_a)

m1 <- do_plot(share_geom_2000, "1990-1999")
m2 <- do_plot(share_geom_2010, "2000-2009")
m3 <- do_plot(share_geom_2020, "2010-2019")
m4 <- do_plot(share_geom_2023, "2020-2023")

(m1 | m2 | m3 | m4) + 
  plot_layout(nrow = 2) +
  plot_annotation(title = "Laitilaan muuttaneet",
                  subtitle = "Lukumäärä",
                  caption = "Tilastokeskus | geofi: Access Finnish Geospatial Data | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5),
                                plot.subtitle = element_text(size = 10, hjust = .5),
                                plot.caption = element_text(size = 6)))

ggsave("Laitilaan_muuttaneet.png", dpi = 600, height = 10.4, width = 10.8, units = "cm")


#---- Maps of departure

before2000 <- do_filter(migr_d_m_data, "Vuosi < 2000")
before2010 <- do_filter(migr_d_m_data, "Vuosi >= 2000 & Vuosi < 2010")
after2010 <- do_filter(migr_d_m_data, "Vuosi >= 2010 & Vuosi < 2020")
after2020 <- do_filter(migr_d_m_data, "Vuosi >= 2020")

share_geom_2000_d <- add_geom(before2000)
share_geom_2010_d <- add_geom(before2010)
share_geom_2020_d <- add_geom(after2010)
share_geom_2023_d <- add_geom(after2020)

m1d <- do_plot(share_geom_2000_d, "1990-1999")
m2d <- do_plot(share_geom_2010_d, "2000-2009")
m3d <- do_plot(share_geom_2020_d, "2010-2019")
m4d <- do_plot(share_geom_2023_d, "2020-2023")

(m1d | m2d | m3d | m4d) + 
  plot_layout(nrow = 2) +
  plot_annotation(title = "Laitilasta muuttaneet",
                  subtitle = "Lukumäärä",
                  caption = "Tilastokeskus | geofi: Access Finnish Geospatial Data | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5),
                                plot.subtitle = element_text(size = 10, hjust = .5),
                                plot.caption = element_text(size = 6)))

ggsave("Laitilasta_muuttaneet.png", dpi = 600, height = 10.4, width = 10.8, units = "cm")

