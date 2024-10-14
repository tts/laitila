library(readxl)
library(tidyverse)
library(pxweb)
library(geofi)
library(sf)
library(patchwork)

#---------------------------------
# Data licensed from Tilastokeskus. 
# Number of people by municipality 
# in 2023, born in Laitila
#---------------------------------
data <- read_excel("24PIENEP_418.xlsx", skip = 2)
laitila <- data %>% 
  filter(Kuntakoodi != "SSS") %>% 
  rename(laitilasta = `2023`)

#---------------------------------------------
# Open data on municipalities, population 2023
#---------------------------------------------

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

#--------------------------
# Municipalities by geofi
#--------------------------
d1 <- get_municipalities(year = 2023)
saveRDS(d1, "kunnat.RDS")

d1 <- readRDS("kunnat.RDS")

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

share_geom <- readRDS("laitila_muualla.RDS")

m1 <- ggplot() +
  geom_sf(data = d1) +
  geom_sf(data = share_geom, 
       aes(fill = Osuus)) +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  guides(fill = guide_legend(title = "Osuus väestöstä %")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

m2 <- ggplot() +
  geom_sf(data = d1) +
  geom_sf(data = share_geom, 
          aes(fill = Lkm)) +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  guides(fill = guide_legend(title = "Lukumäärä")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

(m2 | m1) + 
  plot_layout(nrow = 1) +
  plot_annotation(title = "Laitilassa syntyneiden asuinkunta vuonna 2023")

#-----------------------------------------------------
# Nationality of ppl in Laitila 2023 (11rh)
#-----------------------------------------------------

m <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")
l_data <- m$data

l_data <- l_data %>% 
  filter(!Kansalaisuus %in% c("AMERIKKA", "EUROOPPA", "AASIA", 
                              "AFRIKKA", "OSEANIA", "ULKOMAAT YHTEENSÄ")) %>% 
  filter(!is.na(`Väestö 31.12.`)) %>% 
  rename(Lkm = `Väestö 31.12.`) %>% 
  select(Kansalaisuus, Lkm)

saveRDS(l_data, "Laitila_kansalaisuus.RDS")

#-----------------------------------------------------
# Intermunicipal migration by area of arrival, 
# 1990-2023 (11a1)
#-----------------------------------------------------

migr <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")

migr_data <- migr$data

migr_data <- migr_data %>% 
  filter(`Kuntien välinen muutto` != 0)

saveRDS(migr_data, "tulleet_laitilaan.RDS")

#-----------------------------------------------------
# Intermunicipal migration by area of departure, 
# 1990-2023 (11a1)
#-----------------------------------------------------
migr_d <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi")

migr_d_data <- migr_d$data

migr_d_data <- migr_d_data %>% 
  filter(`Kuntien välinen muutto` != 0)

saveRDS(migr_d_data, "lahteneet_laitilasta.RDS")
