library(readxl)
library(tidyverse)
library(pxweb)
library(geofi)
library(sf)
library(patchwork)
#library(ggsankey)

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

point <- geofi::municipality_central_localities
point$municipality_code <- as.integer(point$kuntatunnus)

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
  labels = c("0-0.5", "0.5-1", "1-10", "10-60"),
  include.lowest = TRUE,
  dig.lab=4
)
share_geom$Lkm <- cut(
  share_geom$laitilasta,
  breaks = c(1, 10, 50, 500, 1000, 5000),
  labels = c("1-10", "10-50", "50-500", "500-1000", "1000-5000"),
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

m1 <- do_plot("Lkm", "Henkilöitä")
m2 <- do_plot("Osuus", "Osuus väestöstä %")

(m1 | m2) + 
  plot_layout(nrow = 1) +
  plot_annotation(title = "Laitilassa syntyneiden asuinkunta vuonna 2023",
                  caption = "Tilastokeskus | geofi: Access Finnish Geospatial Data | Tuija Sonkkila",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5)))

ggsave("Laitilassa_syntyneet.png", dpi = 600, height = 12, width = 20, units = "cm")

