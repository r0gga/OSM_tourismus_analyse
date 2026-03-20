library(sf)
library(MASS)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(spatstat)
library(viridis)
library(units)
library(lwgeom)
library(stringr)
library(gt)


sf_use_s2(FALSE)
files <- list.files(
  "data/",
  pattern = ".geojson",
  full.names = TRUE
)

analyse_region <- function(filepath, region_name) {
  
  poi_data <- st_read(filepath, quiet = TRUE)
  poi_data <- st_make_valid(poi_data)
  poi_data <- poi_data[!st_is_empty(poi_data), ]
  
  poi_points <- suppressWarnings(st_centroid(poi_data))
  poi_points <- poi_points[!st_is_empty(poi_points), ]
  
  real_points <- poi_data[st_geometry_type(poi_data) %in% c("POINT", "MULTIPOINT"), ]
  exact_bbox <- st_bbox(real_points)
  
  poi_points_clean <- suppressWarnings(st_crop(poi_points, exact_bbox))
  
  anzahl_poi <- nrow(poi_points_clean)

  bbox_polygon <- st_as_sfc(exact_bbox)
  
  area_m2 <- st_area(bbox_polygon)
  
  area_km2 <- set_units(area_m2, km^2)
  

  museen_data <- poi_points %>% filter(tourism == "museum")

  museen <- suppressWarnings(st_centroid(museen_data))
  anzahl_museen <- nrow(museen)
  
  dichte <- anzahl_poi / as.numeric(area_km2)
  dichte_museen <- anzahl_museen / as.numeric(area_km2)
  
  result <- data.frame(
    Region = region_name,
    Anzahl_POI = anzahl_poi,
    Flaeche_km2 = round(as.numeric(area_km2), 2),
    POI_Dichte_pro_km2 = round(dichte, 2),
    Museen_pro_km2 = round(dichte_museen, 2)
  )
  
  return(result)
}

create_table <- lapply(files, function(path){
  
  name_rein <- basename(path)
  name_sauber <- gsub(".geojson", "", name_rein)
  result <- analyse_region(
    filepath = path,
    region_name = name_sauber
  )
})

all_regions <- bind_rows(create_table)

dichte_all_regions <- all_regions %>%
  mutate(
    Typ = case_when(
      Region %in% c("berlin", "prag", "wien", "amsterdam") ~ "Stadt",
      Region %in% c("rimini", "alcudia", "arcachon", "westerland") ~ "Strand",
      Region %in% c("zermatt", "ischgl", "chamonix", "garmisch_partenkirchen") ~ "Berg",
      Region %in% c("luebbenau", "keswick", "bayrischer_wald", "baiersbronn") ~ "Land",
      TRUE ~ "Unbekannt"
    )
  )
print(dichte_all_regions)

dichte_pro_typ <- dichte_all_regions %>%
  group_by(Typ) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

dichte_export <- dichte_pro_typ %>%
  gt() %>%
  tab_header(
    title = "POI Dichte nach Regionstyp",
    subtitle = "Durschschnittliche Werte pro Regionstyp"
  ) %>% 
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  )
gtsave(dichte_export, filename = "results/tabelle_dichte.png")

boxplot_dichte <- ggplot(dichte_df, aes(x = Typ, y = POI_Dichte_pro_km2, fill = Typ)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Vergleich der touristischen POI-Dichte nach Regionstyp",
    x = "Regionstyp",
    y = "POI-Dichte (Anzahl pro km²)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 12))

print(boxplot_dichte)
ggsave("results/boxplot_dichte_vergleich.png", plot = boxplot_dichte, width = 8, height = 6, dpi = 300)

load_pois_with_type <- function(filepath, region_name, region_typ) {
  
  poi_data <- st_read(filepath, quiet = TRUE) %>% st_make_valid()
  poi_data <- st_make_valid(poi_data)
  
  if("tourism" %in% names(poi_data)) {
    poi_sauber <- poi_data %>%
      st_drop_geometry() %>%
      select(tourism) %>%
      filter(!is.na(tourism) & tourism %in% c("attraction", "gallery", "guest_house", "hostel", "hotel", "museum")) %>%  
      mutate(
        Region = region_name,
        Typ = region_typ
      )
    return(poi_sauber)
  } else {
    return(NULL)
  }
}

data_berlin <- load_pois_with_type("data/berlin.geojson", "Berlin", "Stadt")
data_prag <- load_pois_with_type("data/prag.geojson", "Prag", "Stadt")
data_wien <- load_pois_with_type("data/wien.geojson", "Wien", "Stadt")
data_amsterdam <- load_pois_with_type("data/amsterdam.geojson", "Amsterdam", "Stadt")
data_rimini <- load_pois_with_type("data/rimini.geojson", "Rimini", "Küste")
data_alcudia <- load_pois_with_type("data/alcudia.geojson", "Alcudia", "Küste")
data_arcachon <- load_pois_with_type("data/arcachon.geojson", "Arcachon", "Küste")
data_westerland <- load_pois_with_type("data/westerland.geojson", "Westerland", "Küste")
data_zermatt <- load_pois_with_type("data/zermatt.geojson", "Zermatt", "Berge")
data_ischgl <- load_pois_with_type("data/ischgl.geojson", "Ischgl", "Berge")
data_chamonix <- load_pois_with_type("data/chamonix.geojson", "Chamonix", "Berge")
data_garmisch_partenkirchen <- load_pois_with_type("data/garmisch_partenkirchen.geojson", "Garmisch Partenkirchen", "Berge")
data_luebbenau <- load_pois_with_type("data/luebbenau.geojson", "Lübbenau", "Land")
data_baiersbronn <- load_pois_with_type("data/baiersbronn.geojson", "Baiersbronn", "Land")
data_keswick <- load_pois_with_type("data/keswick.geojson", "Keswick", "Land")
data_bayrischer_wald <- load_pois_with_type("data/bayrischer_wald.geojson", "Bayrischer Wald", "Land")

all_poi_with_type <- bind_rows(data_berlin, data_prag, data_wien, data_amsterdam, data_rimini, data_alcudia, data_arcachon, data_westerland,
                              data_zermatt, data_ischgl, data_chamonix, data_garmisch_partenkirchen, data_luebbenau,
                              data_baiersbronn, data_keswick, data_bayrischer_wald)


zusammensetzung_plot <- ggplot(all_poi_with_type, aes(x = Typ, fill = tourism)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "viridis", name = "Tourismus-Kategorie") +
  labs(
    title = "Relative Zusammensetzung der touristischen Infrastruktur",
    subtitle = "Anteile der OSM-Kategorien nach Regionstyp",
    x = "Regionstyp",
    y = "Relativer Anteil"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(zusammensetzung_plot)
ggsave("results/zusammensetzung_pois.png", plot = zusammensetzung_plot, width = 10, height = 6, dpi = 300)


create_heatmap <- function(filepath, region_name, export_filename = NULL) {

  if (!file.exists(filepath)) return(NULL)
  poi_data <- st_read(filepath, quiet = TRUE) %>% st_make_valid()
  
  poi_points <- suppressWarnings(st_centroid(poi_data))
  
  real_points <- poi_data %>% filter(st_geometry_type(geometry) == "POINT")
  exact_bbox <- st_bbox(real_points)
  
  poi_points_clean <- suppressWarnings(st_crop(poi_points, exact_bbox))

  coords <- st_coordinates(poi_points_clean)
  poi_df <- as.data.frame(coords)
  
  p <- ggplot() +
    geom_sf(data = poi_points_clean, color = NA) +
    #osm als Grundkarte
    annotation_map_tile(type = "osm", zoomin = -1, alpha = 1) +

    #heatmap als obere Schicht
    stat_density_2d_filled(
      data = poi_df, 
      aes(x = X, y = Y, fill = after_stat(level)), 
      alpha = 0.65
    ) +
    
    scale_fill_viridis_d(option = "inferno", name = "POI Dichte") +
    
    coord_sf(crs = 4326, expand = FALSE) +
    
    labs(
      title = paste("Tourismus-Hotspots:", region_name),
      subtitle = "Überlagerung mit Kartendetails von OpenStreetMap",
      x = "Längengrad", y = "Breitengrad",
      caption = "Kartendaten © OpenStreetMap"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
  
    if (!is.null(export_filename)) {
    ggsave(export_filename, plot = p, width = 10, height = 8, dpi = 300)
  }
  
  return(p)
}

heatmaps <- lapply(files, function(path) {
  
  name_rein <- basename(path)
  name_sauber <- gsub(".geojson", "", name_rein)
  
  print(paste("Erstelle Heatmap für:", name_sauber))
  
  result <- create_heatmap(
    filepath = path,
    region_name = name_sauber,
    export_filename = paste0("results/heatmaps/heatmap_", name_sauber, ".png")
  )
})


gruppiere_attraktionen <- function(filepath, region_name) {
  
  poi_data <- st_read(filepath, quiet = TRUE) %>% st_make_valid()
  
  if (!"tourism" %in% names(poi_data) | !"name" %in% names(poi_data)) {
    return(NULL) 
  }
  
  attraktionen <- poi_data %>% 
    filter(tourism == "attraction") %>%
    mutate(name_klein = tolower(name))
  
  if (nrow(attraktionen) == 0) return(NULL)
  
  if (!"historic" %in% names(attraktionen)) attraktionen$historic <- NA_character_
  
  if (!"natural" %in% names(attraktionen)) attraktionen$natural <- NA_character_
  
  attraktionen_kategorisiert <- attraktionen %>%
    mutate(
      Kategorie = case_when(
        
        #HISTORISCHES
        !is.na(historic) | str_detect(name_klein, "schloss|burg|ruine|monument|denkmal|castle|palace|history|museum|galerie|gallery|theater|theatre|oper|festung|fort|memorial|gedenkstätte|mühle|mill|ausstellung|archäologie|archaeology") ~ "Historisch & Kultur",
        
        #NATUR & LANDSCHAFT
        !is.na(natural) | str_detect(name_klein, "park|wasserfall|cave|höhle|gorge|schlucht|natur|strand|beach|see|lake|wald|forest|berg|mountain|gipfel|peak|tal|valley|garten|garden|botanic|küste|coast|bucht|bay|insel|island|quelle|spring") ~ "Natur & Landschaft",
        
        #FREIZEIT & FAMILIE
        str_detect(name_klein, "zoo|tierpark|aquarium|freizeitpark|amusement|spielplatz|playground|schwimmbad|pool|therme|waterpark|minigolf|trampolin|escape|kino|cinema|stadion|stadium|arena|sport") ~ "Freizeit & Familie",
        
        #AUSSICHTSPUNKTE & TÜRME
        str_detect(name_klein, "turm|tower|viewpoint|aussicht|blick|panorama|observation|plattform|platform|skywalk|belvedere|spitze") ~ "Aussichtspunkte",
        
        #RELIGIÖSES
        str_detect(name_klein, "kirche|dom|kathedrale|kloster|abbey|church|mosque|kapelle|chapel|tempel|temple|synagoge|synagogue|schrein|shrine|münster|basilika|basilica|friedhof|cemetery") ~ "Religiöse Stätten",
        
        #REST
        TRUE ~ "Sonstige Attraktionen"
      )
    )
  statistik <- attraktionen_kategorisiert %>%
    st_drop_geometry() %>%
    group_by(Kategorie) %>%
    summarise(
      Anzahl = n(),
      Region = region_name
    )
  return(statistik)
}

meta_groups <- lapply(files, function(path) {
  
  name_rein <- basename(path)
  name_sauber <- gsub(".geojson", "", name_rein)
  
  result <- gruppiere_attraktionen(
    filepath = path,
    region_name = name_sauber)
})
meta_groups_table <- bind_rows(meta_groups)

categories_all_regions <- meta_groups_table %>%
  mutate(
    Typ = case_when(
      Region %in% c("berlin", "prag", "wien", "amsterdam") ~ "Stadt",
      Region %in% c("rimini", "alcudia", "arcachon", "westerland") ~ "Strand",
      Region %in% c("zermatt", "ischgl", "chamonix", "garmisch_partenkirchen") ~ "Berg",
      Region %in% c("luebbenau", "keswick", "bayrischer_wald", "baiersbronn") ~ "Land",
      TRUE ~ "Unbekannt"
    )
  )

average_categories <- categories_all_regions %>%
  group_by(Typ, Kategorie) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
print(average_categories)

average_categories_plot <- ggplot(average_categories, aes(x = Typ, y = Anzahl, fill = Kategorie)) +
  
  geom_col(position = "fill", color = "white", linewidth = 0.2) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_fill_viridis_d(option = "viridis", name = "Kategorie") +
  
  labs(
    title = "Gruppierung der POI nach Namensspalte",
    subtitle = "Durchschnittliche Anteile der Kategorien nach Regionstyp",
    x = "Regionstyp",
    y = "Relativer Anteil"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
print(average_categories_plot)
ggsave("results/zusammensetzung_pois_new_categories.png", plot = average_categories_plot, width = 10, height = 6, dpi = 300)
