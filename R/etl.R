# ==============================================================================
# EURÓPAI KERÉKPÁR-MEGOSZTÓ RENDSZEREK (BSS) – TELJES KUTATÁSI ADATBÁZIS,
# ADATÁTALAKÍTÓ ÉS LEÍRÓ STATISZTIKAI / VIZUALIZÁCIÓS PIPELINE
# ==============================================================================
#
# FELÉPÍTÉS (FEJEZETEK):
#   0. FEJEZET: Környezet beállítása, csomagok betöltése és globális paraméterek
#   1. FEJEZET: Használati adatok gyűjtése és városok kiválogatása (GBFS, Nextbike, egyéb)
#   2. FEJEZET: Koordináták, időjárási (Open-Meteo) és naptári (munkanap) adatok
#   3. FEJEZET: Állomásszámok, városhatárok, infrastruktúra (OSM/ohsome) és domborzat
#   4. FEJEZET: Nyers panel adatbázis (panel_raw.csv) összefűzése és városszintű tisztítás
#   5. FEJEZET: Adatátalakítás, idősávos szűrés és végső panel (panel_data.csv) előállítása
#   6. FEJEZET: Leíró statisztikák és vizualizációk (Európa-térkép, Hiányzó adatok hőtérképe)
# ==============================================================================


# ==============================================================================
# 0. FEJEZET: KÖRNYEZET BEÁLLÍTÁSA, CSOMAGOK ÉS GLOBÁLIS PARAMÉTEREK
# ==============================================================================

# --- 0.1 Munkakönyvtár és mappastruktúra beállítása ---
setwd("C:/Users/aronm/0corvinus/7felev/szakdolgozat")
dir.create("data/gbfs_snapshots", recursive = TRUE, showWarnings = FALSE)

# --- 0.2 Szükséges R csomagok betöltése (funkcionális magyarázatokkal) ---
# 1) Térinformatika (GIS), poligonok, domborzati raszterek és alaptérképek
library(maps)         # Világ- és Európa-térképi alappoligonok (map_data) betöltése
library(sf)           # Vektoros térinformatikai adatok (városhatár-poligonok) kezelése, egyszerűsítése és területszámítás
library(elevatr)      # Digitális domborzatmodellek (DEM raszterek) letöltése (AWS / OpenTopography)
library(terra)        # Raszteres térinformatikai műveletek: magasságértékek kinyerése és lejtőszög (slope) számítása

# 2) Webes API-kommunikáció és geokódolás
library(httr2)        # HTTP kérések építése, újrapróbálkozás (req_retry) és JSON válaszok feldolgozása
library(jsonlite)     # JSON és GeoJSON adatszerkezetek olvasása és konvertálása
library(tidygeocoder) # Geokódolási segédfüggvények címek és településnevek kezeléséhez

# 3) Gyors adatbeolvasás és fájlkezelés
library(data.table)   # Nagyméretű (több gigabájtos) fájlok villámgyors beolvasása (fread) és memóriahatékony szűrése
library(readr)        # Gyors és konzisztens CSV beolvasás (read_csv) és Excel-kompatibilis mentés (write_excel_csv)

# 4) Adatmanipuláció, adattisztítás, dátumkezelés és vizualizáció (Tidyverse mag)
library(tibble)       # Modern, átláthatóbb data.frame (tibble) struktúrák és tribble() létrehozása
library(stringr)      # Szöveges változók (stringek) tisztítása, darabolása és mintaillesztése
library(stringi)      # Karakterkódolások és ékezetek eltávolítása (pl. Latin-ASCII transzformáció)
library(lubridate)    # Dátumok és időpontok kezelése (év, hét napja, időintervallumok)
library(tidyr)        # Adatok alakítása (unnest, expand_grid, complete, drop_na)
library(purrr)        # Ciklusok kiváltása funkcionális iterációkkal (map, map2, pmap, possibly)
library(dplyr)        # Adattáblák szűrése (filter), rendezése (arrange), összekapcsolása (join) és aggregálása
library(ggplot2)      # Publikációs minőségű grafikonok, hőtérképek és térképek készítése

map    <- purrr::map
select <- dplyr::select
filter <- dplyr::filter
first  <- dplyr::first
year   <- lubridate::year

# --- 0.3 Globális konstansok és paraméterek ---
START_DATE <- "2022-08-01"
END_DATE   <- "2023-07-31"
OSM_DATE   <- "2023-01-31"
REPO       <- "MobilityData/gbfs"
FILE_NAME  <- "systems.csv"
FOLDER     <- "data/gbfs_snapshots"
GH_TOKEN   <- Sys.getenv("GITHUB_PAT")

european_countries <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT",
  "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB","CH","NO",
  "IS","LI","RS","BA","ME","MK","AL","XK","AD","MC","SM","VA","MD","UA","BY",
  "RU","TR"
)


# ==============================================================================
# 1. FEJEZET: HASZNÁLATI ADATOK GYŰJTÉSE ÉS VÁROSOK KIVÁLOGATÁSA
# ==============================================================================

# --- 1.1 GitHub Commitok lekérése ---
get_github_commits <- function(...) {
  req <- request(paste0("https://api.github.com/repos/", REPO, "/commits")) |>
    req_url_query(path = FILE_NAME, ...) |>
    req_user_agent("bss-research-bot") |>
    req_headers(Accept = "application/vnd.github+json") |>
    req_retry(max_tries = 4)
  if (nzchar(GH_TOKEN)) req <- req_auth_bearer_token(req, GH_TOKEN)
  resp_body_json(req_perform(req))
}

extract_commit_data <- function(l) tibble(sha = l$sha, date = l$commit$committer$date)

# Kezdeti állapot (START_DATE előtti utolsó állapot)
base_commit <- get_github_commits(until = paste0(START_DATE, "T23:59:59Z"), per_page = 1)
base_commit <- map_dfr(base_commit, extract_commit_data)

# Módosítások a vizsgált időszakban
commits_list <- list()
page_num <- 1
repeat {
  r <- get_github_commits(since = paste0(START_DATE, "T00:00:00Z"),
                          until = paste0(END_DATE,    "T23:59:59Z"),
                          per_page = 100, page = page_num)
  if (length(r) == 0) break
  commits_list <- c(commits_list, map(r, extract_commit_data))
  if (length(r) < 100) break
  page_num <- page_num + 1
}

all_commits <- bind_rows(base_commit, bind_rows(commits_list)) |>
  distinct(sha, .keep_all = TRUE) |>
  arrange(date) |>
  mutate(state_id = row_number())

# [BIZTONSÁGI MENTÉS]: GitHub commit lista
write_excel_csv(all_commits, "data/gbfs_commits.csv")


# --- 1.2 GBFS snapshotok letöltése és beolvasása ---
download_snapshot <- function(sha) {
  file_path <- file.path(FOLDER, paste0(sha, ".csv"))
  if (!file.exists(file_path)) {
    content <- request(paste0("https://raw.githubusercontent.com/", REPO, "/", sha, "/", FILE_NAME)) |>
      req_user_agent("bss-research-bot") |>
      req_retry(max_tries = 4) |>
      req_perform() |>
      resp_body_string(encoding = "UTF-8")
    writeLines(content, file_path, useBytes = TRUE)
    Sys.sleep(0.2)
  }
  file_path
}

read_snapshot <- function(sha, state_id) {
  read_csv(download_snapshot(sha), show_col_types = FALSE,
           col_types = cols(.default = col_character())) |>
    transmute(
      state_id    = state_id,
      country     = `Country Code`,
      system_name = Name,
      location    = Location,
      system_id   = `System ID`,
      feed_url    = `Auto-Discovery URL`
    )
}

systems_data <- map2_dfr(all_commits$sha, all_commits$state_id, read_snapshot)


# --- 1.3 Városnevek normalizálása és monopol Nextbike rendszerek keresése ---
extract_city <- function(loc, cc) {
  parts <- str_trim(str_split(loc, ",")[[1]])
  if (length(parts) >= 2 && tail(parts, 1) == cc) return(paste(head(parts, -1), collapse = ", "))
  if (length(parts) == 1 && parts == cc) return(NA_character_)
  paste(parts, collapse = ", ")
}

normalize_name <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |> str_trim()
}

systems_data <- systems_data |>
  filter(country %in% european_countries) |>
  mutate(
    city_raw    = map2_chr(location, country, extract_city),
    city_key    = normalize_name(city_raw),
    is_nextbike = str_detect(feed_url, fixed("gbfs.nextbike.net")) | str_starts(system_id, "nextbike_")
  ) |>
  filter(!is.na(city_raw))

monopoly_analysis <- systems_data |>
  group_by(country, city_key) |>
  summarise(
    city_raw          = first(city_raw),
    total_states      = nrow(all_commits),
    nextbike_states   = n_distinct(state_id[is_nextbike]),
    competitor_states = n_distinct(state_id[!is_nextbike]),
    .groups = "drop"
  ) |>
  filter(nextbike_states > 0) |>
  mutate(is_full_monopoly = competitor_states == 0 & nextbike_states == total_states)

nextbike_monopolies <- monopoly_analysis |> 
  filter(is_full_monopoly == TRUE) |> 
  select(country, city = city_raw)


# --- 1.4 Nextbike Dataset feldolgozása (Utazások) ---
dt_trips <- fread("european-bike-sharing-dataset/full/trips.csv", select = c("city_id", "time_start"))
dt_trips[, date := as.Date(as.POSIXct(time_start, origin = "1970-01-01", tz = "UTC"))]

# Szűrés a releváns időszakra, hogy csökkentsük a memóriaméretet
dt_trips <- dt_trips[date >= as.Date(START_DATE) & date <= as.Date(END_DATE)]

city_active_days <- dt_trips[, .(active_days = uniqueN(date)), by = city_id]
valid_nextbike_cities <- city_active_days[active_days > quantile(active_days, 0.25)]

# Városnevek társítása a GitHub azonosítókkal
github_city_keys <- read_csv("european-bike-sharing-dataset/full/cities.csv", show_col_types = FALSE) |>
  rename(city = name)

# Itt a city (ami az eredeti github név) alapján összekötjük a monopol listával.
# FIGYELEM: Megtartjuk a github_city_keys-ből jövő eredeti 'city' nevet!
research_db <- valid_nextbike_cities |>
  left_join(github_city_keys, by = c("city_id" = "id")) |>
  inner_join(nextbike_monopolies, by = c("city" = "city", "country" = "country"))

# Utazásszámok napi aggregálása a monopol Nextbike városokra
daily_trips_nextbike <- dt_trips |>
  semi_join(research_db, by = "city_id") |>
  group_by(city_id, date) |>
  summarise(trips = n(), .groups = "drop")

# Memória felszabadítása a nagyméretű dt_trips táblától
rm(dt_trips)
gc()

full_calendar <- seq(as.Date(START_DATE), as.Date(END_DATE), by = "day")

nextbike_usage <- expand_grid(city_id = research_db$city_id, date = full_calendar) |>
  left_join(daily_trips_nextbike, by = c("city_id", "date")) |>
  left_join(research_db |> select(city_id, city, country), by = "city_id") |>
  select(country, city, date, trips)


# --- 1.5 Egyéb szolgáltatók (Budapest, Bergen, Oslo, Trondheim) ---

# Budapest
budapest_usage <- read_delim("data/Budapest/202208-202307.csv", delim = ";", show_col_types = FALSE) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d"), country = "HU", city = "Budapest") |>
  select(country, city, date, trips)

# Norvég városok beolvasó függvénye
read_norwegian_city <- function(folder_path, city_name) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble())
  map_df(files, ~fread(.x, select = "started_at")) |>
    mutate(date = as.Date(started_at)) |>
    count(date, name = "trips") |>
    mutate(city = city_name, country = "NO") |>
    select(country, city, date, trips)
}

bergen_usage    <- read_norwegian_city("data/Bergen", "Bergen")
oslo_usage      <- read_norwegian_city("data/Oslo", "Oslo")
trondheim_usage <- read_norwegian_city("data/Trondheim", "Trondheim")

other_usage <- bind_rows(budapest_usage, bergen_usage, oslo_usage, trondheim_usage) |>
  filter(date >= as.Date(START_DATE) & date <= as.Date(END_DATE))


# --- 1.6 Előzetes használati adatbázis és városlista egyesítése ---
final_usage_temp  <- bind_rows(nextbike_usage, other_usage)
final_cities_temp <- final_usage_temp |> distinct(country, city)

# [BIZTONSÁGI MENTÉS]: Geokódolás előtti nyers egyesített használati adatok
write_excel_csv(final_usage_temp, "data/usage_raw_combined.csv")


# ==============================================================================
# 2. FEJEZET: KOORDINÁTÁK, IDŐJÁRÁSI ÉS NAPTÁRI ADATOK GYŰJTÉSE
# ==============================================================================

# --- 2.1.a Koordináták lekérése a megtartott eredeti nevek alapján ---
get_coords <- function(city_name, country_code) {
  Sys.sleep(1)
  r <- request("https://geocoding-api.open-meteo.com/v1/search") |>
    req_url_query(name = city_name, country_code = country_code, count = 1, language = "en") |>
    req_retry(max_tries = 3) |> req_perform() |> resp_body_json()
  
  if (is.null(r$results)) return(tibble(lat = NA_real_, lon = NA_real_))
  
  # 5 tizedesjegyre kerekítünk
  tibble(
    lat = round(r$results[[1]]$latitude, 5), 
    lon = round(r$results[[1]]$longitude, 5)
  )
}


city_coords <- final_cities_temp |>
  mutate(coords = map2(city, country, possibly(get_coords, tibble(lat = NA_real_, lon = NA_real_)))) |>
  unnest(coords)


# --- 2.1.b Üres koordinátás "városok" (régiók / egyetemek) eltávolítása ---
# Több helyhez sem talált az API segítségével koordinátát az algoritmus; ez azért van, 
# mert ezek nem egy-egy várost jelöltek, hanem egy nagyobb egyetemi közeget/régiót lefedő területet.
# Így ezeket a helyeket kivesszük az adatok közül (10 ilyen hely:
# Bergisches e-Bike, RSVG, RVK e-Bike, Eifel e-Bike, Ibiza-City, Berounsko,
# Swansea University, Rychnovsko, Winsen (Luhe), Brunel University).

invalid_cities <- city_coords |> filter(is.na(lat) | is.na(lon)) |> select(country, city)

if (nrow(invalid_cities) > 0) {
  message("Eltávolítva a kutatásból (nincs koordináta):")
  print(invalid_cities)
  
  # Csak azokat a városokat tartjuk meg, amiknek van koordinátájuk
  city_coords <- city_coords |> filter(!is.na(lat) & !is.na(lon))
  
  # A végső városlistából is kivesszük őket
  final_cities <- final_cities_temp |> semi_join(city_coords, by = c("country", "city"))
  
  # A használati adatokból is töröljük a hozzájuk tartozó összes sort
  final_usage <- final_usage_temp |> semi_join(city_coords, by = c("country", "city"))
} else {
  final_cities <- final_cities_temp
  final_usage  <- final_usage_temp
}

# [BIZTONSÁGI MENTÉS]: Tisztított használati adatok, városlista és koordináták.
write_excel_csv(final_usage, "data/usage_daily.csv")
write_excel_csv(final_cities, "data/cities.csv")
write_excel_csv(city_coords, "data/city_coords.csv")


# --- 2.2 Időjárás API hívás (Open-Meteo Archive) ---
get_weather <- function(city_name, country_code, latitude, longitude) {
  Sys.sleep(1)
  r <- request("https://archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude = latitude, longitude = longitude,
      start_date = START_DATE, end_date = END_DATE,
      daily = "temperature_2m_mean,precipitation_sum,wind_speed_10m_mean,relative_humidity_2m_mean",
      wind_speed_unit = "ms", timezone = "auto"
    ) |>
    req_retry(max_tries = 5) |> req_perform() |> resp_body_json(simplifyVector = TRUE)
  
  as_tibble(r$daily) |> 
    mutate(country = country_code, city = city_name, lat = latitude, lon = longitude, .before = 1)
}

weather_data <- city_coords |>
  filter(!is.na(lat)) |>
  select(city_name = city, country_code = country, latitude = lat, longitude = lon) |>
  pmap_dfr(possibly(get_weather, otherwise = NULL)) |>
  rename(
    date             = time, 
    temp_celsius     = temperature_2m_mean, 
    precip_mm        = precipitation_sum,
    wind_ms          = wind_speed_10m_mean, 
    humidity_percent = relative_humidity_2m_mean
  )

# [BIZTONSÁGI MENTÉS]: Napi időjárási adatok
write_excel_csv(weather_data, "data/weather_daily.csv")


# --- 2.3 Munkanapok és ünnepnapok meghatározása (Nager.Date API) ---
unique_country_years <- final_usage |>
  mutate(year = year(date)) |>
  distinct(country, year) |>
  drop_na()

get_holidays <- function(country_code, target_year) {
  Sys.sleep(0.5)
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", target_year, "/", country_code)
  
  res <- request(url) |>
    req_error(is_error = function(resp) FALSE) |> 
    req_retry(max_tries = 3) |>
    req_perform()
  
  if (resp_status(res) == 200) {
    json_data <- resp_body_json(res)
    tibble(country = country_code, holiday_date = as.Date(map_chr(json_data, "date")))
  } else {
    tibble(country = character(), holiday_date = as.Date(character()))
  }
}

holidays_db <- map2_dfr(unique_country_years$country, unique_country_years$year, get_holidays) |>
  distinct()

# [BIZTONSÁGI MENTÉS]: Ünnepnapok nyers adatbázisa
write_excel_csv(holidays_db, "data/holidays_raw.csv")

usage_with_workdays <- final_usage |>
  mutate(date = as.Date(date)) |>
  left_join(
    holidays_db |> mutate(holiday_flag = TRUE), 
    by = c("country" = "country", "date" = "holiday_date")
  ) |>
  mutate(
    is_holiday = ifelse(is.na(holiday_flag), FALSE, TRUE),
    is_weekend = lubridate::wday(date, week_start = 1) %in% c(6, 7),
    is_workday = ifelse(is_weekend | is_holiday, 0, 1) # Ha hétvége VAGY ünnep = 0, amúgy 1
  ) |>
  select(-is_weekend, -is_holiday, -holiday_flag)

# [BIZTONSÁGI MENTÉS]: Használati adatok munkanap-jelölővel
write_excel_csv(usage_with_workdays, "data/usage_and_workdays.csv")


# ==============================================================================
# 3. FEJEZET: ÁLLOMÁSSZÁMOK, VÁROSHATÁROK, INFRASTRUKTÚRA ÉS DOMBORZAT
# ==============================================================================

# --- 3.1 Állomások (dokkolók) száma GitHub és manuális adatokból ---
gh_cities   <- read_csv("european-bike-sharing-dataset/full/cities.csv", show_col_types = FALSE) |> rename(city = name)
gh_stations <- read_csv("european-bike-sharing-dataset/full/stations.csv", show_col_types = FALSE)

station_counts <- gh_stations |>
  left_join(gh_cities, by = c("city_id" = "id")) |>
  group_by(city, country) |>
  summarise(stations_github = n(), .groups = "drop")

final_station_counts <- final_cities |>
  left_join(station_counts, by = c("city", "country"))

# Az ismert Nextbike állomások mellé az egyéb városok állomásszámát manuálisan visszakerestük:
# Budapest: 178 (2023.01), Bergen: 107 (2023.01), Oslo: 242 (2023.01), Trondheim: 51 (2023.03)
manual_stations <- tribble(
  ~city,       ~country, ~manual_count,
  "Budapest",  "HU",     178,
  "Bergen",    "NO",     107,
  "Oslo",      "NO",     242,
  "Trondheim", "NO",     51
)

final_station_counts <- final_station_counts |>
  left_join(manual_stations, by = c("city", "country")) |>
  mutate(stations_github = coalesce(manual_count, stations_github)) |>
  select(-manual_count)

# [BIZTONSÁGI MENTÉS]: Állomásszámok városonként
write_excel_csv(final_station_counts, "data/stations_from_github.csv")


# --- 3.2 Városhatárok (Poligonok) letöltése (Nominatim) és területszámítás ---
get_polygon <- function(city_name, country_code) {
  Sys.sleep(1.1)
  res <- tryCatch(
    request("https://nominatim.openstreetmap.org/search") |>
      req_user_agent("bss-research-bot") |>
      req_url_query(q = paste(city_name, country_code, sep = ","), format = "jsonv2",
                    polygon_geojson = 1, polygon_threshold = 0.0005, featuretype = "settlement", limit = 1) |>
      req_retry(max_tries = 3) |> req_perform() |> resp_body_json(),
    error = function(e) { list() }
  )
  for (h in res) {
    if (isTRUE(h$geojson$type %in% c("Polygon", "MultiPolygon"))) {
      return(tibble(
        geom_json = as.character(toJSON(h$geojson, auto_unbox = TRUE, digits = NA))
      ))
    }
  }
  tibble(geom_json = NA_character_)
}

city_boundaries <- final_cities |>
  mutate(poly = map2(city, country, get_polygon)) |>
  unnest(poly)

# Geometria egyszerűsítése (30m tűrés) a lekérdezések gyorsításához
simplify_geom <- function(json_str) {
  if (is.na(json_str)) return(NA_character_)
  g <- st_read(json_str, quiet = TRUE) |> st_make_valid() |> st_transform(3857) |>
    st_simplify(dTolerance = 30, preserveTopology = TRUE) |> st_transform(4326)
  
  tmp <- tempfile(fileext = ".geojson")
  st_write(g, tmp, driver = "GeoJSON", quiet = TRUE, layer_options = "COORDINATE_PRECISION=5")
  out <- paste(readLines(tmp, warn = FALSE), collapse = "")
  unlink(tmp)
  out
}
city_boundaries$fc_json <- map_chr(city_boundaries$geom_json, simplify_geom)

# Területszámítás (km2)
calc_area <- function(fc) {
  if (is.na(fc)) return(NA_real_)
  g <- st_read(fc, quiet = TRUE) |> st_make_valid()
  round(as.numeric(sum(st_area(g))) / 1e6, 2)
}
city_boundaries$area_km2 <- map_dbl(city_boundaries$fc_json, calc_area)

# [BIZTONSÁGI MENTÉS]: Városhatár-poligonok és területek
write_excel_csv(city_boundaries |> select(country, city, area_km2, fc_json), "data/city_boundaries.csv")


# --- 3.3 Infrastruktúra mutatók letöltése (ohsome API) ---
ELEM <- "(type:node or type:way or type:relation)"
cykl <- "(lane,track,opposite_lane,opposite_track,share_busway)"

indicators <- tribble(
  ~metric_name,      ~endpoint, ~filter_query,
  "bike_path_km",    "length",  paste0("type:way and (highway=cycleway or cycleway in ", cykl, " or cycleway:left in ", cykl, " or cycleway:right in ", cykl, " or cycleway:both in ", cykl, ")"),
  "car_road_km",     "length",  "type:way and highway in (motorway,trunk,primary,secondary,tertiary,unclassified,residential,living_street,service,motorway_link,trunk_link,primary_link,secondary_link,tertiary_link)",
  "pedestrian_km",   "length",  "type:way and (highway in (footway,pedestrian,path,steps) or footway=sidewalk)",
  "mobility_poi",    "count",   paste0("(highway=bus_stop or railway in (station,halt,tram_stop) or station=subway) and ", ELEM),
  "other_poi",       "count",   paste0("(amenity in (university,college,school,restaurant,cafe,fast_food,bar,pub) or (shop=* and shop!=vacant) or office=* or tourism in (attraction,museum,gallery,viewpoint,zoo,theme_park,hotel,hostel,guest_house,apartment,motel) or leisure=park) and ", ELEM)
)

get_ohsome <- function(endpoint, fc, query) {
  if (is.na(fc)) return(NA_real_)
  resp <- tryCatch(
    request(paste0("https://api.ohsome.org/v1/elements/", endpoint)) |>
      req_body_form(bpolys = fc, filter = query, time = OSM_DATE) |>
      req_timeout(300) |> req_retry(max_tries = 3) |> req_perform() |> resp_body_json(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_real_)
  val <- resp$result[[1]]$value
  if (endpoint == "length") val / 1000 else val
}

infrastructure_results <- city_boundaries |> select(country, city, fc_json)
for (m in indicators$metric_name) infrastructure_results[[m]] <- NA_real_

for (i in seq_len(nrow(infrastructure_results))) {
  for (k in seq_len(nrow(indicators))) {
    if (is.na(infrastructure_results$fc_json[i])) next
    message(i, "/", nrow(infrastructure_results), " ", infrastructure_results$city[i], " - ", indicators$metric_name[k])
    infrastructure_results[[indicators$metric_name[k]]][i] <- get_ohsome(
      indicators$endpoint[k], 
      infrastructure_results$fc_json[i], 
      indicators$filter_query[k]
    )
    Sys.sleep(0.5)
  }
}

final_infrastructure <- city_boundaries |> 
  select(country, city, area_km2) |>
  left_join(infrastructure_results |> select(-fc_json), by = c("country", "city"))

# [BIZTONSÁGI MENTÉS]: Városi infrastruktúra és terület adatok
write_excel_csv(final_infrastructure, "data/city_features.csv")


# --- 3.4 Domborzati adatok (szintkülönbség és átlagos lejtőszög) ---
calc_elevation <- function(fc) {
  if (is.na(fc) || fc == "") return(tibble(elevation_diff_m = NA_real_, mean_slope_deg = NA_real_))
  tryCatch({
    poly <- st_read(fc, quiet = TRUE)
    suppressMessages(dem <- get_elev_raster(poly, z = 10, clip = "locations"))
    dem_t <- rast(dem)
    elevs <- values(dem_t, mat = FALSE)
    elevs <- elevs[!is.na(elevs)]
    slope <- terrain(dem_t, v = "slope", unit = "degrees")
    tibble(
      elevation_diff_m = max(elevs) - min(elevs),
      mean_slope_deg   = mean(values(slope, mat = FALSE), na.rm = TRUE)
    )
  }, error = function(e) tibble(elevation_diff_m = NA_real_, mean_slope_deg = NA_real_))
}

elevation_data  <- map_dfr(city_boundaries$fc_json, calc_elevation)
final_elevation <- bind_cols(city_boundaries |> select(country, city), elevation_data)

# [BIZTONSÁGI MENTÉS]: Domborzati adatok
write_excel_csv(final_elevation, "data/elevation.csv")


# ==============================================================================
# 4. FEJEZET: NYERS PANEL ADATBÁZIS ÖSSZEFŰZÉSE ÉS VÁROSSZINTŰ TISZTÍTÁS
# ==============================================================================

# --- 4.1 Rész-adatbázisok beolvasása ---
base_df      <- read_csv("data/usage_and_workdays.csv", show_col_types = FALSE)
weather_df   <- read_csv("data/weather_daily.csv", show_col_types = FALSE)
features_df  <- read_csv("data/city_features.csv", show_col_types = FALSE)
elevation_df <- read_csv("data/elevation.csv", show_col_types = FALSE)
stations_df  <- read_csv("data/stations_from_github.csv", show_col_types = FALSE)
pop_df       <- read_csv2("data/population.csv", show_col_types = FALSE)

# Biztosítjuk, hogy a naptár teljesen kiegyensúlyozott legyen (ha külön futtatnánk a 4. fejezetet)
min_date   <- min(base_df$date, na.rm = TRUE)
max_date   <- max(base_df$date, na.rm = TRUE)
full_dates <- seq(min_date, max_date, by = "day")

base_complete <- base_df |>
  complete(nesting(country, city), date = full_dates)

# Ha a complete() miatt bárhol NA keletkezett volna az is_workday-ben, a mentett ünnepnapokból pótoljuk:
if (any(is.na(base_complete$is_workday))) {
  holidays_saved <- read_csv("data/holidays_raw.csv", show_col_types = FALSE) |>
    distinct() |>
    mutate(holiday_flag = TRUE)
  
  base_complete <- base_complete |>
    select(-is_workday) |>
    left_join(holidays_saved, by = c("country" = "country", "date" = "holiday_date")) |>
    mutate(
      is_holiday = ifelse(is.na(holiday_flag), FALSE, TRUE),
      is_weekend = lubridate::wday(date, week_start = 1) %in% c(6, 7),
      is_workday = ifelse(is_weekend | is_holiday, 0, 1)
    ) |>
    select(-is_holiday, -is_weekend, -holiday_flag)
}

# --- 4.2 Táblák előkészítése és összekapcsolása (Left Join) ---
weather_clean <- weather_df |>
  select(country, city, date, temp_celsius, precip_mm, wind_ms, humidity_percent)

stations_clean <- stations_df |>
  rename(stations = stations_github)

panel_raw <- base_complete |>
  left_join(weather_clean,  by = c("country", "city", "date")) |>
  left_join(features_df,    by = c("country", "city")) |>
  left_join(elevation_df,   by = c("country", "city")) |>
  left_join(stations_clean, by = c("country", "city")) |>
  left_join(pop_df,         by = c("country", "city"))


# --- 4.3 Ellenőrzések és adatminőségi összefoglaló (Summary) ---

# 1. Sorok számának ellenőrzése
expected_rows <- n_distinct(panel_raw$city) * length(full_dates)
actual_rows   <- nrow(panel_raw)
cat("Várt sorok száma:", expected_rows, "| Tényleges sorok száma:", actual_rows, "")

# 2. Duplikációk ellenőrzése
duplicates <- panel_raw |> count(country, city, date) |> filter(n > 1)
cat("Duplikált (ország, város, nap) sorok száma:", nrow(duplicates), "")

# 3. NA-k száma változónként
na_vars <- colSums(is.na(panel_raw))
print(na_vars[na_vars > 0])

# 4. NA-k száma városonként (Top 10 leginkább hiányos város)
na_by_city <- panel_raw |>
  group_by(country, city) |>
  summarise(across(everything(), ~sum(is.na(.))), .groups = "drop") |>
  rowwise() |>
  mutate(total_na = sum(c_across(where(is.numeric)))) |>
  filter(total_na > 0) |>
  arrange(desc(total_na)) |>
  select(country, city, total_na, trips, everything())

print(head(na_by_city, 10))


# --- 4.4 Hibás / hiányos városok kiszűrése ---

# A) Az 5 város kiszűrése, ahol nem sikerült OSM / városhatár adatot lekérni
cities_to_remove_osm <- head(na_by_city, 5) |> select(country, city)
panel_raw <- panel_raw |>
  anti_join(cities_to_remove_osm, by = c("country", "city"))

# B) 2 város kiszűrése a kiugróan nagy (hibás közigazgatási) területe miatt
# Ellenőrző lista a legnagyobb területű városokról:
cdat_check <- panel_raw |> distinct(city, .keep_all = TRUE)
print(
  cdat_check |>
    arrange(desc(area_km2)) |>
    select(city, country, area_km2, population, stations) |>
    head(6)
)

kizart_varosok_terulet <- panel_raw |>
  distinct(city, .keep_all = TRUE) |>
  slice_max(area_km2, n = 2) |>
  select(city, country, area_km2, population)


panel_raw <- panel_raw |>
  filter(!city %in% kizart_varosok_terulet$city)

# [BIZTONSÁGI MENTÉS]: Nyers (teljes naptáras, de városszinten tisztított) panel adatbázis
write_excel_csv(panel_raw, "data/panel_raw.csv")


# ==============================================================================
# 5. FEJEZET: ADATÁTALAKÍTÁS, IDŐSZAKI SZŰRÉS ÉS VÉGSŐ PANEL ELŐÁLLÍTÁSA
# ==============================================================================
# A nyers (városnap-szintű) adatbázisból elemzésre kész panel készítése:
#   - származtatott (normalizált) változók a tanulmány definíciói szerint
#   - mintaszűrés: hiányzó / nulla utazásszám ÉS globálisan hiányos (<10%) napok kizárása
#   - within / between bontás az időben változó (időjárási) változókra a VÉGSŐ mintán
# ==============================================================================

# --- 5.1 Beolvasás és származtatott változók létrehozása ---
raw_panel <- read_csv("data/panel_raw.csv", show_col_types = FALSE)

df <- raw_panel |>
  mutate(
    bike_path_density    = bike_path_km / area_km2,
    station_density      = stations / area_km2,
    mobility_poi_density = mobility_poi / area_km2,
    other_poi_density    = other_poi / area_km2,
    bike_path_proportion = bike_path_km / (bike_path_km + car_road_km + pedestrian_km),
    log_population       = log(population),
    month                = factor(format(date, "%m")),
    t_index              = as.integer(date - min(date)) + 1L
  )

# --- 5.2 Mintaszűrés (Érvényes utazásszámok és <10%-os lefedettségű idősávok) ---

# 1. lépés: Hiányzó vagy nulla utazásszámú napok kiszűrése (log(0) nem értelmezhető)
df_valid <- df |>
  filter(!is.na(trips), trips > 0) |>
  mutate(log_trips = log(trips))

# 2. lépés: A hiányzó napok elemzése alapján kivesszük azokat az idősávokat, amikor
# a városok kevesebb mint 10%-ánál van csak adat. Ez 53 napot jelent (4 összefüggő blokkban):
#   blokk   kezd        vege        napok
#    1    2022-08-01  2022-08-25     25
#    2    2022-09-18  2022-09-18      1
#    3    2023-05-06  2023-05-16     11
#    4    2023-07-16  2023-07-31     16
# a kéősbbi ábra alapján (6.3.2)
n_varos   <- n_distinct(df_valid$city)
kozos_nap <- df_valid |>
  count(date) |>
  filter(n < 0.1 * n_varos) |>
  pull(date)

cat("Kizárt (<10%-os lefedettségű) napok száma:", length(kozos_nap), "    ")
cat("Kizárt városnapok száma:", sum(df_valid$date %in% kozos_nap),
    "(", round(100 * mean(df_valid$date %in% kozos_nap), 1), "%)") # 212 (1%)

kizart_idosavok_tbl <- data.frame(date = sort(kozos_nap)) |>
  mutate(blokk = cumsum(c(TRUE, diff(date) > 1))) |>
  group_by(blokk) |>
  summarise(kezd = min(date), vege = max(date), napok = n(), .groups = "drop")

print(kizart_idosavok_tbl)

# Kiszűrjük a <10%-os lefedettségű napokat
df_final <- df_valid |>
  filter(!date %in% kozos_nap)


# --- 5.3 Within / Between bontás az időjárási változókra ---
df_final <- df_final |>
  group_by(city) |>
  mutate(
    temp_b   = mean(temp_celsius),     temp_w   = temp_celsius - temp_b,
    precip_b = mean(precip_mm),        precip_w = precip_mm - precip_b,
    wind_b   = mean(wind_ms),          wind_w   = wind_ms - wind_b,
    humid_b  = mean(humidity_percent), humid_w  = humidity_percent - humid_b
  ) |>
  ungroup()

# Ellenőrzés: a within részek városonkénti átlaga 0 kell legyen (numerikus hibahatáron belül)
cat("Within (temp_w) városonkénti átlagának maximuma:",
    max(abs(tapply(df_final$temp_w, df_final$city, mean))), "
")


# --- 5.4 Végső elemzési panel (panel_data.csv) rendezése és kimentése ---
panel_data <- df_final |>
  select(
    country, city, date, t_index, month, is_workday, 
    trips, log_trips,
    # időjárás (nyers)
    temp_celsius, precip_mm, wind_ms, humidity_percent,
    # within / between bontás
    temp_w, precip_w, wind_w, humid_w,
    temp_b, precip_b, wind_b, humid_b,
    # városszintű mutatók
    population, log_population, area_km2, stations,
    bike_path_density, station_density, mobility_poi_density, other_poi_density,
    bike_path_proportion, elevation_diff_m, mean_slope_deg
  ) |>
  arrange(city, date)

# [BIZTONSÁGI MENTÉS]: Végső, elemzésre kész panel adatbázis
write_excel_csv(panel_data, "data/panel_data.csv")


# ==============================================================================
# 6. FEJEZET: LEÍRÓ STATISZTIKÁK ÉS VIZUALIZÁCIÓK (ÁBRÁK)
# ==============================================================================

# --- 6.1 Leíró statisztikák a végső mintán ---
panel_data <- read_csv("data/panel_data.csv", show_col_types = FALSE)

cat("--- IDŐBEN VÁLTOZÓ (NAPI) VÁLTOZÓK LEÍRÓ STATISZTIKÁJA ---")
print(summary(panel_data[c("trips", "temp_celsius", "precip_mm", "wind_ms", "humidity_percent", "is_workday")]))

cdat <- panel_data |> distinct(city, .keep_all = TRUE)

cat("--- VÁROSSZINTŰ (IDŐBEN ÁLLANDÓ) VÁLTOZÓK LEÍRÓ STATISZTIKÁJA ---")
print(summary(cdat[c("population", "area_km2", "bike_path_density", "station_density",
                     "mobility_poi_density", "other_poi_density", "elevation_diff_m", "mean_slope_deg")]))

cat("--- LEGNAGYOBB TERÜLETŰ VÁROSOK (ELLENŐRZÉS A SZŰRÉS UTÁN) ---")
print(cdat |> arrange(desc(area_km2)) |> select(city, country, area_km2, population, stations, bike_path_density, station_density) |> head(6))

cat("--- LEGKISEBB NÉPESSÉGŰ VÁROSOK ---")
print(cdat |> arrange(population) |> select(city, country, population, area_km2, stations) |> head(6))

cat("--- VÁROSOK SZÁMA ORSZÁGONKÉNT ---")
country_counts <- cdat |> count(country, sort = TRUE)
print(country_counts)


# --- 6.2 1. Ábra: Európa-térkép a vizsgált városokkal ---
if (file.exists("data/city_coords.csv")) {
  coords_lookup <- read_csv("data/city_coords.csv", show_col_types = FALSE)
  plot1_data <- panel_data |>
    distinct(country, city) |>
    left_join(coords_lookup, by = c("country", "city"))
} else {
  plot1_data <- panel_data |>
    distinct(country, city) |>
    mutate(coords = map2(city, country, possibly(get_coords, tibble(lat = NA_real_, lon = NA_real_)))) |>
    unnest(coords)
  write_excel_csv(plot1_data, "data/city_coords.csv")
}

world_map <- map_data("world")

plot_map <- ggplot() +
  # Alaptérkép megrajzolása
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray85", color = "white", linewidth = 0.2) +
  # Városok pontjainak felvitele
  geom_point(data = plot1_data, aes(x = lon, y = lat), 
             color = "darkred", size = 2, alpha = 0.8) +
  # Európára való fókuszálás (kivágat)
  coord_quickmap(xlim = c(-12, 35), ylim = c(36, 68)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        panel.grid.major = element_line(color = "white", linetype = "dotted"))

print(plot_map)


# --- 6.3 2. Ábra: Hiányzó napok hőtérképe (Heatmap) ---
# Adat beolvasása a nyers panelből (ahol az NA utazásszámok még látszanak) és dátum formázása
plot2_data <- read_csv("data/panel_raw.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date))

plot_heatmap <- plot2_data |>
  mutate(is_missing = ifelse(is.na(trips), "Hiányzó adat (NA)", "Rendelkezésre áll")) |>
  ggplot(aes(x = date, y = reorder(city, desc(city)), fill = is_missing)) +
  geom_tile() +
  scale_fill_manual(values = c("Hiányzó adat (NA)" = "darkred", 
                               "Rendelkezésre áll" = "gray85")) +
  labs(x = "Dátum", y = "Város", fill = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5), # Kisebb betűméret a ~80 város miatt
        legend.position = "top",
        panel.grid = element_blank())

print(plot_heatmap)
