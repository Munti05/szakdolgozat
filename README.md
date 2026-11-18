# Kerékpármegosztó rendszerek (BSS) használatára való hajlandóság vizsgálata európai városokban

**Szakdolgozat – Muntean Áron** · Budapesti Corvinus Egyetem, Adatelemzés és Informatika Intézet, Statisztika Tanszék · Gazdaságinformatikus BSc, 2026 · Konzulens: Keresztély Tibor

Ez a repository a szakdolgozat **teljes számítási hátterét** tartalmazza: az adatgyűjtő (ETL) scriptet, a modellező scriptet, a nyers és köztes adatfájlokat, valamint az ábrákat. A cél, hogy a dolgozatban szereplő minden adat, számítás és ábra **visszakövethető és újrafuttatás nélkül is ellenőrizhető** legyen.

---

## Tartalomjegyzék

1. [A kutatás röviden](#1-a-kutatás-röviden)
2. [Adatforrások és köszönet](#2-adatforrások-és-köszönet)
3. [A repository szerkezete](#3-a-repository-szerkezete)
4. [Gyorsindítás: melyik fájlt nyissam meg?](#4-gyorsindítás)
5. [ETL folyamat (`R/etl.R`)](#5-etl-folyamat-retlr)
6. [Modellezés (`R/modellezes.R`)](#6-modellezés-rmodellezesr)
7. [A végső panel változói (`panel_data.csv`)](#7-a-végső-panel-változói-panel_datacsv)
8. [Teljes újrafuttatás lépésről lépésre](#8-teljes-újrafuttatás-lépésről-lépésre)
9. [Ismert korlátok](#9-ismert-korlátok)
10. [Licenc és hivatkozás](#10-licenc-és-hivatkozás)

---

## 1. A kutatás röviden

**Kérdés:** mely külső tényezők befolyásolják egy városi kerékpármegosztó rendszer (BSS) napi használatát?

**Minta:** 78 európai város (12 ország, közép-európai súlypont: Németország, Lengyelország, Csehország), kiegyensúlyozatlan panel, kb. 2022. 08. 26. – 2023. 07. 15. között. A mintába azok a városok kerültek, ahol a **nextbike** volt az egyetlen BSS-szolgáltató (monopolhelyzet), kiegészítve négy egyénileg gyűjtött várossal: **Budapest, Bergen, Oslo, Trondheim**.

**Függő változó:** a napi utazásszám természetes logaritmusa (`log_trips`), így az együtthatók közelítőleg százalékos hatásként értelmezhetők.

### Kutatási kérdések és hipotézisek

| Kérdés | Hipotézis (H0 / H1) | Irány |
|---|---|---|
| **K1** – Magasabb használatot eredményez-e a jobb kerékpáros infrastruktúra? | H1₀: nincs hatás · H1₁: a nagyobb kerékpárút-sűrűség nagyobb használatot eredményez | egyoldali (>) |
| **K2** – Hogyan függ össze a használat a BSS saját ellátottságával és a tömegközlekedési csomópontok sűrűségével? | H2₀/H2₁: BSS állomássűrűség · H3₀/H3₁: tömegközlekedési csomópontok sűrűsége | egyoldali (>) |
| **K3** – Csökkenti-e a kerékpáros infrastruktúra a kedvezőtlen időjárás visszafogó hatását? | H4₀: az infrastruktúra nem módosítja az időjárás hatását · H4₁: csökkenti (cross-level interakció) | csapadék, szél: egyoldali (>); hőmérséklet: kétoldali |

### Módszertan

- **REWB (Random-Effects Within-Between, Bell & Jones, 2015)** modell: az időben változó (időjárási) változók városon belüli (`_w`) és városok közötti (`_b`) részre bontva szerepelnek. Így a napi hatások a fix hatású becslésnek megfelelően tiszták, miközben az időben állandó városszintű változók (infrastruktúra) is becsülhetők.
- **Városszintű klaszterezett (Arellano-típusú, HC1) standard hibák**, t-eloszlással, `G−1` szabadságfokkal.
- **Előre rögzített irányú egyoldali tesztek** H1–H3-ra, kétoldali teszt a hőmérséklet-interakcióra.
- Fontos: az infrastruktúra-hatások (H1–H3) **asszociációk, nem ok-okozati összefüggések**, és csak **78 városszintű megfigyelésre** épülnek – ez a fő statisztikai korlát.

---

## 2. Adatforrások és köszönet

Az adatgyűjtés két nyilvános GitHub-projektre épül, amelyek meghatározó szerepet játszottak a kutatásban:

### [TUMFTM/european-bike-sharing-dataset](https://github.com/TUMFTM/european-bike-sharing-dataset/)
A Müncheni Műszaki Egyetem (TUM) és az Edinburgh-i Egyetem kutatócsoportjának európai nextbike-adatbázisa (Waldner et al., 2025; CC BY-NC 4.0). Ebből származik: az **utazásszámok** (`trips.csv`), a városlista és azonosítók (`cities.csv`) és az **állomások** (`stations.csv`).
> A repóból az adatokat letöltöttem, és a `european-bike-sharing-dataset/` mappába helyeztem. **A kód futtatásához ugyanilyen mappastruktúrát kell kialakítani** (lásd [3. szakasz](#3-a-repository-szerkezete) és [8. szakasz](#8-teljes-újrafuttatás-lépésről-lépésre)).

### [MobilityData/gbfs](https://github.com/MobilityData/gbfs/tree/master)
A GBFS (General Bikeshare Feed Specification) rendszerkatalógusa (`systems.csv`). A teljes **verziótörténetét** a GitHub API-n keresztül kérdeztem le, hogy a vizsgált időszak minden állapotában azonosítani lehessen, mely városokban működött egyedüli szolgáltatóként a nextbike (monopolszűrés).

### További források
 
| Adat | Forrás | Hol használja a kód |
|---|---|---|
| Napi időjárás (átlaghőmérséklet, csapadék, szél, páratartalom) és koordináták | [Open-Meteo](https://open-meteo.com/) Archive és Geocoding API | ETL 2.1–2.2 |
| Munkanapok / ünnepnapok | [Nager.Date](https://date.nager.at/) API | ETL 2.3 |
| Városhatár-poligonok | [Nominatim (OpenStreetMap)](https://nominatim.openstreetmap.org/) | ETL 3.2 |
| Kerékpárút, úthálózat, POI-k (2023. 01. 31-i állapot) | [ohsome API](https://api.ohsome.org/) (OSM) | ETL 3.3 |
| Domborzat (szintkülönbség, átlagos lejtés) | `elevatr` (AWS terrain tiles) → `terra` | ETL 3.4 |
| Népesség | GeoNames, Huwise open-data portál (Excelben szűrve, 7 város kézzel pótolva) | `data/00Letoltott/` |
| Budapest napi utazásszám, állomásszám | BKK (nyilvános adatok) | `data/Budapest/` |
| Bergen utazásai | [Bergen Bysykkel – historical open data](https://bergenbysykkel.no/en/open-data/historical) (NLOD 2.0) | `data/Bergen/` (a repóban üres, kézzel kell letölteni) |
| Oslo utazásai | [Oslo Bysykkel – historical open data](https://oslobysykkel.no/en/open-data/historical) (NLOD 2.0) | `data/Oslo/` (a repóban üres, kézzel kell letölteni) |
| Trondheim utazásai | [Trondheim Bysykkel – historical open data](https://trondheimbysykkel.no/en/open-data/historical) (NLOD 2.0) | `data/Trondheim/` (a repóban üres, kézzel kell letölteni) |
 
A pontos hivatkozások (Adat1–Adat12) a szakdolgozat irodalomjegyzékének *Adatok* fejezetében találhatók.
---

## 3. A repository szerkezete
 
```
.
├── R/
│   ├── etl.R                      # Adatgyűjtés, -tisztítás, panel előállítása, leíró ábrák
│   └── modellezes.R               # REWB modellek, tesztek, robusztussági vizsgálatok
├── data/
│   ├── 00Letoltott/               # Manuálisan letöltött adatok (népesség)
│   ├── Budapest/                  # Budapest napi utazásszámai
│   ├── Bergen/                    # ÜRES mappa – az utazási adatokat a teljes futtatáshoz le kell tölteni (lásd 8/B)
│   ├── Oslo/                      # ÜRES mappa – ugyanígy
│   ├── Trondheim/                 # ÜRES mappa – ugyanígy
│   └── outputs/                   # Az ETL script futás közben kimentett összes CSV-je
│       └── panel_data.csv         # ← a modellezés bemenete (végső panel)
├── european-bike-sharing-dataset/ # TUMFTM adatbázis letöltött másolata (lásd 2. szakasz)
│   └── full/
│       ├── trips.csv
│       ├── cities.csv
│       └── stations.csv
├── plots/                         # A kód által készített 3 ábra
│                                  #   (Európa-térkép, hiányzó adatok hőtérképe, leave-one-out hisztogram)
├── figure/                        # Az ETL folyamatot bemutató ábra (draw.io)
└── README.md
```
 
> **Megjegyzés a norvég városokról:** a `Bergen/`, `Oslo/` és `Trondheim/` mappák a repóban üresek (csak a mappastruktúra miatt vannak itt). Ezek csak az ETL teljes újrafuttatásához kellenek; a feldolgozott eredményük már benne van a `data/outputs/` fájlokban (pl. `usage_raw_combined.csv`, `panel_data.csv`).
>

> **Megjegyzés a méretről:** a `trips.csv` több gigabájtos, ezért a GitHub-méretkorlát miatt nincs a repóban. A [TUMFTM repóból](https://github.com/TUMFTM/european-bike-sharing-dataset/) kell letölteni és a fenti helyre tenni. **Ha csak a modellezés érdekel, erre nincs szükség**, mert az eredmények a `data/outputs/` mappából reprodukálhatók.

---

## 4. Gyorsindítás

| Mit szeretnék? | Mit kell hozzá? |
|---|---|
| **Csak a modellezést / eredményeket** reprodukálni | `R/modellezes.R` + `data/outputs/panel_data.csv` |
| Egy **köztes adatlépést** ellenőrizni (pl. időjárás, városhatárok, infrastruktúra) | a megfelelő CSV a `data/outputs/` mappából (lásd alább) |
| A **teljes adatgyűjtést** újrafuttatni | `R/etl.R` + a nyers adatmappák + `european-bike-sharing-dataset/` + internet (API-k) |

A `data/outputs/` mappa az ETL script **biztonsági mentései**. Azért töltöttem fel őket, hogy az adatgyűjtés reprodukálhatósága ne az API-k állandó elérhetőségén múljon (az API-k válaszai idővel változhatnak, egyes szolgáltatások lekérdezései lassúak és sok órát vesznek igénybe).

| Fájl (`data/outputs/`) | Tartalom | ETL szakasz |
|---|---|---|
| `gbfs_commits.csv` | GBFS `systems.csv` commitlista a vizsgált időszakra | 1.1 |
| `usage_raw_combined.csv` | Napi utazásszámok (nextbike + egyéni városok), geokódolás előtt | 1.6 |
| `usage_daily.csv` | Tisztított napi utazásszámok | 2.1 |
| `cities.csv` | A végső városlista | 2.1 |
| `city_coords.csv` | Városok koordinátái (Open-Meteo Geocoding) | 2.1 |
| `weather_daily.csv` | Napi időjárási adatok városonként | 2.2 |
| `holidays_raw.csv` | Ünnepnapok országonként (Nager.Date) | 2.3 |
| `usage_and_workdays.csv` | Utazásszámok + munkanap-jelző | 2.3 |
| `stations_from_github.csv` | Állomásszámok városonként | 3.1 |
| `city_boundaries.csv` | Városhatár-poligonok (GeoJSON) és területek (km²) | 3.2 |
| `city_features.csv` | Kerékpárút-, úthálózat- és POI-mutatók | 3.3 |
| `elevation.csv` | Szintkülönbség és átlagos lejtés | 3.4 |
| `panel_raw.csv` | Összefűzött, városszinten tisztított, teljes naptáras panel | 4 |
| **`panel_data.csv`** | **Végső elemzési panel (a modellezés bemenete)** | 5 |

(A GBFS-snapshotok – `gbfs_snapshots/*.csv` – a `systems.csv` egyes commitjainak letöltött másolatai.)

---

## 5. ETL folyamat (`R/etl.R`)

Az ETL folyamat vázlatos ábrája a `figure/` mappában található (draw.io). A script fejezetei:

| Fejezet | Tartalom |
|---|---|
| **0** | Környezet, csomagok, globális paraméterek (`START_DATE = 2022-08-01`, `END_DATE = 2023-07-31`, `OSM_DATE = 2023-01-31`) |
| **1** | GBFS commitok lekérése; **nextbike-monopóliumú városok** azonosítása; utazásszámok napi aggregálása (nextbike dataset); Budapest, Bergen, Oslo, Trondheim utazásainak beolvasása |
| **2** | Koordináták, időjárás (Open-Meteo), munkanap-jelző (Nager.Date) |
| **3** | Állomásszámok, városhatárok (Nominatim), infrastruktúra (ohsome/OSM), domborzat |
| **4** | Nyers panel összefűzése; hibás városok kiszűrése |
| **5** | Származtatott változók, mintaszűrés, within/between bontás → `panel_data.csv` |
| **6** | Leíró statisztikák és ábrák (Európa-térkép, hiányzó adatok hőtérképe) |

### Mintaszűrés lépései (a 78 város és az érvényes napok meghatározása)

1. **Monopolszűrés:** egy város csak akkor kerül be, ha a GBFS-katalógus *minden* vizsgált állapotában szerepel nextbike, és *egyetlen* állapotban sincs mellette más szolgáltató.
2. **Aktív napok szűrése:** a nextbike-adatban csak azok a városok maradnak, ahol az aktív napok száma meghaladja az eloszlás alsó kvartilisét.
3. **Koordináta nélküli „városok” kizárása (10 db):** régiók / egyetemi rendszerek, pl. Bergisches e-Bike, RSVG, RVK e-Bike, Eifel e-Bike, Ibiza-City, Berounsko, Swansea University, Rychnovsko, Winsen (Luhe), Brunel University.
4. **OSM-adat nélküli városok kizárása (5 db):** ahol a városhatár / OSM-lekérdezés nem sikerült (a legtöbb hiányzó értékkel rendelkező 5 város).
5. **Kiugró területű városok kizárása (2 db):** hibás (régiós léptékű) közigazgatási határ miatt → **80 városból 78 maradt**.
6. **Hiányzó / nulla utazásszámú városnapok kiszűrése** (a `log(0)` nem értelmezhető).
7. **<10%-os lefedettségű napok kizárása:** azok a napok, amikor a városok kevesebb mint 10%-ánál van adat (4 összefüggő blokk: 2022-08-01 – 08-25, 2022-09-18, 2023-05-06 – 05-16, 2023-07-16 – 07-31; ~1% városnap). A kizárt napokat a script kiírja, az indoklást a hiányzó adatok hőtérképe (`plots/`) adja.

### Származtatott változók

| Változó | Képlet |
|---|---|
| `bike_path_density` | kerékpárút hossza (km) / városterület (km²) |
| `station_density` | BSS-állomások száma / városterület (km²) |
| `mobility_poi_density` | tömegközlekedési csomópontok (buszmegálló, vasút-, villamos-, metróállomás) / km² |
| `other_poi_density` | egyéb POI-k (oktatás, vendéglátás, bolt, iroda, turisztikai célpont, park) / km² |
| `bike_path_proportion` | kerékpárút / (kerékpárút + autóút + gyalogos úthálózat) – robusztussági mérőszám |
| `log_population` | a népesség természetes logaritmusa |
| `temp_w`, `precip_w`, `wind_w`, `humid_w` | időjárási változó eltérése a város saját időszaki átlagától (within) |
| `temp_b`, `precip_b`, `wind_b`, `humid_b` | a város időszaki átlaga (between) |

Az OSM-szűrők pontos definíciója az `R/etl.R` 3.3 szakaszában (`indicators` tábla) található.

---

## 6. Modellezés (`R/modellezes.R`)

A script bemenete a `panel_data.csv`. Szakaszai:

| Szakasz | Tartalom |
|---|---|
| **0** | Beolvasás, városszintű változók z-standardizálása (`*_z`, az interakciós modellhez), hét napja és hónap faktorok, panelstruktúra |
| **Segédfüggvények** | `tidy_cl()` – klaszterezett SE-vel számolt együtthatótábla (t, kétoldali p, `G−1` szabadságfok); `add_one_sided()` – előre rögzített irányú egyoldali p-értékek |
| **1** | VIF-teszt (multikollinearitás) |
| **2** | Modellválasztó és diagnosztikai tesztek: Hausman, Breusch–Godfrey, reziduum AR(1) |
| **3** | REWB alapmodell, Wald-féle szűkítési tesztek (klaszterezett vcov-val) |
| **4** | Végleges modellek: **M2** (főmodell H1–H3), **M3** (interakciók, H4); R², hipotézisek tesztelése |
| **5** | Robusztusság: **leave-one-out** (78 újrabecslés) és **R2** modell (kerékpárút-arány a sűrűség helyett) |

### Modellek

- **M2 (főmodell, K1–K2):** `log_trips ~ temp_w + precip_w + wind_w + humid_w + is_workday + temp_b + bike_path_density + station_density + mobility_poi_density + other_poi_density + log_population + mean_slope_deg + hónap + hét napja` (REWB, `plm`, `random.method = "walhus"`).
- **M3 (K3):** az M2 z-standardizált infrastruktúra-változókkal, kiegészítve a `precip_w`, `wind_w`, `temp_w` × `bike_path_density_z` interakciókkal.
- **R2 (robusztusság):** mint az M2, de `bike_path_proportion` a `bike_path_density` helyett.

---

## 7. A végső panel változói (`panel_data.csv`)

| Csoport | Változók |
|---|---|
| Azonosítók | `country`, `city`, `date`, `t_index`, `month` |
| Napi változók | `is_workday`, `trips`, `log_trips` |
| Nyers időjárás | `temp_celsius`, `precip_mm`, `wind_ms`, `humidity_percent` |
| Within / between | `temp_w`, `precip_w`, `wind_w`, `humid_w`, `temp_b`, `precip_b`, `wind_b`, `humid_b` |
| Városszintű (időben állandó) | `population`, `log_population`, `area_km2`, `stations`, `bike_path_density`, `station_density`, `mobility_poi_density`, `other_poi_density`, `bike_path_proportion`, `elevation_diff_m`, `mean_slope_deg` |

Egy sor = egy város egy napja. Kiegyensúlyozatlan panel.

---

## 8. Teljes újrafuttatás lépésről lépésre

**Szükséges R-csomagok**

```r
install.packages(c(
  # ETL
  "maps", "sf", "elevatr", "terra", "httr2", "jsonlite", "tidygeocoder",
  "data.table", "readr", "tibble", "stringr", "stringi", "lubridate",
  "tidyr", "purrr", "dplyr", "ggplot2",
  # Modellezés
  "plm", "lmtest", "sandwich", "car"
))
```

**A) Csak a modellezés**

1. Klónozd a repót, és állítsd a munkakönyvtárat a repo gyökerére.
2. A `modellezes.R` az elején a `data/panel_data.csv` fájlt olvassa be (és `setwd()` is szerepel benne, amit a saját elérési utadra kell átírni). A fájl a repóban a `data/outputs/panel_data.csv` helyen található – másold át a `data/` mappába, vagy módosítsd az elérési utat a script elején.
3. Futtasd az `R/modellezes.R`-t.

**B) A teljes adatgyűjtés újrafuttatása**
 
1. Töltsd le a [TUMFTM adatbázist](https://github.com/TUMFTM/european-bike-sharing-dataset/), és a `trips.csv`, `cities.csv`, `stations.csv` fájlokat tedd a `european-bike-sharing-dataset/full/` mappába.
2. A kézzel gyűjtött adatok közül a **népesség** és a **Budapest** adatai a repóban megvannak, ezeket a szükséges helyre kell tenni (`data/population.csv`, `data/Budapest/202208-202307.csv`). Az ETL ezeket így várja.
3. **Bergen, Oslo és Trondheim:** a repóban a `data/Bergen/`, `data/Oslo/` és `data/Trondheim/` mappák üresek. A teljes futtatáshoz töltsd le a **2022. augusztus – 2023. július** közötti időszak havi utazási adatait (CSV) a szolgáltatók nyilvános adatoldaláról, és tedd az adott város mappájába:
   - Bergen: <https://bergenbysykkel.no/en/open-data/historical>
   - Oslo: <https://oslobysykkel.no/en/open-data/historical>
   - Trondheim: <https://trondheimbysykkel.no/en/open-data/historical>
   Az adatok licence: [Norwegian Licence for Open Government Data (NLOD) 2.0](https://data.norge.no/nlod/no/2.0); szolgáltató: Bergen / Oslo / Trondheim Bysykkel (UIP Bauer Media Outdoor Norge AS). A letöltött fájlokat nem kell átnevezni vagy összefűzni: a script a mappa összes `.csv` fájlját beolvassa, és a `started_at` oszlopból számol napi utazásszámot (a vizsgált időszakon kívüli napokat kiszűri). Trondheimből az eredeti kutatásban is hiányzott két hónap.
4. *(Opcionális)* GitHub Personal Access Token a rate limit elkerülésére: `Sys.setenv(GITHUB_PAT = "...")`.
5. Írd át a `setwd()` sort a script elején, majd futtasd az `R/etl.R`-t. Az ETL az eredményeit `data/` alá menti (ezek a repóban a `data/outputs/` alatt vannak).
> ⚠️ **Elérési utak.** A scriptek jelenleg a `data/…` útvonalat használják a köztes CSV-kre, a repóban viszont ezek a `data/outputs/` alatt vannak. Ha a scriptet változtatás nélkül futtatod, vagy másold a fájlokat a `data/` mappába, vagy írd át az útvonalakat.
>
> ⚠️ **Az API-k válaszai változhatnak** (pl. a Nominatim poligonjai, az Open-Meteo koordináták). A lekérdezések lassúak (a szkript szándékosan vár a kérések között). Pontos egyezés a `data/outputs/` fájlokból garantált, az újralekérdezésnél kisebb eltérések lehetnek.
 
---

## 9. Ismert korlátok

- **78 városszintű megfigyelés** a H1–H3 hipotézisekhez; a hatások asszociációk, nem ok-okozatiak.
- A nextbike-monopóliumos minta **közép-európai súlypontú**, közepes-nagy városokat tartalmaz; nagy, többszolgáltatós világvárosokra nem általánosítható.
- Az **állomásszám egy időpont pillanatfelvétele**, nem a kerékpár- vagy dokkolószám; az állomások kapacitása eltérhet.
- A **városhatárok** különbözőek (belváros vs. teljes település), ami a területre normalizált mutatókat torzíthatja.
- **E-kerékpár és hagyományos kerékpár nem különíthető el** az adatokban.
- Rövid (≈ 1 év) időszak: az eredmények szezonálisan csak erre az időszakra általánosíthatók.

---

## 10. Licenc és hivatkozás

- A **TUMFTM european-bike-sharing-dataset** a [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/) licenc alatt érhető el: kereskedelmi célra nem használható, hivatkozás kötelező. Az onnan származó adatokra ez a licenc vonatkozik.
- Az OpenStreetMap-adatokra az ODbL licenc vonatkozik (© OpenStreetMap contributors).

**Hivatkozás:** Muntean Áron (2026): *Kerékpármegosztó rendszerek (Bike-Sharing Systems) használatára való hajlandóság vizsgálata Európai városokban.* Szakdolgozat, Budapesti Corvinus Egyetem.
