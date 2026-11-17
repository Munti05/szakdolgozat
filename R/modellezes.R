# =============================================================================
# Modell: Random-effects Within-Between (REWB), klaszterezett SE-vel.
#
# K1: A városi kerékpáros infrastruktúra jobb kiépítettsége magasabb használatot eredményez a kerékpár-megosztó rendszernek?
#   H10: A kerékpáros infrastruktúrának nincs hatása a napi használatra.
#   H11: A nagyobb kerékpáros infrastruktúra nagyobb használatot eredményez.
# K2: Hogyan függ össze a kerékpár-megosztó rendszernek a napi használata a saját ellátottságával és a tömegközlekedési csomópontok sûrûségével?
#   H20: A BSS kiépítettsége nem befolyásolja a napi használatot
#   H21: A jobban kiépített BSS növeli a napi használatot
#
#   H30: A tömegközlekedés kiépítettsége nem befolyásolja a napi használatot.
#   H31: A jobban kiépített tömegközlekedés növeli a napi használatot.
# K3: Csökkenti-e a kiépített kerékpáros infrastruktúra a kedvezõtlen idõjárásnak a használatot visszafogó hatását?
#   H40: A városi infrastruktúra nem módosítja az idõjárás hatását a napi használatra nézve.
#   H41: A jobban kiépített városi infrastruktúra csökkenti az idõjárásnak a napi használatra vonatkozó hatást.
# ==============================================================================

# ==============================================================================
# ÁTTEKINTÉS

# Adat
# 78 város (i) x legfeljebb 365 nap (t), kiegyensúlyozatlan panel. Függõ változó:
# log(napi utazásszám). A log miatt az együtthatók közelítõleg százalékos hatások:
# egy b együttható kb. 100*(exp(b)-1) százalékos változást jelent.
#
# A fõ kérdés
#   1) A városok között: az infrastruktúra városszintû és idõben állandó -> H1-H3
#   Ezek a hipotézisek 78 megfigyelésre épülnek, nem több ezer városnapra (ez a fõ statisztikai korlát).
#
#   2) A városokon belül: a napi idõjárás ingadozás (idõjárási hatások) és ennek a kölcsönhatása
#   az infrastruktúrával (cross-level interakció) ->H4
#
# Modellválasztás
#   - Fix hatású becslés: a városonkénti átlag kiejti az idõben állandó változókat (H1-H3 nem becsülhetõ).
#   - Pooled OLS: a városok állandó adottságait a hibatagba teszi és ez korrelálhat a magyarázóváltozókkal (+ a hibatagok városon belül valószínû erõsen korrelálnak).
#   - Random hatású becslés: egy közös együttható becsül az idõjárásra, pedig a városon belüli és városok közötti hatás különbözhet.
#   - REWB (Random-Effects Within-Between): az idõben váltzó X változókat felbontjuk és mindkét részt külön együtthatóval szerepltetjük.
#     Ez tartalmazza a random hatású és a fix hatású becslést is speciális esetként, ezért lehet jó választás.
#
#   Fontos korlát: feltesszük, hogy az infrastrutúra nem korrelál a városi random hatással a kontrollok mellett. E miatt a H1-H3
#   hipotézisek asszociációk, nem ok-okozati összefüggések.
#
# Következtetés
# A városszintû klaszterezett standard hibák (Arellano-típusú), t-eloszlással G-1 szabadságfokkal és elõre rögzített irányú egyoldali tesztek a H1-H4-re.
# ==============================================================================

setwd("C:/Users/aronm/0corvinus/7felev/szakdolgozat")

library(plm) # panelmodellek, panel-tesztek
library(lmtest) # általános tesztek
library(sandwich) # robosztus kovariancamátrixok
library(car) # vif(), linearHypothesis()
library(dplyr) 
library(readr)
library(tibble)

# ---- 0. Adat beolvasás és Hibrid változók elõállítása -------------------------
dat <- read_csv("data/panel_data.csv", show_col_types = FALSE) 

# Kiszámoljuk az infrastruktúra változók Z-standardizált (átlag=0, szórás=1) verzióit (az interakciós modellhez kellenek),
# de megtartjuk az eredeti nyers sûrûség oszlopokat is a fõhatások becsléséhez.
city_df <- dat |> 
  group_by(city) |> 
  slice(1) |> 
  ungroup() |>
  mutate(across(
    c(bike_path_density, station_density, mobility_poi_density, other_poi_density, 
      bike_path_proportion, log_population, elevation_diff_m, mean_slope_deg),
    ~ as.numeric(scale(.)), 
    .names = "{.col}_z" # Az új oszlopok kapnak egy _z végzõdést
  )) |>
  select(city, ends_with("_z"))

dat <- dat |> left_join(city_df, by = "city")

dat$month <- factor(dat$month)
dat$dow   <- factor(format(dat$date, "%u"))

# Panelstruktúra deklarálása: a megfigyelési egység a város (city), az idõdimenzió a nap (date)
pdat <- pdata.frame(dat, index = c("city", "date"))
G <- pdim(pdat)$nT$n # Klaszterek (városok) száma a szabadságfokhoz


# ---- Segédfüggvények a robusztus inferenciához --------------------------------
# Panel adatoknál a hibatagok egy városon belül korrelálnak (autokorreláció) és 
# a szórásuk is eltérhet (heteroszkedaszticitás). A vcovHC "HC1" típusú (kismintás szabadságfok korreláció),
# csoportra (városra) klaszterezett standard hibája ezt korrigálja, elkerülve a fals szignifikanciát.
# Amit visszaad, az egy tábla, amely tartalmazza az együtthatókat, a klaszterezett SE-ket, a t-értékeket és a kétoldali p-értékeket.
tidy_cl <- function(m) {
  V   <- vcovHC(m, type = "HC1", cluster = "group") # Kovarianciamátrix a városok szerint klaszterezve
  est <- coef(m)
  se  <- sqrt(diag(V)) # A kovarianci mátrix  átlójában az együtthatók varianciáji vannak, ezek négyzetgyöke az SE
  t   <- est / se
  tibble(term = names(est), estimate = est, se = se, t = t,
         p_two = 2 * pt(-abs(t), df = G - 1))
}
# Korlátai: feltételezi, hogy a városok függetlenek;a pontbecslés ugyanaz marad, csak a bizonytalanság becslése változik


# Egyoldali tesztelés a tanulmány specifikációja alapján (H1, H2, H3 irányítottak)
add_one_sided <- function(tab, hyp) {
  tab |>
    inner_join(hyp, by = "term") |>
    mutate(p_one = case_when(
      dir == ">"   ~ pt(t, df = G - 1, lower.tail = FALSE), # a t-eloszlás felsõ farka
      dir == "<"   ~ pt(t, df = G - 1, lower.tail = TRUE), # a t-eloszlás alsó farka
      TRUE         ~ p_two), # kétoldali teszt
      reject_5pct = p_one < 0.05) # 5%-os szinten döntünk a H0-ról
}


# ---- 1. VIF teszt (multikollinearitás) ----------------------------------------
# Megvizsgáljuk, hogy az infrastruktúra változók nem mozognak-e túlzottan együtt.
# Ha a VIF > 5 (vagy 10), akkor kollinearitás van, ami felfújja a standard hibákat.
infra_raw <- c("bike_path_density", "station_density", "mobility_poi_density", "other_poi_density")
ctrl_raw  <- c("log_population", "elevation_diff_m", "mean_slope_deg")

vif_fit <- lm(as.formula(paste("log_trips ~", paste(c(infra_raw, ctrl_raw), collapse = " + "))), data = dat)
cat("\n=== Multikollinearitás Ellenõrzés (VIF) ===\n")
print(round(car::vif(vif_fit), 2))

# A VIF = 1/(1-R2) azt mutatja, hányszorosára nõ egy együttható varianciája ahhoz
# képest, mintha a változó korrelálatlan lenne a többivel (a SE ennek gyökével nõ).
# Küszöb: 5 (óvatos), 10 (megengedõ). Lineáris átskálázás (z-érték) a VIF-et nem változtatja.
# Eredmény: minden érték < 10, két esetben jelentõsebb súlyos kollinearitás áll fent. Ezek az 
# elevation_diff_m (7.79) és mean_slope_deg (8.33). A többi esetben a VIF  értéke 5 alatt van.
# A szintkülönbséget kihagyjuk a modellezésbõl, mert csak erõsen korrelál az átlagos lejtésfokkal
# és egyik hipotézis sem vonatkozik ezekre.
# 78 városnál az infrastruktúrális változók közötti korreláció a fõ kockázat, mert nagyon megnövelheti a szórást.

# Kihagyjuk a modellezésbõl az elevation_diff_m-et, ugyanis nagyon együtt mozog a mean_slope_deg-el
ctrl_raw <- c("log_population", "mean_slope_deg")
vif_fit <- lm(as.formula(paste("log_trips ~", paste(c(infra_raw, ctrl_raw), collapse = " + "))), data = dat)
cat("\n=== Multikollinearitás Ellenõrzés (VIF) ===\n")
print(round(car::vif(vif_fit), 2))


# ---- 2. Modellválasztó és diagnosztikai tesztek (IDÕBEN VÁLTOZÓKRA) -----------
# Mivel a Fix Hatás (FE) a városszintû infrastruktúrát kiejtené, a teszteket 
# csak a napi szinten változó (idõjárás) változókon futtatjuk le.
f_tv <- log_trips ~ temp_celsius + precip_mm + wind_ms + humidity_percent + is_workday

m_pool_tv <- plm(f_tv, data = pdat, model = "pooling") # Egyszerû OLS, egyetlen közös konstans, a városi hatást a hibatagra teszi
m_fe_tv   <- plm(f_tv, data = pdat, model = "within")  # Fix hatás (FE), városonként levonja az átlagot, a városi állandókat kiszûri
m_re_tv   <- plm(f_tv, data = pdat, model = "random")  # Random hatás (RE), nagy T-nél lambda->1, vagyis megközeletíi a within becslést

# 1. Hausman-próba: Azt teszteli, hogy a hibatag korrelál-e a magyarázóváltozókkal.
# Ha p < 0.05, a sima Random Hatás torzít, és FE-re (vagy a mi esetünkben REWB-re) van szükség.
cat("\n-- Hausman-próba (FE vs RE) --\n")
hausman_test <- phtest(m_fe_tv, m_re_tv)
print(hausman_test)
# H0: az RE konzisztens (a városi hatás nem korrelál a magyarázókkal).
# Eredmény: chi2 = 0,52, p = 0,99, nincs eltérés az FE és RE becslés között.
# Értelmezés: (1) Hosszú panelnél (T nagy) az RE transzformáció közel a within
# transzformációhoz, ezért a két becslés mechanikusan hasonló; a teszt ereje alacsony.
# (2) A teszt csak az idõjárás within-hatására vonatkozik, a városszintû infrastruktúra
# nincs benne (az FE kiejti), így az infrastruktúra és a városi hatás függetlenségét
# NEM igazolja; ez nem tesztelhetõ feltevés, a H1-H3 eredmények asszociációk.
# (3) A klasszikus Hausman az RE hatékonyságát feltételezi (nincs autokorreláció,
# homoszkedaszticitás), amit az alábbi BG-teszt megcáfol, ezért a p-érték csak
# tájékoztató. A releváns, robusztus megfelelõje a REWB within = between Wald-tesztje
# (klaszterezett vcov-val, lásd 4. szakasz). A REWB választása nem a Hausmanon múlik:
# az RE-t speciális esetként tartalmazza.


# 2. Breusch-Godfrey teszt: Azt vizsgálja, maradt-e idõbeli autokorreláció a hibatagban.
# Ha p < 0.05, jelentõs az autokorreláció.
cat("\n-- Breusch-Godfrey autokorreláció teszt (1 hetes késleltetés) --\n")
bg_test <- pbgtest(m_re_tv, order = 7)
print(bg_test)
# H0: nincs sorozatkorreláció az idioszinkratikus hibában. Az order = 7 az 1-7. napi
# késleltetést EGYÜTT teszteli.
# Eredmény: chi2 = 7912, p < 2,2e-16, erõs autokorreláció. Ez várható napi
# használati adatnál: a használat perzisztens, és a modell csak idõjárást tartalmaz,
# így a kihagyott szezonalitás (hónap, hét napja, ünnepnap) a hibatagban marad.
# Következmény: a klasszikus SE érvénytelen -> klaszterezett (városszintû) SE használata
# indokolt, ez tetszõleges városon belüli sorozatkorrelációra érvényes.
# Megjegyzés: a panel kiegyensúlyozatlan, ezért a késleltetés a megfigyelések sorrendjén
# alapul, nem naptári napon, a teszt tájékoztató jellegû.

# Ekkora N*T mellett kis autokorreláció is elutasítást ad, ezért a nagyságrendet
# külön megnézem (reziduumok AR(1) korrelációja).
# Városonként kiszámoljuk a reziduum és egynapos késleltettje korrelációját, majd átlagoljuk városonként
e <- residuals(m_re_tv)
rho <- mean(tapply(e, index(m_re_tv)$city, function(x) cor(x[-1], x[-length(x)])), na.rm = TRUE)
print(rho) # rho = 0.53: Ez mérsékelten erõs sorozatkorrelációt jelent, tehát nem csak a nagy N*T miatti statisztikai zaj.


# Fejlesztés közben ellenõrizve: a hónap- és hétnap-dummyk bevonása ezen alig változtat (0,532 -> 0,528),
# vagyis a perzisztencia (tartós fennmaradás) valódi napi perzisztencia, és a klaszterezett SE kezeli.


# =============================================================================
# 3. Modellspecifikáció és szûkítési tesztek (REWB alapmodell)
# =============================================================================

# Változócsoportok definiálása
w_terms <- c("temp_w", "precip_w", "wind_w", "humid_w", "is_workday") # Within: a napi értékek eltérése a városi saját átlagtól
b_terms <- c("temp_b", "precip_b", "wind_b", "humid_b") # Between: a városi átlagok a teljes mintán
season <- c("month", "dow") # Szezonális kontrollok: hónap és hét napja dummy-k

infra_z <- paste0(infra_raw, "_z")
ctrl_z  <- paste0(ctrl_raw, "_z")

# A megadott változókból elkészít egy log_trips-t magyarázó formulát
mk_f <- function(rhs) as.formula(paste("log_trips ~", paste(rhs, collapse = " + ")))

# --- Kezdeti (teljes) REWB modell becslése a specifikációs tesztekhez ---
m2_full <- plm(mk_f(c(w_terms, b_terms, infra_raw, ctrl_raw, season)), 
          data = pdat, model = "random", random.method = "walhus")
V_m2_full <- vcovHC(m2_full, type = "HC1", cluster = "group")

# ---Specifikációs és modellszûkítési Wald-próbák (klaszterezett SE-vel) ---

# a) REWB specifikációs teszt (Within = Between egyenlõség tesztelése)
# H0: A napi (within) és a városszintû (between) idõjárási hatások megegyeznek (elég lenne a sima RE modell).
# Eredmény: F = 4.73, p = 0.0008 -> H0-t elvetjük, a REWB szétbontás statisztikailag szükséges.
linearHypothesis(m2_full, c("temp_w = temp_b",
                                  "precip_w = precip_b",
                                  "wind_w = wind_b",
                                  "humid_w = humid_b"),
                       vcov. = V_m2_full, test = "F")

# b) Szezonális kontrollok (hónap dummyk) együttes szignifikanciája
# H0: A hónap dummyk együtthatói együttesen nullák (nincs szezonalitás az idõjáráson túl).
# Eredmény: F = 13.76, p < 2.2e-16 -> Erõsen szignifikáns, a szezonális kontrollok kihagyhatatlanok.
linearHypothesis(m2_full, grep("^month", names(coef(m2_full)), value = TRUE), 
                       vcov. = V_m2_full, test = "F")

# c) Nem szignifikáns városszintû klímaátlagok (precip_b, wind_b, humid_b) elhagyhatósága
# H0: precip_b = wind_b = humid_b = 0 (nincs együttes magyarázóerejük).
# Eredmény: F = 0.37, p = 0.7741 -> H0-t megtartjuk! Mivel a városszintû elemzés csak 78 városra
# épül, a felesleges változók elhagyása növeli a szabadságfokot és a becslés hatékonyságát.
linearHypothesis(m2_full, c("precip_b = 0", "wind_b = 0", "humid_b = 0"),
                       vcov. = V_m2_full, test = "F")

# =============================================================================
# 4. Végleges (szûkített) modellek becslése (M2 és M3) és eredmények
# =============================================================================

b_terms <- c("temp_b") # A Wald-teszt alapján a between változók közül csak a szignifikáns átlaghõmérsékletet tartjuk meg

# --- M2 Modell: Végleges fõmodell K1, K2 vizsgálatára (NYERS sûrûséggel) ---
m2 <- plm(mk_f(c(w_terms, b_terms, infra_raw, ctrl_raw, season)), 
          data = pdat, model = "random", random.method = "walhus")

# --- M3 Modell: K3 kutatási kérdés (Z-STANDARDIZÁLT változókkal az interakciókhoz) ---
# Az M2 kiegészítése akerékpárút sûrûség és az idõjárási változók közötti interakciókkal.
# Az idõjárás hatása egy városan: b_w + b_int * z: mivel z átlaga 0, a _w fõhatás az átlagos kerékpárút-sûrûségû városra vonatkozik
# Hány egységgel változik az idõjárás hatása, ha a város 1 szórásnyival jobban elállátott kerékpáros infrastruktúrával.
m3 <- plm(mk_f(c(w_terms, b_terms, infra_z, ctrl_z, season,
                 "precip_w:bike_path_density_z",
                 "wind_w:bike_path_density_z",
                 "temp_w:bike_path_density_z")),
          data = pdat, model = "random", random.method = "walhus")

# --- Modell-összehasonlítás: M2 vs. M3 (Interakciók együttes Wald-próbája és R2) ---
# H0: A három keresztszintû interakciós tag együttesen nulla (az M3 nem jobb az M2-nél).
V_m3 <- vcovHC(m3, type = "HC1", cluster = "group")
linearHypothesis(m3, c("precip_w:bike_path_density_z = 0",
                             "wind_w:bike_path_density_z = 0",
                             "temp_w:bike_path_density_z = 0"),
                       vcov. = V_m3, test = "F")

cat("\n-- Modellek illeszkedése (R2 és korrigált R2) --\n")
cat("M2 R2:", round(r.squared(m2), 4), "| Korrigált R2:", round(r.squared(m2, dfcor = TRUE), 4), "\n")
cat("M3 R2:", round(r.squared(m3), 4), "| Korrigált R2:", round(r.squared(m3, dfcor = TRUE), 4), "\n")


# Hipotézistáblák az egyoldali tesztekhez: pozitív hatást várunk
hyp_main <- tribble(
  ~term,                     ~hyp,      ~dir,
  "bike_path_density",       "H1 (K1)", ">",
  "station_density",         "H2 (K2)", ">",
  "mobility_poi_density",    "H3 (K2)", ">"
)

# H4:a csapadék és a szél hatása negatív -> az infrastruktúra mérsékeli -> pozitív interakció
# A hõmérsékletnél nem egyértelmû az irány
hyp_h4 <- tribble(
  ~term,                          ~hyp,           ~dir,
  "precip_w:bike_path_density_z", "H4 (Csapadék)",">",  
  "wind_w:bike_path_density_z",   "H4 (Szél)",    ">",
  "temp_w:bike_path_density_z",   "H4 (Hõm.)",    "two"
)

cat("\n=== M2: Végleges REWB fõmodell (Nyers sûrûség), klaszterezett SE ===\n")
res_m2 <- tidy_cl(m2)
print(as.data.frame(res_m2), digits = 3)

cat("\n=== H1-H3 egyoldali tesztek (M2) ===\n")
print(as.data.frame(add_one_sided(res_m2, hyp_main)), digits = 3)

cat("\n=== M3: REWB + interakciók (Z-standardizált), klaszterezett SE ===\n")
res_m3 <- tidy_cl(m3)
print(as.data.frame(res_m3), digits = 3)

cat("\n=== H4 interakciós tesztek (M3) ===\n")
print(as.data.frame(add_one_sided(res_m3, hyp_h4)), digits = 3)


# =============================================================================
# 5. Robusztussági ellenõrzések (Leave-one-out és R2 modell)
# =============================================================================
# Leave-one-out
# Cél: annak vizsgálata, hogy a H1-H3 eredmények nem függenek-e egyetlen várostól.
# Mivel az infrastruktúra-változók városszintûek, a hipotézisek lényegében 78 megfigyelésre
# épülnek, így egy-egy befolyásos város a becslést érdemben elmozdíthatja.
# Eljárás: az M2 modellt 78-szor becsüljük újra úgy, hogy minden körben egy várost
# kihagyunk, és mindegyik körben megõrizzük az infrastruktúra-együtthatók becslését,
# klaszterezett SE-jét és t-értékét.
# Megjegyzés: a tidy_cl() a globális G (=78) szabadságfokot használja; a 77 városos
# újrabecsléseknél ez elhanyagolható eltérés a t-eloszlás kritikus értékében.
loo <- lapply(unique(dat$city), function(cn) {
  d <- pdata.frame(subset(dat, city != cn), index = c("city", "date"))
  m <- plm(formula(m2), data = d, model = "random", random.method = "walhus")
  tidy_cl(m) |> filter(term %in% hyp_main$term) |> mutate(dropped = cn)
}) |> bind_rows()
loo |> filter(term == "station_density") |> summarise(min_t = min(t), max_t = max(t))
# A t-értékek terjedelme (min, max) megmutatja a következtetés stabilitását.
# Egyoldali 5%-os kritikus t-érték kb. 1,66 (77 szabadsági fok): ha a minimum e felett
# marad, a döntés egyetlen város kihagyásával sem fordul meg.
# t = 1.84 és 2.88 között ->tehát mind a 78 újrabecslésnél az egyoldali 5%-os kritikus érték felett van.

crit <- qt(0.95, df = G - 2)   # egyoldali 5%-os kritikus érték, 77 városnál

#LOO vizualizálása
loo |>
  group_by(term) |>
  summarise(min_t = min(t), median_t = median(t), max_t = max(t),
            n_sig = sum(t > crit), n = n())
library(ggplot2)
ggplot(loo, aes(x = t)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = crit, linetype = "dashed") +
  facet_wrap(~ term, scales = "free_x") +
  labs(x = "t-érték (egy város kihagyásával)", y = "Gyakoriság")


# --- R2 modell: kerékpárút-arány a sûrûség helyett ---
# Cél: a H1 eredménye ne a mérõszám megválasztásától függjön. A sûrûség (bike_path_density) a
# kerékpárút hosszát a város TERÜLETÉHEZ viszonyítja (km/km2); az arány (bike_path_proportion) az
# úthálózathoz: kerékpárút / (kerékpárút + autóút + gyalogos). Ha mindkét mérõszámmal ugyanaz a
# következtetés, az eredmény nem a mérõszám mûve.
# A season kontrollok ugyanúgy benne vannak, mint az M2-ben, így a két modell csak a H1 változójában
# tér el, és az összehasonlítás tiszta. Az arány 0 és 1 közötti, ezért az együttható a teljes 0 -> 1
# tartományra vonatkozik; szemléletesebb a 0,1-gyel (10 százalékpont) szorzott érték.
infra_r2 <- sub("bike_path_density", "bike_path_proportion", infra_raw)
r2 <- plm(mk_f(c(w_terms, b_terms, infra_r2, ctrl_raw, season)),
          data = pdat, model = "random", random.method = "walhus")

hyp_r2 <- hyp_main |> mutate(term = sub("bike_path_density", "bike_path_proportion", term))

cat("\n=== R2: Robusztusság kerékpárút-aránnyal ===\n")
print(as.data.frame(add_one_sided(tidy_cl(r2), hyp_r2)), digits = 3)

