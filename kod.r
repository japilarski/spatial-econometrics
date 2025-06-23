library(spdep)
library(sf)
library(tmap)
library(rnaturalearth)
library(dplyr)
library(psych)
library(knitr)
library(kableExtra)

dane <- read.csv("growthley.csv", sep = ",")  # lub sep = "," zależnie od formatu
dane$gdp_growth_1960_1980 <- dane$gdp_growth_1960_1980 / 100
dane$GDPsh560 <- dane$GDPsh560
dane$NEquip_Inv <- dane$NEquip_Inv * 100

coords <- cbind(dane$longitude, dane$latitude)
neighbors <- dnearneigh(coords, 0, 20)
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_merged <- world %>%
  left_join(dane, by = c("name" = "country_name"))

tmap_mode("plot")
tm_shape(world_merged) +
  tm_polygons("gdp_growth_1960_1980", 
              palette = "RdBu", 
              title = "Średnioroczny wzrost PKB w latach 1960-1980") +
  tm_layout(legend.outside = TRUE)

  tm_shape(world_merged) +
  tm_polygons("Life_Exp", 
              palette = "Blues", 
              title = "Średnia przewidywana długość życia") +
  tm_layout(legend.outside = TRUE)

tm_shape(world_merged) +
  tm_polygons("Rule_of_Law", 
              palette = "Blues", 
              title = "Wskaźnik jakości rządów prawa") +
  tm_layout(legend.outside = TRUE)
tm_shape(world_merged) +
  tm_polygons("NEquip_Inv", 
              palette = "Blues", 
              title = "Wydatki inne niż w środki trwałe") +
  tm_layout(legend.outside = TRUE)
tm_shape(world_merged) +
  tm_polygons("GDPsh560", 
              palette = "Blues", 
              title = "Logarytm naturalny PKB per capita w roku 1960") +
  tm_layout(legend.outside = TRUE)

library(sf)
library(sp)
library(spdep)
library(tmap)
library(tmaptools)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
coords_sf <- st_as_sf(dane, coords = c("longitude", "latitude"), crs = 4326)
lines_list <- nb2lines(neighbors, coords = sp::coordinates(as_Spatial(coords_sf)), 
                       proj4string = CRS("+proj=longlat +datum=WGS84"))
lines_sf <- st_as_sf(lines_list)
tmap_mode("plot")
tm_shape(world) +
  tm_borders() +
  tm_shape(lines_sf) +
  tm_lines(col = "red", lwd = 1) +
  tm_shape(coords_sf) +
  tm_dots(col = "blue", size = 0.1) +
  tm_layout(legend.show = FALSE)

W_matrix <- as.matrix(listw2mat(weights))

rownames(W_matrix) <- dane$country_name
colnames(W_matrix) <- dane$country_name

W_matrix_subset <- W_matrix[14:19, 14:19]

kable(W_matrix_subset, caption = "Fragment macierzy wag przestrzennych", digits = 2)

library(tibble)
library(tidyr)

neighbors_df <- tibble(
  country = dane$country_name,
  neighbors = sapply(neighbors, function(n) paste(dane$country_name[n], collapse = ", "))
)

# Piękna tabela
library(knitr)
library(kableExtra)

kable(head(neighbors_df, 10), caption = "Lista sąsiadów wybranych państw")
stats <- describe(dane[, c(
  "Life_Exp",
  "GDPsh560",
  "NEquip_Inv",
  "Rule_of_Law"
)])
stats_selected <- stats[, c("mean", "sd", "min", "median", "max")]
opisy <- c(
  Life_Exp = "Oczekiwana długość życia",
  GDPsh560 = "Logarytm naturalny PKB per capita w roku 1960",
  NEquip_Inv = "Procent wydatków innych niż w środki trwałe",
  Rule_of_Law = "Jakość rządów prawa"
)

stats_selected$Zmienna <- opisy[rownames(stats_selected)]
stats_selected <- stats_selected[, c("Zmienna", setdiff(names(stats_selected), "Zmienna"))]
kable(stats_selected,
      caption = "Podstawowe statystyki opisowe wybranych zmiennych",
      digits = 0,
      col.names = c("Zmienna", "Średnia", "Odchylenie standardowe", "Minimum", "Mediana", "Maksimum"),
      row.names = FALSE)

      library(ggplot2)
library(gridExtra)

vars <- c("Life_Exp", "GDPsh560", "NEquip_Inv", "Rule_of_Law")
histograms <- lapply(vars, function(var) {
  ggplot(dane, aes_string(x = var)) +
    geom_histogram(bins = 20, fill = "#69b3a2", color = "white", alpha = 0.8) +
    theme_minimal() +
    labs(x = var, y = "Liczba obserwacji")
})

do.call(grid.arrange, c(histograms, ncol = 2))

library(ggplot2)
library(tidyr)
library(dplyr)

dane_long <- dane %>%
  select(Life_Exp, GDPsh560, NEquip_Inv, Rule_of_Law) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartość")

ggplot(dane_long, aes(x = Wartość, fill = Zmienna)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~Zmienna, scales = "free") +
  labs(x = "Wartość", y = "Gęstość") +
  theme_minimal() +
  theme(legend.position = "none")
library(knitr)
library(kableExtra)

vars <- c("Life_Exp", "GDPsh560", "NEquip_Inv", "Rule_of_Law")
dane_sub <- dane[, vars]

cor_matrix <- cor(dane_sub, method = "pearson", use = "complete.obs")

kable(cor_matrix, digits = 3, caption = "Macierz korelacji")
dane %>%
  count(SubSahara) %>%
  mutate(
    Kategoria = ifelse(SubSahara == 1, "Tak (Afryka Subsaharyjska)", "Nie"),
    Procent = round(n / sum(n) * 100, 2)
  ) %>%
  select(Kategoria, Liczba = n, Procent) %>%
  kable(
    caption = "Rozkład zmiennej SubSahara",
    col.names = c("Kategoria", "Liczba obserwacji", "Procent"),
    align = "lrr"
  )
moran_result <- moran.test(dane$gdp_growth_1960_1980, weights, zero.policy = TRUE)
model_reduced <- lm(
  gdp_growth_1960_1980 ~
    Life_Exp + 
    GDPsh560 + 
    NEquip_Inv +
    Rule_of_Law +
    SubSahara,
  data = dane
)
coef_table <- data.frame(
  Zmienna = c("Intercept", "Life_Exp", "GDPsh560", "NEquip_Inv", "Rule_of_Law", "SubSahara"),
  Współczynnik = c(10.69607, 0.10593, -2.23097, 0.07501, 2.65482, -2.25614),
  Interpretacja = c(
    "Wartość średniorocznego wzrostu PKB przy zerowych wartościach zmiennych objaśniających",
    "Każdy dodatkowy rok życia zwiększa wzrost PKB o ok. 0.11 punktu procentowego",
    "Wyższy poziom PKB per capita w 1960 r. koreluje z niższym tempem wzrostu (efekt doganiania)",
    "Większy udział wydatków innych niż środki trwałe wiąże się z nieznacznym wzrostem wzrostu PKB",
    "Lepszy indeks praworządności sprzyja wyższemu tempu wzrostu gospodarczego",
    "Kraje Afryki Subsaharyjskiej mają średnio niższe tempo wzrostu PKB o około 2.26 punktu procentowego"
  )
)

kable(coef_table, 
      col.names = c("Zmienna", "Współczynnik", "Interpretacja"), 
      caption = "Wyniki estymacji klasycznego modelu liniowego (OLS)")
model_reduced_residuals <- residuals(model_reduced)

moran_residualds <- moran.test(model_reduced_residuals, weights)
lm_test <- lm.LMtests(model_reduced, listw = weights, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
library(sf)
library(spdep)
library(tmap)

coords_m <- st_coordinates(coords_sf)

neighbors <- dnearneigh(coords, 0, 20000)

weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

local_moran <- localmoran(dane$gdp_growth_1960_1980, weights, zero.policy = TRUE)

coords_sf$Ii <- local_moran[, "Ii"]
coords_sf$Z <- local_moran[, "Z.Ii"]
coords_sf$pvalue <- local_moran[, "Pr(z != E(Ii))"]

mean_value <- mean(dane$gdp_growth_1960_1980, na.rm = TRUE)
mean_Ii <- mean(coords_sf$Ii, na.rm = TRUE)

coords_sf$kwadranty <- with(coords_sf, 
  ifelse(gdp_growth_1960_1980 > mean_value & Ii > mean_Ii, "High-High",
  ifelse(gdp_growth_1960_1980 < mean_value & Ii < mean_Ii, "Low-Low",
  ifelse(gdp_growth_1960_1980 > mean_value & Ii < mean_Ii, "High-Low",
  ifelse(gdp_growth_1960_1980 < mean_value & Ii > mean_Ii, "Low-High", NA)))))

tmap_mode("view")  # interaktywna mapa

tm_shape(coords_sf) +
  tm_dots(col = "kwadranty", palette = c("red", "pink", "lightblue", "blue")) +
  tm_layout(legend.outside = TRUE)
library(sf)
library(spdep)
library(spatialreg)
library(dplyr)

data_sar <- world_merged %>%
  dplyr::select(gdp_growth_1960_1980, Life_Exp, NEquip_Inv, Rule_of_Law, GDPsh560, SubSahara, geometry) %>%
  na.omit()

nb <- poly2nb(data_sar) 
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) 
sar_model <- lagsarlm(
  formula = gdp_growth_1960_1980 ~ Life_Exp + NEquip_Inv + Rule_of_Law + GDPsh560 + SubSahara,
  data = data_sar,
  listw = lw,
  zero.policy = TRUE
)

summary(sar_model)
N <- length(sar_model$fitted.values)

SSR_sar <- sar_model$s2 * N 
SST <- var(data_sar$gdp_growth_1960_1980, na.rm = TRUE) * (nrow(data_sar) - 1) # Całkowita suma kwadratów (SST) — dla zmiennej zależnej (gdp_growth_1960_1980)

pseudo_R2 <- 1 - (SSR_sar / SST)
residuals_sar <- residuals(sar_model)

moran_sar <- moran.test(residuals_sar, lw, zero.policy = TRUE)
library(spdep)

moran.plot(residuals_sar, lw, labels = world_merged$admin,
           xlab = "Reszty", ylab = "Przestrzennie ważone reszty")
library(lmtest)

ols_model <- lm(gdp_growth_1960_1980 ~ Life_Exp + NEquip_Inv + Rule_of_Law + GDPsh560
                + SubSahara,
                data = world_merged)

bptesttt <- bptest(ols_model)
library(spdep)

sem_model <- errorsarlm(
  formula = gdp_growth_1960_1980 ~ Life_Exp + NEquip_Inv + Rule_of_Law + GDPsh560 + SubSahara,
  data = data_sar,
  listw = lw,
  zero.policy = TRUE
)

summary(sem_model)
N <- length(sem_model$fitted.values) 
SSR_sem <- sem_model$s2 * N 
SST <- var(data_sar$gdp_growth_1960_1980, na.rm = TRUE) * (nrow(data_sar) - 1) # Całkowita suma kwadratów (SST) — dla zmiennej zależnej (gdp_growth_1960_1980)

pseudo_R2 <- 1 - (SSR_sem / SST)
Life_Exp_lag <- lag.listw(lw, data_sar$Life_Exp)
NEquip_Inv_lag <- lag.listw(lw, data_sar$NEquip_Inv)
Rule_of_Law_lag <- lag.listw(lw, data_sar$Rule_of_Law)
GDPsh560_lag <- lag.listw(lw, data_sar$GDPsh560)
SubSahara_lag <- lag.listw(lw, data_sar$SubSahara)

model_slx <- lm(gdp_growth_1960_1980 ~ 
                  Life_Exp + NEquip_Inv + Rule_of_Law + GDPsh560 + SubSahara + 
                  Life_Exp_lag + NEquip_Inv_lag + Rule_of_Law_lag + GDPsh560_lag + SubSahara_lag,
                data = data_sar)

summary(model_slx)
sdm_model <- lagsarlm(
  formula = gdp_growth_1960_1980 ~ Life_Exp + NEquip_Inv + Rule_of_Law + GDPsh560 + SubSahara,
  data = data_sar,
  listw = lw,
  type = "Durbin",
  zero.policy = TRUE
)

summary(sdm_model)
residuals_durbin <- residuals(sdm_model)
moran_test <- moran.test(residuals_durbin, lw, zero.policy = TRUE)
library(spdep)

moran.plot(residuals_durbin, lw, labels = world_merged$admin,
           xlab = "Reszty", ylab = "Przestrzennie ważone reszty")
