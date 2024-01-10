if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(dplyr, knitr, kableExtra, haven, acid, DescTools,collapse)

cut <- read.csv(file = here::here(("input/data-original/cut.csv")))

# Casen 2015 --------------------------------------------------------------
# casen2015 <- read_dta(file = "input/data/casen2015.dta")
# save(casen2015,file = "input/data/casen2015.RData")
load(file = here::here("input/data-original/casen2015.RData"))

df15 <-
  casen2015 %>%
  select(
    comuna,
    starts_with("ytrabajo"),
    starts_with("ytot"),
    starts_with("yaut"),
    starts_with("ypc"),
    educacion=e6a,
    expr,
    expc
  )
df15 <- df15 %>% left_join(cut[, c("COMUNA", "COMUNA_15R")], by = c("comuna" =
                                                                      "COMUNA_15R"))
df15$univ<- as.numeric(car::recode(df15$educacion,"15:17=1;else=0;99=NA",as.numeric = T))


gini15 <-
  df15 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =expc)$bcwGini),
            "prop_univ"= collapse::fmean(x = univ,w=expc,na.rm=TRUE),
            "inc_mean"= mean(ypch,na.rm = T)) %>%
  mutate(year = 2015)

# Casen 2017 --------------------------------------------------------------
# casen2017 <- read_dta(file = "input/data/casen2017.dta")
# save(casen2017,file = "input/data/casen2017.RData")
load(file = here::here(("input/data-original/casen2017.RData")))


df17 <-
  casen2017 %>%
  select(
    folio:zona,
    starts_with("ytrabajo"),
    starts_with("ytot"),
    starts_with("yaut"),
    starts_with("ypc"),
    educacion=e6a,
    expr,
    expc,
    COMUNA = comuna
  )

df17$univ<- as.numeric(car::recode(df17$educacion,"15:17=1;else=0;99=NA",as.numeric = T))
summary(df17$univ)
gini17 <-
  df17 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =
                                                           expc)$bcwGini),
            "prop_univ"= collapse::fmean(x = univ,w=expc,na.rm=TRUE),
            "inc_mean"= mean(ypch,na.rm = T)
            ) %>%
  mutate(year = 2017)
names(gini17)

# Casen 2022 --------------------------------------------------------------
# casen2022 <- read_dta(file = "input/data/casen2022.dta")
# casen2022cp <- read_dta(file = "input/data/casen2022-comuna-provincia.dta")
#
# casen22 <-
# left_join(casen2022,casen2022cp,c("id_persona","folio"))
# save(casen22,file = "input/data/casen2022.RData")
load(file = here::here(("input/data-original/casen2022.RData")))

names(casen22)

sjmisc::frq(casen22$e6a)
sjmisc::frq(casen22$e6c_completo)

df22 <-
  casen22 %>%
  select(
    folio:hogar,
    COMUNA = comuna,
    starts_with("ytot"),
    starts_with("yaut"),
    starts_with("ypc"),
    e6a,
    e6c_completo,
    expr,
    expc
  )

# Han cursado y terminado ed superior
df22$univ<- ifelse(df22$e6a %in% c(13,14,15) & df22$e6c_completo ==1,yes = 1,no = 0)
summary(df22$univ)
gini22 <-
  df22 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =
                                                           expc)$bcwGini),
            "prop_univ"= collapse::fmean(x = univ,w=expc,na.rm=TRUE),
            "inc_mean"= mean(ypch,na.rm = T)) %>%
  mutate(year = 2022)


# Generar base de datos acumulada -----------------------------------------
#  Formato long
df_gini_comunas_long <-
  bind_rows(gini15, gini17, gini22) %>%
  rename(COMUNA_COD = COMUNA) %>%
  mutate(COMUNA_NOM = sjlabelled::as_character(COMUNA_COD)) %>%
  arrange("COMUNA_COD") %>%
  left_join(cut, by = c("COMUNA_COD" = "COMUNA")) %>%
  select(
    COMUNA_COD,
    COMUNA_NOM,
    COMUNA_15R,
    REGION,
    REGION_15R,
    PROVINCIA,
    PROVINCIA_15R,
    year,
    everything(),
    -X
  )

#Center within cluster (comuna) GINI
df_gini_comunas_long$gini_gc <-  MLMusingR::group_center(as.numeric(df_gini_comunas_long$gini_ypch), grp = df_gini_comunas_long$COMUNA_COD)
save(df_gini_comunas_long, file = here::here("input/data-proc/df_casen_comunas_long.RData"))

df_gini_comunas_wide <-gini15 %>%
  full_join(gini17, by = "COMUNA", suffix = c("_2015", "_2017")) %>%
  full_join(gini22, by = "COMUNA", suffix = c("_2022", "")) %>%
  select(everything(), starts_with("gini_ypch"), -starts_with("year")) %>%
  rename(COMUNA_COD = COMUNA, gini_ypch_2022 = gini_ypch,prop_univ_2022 = prop_univ, inc_mean_2022 = inc_mean) %>%
  mutate(COMUNA_NOM = sjlabelled::as_character(COMUNA_COD)) %>%
  arrange("COMUNA_COD") %>%
  left_join(cut, by = c("COMUNA_COD" = "COMUNA")) %>%
  select(
    COMUNA_COD,
    COMUNA_NOM,
    COMUNA_15R,
    REGION,
    REGION_15R,
    PROVINCIA,
    PROVINCIA_15R,
    everything(),
    -X
  ) %>%
  mutate(
    ginilag22_15 = gini_ypch_2022 - gini_ypch_2015,
    ginilag22_17 = gini_ypch_2022 - gini_ypch_2017,
    ginilag17_15 = gini_ypch_2017 - gini_ypch_2015,
# educacion universitaria
    univlag22_15 = prop_univ_2022 - prop_univ_2015,
    univlag22_17 = prop_univ_2022 - prop_univ_2017,
    univlag17_15 = prop_univ_2017 - prop_univ_2015,
# income comunal
incomelag22_15 = inc_mean_2022 - inc_mean_2015,
incomelag22_17 = inc_mean_2022 - inc_mean_2017,
incomelag17_15 = inc_mean_2017 - inc_mean_2015
)

summary(df_gini_comunas_wide)

skimr::skim(df_gini_comunas_wide)
save(df_gini_comunas_wide, file = here::here("input/data-proc/df_casen_comunas_wide.RData"))

