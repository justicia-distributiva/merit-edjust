if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(dplyr, knitr, kableExtra, haven, acid, DescTools)

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
    expr,
    expc
  )
df15 <- df15 %>% left_join(cut[, c("COMUNA", "COMUNA_15R")], by = c("comuna" =
                                                                      "COMUNA_15R"))

gini15 <-
  df15 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =
                                                           expc)$bcwGini)) %>%
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
    expr,
    expc,
    COMUNA = comuna
  )


gini17 <-
  df17 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =
                                                           expc)$bcwGini)) %>%
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

df22 <-
  casen22 %>%
  select(
    folio:hogar,
    COMUNA = comuna,
    starts_with("ytot"),
    starts_with("yaut"),
    starts_with("ypc"),
    expr,
    expc
  )
names(df22)

gini22 <-
  df22 %>%
  group_by(COMUNA) %>%
  summarise("gini_ypch" = as.numeric(acid::weighted.gini(x = ypch, w =
                                                           expc)$bcwGini)) %>%
  mutate(year = 2022)


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
save(df_gini_comunas_long, file = here::here("input/data-proc/df_gini_comunas_long.RData"))

df_gini_comunas_wide <-gini15 %>%
  full_join(gini17, by = "COMUNA", suffix = c("_2015", "_2017")) %>%
  full_join(gini22, by = "COMUNA", suffix = c("_2022", "")) %>%
  select(everything(), starts_with("gini_ypch"), -starts_with("year")) %>%
  rename(COMUNA_COD = COMUNA, gini_ypch_2022 = gini_ypch) %>%
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
    ginilag17_15 = gini_ypch_2017 - gini_ypch_2015
  )

summary(df_gini_comunas_wide)

save(df_gini_comunas_wide, file = here::here("input/data-proc/df_gini_comunas_wide.RData"))


list.files('input/data-original/simce', pattern="(*.comuna)", full.names=T,recursive = T)

