if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(dplyr, knitr, kableExtra, haven, acid, DescTools)

cut <- read.csv(file = here::here(("input/data-original/cut.csv")))

list.files('input/data-original/simce', pattern="(*.rbd)", full.names=T,recursive = T)

# Simce 2016 --------------------------------------------------------------
df_16_2do <-
  haven::read_dta(
    here::here(
      "input/data-original/simce/simce16/Simce 2° Medio 2016/Archivos DTA (Stata)/simce2m2016_rbd_publica_final.dta"
    )
  )
df_16_4to <-
  haven::read_dta(
    here::here(
      "input/data-original/simce/simce16/Simce 4° Basico 2016/Archivos DTA (Stata)/simce4b2016_rbd_publica_final.dta"
    )
  )

df_16_6to <-
  haven::read_dta(
    here::here(
      "input/data-original/simce/simce16/Simce 6° Basico 2016/Archivos DTA (Stata)/simce6b2016_rbd_publica_final.dta"
    )
  )

dat_16_4to <-
  df_16_4to %>%
  select(cod_com_rbd,rbd,prom_lect4b_rbd,prom_mate4b_rbd) %>%
  sjmisc::row_means(prom_lect4b_rbd,prom_mate4b_rbd,n = 1, append = TRUE,var="prom_4b_rbd")

dat_16_6to <-
  df_16_6to %>%
  select(cod_com_rbd,rbd,prom_lect6b_rbd,prom_mate6b_rbd,prom_soc6b_rbd) %>%
  sjmisc::row_means(prom_lect6b_rbd,prom_mate6b_rbd,prom_soc6b_rbd,n = 1, append = TRUE,var="prom_6b_rbd")

dat_16_2do <-
  df_16_2do %>%
  select(cod_com_rbd,rbd,prom_lect2m_rbd,prom_mate2m_rbd,prom_nat2m_rbd) %>%
  sjmisc::row_means("prom_lect2m_rbd","prom_mate2m_rbd","prom_nat2m_rbd",n = 1, append = TRUE,var="prom_2m_rbd")

df_2016<-
  full_join(dat_16_4to,dat_16_6to, by=c("rbd","cod_com_rbd")) %>%
  full_join(dat_16_2do,by=c("rbd","cod_com_rbd")) %>%
  select(cod_com_rbd,rbd,prom_4b_rbd,prom_6b_rbd,prom_2m_rbd) %>%
  sjmisc::row_means(prom_4b_rbd,prom_6b_rbd,prom_2m_rbd,n = 1, append = TRUE,var="prom_2016_rbd")


df_2016_agg <-
  df_2016 %>%
  group_by(cod_com_rbd) %>%
  summarise("prom_simce_2016_comuna" = mean(prom_2016_rbd,na.rm = T),
            "var_simce_2016_comuna" = var(prom_2016_rbd,na.rm = T),
            "min_simce_2016_comuna" = min(prom_2016_rbd,na.rm = T),
            "max_simce_2016_comuna" = max(prom_2016_rbd,na.rm = T),
            "ran_simce_2016_comuna" = max(prom_2016_rbd,na.rm = T) - min(prom_2016_rbd,na.rm = T))

summary(df_2016_agg)
df_2016_agg<- left_join(df_2016_agg,cut[,c("COMUNA","COMUNA_15R")], by=c("cod_com_rbd"="COMUNA_15R"))
summary(df_2016_agg)

# Simce 2017 --------------------------------------------------------------
r1="input/data-original/simce/simce17/Simce 4° Basico 2017/Archivos DTA (Stata)/simce4b2017_rbd_publica_final.dta"
r2="input/data-original/simce/simce17/Simce 8° Basico 2017/Archivos DTA (Stata)/simce8b2017_rbd_publica_final.dta"
r3="input/data-original/simce/simce17/Simce 2° Medio 2017/Archivos DTA (Stata)/simce2m2017_rbd_publica_final.dta"

df_17_4b <-haven::read_dta(here::here(r1))
df_17_8b <-haven::read_dta(here::here(r2))
df_17_2m <-haven::read_dta(here::here(r3))

# Esta base no se puede usar porque no tiene un codigo de comuna bien
df_17_4b$prom_lect4b_rbd[df_17_4b$prom_lect4b_rbd==0] <- NA#puntaje 0 dejar como NA
df_17_4b$prom_mate4b_rbd[df_17_4b$prom_mate4b_rbd==0] <- NA#puntaje 0 dejar como NA
dat_17_4b <-
  df_17_4b %>%
  select(rbd,prom_lect4b_rbd,prom_mate4b_rbd) %>%
  sjmisc::row_means(prom_lect4b_rbd,prom_mate4b_rbd,n = 1, append = TRUE,var="prom_4b_rbd")

dat_17_8b <-
  df_17_8b %>%
  select(cod_com_rbd,rbd,prom_lect8b_rbd,prom_mate8b_rbd,prom_nat8b_rbd) %>%
  sjmisc::row_means(prom_lect8b_rbd,prom_mate8b_rbd,prom_nat8b_rbd,n = 1, append = TRUE,var="prom_8b_rbd")

dat_17_2m <-
  df_17_2m %>%
  select(cod_com_rbd,rbd,prom_lect2m_rbd,prom_mate2m_rbd,prom_soc2m_rbd) %>%
  sjmisc::row_means(prom_lect2m_rbd,prom_mate2m_rbd,prom_soc2m_rbd,n = 1, append = TRUE,var="prom_2m_rbd")

df_2017<-
  full_join(dat_17_8b,dat_17_2m, by=c("rbd","cod_com_rbd")) %>%
  full_join(dat_17_4b,by=c("rbd")) %>%
  select(cod_com_rbd,rbd,prom_4b_rbd,prom_8b_rbd,prom_2m_rbd) %>%
  sjmisc::row_means(prom_4b_rbd,prom_8b_rbd,prom_2m_rbd,n = 1, append = TRUE,var="prom_2017_rbd")

summary(df_2017)
# Recuperar COMUNAS desde 2016
summary(df_2016[,c("rbd","cod_com_rbd")])
summary(df_2017[,c("rbd","cod_com_rbd")])

df_bridge_rbd_comuna<- left_join(df_2017[,c("rbd")],df_2016[,c("rbd","cod_com_rbd")])
summary(df_bridge_rbd_comuna)
df_2017$cod_com_rbd <- NULL
df_2017<- left_join(df_2017,df_bridge_rbd_comuna,by="rbd")
summary(df_2017)

df_2017_agg <-
  df_2017 %>%
  group_by(cod_com_rbd) %>%
  summarise("prom_simce_2017_comuna" = mean(prom_2017_rbd,na.rm = T),
            "var_simce_2017_comuna" = var(prom_2017_rbd,na.rm = T))
summary(df_2017)

df_2017_agg<- left_join(df_2017_agg,cut[,c("COMUNA","COMUNA_15R")], by=c("cod_com_rbd"="COMUNA_15R"))
summary(df_2017_agg)

# Simce 2018 --------------------------------------------------------------
r4="input/data-original/simce/simce18/Simce4b2018_publicas_web/Archivos DTA (Stata)/simce4b2018_rbd_publica_final.dta"
r5="input/data-original/simce/simce18/Simce6b2018_publicas_web/Archivos DTA (Stata)/simce6b2018_rbd_publica_final.dta"
r6="input/data-original/simce/simce18/Simce2m2018_publicas_web/Archivos DTA (Stata)/simce2m2018_rbd_publica_final.dta"

df_18_4b <-haven::read_dta(here::here(r4))
df_18_6b <-haven::read_dta(here::here(r5))
df_18_2m <-haven::read_dta(here::here(r6))

dat_18_4b <-
df_18_4b %>%
  select(cod_com_rbd,rbd,prom_lect4b_rbd,prom_mate4b_rbd) %>%
  sjmisc::row_means(prom_lect4b_rbd,prom_mate4b_rbd,n = 1, append = TRUE,var="prom_4b_rbd")

dat_18_6b <-
  df_18_6b %>%
  select(cod_com_rbd,rbd,prom_lect6b_rbd,prom_mate6b_rbd,prom_nat6b_rbd) %>%
  sjmisc::row_means(prom_lect6b_rbd,prom_mate6b_rbd,prom_nat6b_rbd,n = 1, append = TRUE,var="prom_6b_rbd")

dat_18_2m <-
  df_18_2m %>%
  select(cod_com_rbd,rbd,prom_lect2m_rbd,prom_mate2m_rbd,prom_nat2m_rbd) %>%
  sjmisc::row_means(prom_lect2m_rbd,prom_mate2m_rbd,prom_nat2m_rbd,n = 1, append = TRUE,var="prom_2m_rbd")

df_2018<-
  full_join(dat_18_6b,dat_18_4b, by=c("rbd","cod_com_rbd")) %>%
  full_join(dat_18_2m,by=c("rbd","cod_com_rbd")) %>%
  select(cod_com_rbd,rbd,prom_4b_rbd,prom_6b_rbd,prom_2m_rbd) %>%
  sjmisc::row_means(prom_4b_rbd,prom_6b_rbd,prom_2m_rbd,n = 1, append = TRUE,var="prom_2018_rbd")

summary(df_2018)

df_2018_agg <-
  df_2018 %>%
  group_by(cod_com_rbd) %>%
  summarise("prom_simce_2018_comuna" = mean(prom_2018_rbd,na.rm = T),
            "var_simce_2018_comuna" = var(prom_2018_rbd,na.rm = T))
summary(df_2018_agg)

df_2018_agg<- left_join(df_2018_agg,cut[,c("COMUNA","COMUNA_15R")], by=c("cod_com_rbd"="COMUNA"))

# Simce 2019 --------------------------------------------------------------
r7="input/data-original/simce/simce19/Simce8b2019_publicas_web/Archivos DTA (Stata)/simce8b2019_rbd.dta"

df_19_8b <-haven::read_dta(here::here(r7))
dat_19_8b <-
  df_19_8b %>%
  select(cod_com_rbd,rbd,prom_lect8b_rbd,prom_mate8b_rbd,prom_soc8b_rbd) %>%
  sjmisc::row_means(prom_lect8b_rbd,prom_mate8b_rbd,prom_soc8b_rbd,n = 1, append = TRUE,var="prom_8b_rbd")

df_2019<-
  dat_19_8b %>%
  select(cod_com_rbd,rbd,prom_8b_rbd) %>%
  mutate("prom_2019_rbd"=prom_8b_rbd)


df_2019_agg <-
  df_2019 %>%
  group_by(cod_com_rbd) %>%
  summarise("prom_simce_2019_comuna" = mean(prom_2019_rbd,na.rm = T),
            "var_simce_2019_comuna" = var(prom_2019_rbd,na.rm = T))
summary(df_2019_agg)

df_2019_agg<- left_join(df_2019_agg,cut[,c("COMUNA","COMUNA_15R")], by=c("cod_com_rbd"="COMUNA"))
summary(df_2019_agg)

# Simce 2022 --------------------------------------------------------------
r8="input/data-original/simce/simce22/Simce 4° básico 2022/Archivos DTA (Stata)/Simce4b2022_rbd_final.dta"
r9="input/data-original/simce/simce22/Simce 2° medio 2022/Archivos DTA (Stata)/Simce2m2022_rbd_final.dta"

df_22_4b <-haven::read_dta(here::here(r8))
df_22_2m <-haven::read_dta(here::here(r9))

table(df_22_4b$prom_lect4b_rbd==0)
df_22_4b$prom_lect4b_rbd[df_22_4b$prom_lect4b_rbd==0] <- NA
df_22_4b$prom_mate4b_rbd[df_22_4b$prom_mate4b_rbd==0] <- NA

dat_22_4b <-
  df_22_4b %>%
  select(cod_com_rbd,rbd,prom_lect4b_rbd,prom_mate4b_rbd) %>%
  sjmisc::row_means(prom_lect4b_rbd,prom_mate4b_rbd,n = 1, append = TRUE,var="prom_4b_rbd")

dat_22_2m <-
  df_22_2m %>%
  select(cod_com_rbd,rbd,prom_lect2m_rbd,prom_mate2m_rbd) %>%
  sjmisc::row_means(prom_lect2m_rbd,prom_mate2m_rbd,n = 1, append = TRUE,var="prom_2m_rbd")

df_2022<-
  full_join(dat_22_4b,dat_22_2m, by=c("rbd","cod_com_rbd")) %>%
  select(cod_com_rbd,rbd,prom_4b_rbd,prom_2m_rbd) %>%
  sjmisc::row_means(prom_4b_rbd,prom_2m_rbd,n = 1, append = TRUE,var="prom_2022_rbd")

df_2022_agg <-
  df_2022 %>%
  group_by(cod_com_rbd) %>%
  summarise("prom_simce_2022_comuna" = mean(prom_2022_rbd,na.rm = T),
            "var_simce_2022_comuna" = var(prom_2022_rbd,na.rm = T))
summary(df_2022_agg)

df_2022_agg<- left_join(df_2022_agg,cut[,c("COMUNA","COMUNA_15R")], by=c("cod_com_rbd"="COMUNA"))
summary(df_2022_agg)

join1<- full_join(df_2016_agg,df_2017_agg,by="COMUNA")
join2<- left_join(join1,df_2018_agg,by=c("COMUNA"="cod_com_rbd"))
join3<- left_join(join2,df_2019_agg,by=c("COMUNA"="cod_com_rbd"))
join4<- left_join(join3,df_2022_agg,by=c("COMUNA"="cod_com_rbd"))

df_comuna_rbd <- join4 %>% select(COMUNA,COMUNA_15R,starts_with("prom_simce"),starts_with("var_simce"))
names(df_comuna_rbd)

df_comuna_rbd<- df_comuna_rbd[!is.na(df_comuna_rbd$COMUNA),]
save(df_comuna_rbd,file = here::here("input/data-proc/df_simce_comunas.RData"))

