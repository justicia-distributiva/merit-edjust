if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(dplyr, knitr, kableExtra, haven, acid, DescTools)

cut <- read.csv(file = here::here(("input/data-original/cut.csv")))

list.files('input/data-original/matricula', pattern="(*.Matricula)", full.names=T,recursive = T)


# Matricula 2016 ----------------------------------------------------------
mat16 <- read.csv(here::here("input/data-original/matricula/Resumen_Matricula_EE_Oficial_2016.csv"),sep = ";")

table(mat16$cod_depe2) # 3=particular pagado

df16 <-
  mat16 %>%
  dplyr::select(rbd,cod_depe2,cod_com_rbd,mat_total) %>%
  mutate(ppagado=ifelse(cod_depe2==3,1,0)) %>%
  mutate(mat_pagado=ifelse(ppagado==1,mat_total,0))

mat_16 <-
  df16 %>%
  group_by(cod_com_rbd) %>%
  summarise(total_mat_pagado_16=sum(mat_pagado),total_mat_16=sum(mat_total)) %>%
  mutate(prop_pagado_16=total_mat_pagado_16/total_mat_16) %>%
  left_join(.,cut[,c("COMUNA","COMUNA_15R")],by=c("cod_com_rbd"="COMUNA_15R")) %>%
  select(cod_com_rbd_15=cod_com_rbd,cod_com_rbd_16=COMUNA,everything())

summary(mat_16)

# Matricula 2017 ----------------------------------------------------------
mat17 <- read.csv(here::here("input/data-original/matricula/Resumen_Matricula_EE_Oficial_2017.csv"),sep = ";")
summary(mat17$COD_COM_RBD)

table(mat17$COD_DEPE2) # 3=particular pagado

df17 <-
  mat17 %>%
  dplyr::select(RBD,COD_DEPE2,COD_COM_RBD,MAT_TOTAL) %>%
  mutate(ppagado=ifelse(COD_DEPE2==3,1,0)) %>%
  mutate(mat_pagado=ifelse(ppagado==1,MAT_TOTAL,0))

names(df17) <- tolower(names(df17))

mat_17 <-
  df17 %>%
  group_by(cod_com_rbd) %>%
  summarise(total_mat_pagado_17=sum(mat_pagado),total_mat_17=sum(mat_total)) %>%
  mutate(prop_pagado_17=total_mat_pagado_17/total_mat_17) %>%
  left_join(.,cut[,c("COMUNA","COMUNA_15R")],by=c("cod_com_rbd"="COMUNA_15R")) %>%
  select(cod_com_rbd_15=cod_com_rbd,cod_com_rbd_16=COMUNA,everything())

summary(mat_17)

# Matricula 2018 ----------------------------------------------------------
mat18 <- read.csv(here::here("input/data-original/matricula/Resumen_Matricula_EE_Oficial_2018.csv"),sep = ";")
summary(mat18$COD_COM_RBD)

table(mat17$COD_DEPE2) # 3=particular pagado

df18 <-
  mat18 %>%
  dplyr::select(RBD,COD_DEPE2,COD_COM_RBD,MAT_TOTAL) %>%
  mutate(ppagado=ifelse(COD_DEPE2==3,1,0)) %>%
  mutate(mat_pagado=ifelse(ppagado==1,MAT_TOTAL,0))

names(df18) <- tolower(names(df18))

mat_18 <-
  df18 %>%
  group_by(cod_com_rbd) %>%
  summarise(total_mat_pagado_18=sum(mat_pagado),total_mat_18=sum(mat_total)) %>%
  mutate(prop_pagado_18=total_mat_pagado_18/total_mat_18) %>%
  left_join(.,cut[,c("COMUNA","COMUNA_15R")],by=c("cod_com_rbd"="COMUNA_15R")) %>%
  select(cod_com_rbd_15=cod_com_rbd,cod_com_rbd_16=COMUNA,everything())

summary(mat_18)

# Matricula 2019 ----------------------------------------------------------
mat19 <- read.csv(here::here("input/data-original/matricula/Resumen_Matricula_EE_Oficial_2019.csv"),sep = ";")
summary(mat19$COD_COM_RBD)# usar COMUNA

table(mat19$COD_DEPE2) # 3=particular pagado

df19 <-
  mat19 %>%
  dplyr::select(RBD,COD_DEPE2,COD_COM_RBD,MAT_TOTAL) %>%
  mutate(ppagado=ifelse(COD_DEPE2==3,1,0)) %>%
  mutate(mat_pagado=ifelse(ppagado==1,MAT_TOTAL,0))

names(df19) <- tolower(names(df19))

mat_19 <-
  df19 %>%
  group_by(cod_com_rbd) %>%
  summarise(total_mat_pagado_19=sum(mat_pagado),total_mat_19=sum(mat_total)) %>%
  mutate(prop_pagado_19=total_mat_pagado_19/total_mat_19) %>%
  left_join(.,cut[,c("COMUNA","COMUNA_15R")],by=c("cod_com_rbd"="COMUNA")) %>%
  select(cod_com_rbd_15=COMUNA_15R,cod_com_rbd_16=cod_com_rbd,everything())

summary(mat_19)

# Matricula 2022 ----------------------------------------------------------
mat22 <- read.csv(here::here("input/data-original/matricula/Resumen_Matricula_EE_Oficial_2022.csv"),sep = ";")
summary(mat22$COD_COM_RBD)# usar COMUNA


table(mat19$COD_DEPE2) # 3=particular pagado

df22 <-
  mat22 %>%
  dplyr::select(RBD,COD_DEPE2,COD_COM_RBD,MAT_TOTAL) %>%
  mutate(ppagado=ifelse(COD_DEPE2==3,1,0)) %>%
  mutate(mat_pagado=ifelse(ppagado==1,MAT_TOTAL,0))

names(df22) <- tolower(names(df22))

mat_22 <-
  df22 %>%
  group_by(cod_com_rbd) %>%
  summarise(total_mat_pagado_22=sum(mat_pagado),total_mat_22=sum(mat_total)) %>%
  mutate(prop_pagado_22=total_mat_pagado_22/total_mat_22) %>%
  left_join(.,cut[,c("COMUNA","COMUNA_15R")],by=c("cod_com_rbd"="COMUNA")) %>%
  select(cod_com_rbd_15=COMUNA_15R,cod_com_rbd_16=cod_com_rbd,everything())


join1 <- left_join(mat_16,mat_17,by=c("cod_com_rbd_15","cod_com_rbd_16"))
join2 <- left_join(join1,mat_18,by=c("cod_com_rbd_15","cod_com_rbd_16"))
join3 <- left_join(join2,mat_19,by=c("cod_com_rbd_15","cod_com_rbd_16"))
summary(join3)
join4 <- left_join(join3,mat_22,by=c("cod_com_rbd_15","cod_com_rbd_16"))
summary(join4)

df_matricula_comunas_wide <-
  join4 %>%
  select(cod_com_rbd_15,cod_com_rbd_16,
         starts_with("total_mat_pagado"),
         starts_with("total_mat"),
         starts_with("prop_pagado")) %>%
  mutate(pagadolag22_16 = prop_pagado_22 - prop_pagado_16)

skimr::skim(df_matricula_comunas_wide)
save(df_matricula_comunas_wide, file = here::here("input/data-proc/df_matricula_comunas_wide.RData"))

mat_16_long <- mat_16;names(mat_16_long) <-
  c("cod_com_rbd_15","cod_com_rbd_16","total_mat_pagado","total_mat","prop_pagado")
mat_16_long$year <- 2016

mat_17_long <- mat_17;names(mat_17_long) <-
  c("cod_com_rbd_15","cod_com_rbd_16","total_mat_pagado","total_mat","prop_pagado")
mat_17_long$year <- 2017

mat_18_long <- mat_18;names(mat_18_long) <-
  c("cod_com_rbd_15","cod_com_rbd_16","total_mat_pagado","total_mat","prop_pagado")
mat_18_long$year <- 2018

mat_19_long <- mat_19;names(mat_19_long) <-
  c("cod_com_rbd_15","cod_com_rbd_16","total_mat_pagado","total_mat","prop_pagado")
mat_19_long$year <- 2019

mat_22_long <- mat_22;names(mat_22_long) <-
  c("cod_com_rbd_15","cod_com_rbd_16","total_mat_pagado","total_mat","prop_pagado")
mat_22_long$year <- 2022


df_matricula_comunas_long<-
  bind_rows(mat_16_long,mat_17_long,mat_18_long,mat_19_long,mat_22_long) %>%
  select(cod_com_rbd_15,cod_com_rbd_16,year,everything()) %>%
  arrange(cod_com_rbd_15)

save(df_matricula_comunas_long, file = here::here("input/data-proc/df_matricula_comunas_long.RData"))

