#### Limpieza de datos alumnos ####

# Autor: Rodrigo Villegas Salgado
# version: 04-10-21
# email: rdvillegas@uc.cl
# status: development
# rol: data cleaning


library(tidyverse)
library(data.table)
library(here)
library(glue)


rawdatadir <- here("data/raw/")

simcedir <- glue(rawdatadir, "/simce/")
tabsdir <- glue(here(), "/data/temp/tablas_conversion/")
rbddir <- glue(rawdatadir, "/rbd/")
aludir <- glue(rawdatadir, "/registro/")

# Leer funciones auxiliares
source("src/fun/simceReader.R")



##### Leer tablas #####

### Leer tablas de escuelas ###

# Estas tablas contienen conversion de rangos a numerico en rbds
costo_matricula <- readRDS(glue("{tabsdir}/costo_matricula.RDS"))
costo_mensual <- readRDS(glue("{tabsdir}/costo_mensual.RDS"))

rbd_rms <- paste0(rbddir, "directorio_oficial_EE_2016_Oficial.csv") %>% 
  fread(encoding = "UTF-8") %>%
  filter(cod_pro_rbd == 131 | cod_com_rbd %in% c(13401, 13201), latitud < 0, 
         pago_mensual != "SIN INFORMACION", pago_matricula != "SIN INFORMACION") %>%
  select(rbd, cod_depe2, latitud, longitud, pago_matricula, pago_mensual) %>%
  left_join(costo_matricula) %>%
  left_join(costo_mensual) %>%
  rename(rbdlat = latitud,
         rbdlon = longitud) %>%
  mutate(cod_depe2 = recode(cod_depe2, `1` = "Municipal", 
                            `2` = "Subvencionado", 
                            `3` = "Particular", 
                            `4` = "Otro")) %>%
  select(-pago_mensual, -pago_matricula)

### Leer tablas de simce ###
# Estas tablas contienen conversion de rangos a numerico en ingreso
hh_income <- readRDS(glue("{tabsdir}/hh_income.RDS"))

# 4to Básico
simce4b <- simceTableReader("4b")
# 6to Básico 
simce6b <- simceTableReader("6b")
# 2do Medio
simce2m <- simceTableReader("2m")
# Unir tablas
simce <- data.frame() %>%
  rbind(simce4b) %>%
  rbind(simce6b) %>%
  rbind(simce2m) %>%
  left_join(hh_income, by = c("ingrso_hog" = "codigo")) %>%
  filter(rbd %in% rbd_rms$rbd) 

#### Calcular puntaje simce por colegio y grado ####

ptjesimce <- simce %>%
  select(rbd, grado, ptje_mate_alu, ptje_lect_alu) %>%
  group_by(rbd) %>%
  mutate(simce_rbd = mean(c(ptje_mate_alu, ptje_lect_alu), na.rm = T)) %>%
  group_by(rbd, grado) %>%
  summarise(simce_grado = mean(c(ptje_mate_alu, ptje_lect_alu), na.rm = T),
         simce_rbd = unique(simce_rbd))
  
#### Calcular NSE ####
# Para la escolaridad, como se desconoce quién es el jefe de hogar y su educación,
# Pero se tiene la escolaridad del padre y la madre, se asume que el jefe de hogar
# Es el padre o madre de mayor escolaridad.
# El código para hacer la clasificación se encuentra en nseClassifier.R

source("src/fun/nseClassifier.R")

##### Calcular Entropía #####

# Para calcular entropía se calcula el total de estudiantes por grado escolar, y escuela
# Luego se calcula la entropía según ambas escalas

# Cargar funciones de ayuda
source("src/fun/entropy.R")
# Calcular entropía a nivel de grado y escuela

entropia_grado <- entropy_simce(simce, levelName = "grado", rbd, grado)
entropia_rbd <- entropy_simce(simce, levelName = "rbd", rbd)

# Unir ambas tablas.

simce_segre <- entropia_rbd %>%
  left_join(entropia_grado) %>%
  select(rbd, grado, entropy_rbd, entropy_grado)

##### Generar Indicador Selectividad #####
# El indicador de selectividad se divide en 3 dimensiones:
# 1 - Selectividad académica: game sessions or written exams
# 2 - Selectividad socioeconómica: wage income certificate or attend interview
# 3 - Selectividad religiosa: baptismal or religious marriage certificate

selectividad_alu <- simce %>%
  select(rbd, mrun, idalumno, cert_nacim, eval_presc, cert_notas, cert_matri, cert_bauti,
         cert_remun,entr_padrs,info_psico,prba_ingrs,sesn_juego,otro_requi) %>%
  mutate(s_academica = case_when(sesn_juego == T | prba_ingrs == T  ~ 1,
                                 T ~ 0),
         s_socioecon = case_when(cert_remun == T | entr_padrs == T  ~ 1,
                                 T ~ 0),
         s_religiosa = case_when(cert_matri == T | cert_bauti == T ~ 1,
                                 T ~ 0)) %>%
  select(rbd, mrun, idalumno, s_academica, s_socioecon, s_religiosa)
summary(selectividad_alu)

selectividad_rbd <- simce %>%
  select(rbd, mrun, idalumno, cert_nacim, eval_presc, cert_notas, cert_matri, cert_bauti,
         cert_remun,entr_padrs,info_psico,prba_ingrs,sesn_juego,otro_requi) %>%
  mutate(s_academica = case_when(sesn_juego == T | prba_ingrs == T  ~ 1,
                                 T ~ 0),
         s_socioecon = case_when(cert_remun == T | entr_padrs == T  ~ 1,
                                 T ~ 0),
         s_religiosa = case_when(cert_matri == T | cert_bauti == T ~ 1,
                                 T ~ 0)) %>%
  select(rbd, mrun, idalumno, s_academica, s_socioecon, s_religiosa) %>%
  group_by(rbd) %>%
  summarise(s_academica_rbd = sum(s_academica) / n(),
            s_socioecon_rbd = sum(s_socioecon) / n(),
            s_religiosa_rbd = sum(s_religiosa) / n())
summary(selectividad_rbd)


# selectividad <- simce %>%
#   select(rbd, cert_nacim,eval_presc,cert_notas,cert_matri,cert_bauti,
#          cert_remun,entr_padrs,info_psico,prba_ingrs,sesn_juego,otro_requi) %>%
#   group_by(rbd) %>%
#   summarise(cert_nacim = sum(cert_nacim)/n(), # Social
#             eval_presc = sum(eval_presc)/n(), # Académico
#             cert_notas = sum(cert_notas)/n(), # Académico
#             cert_matri = sum(cert_matri)/n(), # Religioso
#             cert_bauti = sum(cert_bauti)/n(), # Religioso
#             cert_remun = sum(cert_remun)/n(), # Social
#             entr_padrs = sum(entr_padrs)/n(), # Social
#             info_psico = sum(info_psico)/n(), # Social
#             prba_ingrs = sum(prba_ingrs)/n(), # Académico
#             sesn_juego = sum(sesn_juego)/n(), # Académico
#             otro_requi = sum(otro_requi)/n(),
#             s_academica = case_when(eval_presc > 0.5 | cert_notas > 0.5 | sesn_juego > 0.5 | prba_ingrs > 0.5 ~ T,
#                                     T ~ F),
#             s_socioecom = case_when(cert_nacim > 0.5 | cert_remun > 0.5 | entr_padrs > 0.5 | info_psico > 0.5 ~ T,
#                                     T ~ F),
#             s_religiosa = case_when(cert_matri > 0.5 | cert_bauti > 0.5 ~ T,
#                                     T ~ F)) 
##### Generar tabla de alumnos #####
simce_alu <- simce %>%
  mutate(sexo = if_else(gen_alu == 1, "M", "F"))%>%
  select(mrun, idalumno, rbd, sexo, cod_curso, grado) 
##### Generar tabla de transporte #####

#### Hay cerca de 2000 casos con valores NA. ####
# Voy a tratar de rescatar la mayoría , ya que sí tienen coords de destino 
# y el análisis revela que no son zonas aisladas

simce_tran <- paste0(aludir, "basefinal_0419_nomissscore.csv") %>% read_csv2 %>%
  mutate(sexo = if_else(gen_alu == 1, "M", "F")) %>%
  filter(mrun %in% simce$mrun) %>%
  mutate(hogarlat = str_extract(coordsorigen, ".*(?=[,])") %>% as.numeric,
         hogarlon = str_remove(coordsorigen, ".*,") %>% as.numeric) %>%
  select(mrun, rbd, sexo, dist_walk, dist_tran, time_walk, time_tran, hogarlat, hogarlon)

##### Obtener NSE del barrio #####
library(sf)

nse_barrio_zc <- read_sf(glue(rootdir, "spatial microsimulation/output/entropy_2scales_v1.4.1.shp")) %>%
  st_transform(32719) %>%
  select(ENTWGT)

nse_barrio <- paste0(aludir, "basefinal_0419_nomissscore.csv") %>% read_csv2 %>%
  filter(mrun %in% simce$mrun) %>%
  mutate(sexo = if_else(gen_alu == 1, "M", "F"),
         hogarlat = str_extract(coordsorigen, ".*(?=[,])") %>% as.numeric,
         hogarlon = str_remove(coordsorigen, ".*,") %>% as.numeric) %>%
  select(mrun, rbd, sexo, hogarlat, hogarlon) %>%
  st_as_sf(coords = c("hogarlon", "hogarlat"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(32719) %>%
  st_intersection(nse_barrio_zc) %>%
  `st_geometry<-`(NULL)


load(glue(rootdir, "distances_v2.RDS"))



##### Combinar datos #####
# Los casos con MRUN duplicados se eliminarn
simce_out <- simce_alu %>%
  right_join(rbd_rms) %>%
  right_join(simce_segre) %>%
  right_join(simce_tran) %>%
  right_join(selectividad_alu) %>%
  right_join(selectividad_rbd) %>%
  left_join(ptjesimce, by = c("rbd", "grado")) %>%
  inner_join(nse) %>%
  inner_join(nse_barrio, by = c("mrun", "rbd", "sexo")) %>%
  filter(!duplicated(mrun)) %>%
  mutate(id = paste(mrun, idalumno)) %>%
  left_join(distances) %>%
  select(-dist_walk, id) %>%
  rename(dist_walk = trip.distance)

simce_shp <- simce_out %>%
  st_as_sf(coords = c("hogarlon", "hogarlat"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(32719) %>%
  st_write(glue(rootdir, "shp/simce_sf/simce_alu.shp"), driver = "ESRI Shapefile", append=FALSE)

save(simce_out, file = glue(rootdir, "alu_data.RDS"))



