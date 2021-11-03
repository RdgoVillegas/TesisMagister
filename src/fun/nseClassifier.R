# Script para calcular NSE
# Toma dos archivos: tramos de ingreso, y nse por tramo y escolaridad
# Luego toma los datos del simce y los combina.

#### Procesamiento ####

# 1. Tramos de ingreso

# Leer los tramos de ingreso, y generar columnas de ingreso mínimo y máximo según tamaño del hogar
tramos <- read.csv2("data/raw/nse/tramos.csv") %>%
  rownames_to_column("tramo") %>%
  pivot_longer(names_to = "numper", values_to = "income", cols = -tramo) %>%
  group_by(numper) %>%
  mutate(numper = str_extract(numper, "\\d+") %>% as.numeric(),
         min = lag(income),
         min = case_when(is.na(min) ~ 0.0, T ~ as.numeric(min)),
         max = income) %>%
  select(-income)

# 2. Clasificación nse según tramo y escolaridad
# Leer excel que contiene el nse según tramo de ingreso y nivel educativo
nseclass <- readxl::read_xlsx("data/raw/nse/reclass_nse.xlsx") %>%
  pivot_longer(names_to = "tramo", cols = -1) %>%
  rename("NivelEducativo" = 1, nse = value)
# Generar nombres para compatibilizar con datos simce
nsevarnames <- data.frame(NivelEducativo = unique(nseclass$NivelEducativo),
                          varName = c("basica_inc", "basica_com", "media_inc", 
                                      "media_com", "univ_inc/tec_com", "univ_com"))
# Unir tablas
nseclass <- left_join(nseclass, nsevarnames)

# 3. Calcular nse

# Calcular nse
nse <- simce %>%
  filter(!is.na(ingreso_num), !numper %in% c(0, 99)) %>%
  # Definir escolaridad de padres
  mutate(esc_padre = case_when(cpad_p07 <= 8 ~ "basica_inc",
                               cpad_p07 == 9 ~ "basica_com",
                               cpad_p07 %in% 10:12 ~ "media_inc",
                               cpad_p07 %in% 13:14 ~ "media_com",
                               cpad_p07 %in% 15:17 ~ "univ_inc/tec_com",
                               cpad_p07 %in% 18:20 ~ "univ_com",
                               T ~ NA_character_) %>% 
           factor(levels = c("basica_inc", "basica_com", "media_inc", 
                             "media_com", "univ_inc/tec_com", "univ_com"), ordered = T),
         esc_madre = case_when(cpad_p08 <= 8 ~ "basica_inc",
                               cpad_p08 == 9 ~ "basica_com",
                               cpad_p08 %in% 10:12 ~ "media_inc",
                               cpad_p08 %in% 13:14 ~ "media_com",
                               cpad_p08 %in% 15:17 ~ "univ_inc/tec_com",
                               cpad_p08 %in% 18:20 ~ "univ_com",
                               T ~ NA_character_) %>% 
           factor(levels = c("basica_inc", "basica_com", "media_inc", 
                             "media_com", "univ_inc/tec_com", "univ_com"), ordered = T)) %>%
  # Definir escolaridad del hogar como el máximo entre la madre y el padre
  transform(esc_name = pmax(esc_padre, esc_madre, na.rm = T)) %>%
  select(mrun, idalumno, esc_name, ingreso_num, numper) %>%
  rename(ytoth = ingreso_num) %>%
  # Calcular ingresos del hogar. a numper se le suma 1 para considerar el estudiante
  mutate(ytoth = ytoth / 1000,
         numper = numper + 1) %>%
  # Unir tabla con tramos de ingreso
  left_join(tramos) %>%
  filter(ytoth >= min & ytoth < max) %>%
  # Asignar nse
  left_join(nseclass, by = c("tramo", "esc_name" = "varName")) %>%
  select(mrun, idalumno, nse) %>%
  filter(!is.na(nse))





