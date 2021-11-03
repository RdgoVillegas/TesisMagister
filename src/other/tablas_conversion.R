#### Tablas de conversión ####

# Autor: Rodrigo Villegas Salgado
# version: 04-10-21
# email: rdvillegas@uc.cl
# status: production
# rol: data cleaning support

# Este script genera algunas tablas de apoyo para convertir variables categóricas a numéricas.

library(dplyr)
library(readr)

# Tabla de ingresos del hogar, para pasar de factor a numérico.
hh_income <- "codigo:ingreso_cat:ingreso_num
0:NA:NA
1:Menos de $100.000:50000
2:Entre $100.001 y $200.000:150000
3:Entre $200.001 y $300.000:250000
4:Entre $300.001 y $400.000:350000
5:Entre $400.001 y $500.000:450000
6:Entre $500.001 y $600.000:550000
7:Entre $600.001 y $800.000:700000
8:Entre $800.001 y $1.000.000:900000
9:Entre $1.000.001 y $1.200.000:1100000
10:Entre $1.200.001 y $1.400.000:1300000
11:Entre $1.400.001 y $1.600.000:1500000
12:Entre $1.600.001 y $1.800.000:1700000
13:Entre $1.800.001 y $2.000.000:1900000
14:Entre $2.000.001 y $2.200.000:2100000
15:Más de $2.200.000:2300000
99:NA:NA" %>% 
  read_delim(delim = ":")
# Costo de matricula
costo_matricula <- "pago_matricula:costo_matricula
GRATUITO:0
$1.000 A $10.000:5000
$10.001 A $25.000:17500
25.001 A $50.000:37500
$50.001 A $100.000:75000
MAS DE $100.000:150000" %>% 
  read_delim(delim = ":")
# Costo mensual
costo_mensual <- "pago_mensual:costo_mensual
GRATUITO:0
$1.000 A $10.000:5000
$10.001 A $25.000:17500
$25.001 A $50.000:37500
$50.001 A $100.000:75000
MAS DE $100.000:150000" %>% 
  read_delim(delim = ":")

saveRDS(hh_income, file = "data/temp/tablas_conversion/hh_income.RDS")
saveRDS(costo_matricula, file = "data/temp/tablas_conversion/costo_matricula.RDS")
saveRDS(costo_mensual, file = "data/temp/tablas_conversion/costo_mensual.RDS")
