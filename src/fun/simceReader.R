# Input: Toma los txt con los datos de simce para un curso
# Output: Retorna una tabla con datos de alumnos y padres
curso <- "2b"


simceTableReader <- function(curso){
  ### Leer tabla de alumnos ###
  # Nombre del archivo de alumnos
  alufile <-  glue("simce{curso}2016_alu_publica_final.txt")
  # Los nombres de las variables que tienen matemáticas y lenguaje cambian con el curso.
  # Por lo tanto, se guarda su nombre en una variable
  mateVar <- glue("ptje_mate{curso}_alu")
  lectVar <- glue("ptje_lect{curso}_alu")
  simce_alu <- glue("{simcedir}{alufile}") %>% 
    read_delim(delim = "|") %>%
    select(mrun, rbd, idalumno, grado, cod_curso, gen_alu, 
           mateVar, lectVar) %>%
    rename(ptje_mate_alu = mateVar, 
           ptje_lect_alu = lectVar)
  
  ### Leer tabla de padres ###
  # Nombre archivo padres
  pdrfile <-  glue("simce{curso}2016_cpad_publica_final.txt")
  # El codigo de las preguntas en torno a selección varia con el curso:
  # p15 para 4to básico, p12 para 6to y 2do medio.
  # Por lo tanto, se genera una variable según el curo
  seleVarName <- if_else(curso == "4b", "p15", "p12")
  # Generar los nombres de las variables de selección
  seleVars <- paste0(glue("cpad_{seleVarName}_"),str_pad(rep(1:11), width = "2", pad = "0"))
  
  simce_pdr <- glue("{simcedir}{pdrfile}") %>%
    read_delim(delim = "|") %>%
    select(idalumno,cpad_p01, cpad_p02, cpad_p03, cpad_p07,
           cpad_p08, cpad_p05, cpad_p10, 
           all_of(seleVars)) %>%
    # renombrar variables
    rename(numper = cpad_p05,
           ingrso_hog = cpad_p10,
           cert_nacim = glue("cpad_{seleVarName}_01"),
           eval_presc = glue("cpad_{seleVarName}_02"),
           cert_notas = glue("cpad_{seleVarName}_03"),
           cert_matri = glue("cpad_{seleVarName}_04"),
           cert_bauti = glue("cpad_{seleVarName}_05"),
           cert_remun = glue("cpad_{seleVarName}_06"),
           entr_padrs = glue("cpad_{seleVarName}_07"),
           info_psico = glue("cpad_{seleVarName}_08"),
           prba_ingrs = glue("cpad_{seleVarName}_09"),
           sesn_juego = glue("cpad_{seleVarName}_10"),
           otro_requi = glue("cpad_{seleVarName}_11"))
  # Unir ambas tablas
  
  simce_joined <- simce_alu  %>%
    inner_join(simce_pdr)
  
  return(simce_joined)
           
}
