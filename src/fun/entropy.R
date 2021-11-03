# Theil's Entropy Index.
# Input: un vector con las proporciones de grupos socioeconómicos
# Output: el indice de entropía de Theil para una unidad dada 

entropy_index <- function(...){
  input_vector <- c(...)
  modified_vector <- input_vector[input_vector != 0]
  output <- -1*sum(modified_vector * log(modified_vector))/log(length(input_vector))
  return(output)
}

# Indice de entropia para un dataset
# Input: Una tabla, y la variable con la cual agrupar
# Output: El índice de entropía para cada fila en función de su variable de agrupación.
# Actualmente funciona sólo con grupos socioeconómicos pre-establecidos: ABC1, C2, C3, D y E
# A futuro me gustaría implementar grupos customizables.
entropy_grupo <- function(.data, ...) {
  .data %>%
    mutate(ABC1 = ABC1/Total,
           C2 = C2/Total,
           C3 = C3/Total,
           D = D/Total,
           E = E/Total,
           Weight = Total/sum(Total)) %>%
    group_by( ... ) %>%
    mutate(entropy = (entropy_index(ABC1, C2, C3, D, E)),
           entropy_weighted = Weight * entropy)
}

# Toma los datos del simce, y calcula en indice de entropia según grupo: escuela o grado.
# Podría servir para más grupos de ser de interés.
# Input: datos simce, nombre de la agrupación, variables de agrupación
# Output: tabla con variables de agupamiento y el indice de entropía para esa agrupación.
entropy_simce <- function(.data, levelName, ...){
  levelName = paste0("entropy_", levelName)
  .data %>%
    select(rbd, mrun, grado, idalumno) %>%
    right_join(nse) %>%
    group_by(..., nse) %>%
    summarise(value = n()) %>%
    mutate(Total = sum(value)) %>%
    pivot_wider(names_from = nse, values_from = value, values_fill = 0) %>%
    ungroup %>%
    entropy_grupo(...) %>%
    rename( "{levelName}" := entropy) %>%
    select(..., levelName)
}
