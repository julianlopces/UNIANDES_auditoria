#### Importar datos desde Survey ####

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("email") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


data_ejemplo <-read_excel("data/data_ejemplo_uniandes.xlsx")
vars_needed <- colnames(data_ejemplo)
vars_needed <- c(vars_needed,"violence_reasons_12",
  paste0("social_friends_o_", 1:15),
  paste0("social_recess_", 1:15),
  paste0("social_recess_o_", 1:15),
  paste0("social_house_1_", 1:15),
  paste0("social_house_1_o_", 1:15),
  paste0("social_study_1_", 1:15),
  paste0("social_study_1_o_", 1:15),
  paste0("social_game_1_", 1:15),
  paste0("social_game_1_o_", 1:15),
  paste0("social_academic_1_", 1:15),
  paste0("social_academic_1_o_", 1:15),
  paste0("social_academic2_1_", 1:15),
  paste0("social_academic2_1_o_", 1:15),
  paste0("social_personal_1_", 1:15),
  paste0("social_personal_1_o_", 1:15),
  paste0("social_personal2_1_", 1:15),
  paste0("social_personal2_1_o_", 1:15),
  paste0("social_friend_wish_1_", 1:15),
  paste0("social_friend_wish_1_o_", 1:15),
  paste0("social_friend_wish_2_", 1:15),
  paste0("social_friend_wish_2_o_", 1:15),
  paste0("social_house_2_", 1:15),
  paste0("social_house_2_o_", 1:15),
  paste0("social_study_2_", 1:15),
  paste0("social_study_2_o_", 1:15),
  paste0("social_game_2_", 1:15),
  paste0("social_game_2_o_", 1:15),
  paste0("social_academic_2_", 1:15),
  paste0("social_academic_2_o_", 1:15),
  paste0("social_academic2_2_", 1:15),
  paste0("social_academic2_2_o_", 1:15),
  paste0("social_personal_2_", 1:15),
  paste0("social_personal_2_o_", 1:15),
  paste0("social_personal2_2_", 1:15),
  paste0("social_personal2_2_o_", 1:15),
  paste0("social_work_1_o_",        1:5),
  paste0("social_work2_1_o_",       1:5),
  paste0("social_work3_1_o_",       1:5),
  paste0("social_leadership_1_o_",  1:5),
  paste0("social_academic_skills_1_o_", 1:5),
  paste0("social_popularity_1_o_",  1:5),
  paste0("social_shyness_1_o_",     1:5),
  
  paste0("social_work_2_o_",        1:5),
  paste0("social_work2_2_o_",       1:5),
  paste0("social_work3_2_o_",       1:5),
  paste0("social_leadership_2_o_",  1:5),
  paste0("social_academic_skills_2_o_", 1:5),
  paste0("social_popularity_2_o_",  1:5),
  paste0("social_shyness_2_o_",     1:5)
)


# Completar aquí para descargar datos de survey 

# Download de API ----------------------------------------------------------

## Conect to SurveyCTO ----------------------------------------------------------------

API <- paste0('https://',server,'.surveycto.com/api/v2/forms/data/wide/json/',formid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 10
attempt <- 1

repeat {
  # Llamada a la API
  dataset_json <- POST(
    url = API,
    config = authenticate(email, password),
    add_headers("Content-Type: application/json"),
    encode = 'json'
  )
  
  # Convertir JSON a data frame
  data <- jsonlite::fromJSON(rawToChar(dataset_json$content), flatten = TRUE)
  
  # Si df es un data frame válido, salir del ciclo
  if (is.data.frame(data)) break
  
  # Si se alcanzó el número máximo de intentos, lanzar error y salir
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}

# Transformar base de datos ----------------------------------------------------


for (v in vars_needed) {
  if (!(v %in% names(data))) {
    data[[v]] <- rep(NA, nrow(data))
  }
}

# Organizar variables

# Reordenar y dejar las demás al final
otras_vars <- setdiff(names(data), vars_needed)
data <- data[ , c(vars_needed, otras_vars)]


# Nombres de las variables de opción múltiple
multi_vars <- c("which_data",
                "house_members",
                "house_appliances",
                "work_days",
                "work_pay_how",
                "student_repeat"
)



for (var in multi_vars) {
  var_cols <- names(data)[startsWith(names(data), paste0(var, "_")) & !grepl("_o$", names(data))]
  
  if (length(var_cols) > 0) {
    data <- data %>%
      rowwise() %>%
      mutate(!!var := {
        vals <- c_across(all_of(var_cols))
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          activos <- which(vals == 1)
          if (length(activos) == 0) NA_character_ else {
            seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
            paste(seleccionados, collapse = ",")
          }
        }
      }) %>%
      ungroup()
  }
}

# # Unificar variables

data <- data %>%
  mutate(
    # Reemplazar strings vacíos en columnas "pull" por NA
    across(contains("pull"), ~if_else(str_squish(.) == "", NA_character_, .)),
    
    # Limpiar nombres
    name1 = na_if(str_squish(student_name1), ""),
    name2 = na_if(str_squish(student_name2), ""),
    name3 = na_if(str_squish(student_name3), ""),
    name4 = na_if(str_squish(student_name4), ""),
    
    # Convertir "9999" en NA explícitamente
    name2 = if_else(name2 == "9999" | name2 %in% c("NO TIENE","No Tiene","NO","no tiene"), NA_character_, name2),
    name4 = if_else(name4 == "9999" | name4 %in% c("NO TIENE","No Tiene","NO","no tiene"), NA_character_, name4),
    
    # Construcción condicional del nombre completo
    nombre_concatenado = case_when(
      is.na(name2) & !is.na(name3) & !is.na(name4) ~ str_c(name1, name3, name4, sep = " "),
      is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name3, sep = " "),
      is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name4, sep = " "),
      is.na(name2) & is.na(name3) & is.na(name4) ~ name1,
      !is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name2, name3, sep = " "),
      !is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name2, name4, sep = " "),
      !is.na(name2) & is.na(name3) & is.na(name4) ~ str_c(name1, name2, sep = " "),
      TRUE ~ str_c(name1, name2, name3, name4, sep = " ")
    ),
    
    # Limpiar espacios finales
    nombre_concatenado = str_squish(nombre_concatenado),
    age_final = coalesce(edad_pull,edad_corr,student_age),
    gender_final = coalesce(genero_pull,gender),
    gender_final = case_when(
      gender_final == "MUJER" ~ "2",
      gender_final == "HOMBRE" ~ "1",
      TRUE ~ gender_final)) %>%
  mutate(
    # Reemplazar strings vacíos en columnas específicas de nombres "reject" por NA
    across(all_of(c("student_name1_reject", "student_name2_reject", 
                    "student_name3_reject", "student_name4_reject")), 
           ~if_else(str_squish(.) == "", NA_character_, .)),
    
    # Limpiar nombres
    name1_reject = na_if(str_squish(student_name1_reject), ""),
    name2_reject = na_if(str_squish(student_name2_reject), ""),
    name3_reject = na_if(str_squish(student_name3_reject), ""),
    name4_reject = na_if(str_squish(student_name4_reject), ""),
    
    # Convertir "9999" en NA explícitamente
    name2_reject = if_else(name2_reject == "9999", NA_character_, name2_reject),
    name4_reject = if_else(name4_reject == "9999", NA_character_, name4_reject),
    
    # Construcción condicional del nombre completo
    nombre_concatenado_reject = case_when(
      is.na(name2_reject) & !is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name3_reject, name4_reject, sep = " "),
      is.na(name2_reject) & !is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name3_reject, sep = " "),
      is.na(name2_reject) & is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name4_reject, sep = " "),
      is.na(name2_reject) & is.na(name3_reject) & is.na(name4_reject) ~ name1_reject,
      !is.na(name2_reject) & !is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name2_reject, name3_reject, sep = " "),
      !is.na(name2_reject) & is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name2_reject, name4_reject, sep = " "),
      !is.na(name2_reject) & is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name2_reject, sep = " "),
      TRUE ~ str_c(name1_reject, name2_reject, name3_reject, name4_reject, sep = " ")
    ),
    
    # Limpiar espacios finales
    nombre_concatenado_reject = str_squish(nombre_concatenado_reject),
    # Nombre final priorizando nombre_pull sobre el concatenado
    name_final = str_to_upper(coalesce(nombre_pull, nombre_concatenado, nombre_concatenado_reject)),
    name_final = if_else(str_squish(name_final) == "", NA_character_, name_final),
    school_final = coalesce(colegio_pull_id,student_school_reject,student_school))

# Ajustar fechas para campo real


data <- data %>%
  mutate(SubmissionDate = mdy_hms(SubmissionDate),
         starttime = mdy_hms(SubmissionDate))%>%
  filter(SubmissionDate >= mdy_hms("Oct 26, 2025 12:00:00 AM"))

# Ajustar id

data <- data %>%
  mutate(student_id_final = coalesce(student_id,student_id_uuid))


# Importar nombres de colegios

meta_colegios <- read_sheet(id_alertas,
                            sheet = "meta_colegios")

data <- data %>%
  mutate(
    colegio_pull_id = if_else(is.na(colegio_pull_id),school_final,colegio_pull_id))%>%
  left_join(meta_colegios%>%select(COD_MODULAR,COLEGIO)%>%mutate(COD_MODULAR=as.character(COD_MODULAR),
                                                                 COLEGIO = as.character(COLEGIO)),
            by = c("colegio_pull_id"="COD_MODULAR"))%>%
  mutate(colegio_pull = coalesce(colegio_pull,COLEGIO))%>%
  select(-COLEGIO)



