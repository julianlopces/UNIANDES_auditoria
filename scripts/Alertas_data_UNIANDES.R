##### Extraer tracking

#### Alertas ####

alertas <- data 

# Crear duración

alertas <- alertas %>% 
  mutate(duration_minutes = round((as.numeric(duration)/60),2))

# Flag duración
dev_estandar <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(sd_duracion = sd(duration_minutes),
            median_duracion = mean(duration_minutes)) %>%
  pull(sd_duracion)

median_duration <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(sd_duracion = sd(duration_minutes),
            median_duracion = median(duration_minutes)) %>%
  pull(median_duracion)



alertas <- alertas %>%
  mutate(
    flag_duration_mas = if_else(
      ((duration_minutes - median_duration) / dev_estandar > 3), 1, 0, missing = 0
    ),
    flag_duration_menos = if_else(
      ((duration_minutes - median_duration) / dev_estandar < -2), 1, 0, missing = 0
    )
  )

alertas <- alertas %>%
  mutate(flag_duration_mas = if_else(duration_minutes >= 130,0,flag_duration_mas),
         flag_duration_menos = if_else(duration_minutes <= 20,1,flag_duration_menos))


### Missings y saltos


ODK_filtrado <- odkmissing::import_odk_propagate_required("data/Estudiantes_uniandes.xlsx", required_value = "TRUE")


ODK_procesado <- odkmissing::build_spec_for_flags(datos = alertas, ODK_filtrado = ODK_filtrado)


spec_for_flags <- ODK_procesado$spec_for_flags
datos_tokens   <- ODK_procesado$datos_tokens


spec_for_flags <- spec_for_flags %>%
  filter(!str_detect(var,"social"))


# Levantar missings


alertas <- odkmissing::flags_missing_por_variable(
  data          = datos_tokens,
  spec          = spec_for_flags,
  prefix        = "m",
  numeric_conds = TRUE,
  coerce_target = FALSE
)

variables_missing <- names(alertas)[grepl("^m_", names(alertas))]

alertas <- alertas |>
  dplyr::mutate(
    total_missing = rowSums(dplyr::pick(dplyr::all_of(variables_missing)), na.rm = TRUE),
    flag_missing  = ifelse(total_missing > 0, 1, 0)
  )


# Levantar saltos

alertas <- odkmissing::create_skip_vars(
  data          = alertas,
  spec          = spec_for_flags,
  prefix        = "s",
  numeric_conds = TRUE
)

# Alerta de valores numéricos extremos ####


alertas <- alertas %>%
  mutate(
    ex_house_number = if_else(
      abs((as.numeric(house_number) - median(as.numeric(house_number), na.rm = T)) / sd(as.numeric(house_number), na.rm = T)) > 3, 1, 0, missing = 0
    ),
    ex_house_rooms = if_else(
      abs((as.numeric(house_rooms) - median(as.numeric(house_rooms), na.rm = T)) / sd(as.numeric(house_rooms), na.rm = T)) > 3, 1, 0, missing = 0
    ))



## DUPLICADOS ----


# Ajustar ID


caract_especi_mayus <- c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")

alertas <- alertas %>%
  mutate(
    nombre = str_squish(str_replace_all(toupper(name_final), caract_especi_mayus))
  )

alertas <- alertas %>%
  mutate(duplicado = if_else(duplicated(student_id_final,codigo_compuesto) & assent == 1,1,0))

## Exceso de no responde

# Lista completa de variables que deben alertar si tienen el valor 99 "No sé"
vars_99 <- c("work",
    "violence_1","violence_1_12","violence_1_month",
    "violence_2","violence_2_12","violence_2_month",
    "violence_3","violence_3_12","violence_3_month",
    "violence_4","violence_4_12","violence_4_month",
    "violence_5","violence_5_12","violence_5_month",
    "violence_6","violence_6_12","violence_6_month",
    "violence_7","violence_7_12","violence_7_month",
    "violence_8","violence_8_12","violence_8_month",
    "violence_9","violence_9_12","violence_9_month",
    "violence_10","violence_10_12","violence_10_month",
    "violence_11","violence_11_12","violence_11_month",
    "violence_12","violence_12_12","violence_12_month",
    "violence_13","violence_13_12","violence_13_month",
    "violence_14","violence_14_12","violence_14_month",
    "violence_15","violence_15_12","violence_15_month",
    "violence_16","violence_16_12","violence_16_month",
    "violence_17","violence_17_12","violence_17_month",
    "violence_18","violence_18_12","violence_18_month",
    "violence_19","violence_19_12","violence_19_month",
    "violence_20","violence_20_12","violence_20_month",
    "violence_21","violence_21_12","violence_21_month",
    "violence_22","violence_22_12","violence_22_month",
    "violence_reasons_1","violence_reasons_2","violence_reasons_3",
    "violence_reasons_4","violence_reasons_5","violence_reasons_6",
    "violence_reasons_7","violence_reasons_8","violence_reasons_9",
    "violence_reasons_10","violence_reasons_11","violence_reasons_12",
    "violence_where_1","violence_where_2","violence_where_3","violence_where_4",
    "violence_where_5","violence_where_6","violence_where_7","violence_where_8",
    "violence_help_1","violence_help_2","violence_help_3",
    "help_how_1","help_how_2","help_how_3","help_how_4",
    "help_how_5","help_how_6","help_how_7","help_how_66",
    "teacher_behaviour_1","teacher_behaviour_1_12","teacher_behaviour_1_month",
    "teacher_behaviour_2","teacher_behaviour_2_12","teacher_behaviour_2_month",
    "teacher_behaviour_3","teacher_behaviour_3_12","teacher_behaviour_3_month",
    "teacher_behaviour_4","teacher_behaviour_4_12","teacher_behaviour_4_month",
    "teacher_behaviour_5","teacher_behaviour_5_12","teacher_behaviour_5_month",
    "teacher_behaviour_6","teacher_behaviour_6_12","teacher_behaviour_6_month",
    "teacher_behaviour_7","teacher_behaviour_7_12","teacher_behaviour_7_month",
    "teacher_behaviour_8","teacher_behaviour_8_12","teacher_behaviour_8_month",
    "teacher_behaviour_9","teacher_behaviour_9_12","teacher_behaviour_9_month",
    "teacher_behaviour_10","teacher_behaviour_10_12","teacher_behaviour_10_month",
    "teacher_behaviour_11","teacher_behaviour_11_12","teacher_behaviour_11_month",
    "teacher_behaviour_12","teacher_behaviour_12_12","teacher_behaviour_12_month",
    "teacher_behaviour_13","teacher_behaviour_13_12","teacher_behaviour_13_month",
    "teacher_behaviour_14","teacher_behaviour_14_12","teacher_behaviour_14_month",
    "teacher_reason_1","teacher_reason_2","teacher_reason_3",
    "teacher_reason_4","teacher_reason_5","teacher_reason_6","teacher_reason_7",
    "teacher_help_1","teacher_help_2","teacher_help_3",
    "help2_how_1","help2_how_2","help2_how_3","help2_how_4","help2_how_5","help2_how_6","help2_how_66",
    paste0("school_", 45:56),
    paste0("violence_", 1:22, "_freq")
  )
  


# Crear las variables nr con valor 1 si la variable es igual a 99
alertas <- alertas %>%
  mutate(across(
    all_of(vars_99),
    ~ if_else(as.numeric(.x) == 99, 1, 0),
    .names = "nr_{.col}"
  ))


## Sumar total no responde   

variables_nr <- names(alertas %>%
                        select(matches("^nr_")))

alertas <- alertas %>%
  mutate(
    total_nr = rowSums(alertas[,variables_nr], na.rm = T))

media_nr <- mean(alertas$total_nr, na.rm = TRUE)
sd_nr <- sd(alertas$total_nr, na.rm = TRUE)

alertas <- alertas %>%
  mutate(
    flag_nr = if_else(total_nr > media_nr + 3 * sd_nr, 1, 0)
  )


## Exceso de no sabe

vars_88 <- c(vars_99,"student_language","mother_education" ,"father_education",
             paste0("teacher_",c(1:7)),"siseve_3", "student_department",
             "student_province","student_district")

# Crear las variables nr con valor 1 si la variable es igual a 88
alertas <- alertas %>%
  mutate(across(
    all_of(vars_88),
    ~ if_else(as.numeric(.x) == 88, 1, 0),
    .names = "ns_{.col}"
  ))


## Sumar total no sabe   

variables_ns <- names(alertas %>%
                        select(matches("^ns_")))

alertas <- alertas %>%
  mutate(
    total_ns = rowSums(alertas[,variables_ns], na.rm = T))

media_ns <- mean(alertas$total_ns, na.rm = TRUE)
sd_ns <- sd(alertas$total_ns, na.rm = TRUE)

alertas <- alertas %>%
  mutate(
    flag_ns = if_else(total_ns > media_ns + 3 * sd_ns, 1, 0)
  )



# Corregir alertas para encuestas rechazadas

alertas <- alertas %>%
  mutate(across(
    .cols = matches("^flag_|^s_|^m_|^ex_|^ns_|^total_"),
    .fns = ~ if_else(as.numeric(assent) == 2, 0, .x),
    .names = "{.col}"
  ))



# Encuestas rechazadas y duplicados, valores faltante y atípicos


alertas <- alertas %>%
  mutate(
    flag_rejected = if_else(
      as.numeric(assent) == 2 & (is.na(rechazo) | rechazo %in% c("3","4")),1,0),
    flag_ausente = if_else(rechazo == "1",1,0,missing = 0),
    flag_retirado = if_else(rechazo == "2",1,0,missing = 0),
    flag_limitacion = if_else(rechazo == "5",1,0,missing = 0),
    flag_logistica = if_else(rechazo == "6",1,0,missing = 0),
    flag_saltos = if_else(total_skips > 0, 1, 0),
    flag_duplicated = if_else(duplicado == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else(ex_house_number == 1 |
                                    ex_house_rooms == 1,1,0))

### Crear alertas LOOKER

alertas <- alertas %>%
  mutate(rechazo_str = case_when(
    rechazo == "1" ~ "Ausente",
    rechazo == "2" ~ "Retirado",
    rechazo == "3" ~ "Estudiante no desea participar",
    rechazo == "4" ~ "Padres no dieron consentimiento",
    rechazo == "5" ~ "Limitación para participar",
    rechazo == "6" ~ "Pendiente por logística"
  ))


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duration_mas == 0 & flag_duration_menos == 0 & flag_duplicated == 0 &  
                            flag_missing == 0 &  flag_saltos == 0 & flag_extreme_values == 0 & flag_ns == 0 &
                            flag_rejected == 0,1,0),
         Alertas = if_else(flag_duration_mas == 1 | flag_duration_menos == 1 | flag_duplicated == 1 |   
                             flag_missing == 1 | flag_saltos == 1 | flag_extreme_values == 1 | flag_ns == 1,1,0),
         Rechazos = if_else(flag_rejected == 1,1,0),
         tiempos_anomalos_mas = if_else(flag_duration_mas == 1,"Sí","No"),
         tiempos_anomalos_menos = if_else(flag_duration_menos == 1,"Sí","No"),
         duplicado = if_else(flag_duplicated == 1, "Sí","No"),
         valores_faltantes = if_else(flag_missing == 1,"Sí","No"),
         saltos_irregulares = if_else(flag_saltos == 1,"Sí","No"),
         valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No"),
         exceso_ns = if_else(flag_ns == 1, "Sí","No")) 

table(alertas$Exitos)

## Agregar tratamiento/control

colegios_tratamiento <- c("581736", "781336",
                          "832337")

colegios_control <- c(
  "336535","775346"
)

alertas <- alertas %>%
  mutate(
    tratamiento = case_when(
      school_final %in% colegios_tratamiento  ~ "Tratamiento",
      school_final %in% colegios_control  ~ "Control",
      TRUE ~ NA_character_))

### Labels para demográficas ___________________________________________________

# Género

genero_labels <- c("Masculino" = 1, "Femenino" = 2)
alertas$gender_str <- factor(alertas$gender_final, levels = c(1, 2), labels = names(genero_labels))
attr(alertas$gender_str, "label") <- "Género"


## Seguimiento colegios

# Separar los que tienen y no tienen ID
con_id <- alertas %>%
  filter(!is.na(student_id)) %>%
  arrange(endtime)%>%
  group_by(student_id)%>%
  mutate(intento = row_number())%>%
  filter(intento == max(intento))


sin_id <- alertas %>%
  filter(is.na(student_id))

# Unirlos nuevamente
alertas_sin_duplicados <- bind_rows(con_id, sin_id)

# Luego aplicar el resumen por colegio
seguimiento_colegios <- alertas_sin_duplicados %>%
  mutate(
    en_lista = student_id_yesno == "1" & assent == "1",
    sin_lista = student_id_yesno == "2" & assent == "1",
    colegio_final = coalesce(colegio_pull)
  ) %>%
  group_by(colegio_final,colegio_pull_id) %>%
  summarise(
    total_encuestas = n(),
    Rechazos = sum(rechazo == "3", na.rm = TRUE),
    Consentimiento = sum(rechazo == "4", na.rm = TRUE),
    Ausentes = sum(flag_ausente,na.rm=TRUE),
    Retirado = sum(flag_retirado, na.rm=TRUE),
    Limitacion = sum(flag_limitacion, na.rm = TRUE),
    Logistica = sum(flag_logistica, na.rm = TRUE),
    en_lista = sum(en_lista, na.rm = TRUE),
    sin_lista = sum(sin_lista, na.rm = TRUE),
    alertas = sum(Alertas, na.rm = TRUE),
    exitos = sum(Exitos, na.rm = TRUE)
  )%>%
  filter(!is.na(colegio_final))



# Agregar meta de lb

meta_colegios$COD_MODULAR <- as.character(meta_colegios$COD_MODULAR)


seguimiento_colegios_2 <- meta_colegios %>%
  full_join(seguimiento_colegios, by = c("COD_MODULAR" = "colegio_pull_id"))%>%
  arrange(desc(total_encuestas))%>%
  mutate(avance_total = round(((exitos + alertas)/ESTUDIANTES)*100,2))




# Confirmación de finalización
message("Alertas creadas exitosamente.")

