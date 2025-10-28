#### Correcciones NR

# Variables
violence_vars <- paste0("violence_", 1:22)
cols_na <- c(
  "violent_course", "violence_strenght", "violence_reasons", "violence_reasons_note",
  paste0("violence_reasons_", 1:12),
  "violence_where_note", paste0("violence_where_", 1:8),
  "violence_help_note", paste0("violence_help_", 1:3)
)

# Crear flag y, si es 0, poner en NA las columnas indicadas
alertas <- alertas %>%
  mutate(
    flag_violence = as.integer(
      rowSums(across(all_of(violence_vars), ~ .x == 1), na.rm = TRUE) > 0
    )
  ) %>%
  mutate(
    across(
      any_of(cols_na),
      ~ replace(.x, flag_violence == 0L, NA)
    ))



help_vars <- c("violence_help_1", "violence_help_2", "violence_help_3")

alertas <- alertas %>%
  mutate(
    flag_violence_help = as.integer(
      rowSums(across(all_of(help_vars), ~ .x == 1), na.rm = TRUE) > 0
    )
  )


cols_help_na <- c(
  "help_how_note",
  "help_how_1","help_how_2","help_how_3","help_how_4","help_how_5","help_how_6","help_how_7",
  "help_how_66",
  "help_how",
  "help_how_66_o"
)

alertas <- alertas %>%
  mutate(
    across(
      any_of(cols_help_na),
      ~ replace(.x, flag_violence_help == 0L, NA)
    )
  )


teacher_vars <- paste0("teacher_behaviour_", 1:14)
cols_teacher_na <- c(
  "teacher_reasons_note",
  "teacher_reason_1","teacher_reason_2","teacher_reason_3","teacher_reason_4",
  "teacher_reason_5","teacher_reason_6","teacher_reason_7",
  "teacher_reasons_note_2",
  "teacher_help_1","teacher_help_2","teacher_help_3"
)

alertas <- alertas %>%
  mutate(
    # Flag: alguna conducta de docente marcada (=1)
    flag_teacher_behaviour = as.integer(
      rowSums(across(all_of(teacher_vars), ~ .x == 1), na.rm = TRUE) > 0
    )
  ) %>%
  mutate(
    # Si la flag es 0, limpiar (poner en NA) los campos relacionados
    across(
      any_of(cols_teacher_na),
      ~ replace(.x, flag_teacher_behaviour == 0L & !is.na(flag_teacher_behaviour), NA)
    )
  )


alertas <- alertas %>%
  mutate(
    flag_teacher_help = as.integer(
      rowSums(
        across(all_of(c("teacher_help_1", "teacher_help_2", "teacher_help_3")), ~ .x == 1),
        na.rm = TRUE
      ) > 0
    )
  )


cols_help2_na <- c(
  "help2_how_note",
  "help2_how_1","help2_how_2","help2_how_3","help2_how_4","help2_how_5","help2_how_6",
  "help2_how_66",
  "help2_how",
  "help2_how_66_o"
)

alertas <- alertas %>%
  mutate(
    across(
      any_of(cols_help2_na),
      ~ replace(.x, flag_teacher_help == 0L & !is.na(flag_teacher_help), NA)
    )
  )


# Calcular flags de nuevo

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














# Reporte NS - NR

alertas_nr <- alertas %>%
  select(starts_with("nr"))%>%
  pivot_longer(cols = everything(),
               values_to = "nr",
               names_to = "var")%>%
  group_by(var)%>%
  summarise(total_nr = sum(nr,na.rm = T))%>%
  filter(total_nr != 0)%>%
  arrange(desc(total_nr))


alertas_ns <- alertas %>%
  select(starts_with("ns"))%>%
  pivot_longer(cols = everything(),
               values_to = "ns",
               names_to = "var")%>%
  group_by(var)%>%
  summarise(total_ns = sum(ns,na.rm = T))%>%
  filter(total_ns != 0)%>%
  arrange(desc(total_ns))


odk_vars <- ODK_filtrado %>% select(name,label)



reporte_ns <- alertas_ns %>%
  mutate(var =  trimws(str_remove(var,"ns_")))%>%
  left_join(odk_vars, by = c("var"="name"))%>%
  mutate(perc_ns = (total_ns / nrow(data))*100 )


reporte_nr <- alertas_nr %>%
  mutate(var =  trimws(str_remove(var,"nr_")))%>%
  left_join(odk_vars, by = c("var"="name"))%>%
  mutate(perc_nr = (total_nr / nrow(data))*100 )


# Exportar

export_sheet(reporte_ns, sheet, "report ns", label = "No sabe",         pause = 5)
export_sheet(reporte_nr, sheet, "reporte nr", label = "No responde",         pause = 5)






# Reporte alertas preocupantes


vars_rojas <- c(
  "violence_8",
  "violence_10",
  "violence_14",
  "violence_15",
  "violence_16",
  "violence_17",
  "violence_18",
  "violence_13",
  "violence_21",
  "teacher_behaviour_3",
  "teacher_behaviour_5",
  "teacher_behaviour_6",
  "teacher_behaviour_8",
  "teacher_behaviour_9",
  "teacher_behaviour_10",
  "teacher_behaviour_11",
  "teacher_behaviour_12",
  "teacher_behaviour_13",
  "teacher_behaviour_14"
)



alerta_roja <- alertas %>%
  mutate(across(
    all_of(vars_rojas),
    ~ if_else(as.numeric(.x) == 1, 1, 0),
    .names = "alerta_roja_{.col}"
  ))


# Reporte alerta roja

rep_alerta_roja <- alerta_roja %>%
  select(starts_with("alerta_roja_"))%>%
  pivot_longer(cols = everything(),
               values_to = "ar",
               names_to = "var")%>%
  group_by(var)%>%
  summarise(total_ar = sum(ar,na.rm = T))%>%
  filter(total_ar != 0)%>%
  arrange(desc(total_ar))


reporte_ar <- rep_alerta_roja %>%
  mutate(var =  trimws(str_remove(var,"alerta_roja_")))%>%
  left_join(odk_vars, by = c("var"="name"))%>%
  mutate(perc_nr = (total_ar / nrow(data))*100 )


# Reporte alerta amarilla

vars_amarilla <- c(
  "violence_1",
  "violence_2",
  "violence_3",
  "violence_4",
  "violence_5",
  "violence_6",
  "violence_7",
  "violence_9",
  "violence_11",
  "violence_12",
  "violence_19",
  "violence_20",
  "violence_22",
  "teacher_behaviour_1",
  "teacher_behaviour_2",
  "teacher_behaviour_4",
  "teacher_behaviour_7"
)



alerta_amarilla <- alertas %>%
  mutate(across(
    all_of(vars_amarilla),
    ~ if_else(as.numeric(.x) == 1, 1, 0),
    .names = "alerta_amarilla_{.col}"
  ))


# Reporte alerta amarilla

rep_alerta_amarilla <- alerta_amarilla %>%
  select(starts_with("alerta_amarilla_"))%>%
  pivot_longer(cols = everything(),
               values_to = "am",
               names_to = "var")%>%
  group_by(var)%>%
  summarise(total_am = sum(am,na.rm = T))%>%
  filter(total_am != 0)%>%
  arrange(desc(total_am))


reporte_am <- rep_alerta_amarilla %>%
  mutate(var =  trimws(str_remove(var,"alerta_amarilla_")))%>%
  left_join(odk_vars, by = c("var"="name"))%>%
  mutate(perc_nr = (total_am / nrow(data))*100 )



# Exportar

export_sheet(reporte_am, sheet, "alertas amarillas", label = "alertas amarillas",         pause = 5)
export_sheet(reporte_ar, sheet, "alertas rojas", label = "alertas rojas",         pause = 5)




