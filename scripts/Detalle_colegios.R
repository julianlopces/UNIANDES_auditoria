# Seguimiento por colegio detallado


seguimiento_colegios <- alertas_sin_duplicados %>%
  mutate(
    en_lista  = student_id_yesno == "1" & assent == "1",
    sin_lista = student_id_yesno == "2" & assent == "1",
    school_final  = coalesce(colegio_pull_id, student_school, student_school_reject))%>%
  filter(!is.na(school_final)) %>%
  group_by(school_final) %>%
  summarise(
    total_encuestas = n(),
    Rechazos      = sum(flag_rejected  == 1, na.rm = TRUE),
    Ausentes      = sum(flag_ausente   == 1, na.rm = TRUE),
    Retirado      = sum(flag_retirado  == 1, na.rm = TRUE),
    Limitacion    = sum(flag_limitacion== 1, na.rm = TRUE),
    Logistica = sum(flag_logistica == 1, na.rm = TRUE),
    en_lista      = sum(en_lista, na.rm = TRUE),
    sin_lista     = sum(sin_lista, na.rm = TRUE),
    alertas       = sum(Alertas == 1, na.rm = TRUE),
    exitos        = sum(Exitos  == 1, na.rm = TRUE)
  )



# Agregar meta de lb

seguimiento_colegios_detalle <- meta_colegios %>%
  full_join(seguimiento_colegios, by = c("COD_MODULAR" = "school_final"))%>%
  mutate(COD_MODULAR = as.character(COD_MODULAR))


# Estado desconocido 


lista_estudiantes_pendiente  <- read_sheet("1EmeaKe6QrRRTHQhJhXsu0ii84KsGRg0heCV7ksLe2Ko",
                                 sheet = "BASE ESTUDIANTES")%>%
  mutate(ID = as.character(ID))%>%
  filter(!ID %in% alertas_sin_duplicados$student_id)



pendiente_estudiantes_colegios <- lista_estudiantes_pendiente %>%
  group_by(CODIGO_MODULAR)%>%
  summarise(pendientes = n())%>%
  arrange(desc(pendientes))%>%
  mutate(CODIGO_MODULAR = as.character(CODIGO_MODULAR))




# Añadir pendientes


seguimiento_colegios_detalle_final <- seguimiento_colegios_detalle %>%
  left_join(pendiente_estudiantes_colegios, by = c("COD_MODULAR" = "CODIGO_MODULAR"))


# Filtrar colegios relevantes


seguimiento_colegios_detalle_final <- seguimiento_colegios_detalle_final %>%
  filter((ADICIONALES == 0 | COD_MODULAR %in% c("694570",
                                               "762906",
                                               "437731")) & !COD_MODULAR %in% c("209270"))%>%
  filter(!is.na(total_encuestas))%>%
  arrange(pendientes)%>%
  filter(!is.na(pendientes) | Ausentes >= 3)


lista_estudiantes_pendiente <- lista_estudiantes_pendiente %>%
  filter(CODIGO_MODULAR %in% seguimiento_colegios_detalle_final$COD_MODULAR)%>%
  select(ID,NOMBRE_COMPLETO,CODIGO_MODULAR,NOMBRE_COLEGIO,TRATAMIENTO,ID_GRADO,ID_SECCION,NOMBRE_SECCION,JORNADA)



estudiantes <- alertas_sin_duplicados %>%
  select(SubmissionDate,student_id,student_id_yesno,name_final,school_final,assent,rechazo_str)%>%
  left_join(meta_colegios %>% select(COD_MODULAR,COLEGIO), by =c("school_final"="COD_MODULAR"))





# Reporte condensado#

#colegios_sin_docentes  <- read_sheet("1rwGebhR5HBNukMgRCHMQ6w2j9silfhNLPVAN0xUNQy8",sheet = "colegios_sin_docente")[,c(1:4)]

#colegios_sin_rectores <- read_sheet("1rwGebhR5HBNukMgRCHMQ6w2j9silfhNLPVAN0xUNQy8",sheet = "colegios_sin_rector")[,c(1:3)]

#tratamiento <- read_sheet(id_alertas,sheet = "seguimiento_colegios")[c(1,15)]


#Resumen_colegios <- bind_rows(
#  colegios_priorizados  |> select(1, 2) |> setNames(c("cod_colegio", "colegio")),
#  colegios_sin_docentes |> select(1, 3) |> setNames(c("cod_colegio", "colegio")),
#  colegios_sin_rectores |> select(1, 2) |> setNames(c("cod_colegio", "colegio"))
#) %>%
#  distinct(cod_colegio, .keep_all = TRUE)%>%
#  mutate(
#    Estudiantes_ausentes = if_else(cod_colegio %in% colegios_priorizados$COD_COLEGIO,"Sí","NO"),
#    Faltan_docentes = if_else(cod_colegio %in% colegios_sin_docentes$id_colegio,"Sí","NO"),
#    Faltan_rectores = if_else(cod_colegio %in% colegios_sin_rectores$id_colegio,"Sí","NO")
#  )%>%
#  left_join(tratamiento,by = c("cod_colegio" = "COD_COLEGIO"))%>%
#  filter(!as.character(cod_colegio) %in% c("111001016101",
#                                           "111001100072",
#                                           "111001801101"))#


