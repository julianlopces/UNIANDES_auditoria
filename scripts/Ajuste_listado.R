# Ajustar listado campo

base <- read_xlsx("G:/Unidades compartidas/EQUILIBRIUM   Intranet 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/127. UNIANDES - Evaluación Soporte psicológico y convivencia escolar/2. Implementación/Cuantitativo/Instrumentos/Bases_de_datos/BASES_UNIANDES.xlsx",
          sheet = "BASE ESTUDIANTES")


colegios_piloto <- unique(alertas$colegio_pull_id)[-2]


# Filtrar

base_sin_piloto <- base[!base$CODIGO_MODULAR %in% colegios_piloto,]

base_sin_piloto_2 <- base_sin_piloto%>%
  select(-c("TIPO_DOC","DOCUMENTO"))%>%
  mutate(CONSECUTIVO = row_number())%>%
  arrange(NOMBRE_COMPLETO)


# Exportar


readr::write_csv(base_sin_piloto_2,"G:/Unidades compartidas/EQUILIBRIUM   Intranet 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/127. UNIANDES - Evaluación Soporte psicológico y convivencia escolar/2. Implementación/Cuantitativo/Instrumentos/Bases_de_datos/adjunto_estudiantes.csv")


colegios <- unique(base_sin_piloto_2$CODIGO_MODULAR)
