### Exportar alertas

# Verificar si las credenciales están disponibles
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

sheet <- tryCatch({
  gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# Llamadas usando el wrapper
export_sheet(alertas,             sheet, "alertas_estudiantes",  label = "alertas",                 pause = 5)
#export_sheet(data        ,        sheet, "data_raw",             label = "datos crudos",            pause = 5)
export_sheet(seguimiento_colegios_2, sheet, "seguimiento_colegios", label = "colegios",         pause = 5)
export_sheet(alertas_nomi, sheet, "nominaciones", label = "nominaciones",         pause = 5)


sheet2 <- tryCatch({
  gs4_get("1EmeaKe6QrRRTHQhJhXsu0ii84KsGRg0heCV7ksLe2Ko")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

export_sheet(seguimiento_colegios_detalle_final,             sheet2, "seguimiento_estudiantes",  label = "colegios",                 pause = 5)
export_sheet(lista_estudiantes_pendiente,             sheet2, "estudiantes_faltantes",  label = "estudiantes",                 pause = 5)
export_sheet(estudiantes,             sheet2, "estudiantes_realizados",  label = "estudiantes",                 pause = 5)




message("✅ Todos los datos fueron exportados exitosamente.")