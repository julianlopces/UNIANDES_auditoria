# ==========================================
# Agrega métricas de nominaciones al DF
# ==========================================
# data:              data.frame de entrada
# prefix:            prefijo de las dummies (ej. "social_friends")
# var_int:           nombre de la variable entera (ej. "social_friends_int"). Opcional si usas max_nominaciones
# max_nominaciones:  entero máximo a usar cuando NO hay var_int (o quieres forzarlo)
# add_vec:           TRUE para añadir list-column con el vector usado por fila
# ==========================================

agregar_nominaciones <- function(data, prefix, var_int = NULL, max_nominaciones = NULL, add_vec = TRUE) {
  stopifnot(is.data.frame(data), is.character(prefix), length(prefix) == 1)
  
  # -------- Config columnas de nominación --------
  max_cols <- if (is.null(max_nominaciones)) 15L else as.integer(max_nominaciones)
  stopifnot(max_cols > 0L)
  
  cols_prefijo <- sprintf("%s_%d", prefix, 1:max_cols)
  
  # Asegura que existan las columnas de nominación (si faltan, crea NA_character_)
  missing_cols <- setdiff(cols_prefijo, names(data))
  if (length(missing_cols)) data[missing_cols] <- NA_character_
  
  # -------- Parser robusto para el entero --------
  parse_int_bound <- function(x, max_n = 15L) {
    x_chr <- as.character(x)
    if (is.na(x_chr) || trimws(x_chr) == "") return(0L)
    x_num <- suppressWarnings(as.numeric(x_chr))  # soporta "10" o "10.0"
    if (is.na(x_num)) return(0L)
    x_int <- as.integer(floor(x_num))
    max(0L, min(max_n, x_int))
  }
  
  # -------- Preasignación --------
  n <- nrow(data)
  v_unique   <- integer(n)
  v_66       <- integer(n)
  v_99       <- integer(n)
  v_eff      <- integer(n)  # únicos + 66
  flag_dup   <- integer(n)  # 1/0
  flag_salto <- integer(n)  # 1/0
  v_diff     <- integer(n)
  v_vec      <- if (add_vec) vector("list", n) else NULL
  
  # -------- Loop fila a fila --------
  for (i in seq_len(n)) {
    # 1) Vector de respuestas (character) recortado al n declarado
    vals <- unlist(data[i, cols_prefijo], use.names = FALSE)
    vals <- as.character(vals)
    vals <- ifelse(is.na(vals), NA_character_, trimws(vals))
    
    n_int <- if (!is.null(var_int) && var_int %in% names(data)) {
      parse_int_bound(data[[var_int]][i], max_n = length(vals))
    } else {
      max_cols
    }
    
    v <- if (n_int > 0L) vals[seq_len(n_int)] else character(0)
    if (add_vec) v_vec[[i]] <- v
    
    # 2) Conteos
    v_no_na <- v[!is.na(v) & nzchar(v)]
    cnt_66  <- sum(v_no_na == "66")
    cnt_99  <- sum(v_no_na == "99")
    v_valid <- v_no_na[!(v_no_na %in% c("66","99"))]
    cnt_uniq <- length(unique(v_valid))
    eff      <- cnt_uniq + cnt_66
    
    # 3) Flags (1/0)
    non_na_count <- length(v_no_na)
    dup   <- as.integer(non_na_count > (cnt_uniq + cnt_66 + cnt_99)) # duplicados
    salto <- as.integer(eff < n_int)                                  # saltos (no llenó todos)
    
    # 4) Diferencia final: int - (únicos + 66 + 99)
    diff_val <- n_int - (cnt_uniq + cnt_66 + cnt_99)
    
    # 5) Asignar
    v_unique[i]   <- cnt_uniq
    v_66[i]       <- cnt_66
    v_99[i]       <- cnt_99
    v_eff[i]      <- eff
    flag_dup[i]   <- dup
    flag_salto[i] <- salto
    v_diff[i]     <- diff_val
  }
  
  # -------- Agregar columnas al DF --------
  data[[paste0(prefix, "_unique")]]           <- v_unique
  data[[paste0(prefix, "_66")]]               <- v_66
  data[[paste0(prefix, "_99")]]               <- v_99
  data[[paste0(prefix, "_effective")]]        <- v_eff
  data[[paste0(prefix, "_flag_duplicates")]]  <- flag_dup
  data[[paste0(prefix, "_flag_gaps")]]        <- flag_salto
  data[[paste0(prefix, "_diff")]]             <- v_diff
  if (add_vec) {
    data[[paste0(prefix, "_vec")]] <- v_vec
  }
  
  return(data)
}


# Auditar nominaciones ---------------------------------------------------------

alertas_nomi <- data %>% filter(assent == 1)











