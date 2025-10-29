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
  
  # --- Config columnas de nominación ---
  max_cols <- if (is.null(max_nominaciones)) 15L else as.integer(max_nominaciones)
  stopifnot(max_cols > 0L)
  cols_prefijo <- sprintf("%s_%d", prefix, 1:max_cols)
  
  # Asegura que existan las columnas del prefijo
  missing_cols <- setdiff(cols_prefijo, names(data))
  if (length(missing_cols)) data[missing_cols] <- NA_character_
  
  # Parser robusto: "" o NA -> 0; recorta a [0, max_n]
  parse_int_bound <- function(x, max_n = 15L) {
    x_chr <- as.character(x)
    if (is.na(x_chr) || trimws(x_chr) == "") return(0L)
    x_num <- suppressWarnings(as.numeric(x_chr))  # soporta "10" o "10.0"
    if (is.na(x_num)) return(0L)
    x_int <- as.integer(floor(x_num))
    max(0L, min(max_n, x_int))
  }
  
  # --- Preasignación ---
  n <- nrow(data)
  v_unique   <- rep(NA_integer_, n)
  v_66       <- rep(NA_integer_, n)
  v_99       <- rep(NA_integer_, n)
  v_eff      <- rep(NA_integer_, n)  # únicos + 66
  flag_dup   <- rep(NA_integer_, n)  # 1/0
  flag_salto <- rep(NA_integer_, n)  # 1/0
  v_diff     <- rep(NA_integer_, n)
  v_vec      <- if (add_vec) vector("list", n) else NULL
  if (add_vec) v_vec[] <- list(NA)  # también NA si INT falta
  
  # --- Loop fila a fila ---
  for (i in seq_len(n)) {
    # ¿Usamos var_int o max_nominaciones?
    if (!is.null(var_int)) {
      raw_int <- data[[var_int]][i]
      int_missing <- is.na(raw_int) || trimws(as.character(raw_int)) == ""
      if (int_missing) {
        # todo ya está en NA por preasignación; continuar
        next
      }
      n_int <- parse_int_bound(raw_int, max_n = max_cols)
    } else {
      # Sin var_int: usamos el máximo fijo
      n_int <- max_cols
    }
    
    # Extraer respuestas (character) y recortar al n declarado
    vals <- unlist(data[i, cols_prefijo], use.names = FALSE)
    vals <- as.character(vals)
    vals <- ifelse(is.na(vals), NA_character_, trimws(vals))
    v <- if (n_int > 0L) vals[seq_len(n_int)] else character(0)
    if (add_vec) v_vec[[i]] <- v
    
    # Conteos
    v_no_na <- v[!is.na(v) & nzchar(v)]
    cnt_66  <- sum(v_no_na == "66")
    cnt_99  <- sum(v_no_na == "99")
    v_valid <- v_no_na[!(v_no_na %in% c("66","99"))]
    cnt_uniq <- length(unique(v_valid))
    eff      <- cnt_uniq + cnt_66
    
    # Flags (1/0)
    non_na_count <- length(v_no_na)
    dup   <- as.integer(non_na_count > (cnt_uniq + cnt_66 + cnt_99)) # duplicados
    salto <- as.integer(eff < n_int)                                  # saltos
    
    # Diferencia final
    diff_val <- n_int - (cnt_uniq + cnt_66 + cnt_99)
    
    # Asignar
    v_unique[i]   <- cnt_uniq
    v_66[i]       <- cnt_66
    v_99[i]       <- cnt_99
    v_eff[i]      <- eff
    flag_dup[i]   <- dup
    flag_salto[i] <- salto
    v_diff[i]     <- diff_val
  }
  
  # --- Agregar columnas al DF ---
  data[[paste0(prefix, "_unique")]]           <- v_unique
  data[[paste0(prefix, "_66")]]               <- v_66
  data[[paste0(prefix, "_99")]]               <- v_99
  data[[paste0(prefix, "_effective")]]        <- v_eff
  data[[paste0(prefix, "_flag_duplicates")]]  <- flag_dup
  data[[paste0(prefix, "_flag_gaps")]]        <- flag_salto
  data[[paste0(prefix, "_diff")]]             <- v_diff
  if (add_vec) data[[paste0(prefix, "_vec")]] <- v_vec
  
  return(data)
}


# Auditar nominaciones ---------------------------------------------------------

alertas_nomi <- data %>% filter(assent == 1)


alertas_nomi<- agregar_nominaciones(
  data   = alertas_nomi,
  prefix = "social_friends",
  var_int = "social_friends_int",
  add_vec = TRUE 
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_recess",
  var_int = "social_recess_int",
  add_vec = TRUE

)

# Primer orden -----------------------------------------------------------------

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_house_1",
  var_int = "social_house_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_study_1",
  var_int = "social_study_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_game_1",
  var_int = "social_game_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic_1",
  var_int = "social_academic_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic2_1",
  var_int = "social_academic2_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_personal_1",
  var_int = "social_personal_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_personal2_1",
  var_int = "social_personal2_1_int",
  add_vec = TRUE
  
)


alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_friend_wish_1",
  var_int = "social_friend_wish_1_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work_1",
  max_nominaciones = 3,
  add_vec = TRUE
  
)


alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work2_1",
  max_nominaciones = 3,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work3_1",
  max_nominaciones = 3,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_leadership_1",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic_skills_1",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_popularity_1",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_shyness_1",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

# Segundo orden ----------------------------------------------------------------

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_friend_wish_2",
  var_int = "social_friend_wish_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work_2",
  max_nominaciones = 3,
  add_vec = TRUE
  
)


alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work2_2",
  max_nominaciones = 3,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_work3_2",
  max_nominaciones = 3,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_leadership_2",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic_skills_2",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_popularity_2",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_shyness_2",
  max_nominaciones = 5,
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_house_2",
  var_int = "social_house_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_study_2",
  var_int = "social_study_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_game_2",
  var_int = "social_game_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic_2",
  var_int = "social_academic_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_academic2_2",
  var_int = "social_academic2_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_personal_2",
  var_int = "social_personal_2_int",
  add_vec = TRUE
  
)

alertas_nomi <- agregar_nominaciones(
  data = alertas_nomi,
  prefix = "social_personal2_2",
  var_int = "social_personal2_2_int",
  add_vec = TRUE
  
)


# Consolidar alertas


# Crea columna si falta (con el tipo correcto)
ensure_col <- function(df, col, type = c("int","dbl","chr","lgl","list")) {
  type <- match.arg(type)
  if (!col %in% names(df)) {
    df[[col]] <- switch(type,
                        int  = rep(NA_integer_, nrow(df)),
                        dbl  = rep(NA_real_,    nrow(df)),
                        chr  = rep(NA_character_, nrow(df)),
                        lgl  = rep(NA,          nrow(df)),
                        list = replicate(nrow(df), NA, simplify = FALSE)
    )
  }
  df
}

# Unifica y elimina fuentes para un prefijo base entre _1_ y _2_
unificar_y_purgar_prefijo <- function(df, base_prefijo, chooser = "network_random", umbral = 0.5) {
  metrics <- c("unique","66","99","effective","flag_duplicates","flag_gaps","diff")
  
  # 1) Unificar métricas numéricas
  for (m in metrics) {
    c1  <- paste0(base_prefijo, "_1_", m)
    c2  <- paste0(base_prefijo, "_2_", m)
    out <- paste0(base_prefijo, "_",   m)
    
    df <- ensure_col(df, c1, "int")
    df <- ensure_col(df, c2, "int")
    
    # resultado unificado (si chooser es NA → NA)
    df[[out]] <- ifelse(
      is.na(df[[chooser]]), NA_integer_,
      ifelse(df[[chooser]] <= umbral, df[[c1]], df[[c2]])
    )
    
    # eliminar columnas fuente si existen
    df <- df[ , setdiff(names(df), c(c1, c2)), drop = FALSE]
  }
  
  # 2) Unificar list-column *_vec (si existen) y borrar fuentes
  c1_vec  <- paste0(base_prefijo, "_1_vec")
  c2_vec  <- paste0(base_prefijo, "_2_vec")
  out_vec <- paste0(base_prefijo, "_vec")
  
  if ((c1_vec %in% names(df)) || (c2_vec %in% names(df))) {
    df <- ensure_col(df, c1_vec,  "list")
    df <- ensure_col(df, c2_vec,  "list")
    df <- ensure_col(df, out_vec, "list")
    
    nr <- nrow(df)
    for (i in seq_len(nr)) {
      rnd <- df[[chooser]][i]
      if (is.na(rnd)) {
        df[[out_vec]][[i]] <- NA
      } else if (rnd <= umbral) {
        df[[out_vec]][[i]] <- df[[c1_vec]][[i]]
      } else {
        df[[out_vec]][[i]] <- df[[c2_vec]][[i]]
      }
    }
    
    # borrar fuentes
    df <- df[ , setdiff(names(df), c(c1_vec, c2_vec)), drop = FALSE]
  }
  
  df
}

# --------- APLICAR A TODOS LOS BLOQUES ---------
bases <- c(
  "social_house",
  "social_study",
  "social_game",
  "social_academic",
  "social_academic2",
  "social_personal",
  "social_personal2",
  "social_friend_wish",
  "social_work",
  "social_work2",
  "social_work3",
  "social_leadership",
  "social_academic_skills",
  "social_popularity",
  "social_shyness"
)

# Ejecuta: crea una sola variable por métrica y purga *_1_* y *_2_* de cada base
for (b in bases) {
  alertas_nomi <- unificar_y_purgar_prefijo(alertas_nomi, base_prefijo = b, chooser = "network_random", umbral = 0.5)
}

