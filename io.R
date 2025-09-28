# ---------------------------------------------------
# R5.1 – Leer CSV con tipado automático
# ---------------------------------------------------
#' Leer CSV con tipado automático
#' @param file ruta al archivo CSV
#' @param max_levels número máximo de niveles para categorizar
#' @param sep separador del CSV
#' @param dec carácter decimal
#' @param ... otros parámetros para read.csv
#' @return data.frame con tipos de datos ajustados
#' @examples df <- read_typed_csv("datos.csv", max_levels = 5)
#' @export
read_typed_csv <- function(file, max_levels = 5, sep = ",", dec = ".", ...) {
  raw <- read.csv(file, sep = sep, dec = dec, stringsAsFactors = FALSE, ...)
  coerce_col <- function(v) {
    suppressWarnings(num <- as.numeric(v))
    if (all(is.na(v) == is.na(num))) {
      if (all(is.na(num) | (abs(num - round(num)) < .Machine$double.eps^0.5))) {
        return(as.integer(round(num)))
      } else {
        return(num)
      }
    } else {
      if (length(unique(v)) <= max_levels) return(as.factor(v))
      return(v) 
    }
  }
  as.data.frame(lapply(raw, coerce_col), stringsAsFactors = FALSE)
}

# ---------------------------------------------------
# R5.2 – Logging simple
# ---------------------------------------------------
#' Abrir un archivo de log
#' @param path ruta al archivo de log
#' @return conexión al archivo
#' @export
log_open <- function(path) file(path, open = "a", encoding = "UTF-8")

#' Escribir en el archivo de log
#' @param con conexión al archivo de log
#' @param msg mensaje a escribir
#' @export
log_write <- function(con, msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  writeLines(sprintf("[%s] %s", ts, msg), con)
  flush(con)
}

#' Cerrar el archivo de log
#' @param con conexión al archivo de log
#' @export
log_close <- function(con) close(con)

# ---------------------------------------------------
# R5.2 (gráfico) – Entropía normalizada por variable categórica
# ---------------------------------------------------
#' Dibujar entropía normalizada por variable categórica
#' @param df_cat data.frame con variables categóricas
#' @return vector de entropías normalizadas
#' @examples plot_entropy_normalized(df3_EW)
#' @export
plot_entropy_normalized <- function(df_cat) {
  stopifnot(all(sapply(df_cat, is.factor)))
  H <- sapply(df_cat, entropy)
  K <- sapply(df_cat, function(f) length(levels(f)))
  Hn <- H / log2(K)  # entropía normalizada in [0,1]
  barplot(Hn, ylim = c(0, 1), col = "steelblue",
          main = "Entropía normalizada por variable", ylab = "Entropía (0–1)")
  abline(h = seq(0, 1, 0.2), col = "gray90")
  invisible(Hn)
}

# ---------------------------------------------------
# R5.3 – Matriz de correlación (continuas)
# ---------------------------------------------------
#' Dibujar matriz de correlación
#' @param df_num data.frame con variables numéricas
#' @return matriz de correlación
#' @examples plot_cor_matrix(df3)
#' @export
plot_cor_matrix <- function(df_num) {
  C <- cor(df_num)
  op <- par(mar = c(5, 5, 2, 2))
  image(1:ncol(C), 1:ncol(C), t(C[nrow(C):1, ]), axes = FALSE,
        xlab = "", ylab = "", main = "Matriz de correlación")
  axis(1, at = 1:ncol(C), labels = colnames(C), las = 2)
  axis(2, at = 1:ncol(C), labels = rev(colnames(C)), las = 2)
  box()
  par(op)
  invisible(C)
}

# ---------------------------------------------------
# RECURSIVAS – Qué son y ejemplos
# ---------------------------------------------------
# Función recursiva para calcular el factorial
#' Calcular factorial recursivamente
#' @param n número entero no negativo
#' @return factorial de n
#' @examples fact(5)  # 120
#' @export
fact <- function(n) if (n <= 1) 1 else n * fact(n - 1)

# Fibonacci con memoización simple
#' Calcular Fibonacci con memoización
#' @param n número entero no negativo
#' @return valor de Fibonacci en n
#' @export
fib <- local({
  memo <- c(0, 1)  # fib(0)=0, fib(1)=1
  f <- function(n) {
    if (!is.na(memo[n + 1])) return(memo[n + 1])
    val <- f(n - 1) + f(n - 2)
    memo[n + 1] <<- val
    val
  }
  function(n) {
    if (n < 0) stop("n>=0")
    if (length(memo) <= n + 1) memo <<- c(memo, rep(NA_real_, n + 1 - length(memo)))
    f(n)
  }
})