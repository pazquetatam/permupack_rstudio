#' Filtrar múltiplos de k
#' @param x vector numérico
#' @param k entero (>0)
#' @return subvector de \code{x} con múltiplos de k
#' @examples pick_multiples(0:20, 7)
#' @export
pick_multiples <- function(x, k) {
  x[is_multiple_of(x, k)]
}

#' Normalización min-max a [0,1]
#' @param x numérico
#' @return numérico normalizado
#' @examples normalize01(c(10, 20, 30))
#' @export
normalize01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Estandarización (z-score)
#' @param x numérico
#' @return numérico estandarizado
#' @examples standardize(c(10,20,30))
#' @export
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#' Matriz M[i,j] = exp(i - j)
#' @param n tamaño (n x n)
#' @return matriz numérica
#' @examples exp_matrix(5)[1:2,1:2]
#' @export
exp_matrix <- function(n = 5) {
  i <- j <- seq_len(n)
  exp(outer(i, j, function(ii, jj) ii - jj))
}

#' Poner la diagonal de una matriz a 0
#' @param A matriz cuadrada
#' @return matriz con diag=0
#' @examples set_diag_zero(exp_matrix(3))
#' @export
set_diag_zero <- function(A) {
  stopifnot(is.matrix(A), nrow(A) == ncol(A))
  diag(A) <- 0
  A
}

#' Simetrizar una matriz promediando con su traspuesta
#' @param A matriz cuadrada
#' @return matriz simétrica ( (A + t(A))/2 )
#' @examples symmetrize_mean(exp_matrix(3))
#' @export
symmetrize_mean <- function(A) {
  stopifnot(is.matrix(A), nrow(A) == ncol(A))
  (A + t(A)) / 2
}

# -----------------------------------------------------------
# R8.1 – Operados composición permutaciones para objetos S4
# -----------------------------------------------------------
# Clase Permutacion
setClass("Permutacion",
         slots = list(
           valores = "integer"
         ))

# Constructor
Permutacion <- function(valores) {
  new("Permutacion", valores = as.integer(valores))
}

# Mostrar
setMethod("show", "Permutacion", function(object) {
  cat("Permutación:", object@valores, "\n")
})

# Operador de composición (%o%)
setGeneric("%o%", function(p1, p2) standardGeneric("%o%"))

setMethod("%o%", signature(p1 = "Permutacion", p2 = "Permutacion"),
          function(p1, p2) {
            if (length(p1@valores) != length(p2@valores)) {
              stop("Las permutaciones deben tener la misma longitud")
            }
            comp <- p1@valores[p2@valores]
            Permutacion(comp)
          })

# Ejemplo
p1 <- Permutacion(c(2, 3, 1))
p2 <- Permutacion(c(3, 1, 2))
p1 %o% p2  # composición

# -----------------------------------------------------------
# R8.2 – Objeto para diferentes atributos de BBDD
# -----------------------------------------------------------
setClass("Atributo",
         slots = list(
           nombre = "character",
           tipo   = "character",   # "real" o "categorico"
           valores = "vector"
         ))

# Constructor
Atributo <- function(nombre, tipo, valores) {
  new("Atributo", nombre = nombre, tipo = tipo, valores = valores)
}

# Mostrar
setMethod("show", "Atributo", function(object) {
  cat("Atributo:", object@nombre, "\n",
      "Tipo:", object@tipo, "\n",
      "Valores:", head(object@valores), "...\n")
})

# -----------------------------------------------------------
# R8.3 – Diseño de un tipo de objeto en función de BBDD
# -----------------------------------------------------------
setClass("Dataset",
         slots = list(
           atributos = "list",
           clase     = "character"   # nombre de variable clase
         ))

# Constructor
Dataset <- function(atributos, clase = NULL) {
  new("Dataset", atributos = atributos, clase = clase)
}

# Mostrar
setMethod("show", "Dataset", function(object) {
  cat("Dataset con", length(object@atributos), "atributos.\n")
  if (!is.null(object@clase)) {
    cat("Variable clase:", object@clase, "\n")
  }
})

# Acceso a atributo por nombre
setGeneric("getAtributo", function(dataset, nombre) standardGeneric("getAtributo"))
setMethod("getAtributo", signature(dataset = "Dataset", nombre = "character"),
          function(dataset, nombre) {
            dataset@atributos[[nombre]]
          })

# -----------------------------------------------------------
# R8.4 – Funciones de preprocesado BBDD
# -----------------------------------------------------------
# Normalización min-max de atributo real
setGeneric("normalizar", function(atributo) standardGeneric("normalizar"))

setMethod("normalizar", "Atributo", function(atributo) {
  if (atributo@tipo != "real") stop("Solo aplica a atributos reales")
  x <- atributo@valores
  norm_vals <- (x - min(x)) / (max(x) - min(x))
  atributo@valores <- norm_vals
  atributo
})

# Discretización equal-width (reutilizando tu función anterior)
setGeneric("discretizarEW", function(atributo, bins) standardGeneric("discretizarEW"))

setMethod("discretizarEW", "Atributo", function(atributo, bins) {
  if (atributo@tipo != "real") stop("Solo aplica a atributos reales")
  res <- discretizeEW(atributo@valores, bins)
  atributo@valores <- res$categorias
  atributo@tipo <- "categorico"
  atributo
})

# Entropía de atributo categórico
setGeneric("entropia", function(atributo) standardGeneric("entropia"))

setMethod("entropia", "Atributo", function(atributo) {
  if (atributo@tipo != "categorico") stop("Solo aplica a atributos categóricos")
  p <- prop.table(table(atributo@valores))
  -sum(p * log2(p))
})