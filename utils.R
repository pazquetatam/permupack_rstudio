#' Function to generate an identity permutation
#'
#' @description This function creates an identity permutation
#' @param size numeric value indicating the size of the permutation
#' @return A new object of class \code{\linkS4class{Permutation}} equal to the
#' identity permutation of size \code{size}
#'
getIdentityPermutation <- function(size) {
  return(permutation(1:size))
}

#' Function to generate a random permutation
#'
#' @description This function creates a random permutation
#' @param size numeric value indicating the size of the permutation
#' @return A new object of class \code{\linkS4class{Permutation}} representing
#' a random permutation of size \code{size}
#'
getRandomPermutation <- function (size) {
  permu <- sample(1:size)
  return(permutation(permu))
}

#' Function to generate permutations from scores
#'
#' @description This function creates a permutation or a set of permutations
#' based on one or more sets of scores or ratings
#' @param scores either a vector or a matrix with the scores. Each set of
#' scores has to be a row in the matrix
#' @param type a character argument indicating the type of permutation.
#' Valid values are \code{'ranking'} (default value) and \code{'ordering'}
#' @param decreasing a logical value to indicate which score is better. If
#' \code{FALSE} (default value), higher scores are better
#' @return Either an object or a list of objects of class
#' \code{\linkS4class{Permutation}} corresponding to the permutations derived
#' from the scores
#' @examples
#' scores <- matrix(rnorm(100, 1, 3), ncol=10)
#' permus <- getPermutationFromScores (scores)
#' scores[5, ]
#' permus[[5]]
#'
getPermutationFromScores <- function (scores, type="ranking", decreasing=FALSE) {
  if(is.matrix(scores)) {
    getPermutations(scores, type, decreasing)
  } else {
    switch(type,
           "ranking"={
             if (!decreasing) {
               scores <- -1 * scores
             }
             permu <- rank(scores, ties.method="random")
           },
           "ordering"={
             permu <- order(scores, decreasing=decreasing)
           },
           {
             warning("El parametro type solo puede ser 'ranking' u 'ordering'")
             permu <- NULL
           })
    return(permutation(permu))
  }
}

# Esta funcion no hace falta documentarla igual, ya que no ira en la ayuda
# En cualquier caso, y aunque aqui no se haga por espacio, es importante
# documentar bien el codigo para que se comprensible
getPermutations <- function(scores, type, decreasing){
  lst <- lapply(1:nrow(scores),
                FUN=function(i){
                  permu <- getPermutationFromScores(scores[i, ],
                                                    type=type,
                                                    decreasing=decreasing)
                  return(permu)
                })
  return(lst)
}


#' Function to get the first order marginals
#'
#' @description This function estimates, from a list of permutations, the first
#' order marginals
#' @param permutations a list of objects of class \code{\linkS4class{Permutation}}
#' @param smoothed a logical value to indicate whether the estimation should
#' be smoothed (\code{FALSE} by default)
#' @return A matrix containing the first order marginals
#' @examples
#' scores <- matrix(rnorm(100, 1, 3), ncol=10)
#' permus <- getPermutationFromScores (scores)
#' getFirstOrderMarginals(permus)
#'
getFirstOrderMarginals <- function (permutations, smoothed=FALSE) {
  aux <- lapply(permutations, FUN=function(e) return(e@permutation))
  permu.set <- do.call(rbind, aux)
  size <- ncol(permu.set)
  if (!smoothed) {
    getMarginal <- function (vector)
    {
      aux <- sapply(1:size, FUN=function(i) {return(sum(vector==i))})
      return (aux / sum(aux))
    }
  } else {
    getMarginal <- function (vector)
    {
      aux <- sapply(1:size, FUN=function(i) {return(sum(vector==i))})
      return ((aux + 1) / (sum(aux) + size))
    }
  }
  return(apply(permu.set, MARGIN=2, FUN=getMarginal))
}


#####################################


# -----------------------------------------------------------
#   # R3.1 – Discretización de objetos algoritmo Equal Width
# -----------------------------------------------------------

#' Discretiza un vector en intervalos de ancho igual.
#'
#' @param x Vector numérico a discretizar.
#' @param num.bins Número de intervalos.
#' @return Lista con categorías discretizadas y puntos de corte.
#' @examples
#' valores <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' a <- 4 
#' discretizeEW(valores, a)
#' @export
discretizeEW <- function(x, num.bins) {
  min_x <- min(x)
  max_x <- max(x)
  ancho <- (max_x - min_x) / num.bins
  cortes <- min_x + (1:(num.bins-1)) * ancho
  
  bins <- c(min_x, cortes, max_x)
  etiquetas <- paste0("[", head(bins, -1), ", ", tail(bins, -1), "]")
  
  valores_cat_eq_width <- cut(x,
                              breaks = bins,
                              include.lowest = TRUE,
                              labels = etiquetas)
  
  return(list(
    categorias = valores_cat_eq_width,
    puntos_corte = cortes
  ))
}

# -----------------------------------------------------------
#   # R3.1 – Discretización de objetos algoritmo Equal Frequency
# -----------------------------------------------------------

#' Discretiza un vector en intervalos de frecuencia igual.
#'
#' @param x Vector numérico a discretizar.
#' @param num.bins Número de intervalos.
#' @return Lista con categorías discretizadas y puntos de corte.
#' @examples
#' valores <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' a <- 4
#' discretizeEF(valores, a)
#' @export
discretizeEF <- function(x, num.bins) {
  cortes <- quantile(x, probs = seq(0, 1, length.out = num.bins + 1), na.rm = TRUE)
  cortes <- unique(cortes)  # evitar duplicados
  
  etiquetas <- paste0("[", head(cortes, -1), ", ", tail(cortes, -1), "]")
  
  valores_cat_eq_freq <- cut(x,
                             breaks = cortes,
                             include.lowest = TRUE,
                             labels = etiquetas)
  
  return(list(
    categorias = valores_cat_eq_freq,
    puntos_corte = cortes
  ))
}

# ---------------------------------------------------------
#   # R3.3 – Discretización de objetos por puntos de corte
# ---------------------------------------------------------

#' Discretiza un vector basado en puntos de corte definidos.
#'
#' @param x Vector numérico a discretizar.
#' @param cut.points Vector de puntos de corte.
#' @return Vector categorizado.
#' @examples
#' cortes <- c(5.5, 10.5, 15.5)
#' discretize(valores, cortes)
#' @export
discretize <- function(x, cut.points) {
  limites <- c(min(x), cut.points, max(x))
  etiquetas <- paste0("[", head(limites, -1), ", ", tail(limites, -1), "]")
  
  categorias <- cut(x,
                    breaks = limites,
                    include.lowest = TRUE,
                    labels = etiquetas)
  
  return(categorias)
}

#---------------------------------------------------
#  # R3.4 – Entropía de un vector discretizado
#  -------------------------------------------------

#' Calcula la entropía de un vector discretizado.
#'
#' @param x Vector categórico a evaluar.
#' @return Valor de entropía.
#' @examples
#' x <- c('a', 'a', 'c', 'c', 'c')
#' entropy(x)
#' @export
entropy <- function(x) {
  x <- as.factor(x)
  
  freqs <- table(x)
  probs <- freqs / sum(freqs)
  
  H <- -sum(probs * log2(probs), na.rm = TRUE)
  
  return(H)
}

#' Muestreo de números pares con semilla opcional
#' @param max_par máximo par incluido (por defecto 48 -> primeros 25 pares: 0..48)
#' @param k cuántos extraer
#' @param replace con reemplazo
#' @param seed semilla (opcional)
#' @return vector de pares
#' @examples sample_even(k = 5, seed = 123)
#' @export
sample_even <- function(max_par = 48, k = 5, replace = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pool <- seq(0, max_par, by = 2)
  sample(pool, k, replace = replace)
}

#' (Didáctica) Simetrizar con bucles for
#' @param A matriz cuadrada
#' @return matriz simétrica por media
#' @examples symmetrize_mean_loop(exp_matrix(3))
#' @export
symmetrize_mean_loop <- function(A) {
  stopifnot(is.matrix(A), nrow(A) == ncol(A))
  n <- nrow(A)
  B <- A
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      m <- mean(c(A[i, j], A[j, i]))
      B[i, j] <- m
      B[j, i] <- m
    }
  }
  B
}