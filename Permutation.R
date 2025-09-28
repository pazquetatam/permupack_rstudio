# ============================
# Permutations utilities
# ============================

#' ¿Es una permutación 1..n?
#' @param p vector entero con los valores 1..n en algún orden
#' @return TRUE/FALSE
#' @examples is_permutation(c(3,1,2))
#' @export
is_permutation <- function(p){
  is.numeric(p) && length(p) > 0 && all(sort(p) == seq_along(p))
}

#' Valida que p es una permutación 1..n (o lanza error)
#' @param p vector candidato
#' @return (invisible) p
#' @export
perm_check <- function(p){
  if(!is_permutation(p)) stop("`p` no es una permutación 1..n.", call. = FALSE)
  invisible(p)
}

#' Permutación aleatoria de tamaño n
#' @param n tamaño
#' @param seed semilla (opcional)
#' @return vector con una permutación de 1..n
#' @examples perm_random(5, seed = 123)
#' @export
perm_random <- function(n, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  sample.int(n, n)
}

#' Inversa de una permutación
#' @param p permutación válida (1..n)
#' @return inversa de p
#' @examples perm_invert(c(3,1,2))  # -> c(2,3,1)
#' @export
perm_invert <- function(p){
  perm_check(p)
  inv <- integer(length(p))
  inv[p] <- seq_along(p)
  inv
}

#' Composición de permutaciones (p ∘ q)
#'
#' Definimos \eqn{r = p ∘ q} como \eqn{r[i] = p[q[i]]}.
#' Es decir, primero se aplica q y luego p.
#'
#' @param p,q permutaciones 1..n
#' @return r = p ∘ q
#' @examples perm_compose(c(2,3,1), c(3,1,2))  # p(q(i))
#' @export
perm_compose <- function(p, q){
  perm_check(p); perm_check(q)
  if(length(p) != length(q)) stop("Longitudes distintas.", call. = FALSE)
  p[q]
}

#' Aplicar una permutación a un vector (estilo indexado)
#'
#' Usamos la convención habitual de R: \code{y <- x[p]}.
#' Esto equivale a \eqn{y_i = x_{p_i}}.
#'
#' @param x vector/matriz a reordenar (si es matriz, se reordenan filas)
#' @param p permutación 1..n (n = length(x) o n = nrow(x))
#' @return objeto permutado
#' @examples perm_apply(letters[1:5], c(3,5,1,2,4))
#' @export
perm_apply <- function(x, p){
  perm_check(p)
  if(is.matrix(x)){
    if(nrow(x) != length(p)) stop("`p` debe tener longitud nrow(x).", call. = FALSE)
    return(x[p, , drop = FALSE])
  } else {
    if(length(x) != length(p)) stop("`p` debe tener longitud length(x).", call. = FALSE)
    return(x[p])
  }
}

#' Transposición elemental (swap de posiciones i y j)
#'
#' @param n tamaño total o
#' @param p permutación a modificar (si se proporciona, se aplica swap sobre p)
#' @param i índice 1..n
#' @param j índice 1..n
#' @return si solo se pasa n: transposición sobre identidad. Si se pasa p: p con i y j intercambiados.
#' @examples perm_swap_n(5, 2, 5)           # swap en identidad
#' @examples perm_swap_p(c(1,2,3,4,5), 2,5) # swap sobre p
#' @export
perm_swap_n <- function(n, i, j){
  p <- seq_len(n)
  p[c(i,j)] <- p[c(j,i)]
  p
}

#' @rdname perm_swap_n
#' @export
perm_swap_p <- function(p, i, j){
  perm_check(p)
  p[c(i,j)] <- p[c(j,i)]
  p
}

#' Matriz de permutación
#'
#' Construye \eqn{P} tal que \eqn{(P %*% x)_i = x_{p_i}} para vector columna x.
#' Es decir, \code{P[i, p[i]] = 1}.
#'
#' @param p permutación 1..n
#' @return matriz n x n binaria con una 1 por fila/columna
#' @examples perm_matrix(c(3,1,2))
#' @export
perm_matrix <- function(p){
  perm_check(p)
  n <- length(p)
  P <- matrix(0L, n, n)
  P[cbind(seq_len(n), p)] <- 1L
  P
}

#' Paridad de una permutación (signo)
#'
#' Devuelve "even"/"odd" y el signo (+1 / -1) según el número de inversiones.
#'
#' @param p permutación 1..n
#' @return lista con campos \code{parity} y \code{sign}
#' @examples perm_parity(c(3,1,2))$parity
#' @export
perm_parity <- function(p){
  perm_check(p)
  invs <- 0L
  n <- length(p)
  for(i in seq_len(n-1L)){
    invs <- invs + sum(p[i] > p[(i+1L):n])
  }
  list(parity = if(invs %% 2L == 0L) "even" else "odd",
       sign   = if(invs %% 2L == 0L) 1L else -1L)
}

#' Descomposición en ciclos disjuntos
#' @param p permutación 1..n
#' @return lista de ciclos (cada ciclo es un vector)
#' @examples perm_cycles(c(2,3,1,5,4))  # -> list(c(1,2,3), c(4,5))
#' @export
perm_cycles <- function(p){
  perm_check(p)
  n <- length(p)
  seen <- rep(FALSE, n)
  cycles <- list()
  for(i in seq_len(n)){
    if(!seen[i]){
      cyc <- integer(0)
      j <- i
      while(!seen[j]){
        seen[j] <- TRUE
        cyc <- c(cyc, j)
        j <- p[j]
      }
      if(length(cyc) > 1L || p[i] == i) cycles[[length(cycles)+1L]] <- cyc
    }
  }
  cycles
}

##################################
#' An S4 class to represent Permutations
#' 
#' @slot permutation Numeric vector containing the actual permutation
#' @slot size Numeric value indicating the size of the permutation
#'

setClass(Class="Permutation",
         slots=c("permutation"="numeric", "size"="numeric"))

checkValidityPermutation <- function(object) {
  if (length(object@permutation) != object@size) {
    stop("The 'size' parameter is not equal to the length of the vector")
  }
  if (all(sort(unique(object@permutation)) == 1:object@size)){
    return(TRUE)
  } else {
    stop("The vector is not a valid permutation")
  }
}

setValidity(Class="Permutation", method=checkValidityPermutation)

#' Basic constructor of the Permutation class
#'
#' @description This function creates an object of class \code{\linkS4class{Permutation}}
#'
#' @param vector A numeric vector containing the permutation
#' @return An object of class \code{\linkS4class{Permutation}} that represents
#' the permutation passes as an argument
#'
permutation <- function (vector) {
  size <- length(vector)
  object <- new("Permutation", permutation=vector, size=size)
  return(object)
}


setGeneric(name="inverse", def=function(x) standardGeneric("inverse"))

#' Function to invert a permutation
#'
#' @description This returns the inverse of a permutation
#' @param permutation Object of class \code{\linkS4class{Permutation}} to be
#' inverted
#' @return A new object of class \code{\linkS4class{Permutation}} equal
#' to the inverse of \code{permutation}
#' @examples
#' permu <- permutation(order(runif(10)))
#' permu
#' inverse(permu)
#'
setMethod(f="inverse",
          signature="Permutation",
          definition=function(x) {
            return(permutation(order(x@permutation)))
          })

setGeneric(name="%.%", def=function(e1, e2) standardGeneric("%.%"))

#' Operator to compose two permutations
#'
#' @description This operator returns the composition of two permutations
#' @param e1 Object of class \code{\linkS4class{Permutation}} representing the
#' firs permutation in the composition
#' @param e2 Object of class \code{\linkS4class{Permutation}} representing the
#' second permutation in the composition
#' @return A new object of class \code{\linkS4class{Permutation}} equal to the
#' composition of e1 and e2
#' @examples
#' permu <- permutation(order(runif(10)))
#' permu %.% inverse(permu)
setMethod(f="%.%",
          signature="Permutation",
          definition=function(e1, e2) {
            if (e1@size != e2@size) {
              stop("No se pueden componer dos permutaciones de diferente tamaño!")
            }
            return(e1@permutation[e2@permutation])
          })
