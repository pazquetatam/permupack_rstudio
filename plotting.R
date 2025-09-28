#' Function to visualize the first order marginals
#'
#' @description This function estimates from a list of permutations the first
#' order marginals and visualizes them
#' @param permutations a list of objects of class \code{\linkS4class{Permutation}}
#' @param smoothed a logical value to indicate whether the estimation should
#' be smoothed (\code{FALSE} by default)
#' @details This function requires two additional packages, ggplot2 and reshape2
#' @return An object of class ggplot with the visualization
#' @examples
#' scores <- matrix(rnorm(100, 1, 3), ncol=10)
#' permus <- getPermutationFromScores (scores)
#' plotFirstOrderMarginals(permus)
#'
plotFirstOrderMarginals <- function (permutations, type="ranking",
                                     decreasing=FALSE, smoothed=FALSE) {
  if(!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("La función 'plotFirstOrderMarginals' requiere la instalación del
         paquete ggplot2 y reshape2. Instala estos paquetes y
         vuelve a ejecutar la función")
  }
  marginals <- getFirstOrderMarginals(permutations, smoothed)
  df <- reshape2::melt(marginals)
  names(df) <- c("Posicion", "Elemento", "Marginal")
  g <- ggplot2::ggplot(df, ggplot2::aes(x=Elemento, y=Posicion, fill=Marginal)) +
    ggplot2::geom_tile() + ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  return(g)
}

# Función para graficar el ROC
plot_roc <- function(scores, labels, smoothed = FALSE, f = 2/3, ...) {
  # (código)
}

# Cargar las bibliotecas necesarias
library(pROC)
library(corrplot)

# -----------------------------------------------------------
#   # R6.1 – Visualización curva ROC
# -----------------------------------------------------------
#' Dibujar la curva ROC
#' @param A data.frame o matriz con columna de valores reales y columna de probabilidades
#' @param col_real índice o nombre de columna con la variable real (0/1)
#' @param col_prob índice o nombre de columna con probabilidades estimadas
#' @examples dibujar_roc(data.frame(real = c(0, 1, 1, 0), prob = c(0.1, 0.4, 0.8, 0.3)))
#' @export
dibujar_roc <- function(A, col_real = 1, col_prob = 2) {
  real <- A[[col_real]]
  prob <- A[[col_prob]]
  
  roc_obj <- roc(real, prob)
  plot(roc_obj, main = paste("Curva ROC - AUC =", round(auc(roc_obj), 3)),
       col = "blue", lwd = 2)
  abline(0, 1, lty = 2, col = "red")  # diagonal referencia
}

# -----------------------------------------------------------
#   # R6.2 – Barplot de la entropía normalizada por variable
# -----------------------------------------------------------
#' Calcular entropía de una variable categórica
#' @param x vector categórico
#' @return entropía
entropia <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log2(p))
}

#' Dibujar entropía normalizada de todas las columnas
#' @param df data.frame con variables categóricas
#' @return vector de entropías normalizadas
#' @examples dibujar_entropias(data.frame(A = sample(letters[1:3], 100, TRUE)))
#' @export
dibujar_entropias <- function(df) {
  entropias <- sapply(df, entropia)
  maximos <- log2(sapply(df, function(x) length(unique(x))))
  entropias_norm <- entropias / maximos
  
  barplot(entropias_norm,
          main = "Entropía normalizada por variable",
          ylab = "Entropía normalizada (0-1)",
          col = "skyblue", las = 2)
  
  return(entropias_norm)
}

# -----------------------------------------------------------
#   # R6.3 – Visualización matriz de correlaciones
# -----------------------------------------------------------
#' Dibujar matriz de correlaciones
#' @param df data.frame con variables numéricas
#' @return matriz de correlaciones
#' @examples dibujar_correlaciones(mtcars)
#' @export
dibujar_correlaciones <- function(df) {
  M <- cor(df, use = "pairwise.complete.obs")
  corrplot(M, method = "color", type = "upper",
           addCoef.col = "black",
           tl.col = "black", tl.srt = 45,
           main = "Matriz de correlaciones")
  
  return(M)
}