#' ridgereg Class
#'
#' @field formula formula.
#' @field data data.frame.
#' @field lambda numeric.
#' @field beta numeric.
#' @field dataNames character.
#' @field yHat numeric.
#'
#' @return ridgereg object
#' @export ridgereg
#'
#' @examples
#' data(iris)
#' ridgereg <-  ridgereg(Petal.Length ~ Species, data = iris)
#' ridgereg$print()
#' ridgereg$predict()
#' ridgereg$coef()

ridgereg <- setRefClass("ridgereg",

                        fields = list(
                          formula = "formula",
                          data = "data.frame",
                          lambda = "numeric",
                          beta = "numeric",
                          dataNames = "character",
                          yHat = "numeric"
                        ),

                      methods = list(

                        initialize = function(formula = as.formula , data = as.data.frame, lambda = 0) {

                          X <- model.matrix(formula,data)
                          formula <<- formula
                          data <<- data
                          lambda <<- lambda

                          y <- data[,colnames(data) == all.vars(formula)[1], drop = FALSE]

                          for(i in 2:ncol(X)) {
                            X[,i] <- (X[,i] - mean(X[,i] )) / sd(X[,i])
                          }


                          QR_X <- qr(X)
                          Q <- qr.Q(QR_X)
                          R <- qr.R(QR_X)

                          mat_Y <- as.matrix(y)

                          I_mat <- diag(lambda, nrow = ncol(X))
                          beta_hat <- solve( t(R) %*% R + I_mat) %*% ( t(X) %*% mat_Y)
                          beta <<-round(beta_hat[,1], digits = 2)
                          yHat <<- as.numeric(X %*% beta)

                          dataNames <<-  deparse(substitute(data))
                        },

                        predict = function(values = NULL) {

                          if(!(is.null(values))) {
                            values <- data.frame(Intercept = 1 , values)
                            result <- (as.matrix(values) %*% matrix(beta, nrow = length(beta)))
                            return(result[,1])
                          }
                          return(yHat)
                        },

                        coef = function() {
                          return(beta)
                        },

                        print = function() {
                          cat("Call:")
                          cat("\n")
                          statement1 <- paste0("ridgereg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",dataNames,", lambda = ", lambda, ")", "\n", "\n", sep="")
                          cat(statement1)
                          cat("Coefficients:")
                          cat("\n")
                          beta
                        }
                      )
)
