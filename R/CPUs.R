#' Characteristics of computer central processing unit chips over the decades
#'
#' Since single-chip central processing units were introduced in the early 1970s,
#' performance has increased exponentially. Around 1965, Gorden E. Moore, a co-founder
#' of the integrated circuit giant Intel, postulated that capacity of chips, measured as
#' the number of transistors, doubles approximately every two years. Known as Moore's Law, this
#' pattern has held true for decades.
#'
#' @docType data
#' @name CPUs
#' @usage data(CPUs)
#'
#' @keywords datasets
#'
#' @format
#'   A data frame with 128 cases, each a computer CPU.
#'   \itemize{
#'     \item{\code{processor}} {Model name/number of the processor}
#'     \item{\code{transistors}} {Number of transistors}
#'     \item{\code{year}} {Year of market introduction of the CPU}
#'     \item{\code{designer}} {The company that introduced the CPU}
#'     \item{\code{process}} {The width of circuit elements, in nm.}
#'     \item{\code{area}} {Area of the chip in mm-squared.}
#'     \item{\code{density}} {The number of transistors per unit area, simply `transistors/area`.}
#'     }
#'
#' @source Dr. Joe Eichholz
"CPUs"
