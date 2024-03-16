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
#' A data frame with 128 cases, each a computer CPU.
#' - `processor` Model name/number of the processor
#' - `transistors` Number of transistors
#' - `year` Year of market introduction of the CPU
#' - `designer` The company that introduced the CPU
#' - `process` The width of circuit elements, in nm.
#' - `area` Area of the chip in mm-squared.
#' - `density` The number of transistors per unit area, simply `transistors/area`.
#'     
#'
#' @source Dr. Joe Eichholz
"CPUs"
