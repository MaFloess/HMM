#' Create a Hidden Markov Model
#'
#' A HMM has an identifier, states and observable signals
HMMModel <- R6::R6Class("HMMModel", public = list(

  #' @field identifier Name given to this model
  identifier = NULL,
  #' @field states States this HMM can be in
  states = NULL,
  #' @field signal.set Observable signals that this model can produce
  signal.set = NULL,
  #' @field state.transitions Probabilities for transitions between states
  state.transitions = NULL,
  #' @field signal.probabilities Probabilities to produce signals in certain states
  signal.probabilities = NULL,

  #' @description
  #' Create a new HMM object.
  #' @param identifier Identifier.
  #' @param states States.
  #' @param signal.set Signal.set.
  #' @return A new `HMM` object.
  initialize = function(identifier, states, signal.set) {

    checkmate::assert_character(identifier, len = 1, any.missing = FALSE)
    checkmate::assertCharacter(states, any.missing = FALSE)
    checkmate::assertCharacter(signal.set, any.missing = FALSE)

    self$identifier <- identifier
    self$states <- states
    self$signal.set <- signal.set
  },

  #' @description
  #' Set state transition probabilites.
  #' @param state.transitions A matrix of state transition probabilites.
  #' @examples
  #' M <- HMMModel$new("Ex", c("state1", "state2"), c("signalA", "signalB", "signalC"))
  #' M$setStateTransitions(matrix(c(0.4, 0.6, 0.6, 0.4), byrow = TRUE, ncol = 2))
  #' M$state.transitions
  setStateTransitions = function(state.transitions) {

    checkmate::assertMatrix(state.transitions, nrows = length(self$states), ncols = length(self$states))
    checkmate::assertNumeric(rowSums(state.transitions), lower = 1, upper = 1)

    self$state.transitions <- state.transitions
  },

  #' @description
  #' Set signal probabilites.
  #' @param signal.probabilities A matrix of signal probabilites per state.
  #' @examples
  #' M <- HMMModel$new("Ex", c("state1", "state2"), c("signalA", "signalB", "signalC"))
  #' M$setSignalProbability(matrix(c(0.4, 0.4, 0.2, 0.1, 0.1, 0.8), byrow = TRUE, ncol = 3))
  #' M$signal.probabilities
  setSignalProbability = function(signal.probabilities) {

    checkmate::assertMatrix(signal.probabilities, nrows = length(self$states), ncols = length(self$signal.set))
    checkmate::assertNumeric(rowSums(signal.probabilities), lower = 1, upper = 1)

    self$signal.probabilities <- signal.probabilities
  },

  #' @description
  #' Print information for the HMM.
  #' @param ... -
  print = function(...) {

    cat("HMMModel ", self$identifier, " with ", length(self$states), " states produces ",
        paste(self$signal.set, collapse = ", "), " as possible signals.\n", sep = "")


    formatNumber <- function(number) {

      if(nchar(number) < 6) {

        return(paste(c(number, rep(" ", 6 - nchar(number))), collapse = ""))
      }

      number
    }


    if(!is.null(self$state.transitions)) {

      cat("\nState transitions probabilities:\n")

      for (i in seq_len(nrow(self$state.transitions))) {

        cat(sapply(as.character(self$state.transitions[i, ]), formatNumber), "\n", sep = " ")
      }
    }


    if(!is.null(self$signal.probabilities)) {

      cat("\nSignal(", paste(self$signal.set, collapse = ", "),
          ")(column) probability in each state(", paste(self$states, collapse = ", "),
          ")(row):\n", sep = "")

      for (i in seq_len(nrow(self$signal.probabilities))) {

        cat(sapply(as.character(self$signal.probabilities[i, ]), formatNumber), "\n", sep = " ")
      }
    }
  }
))
