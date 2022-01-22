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
  #' @field initial.state.dist Probabilities for states initially
  initial.state.dist = NULL,
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
  #' Set the initial state distribution.
  #' @param initial.state.dist A vector of the initial state distribution.
  setInitialStateDist = function(initial.state.dist) {

    checkmate::assertNumeric(initial.state.dist, len = length(self$states), any.missing = FALSE)
    checkmate::assertNumeric(sum(initial.state.dist), lower = 1, upper = 1)

    self$initial.state.dist <- initial.state.dist
  },

  #' @description
  #' Set state transition probabilites.
  #' @param state.transitions A matrix of state transition probabilites.
  setStateTransitions = function(state.transitions) {

    checkmate::assertMatrix(state.transitions, nrows = length(self$states), ncols = length(self$states))
    checkmate::assertNumeric(rowSums(state.transitions), lower = 1, upper = 1)

    self$state.transitions <- state.transitions
  },

  #' @description
  #' Set signal probabilites.
  #' @param signal.probabilities A matrix of signal probabilites per state.
  setSignalProbability = function(signal.probabilities) {

    checkmate::assertMatrix(signal.probabilities, nrows = length(self$states), ncols = length(self$signal.set))
    checkmate::assertNumeric(rowSums(signal.probabilities), lower = 1, upper = 1)

    self$signal.probabilities <- signal.probabilities
  },

  #' @description
  #' Compute the probability of a signal sequence via the forward procedure.
  #' @param ob.sequence A sequence of signal out of the signal.set
  #' @return A numeric for the probability.
  getSignalProb = function(ob.sequence) {

    checkmate::assertCharacter(ob.sequence, any.missing = FALSE)
    checkmate::assertSubset(ob.sequence, self$signal.set)

    N <- length(self$states)
    alphas <- matrix(0, nrow = N, ncol = 2)

    for (t in seq_len(length(ob.sequence))) {

      t.signal <- ob.sequence[[t]]
      t.signal.col.number <- seq_len(length(self$signal.set))[t.signal == self$signal.set]


      if (t == 1) {

        for (i in seq_len(N)) {

          alphas[i, 2] <- self$initial.state.dist[[i]] *
            self$signal.probabilities[i, t.signal.col.number]
        }
      }

      else {
        for (i in seq_len(N)) {

          alphas[i, 2] <- sum(alphas[, 1] * self$state.transitions[, i]) *
            self$signal.probabilities[i, t.signal.col.number]
        }
      }

      if (t == length(ob.sequence)) {

        return(sum(alphas[, 2]))
      }

      else {

        alphas[, 1] <- alphas[, 2]
      }
    }
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
