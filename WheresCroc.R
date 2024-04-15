#' randomWC
#'
#' Control function for Where's Croc where moves are random.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
# control function for Where's Croc
# control function for Where's Croc using HMM
# control function for Where's Croc using HMM
# control function for Where's Croc using HMM
# Define the number of states, observations, and sensors

# Initialize mem and moves
mem <- list()
moves <- c()

# Define  randomWC function
randomWC <- function(moveInfo, readings, positions, edges, probs) {
  if (moveInfo$mem$status == 0) {
    # Initialize game positions based on internal functions
    points <- getPoints()
    edges <- getEdges()
    probs <- getProbs()
    
    positions <- c(sample(1:40, 1), sample(1:40, 1), sample(1:40, 1), sample(1:40, 1))  # Croc, BP1, BP2, Player
    moveInfo$mem$status <- 1  # Set game status to active
    moveInfo$mem$positions <- positions
  }
      
      # Extract Croc's, BP1's, BP2's, and Player's positions from memory
      positions <- moveInfo$mem$positions
      crocPosition <- positions[1]
      bp1Position <- positions[2]
      bp2Position <- positions[3]
      playerPosition <- positions[4]
      
      # Initialize bp1Move and bp2Move
      bp1Move <- 0
      bp2Move <- 0
      
      # Implement Croc's random movement
      validCrocMoves <- edges[edges[, 1] == crocPosition, 2]
      newCrocPosition <- sample(validCrocMoves, 1)
      positions[1] <- newCrocPosition
      
      # Update memory with new positions
      moveInfo$mem$positions <- positions
      
      # Check if Croc is found by the Player
      if (playerPosition == crocPosition) {
        gameResult <- paste("Congratulations - You found Croc at move", length(moveInfo$moves) + 1)
        return(list(moveInfo, gameResult))
      }
      
      # Implement random moves for BP1 and BP2
      validMovesForBP1 <- edges[edges[, 1] == bp1Position, 2]
      if (length(validMovesForBP1) > 0) {
        bp1Move <- sample(validMovesForBP1, 1)
      }
      
      validMovesForBP2 <- edges[edges[, 1] == bp2Position, 2]
      if (length(validMovesForBP2) > 0) {
        bp2Move <- sample(validMovesForBP2, 1)
      }
      
      # Update BP1's and BP2's positions
      if (bp1Move > 0) {
        positions[2] <- bp1Move
      }
      
      if (bp2Move > 0) {
        positions[3] <- bp2Move
      }
      
      # Update memory with new positions
      moveInfo$mem$positions <- positions
      
      # Check if Croc is found by the Player after BP moves
      if (playerPosition == crocPosition) {
        gameResult <- paste("Congratulations - You found Croc at move", length(moveInfo$moves) + 1)
        return(list(moveInfo, gameResult))
      }
      
      # Implement Player's random move
      validPlayerMoves <- edges[edges[, 1] == playerPosition, 2]
      
      # Check if there are valid moves for the player
      if (length(validPlayerMoves) > 0) {
        newPlayerPosition <- sample(validPlayerMoves, 1)
        positions[4] <- newPlayerPosition
      }
      
      # Update memory with new positions
      moveInfo$mem$positions <- positions
      
      # Check if Croc is found by the Player
      if (playerPosition == crocPosition) {
        gameResult <- paste("Congratulations - You found Croc at move", length(moveInfo$moves) + 1)
        return(list(moveInfo, gameResult))
      }
      
      # If the game is still incomplete, return the updated moveInfo with BP moves
      return(list(moveInfo, c(bp1Move, bp2Move)))
}


#' Control function for Where's Croc that allows manual play using the keyboard.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
manualWC <- function(moveInfo, readings, positions, edges, probs) {
  if (moveInfo$mem$status == 0) {
    # Initialize available moves
    moveInfo$mem$availableMoves <- getEdges()[positions[4] == 1, 2]
    moveInfo$mem$status <- 1
  }
  
  # Use the available moves stored in moveInfo
  availableMoves <- moveInfo$mem$availableMoves
  
  # Prompt the player for moves
  moves <- numeric(2)  # Initialize moves as two zeros
  
  for (i in 1:2) {
    cat(paste("Move", i, "options (plus 0 for search):\n"))
    print(availableMoves)
    mv <- as.character(readline(paste("Move", i, ": ")))
    
    if (mv == "q") {
      stop("Player quit the game.")
    }
    
    if (is.na(as.numeric(mv))) {
      warning("Invalid input. Please enter a numeric move or 'q' to quit.")
      return(moveInfo)  # Return moveInfo to ensure it's updated
    }
    
    mv <- as.integer(mv)
    
    if (mv == 0 || mv %in% availableMoves) {
      moves[i] <- mv
    } else {
      warning("Invalid move. Please select a valid move or '0' to search.")
      return(moveInfo)  # Return moveInfo to ensure it's updated
    }
    
    if (moves[i] != 0) {
      availableMoves <- getEdges()[moves[i] == getEdges()[, 1], 2]
    }
  }
  
  # Ensure that moves is a vector of two elements
  if (length(moves) != 2) {
    warning("Invalid move. Please select a valid move or '0' to search.")
    return(moveInfo)  # Return moveInfo to ensure it's updated
  }
  
  # Update available moves in moveInfo
  moveInfo$mem$availableMoves <- availableMoves
  
  # Set the moves in moveInfo
  moveInfo$moves <- moves
  
  if (all(moves == 0)) {
    # If both moves are 0, this indicates a search
    moves[1] <- positions[4]  # Move to current position
    moves[2] <- 0  # Search
    moveInfo$moves <- moves
  }
  
  return(moveInfo)  # Return moveInfo with the updated moves
}

#' testWC
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#'
#' With the default seed of 21, the mean for the par function on this is 5.444, and the sd is approximately 3.853.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 30 seconds. Note that you will need to reuse objects that do not change
#' from game to game (such as the transition matrix and routing information) in order to achieve this sort
#' of speed.
#'
#' The par function takes approximately 3.85 seconds on my laptop. If it takes longer than 30 seconds on the
#' evaluation machine, the time limit will be increased so as to be 25% slower than the par function.
#'
#' The mem (memory) object you use in the function you create (see the runWheresCroc documentation)
# is passed from game to game. This is so you can reuse whatever you set up there to quickly work out
# what moves to make in different situations. Note that it contains a status field that can be used to work out
# when a game ends and a new game begins. See the runWheresCroc documentation for more details.
#'
#' @param myFunction Your function to be passed to runWheresCroc. See runWheresCroc documentation for details.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runWheresCroc output of the result of each game.
#' @param returnVec See return value.
#' @param n The number of games to run. In the evaluation, this will be 500.
#' @param seed The random seed to use. Pass NA to not set a random seed.
#' @param timeLimit The time limit. If this is breached, an NA is returned.
#'
#' @return If your function is too slow, NA is returned. Otherwise, if returnVec is TRUE, a vector containing
#' all results is returned. If returnVec is FALSE, the average performance is returned.
# @export
testWC <- function(verbose = 0, returnVec = FALSE, n = 500, seed = 21, timeLimit = 30) {
  set.seed(seed)
  seeds <- sample(1:25000, n)
  startTime <- Sys.time()
  mem <- NA
  moves <- numeric(0)
  
  for (s in seeds) {
    midTime <- Sys.time()
    if (as.numeric(midTime) - as.numeric(startTime) > timeLimit) {
      cat("\nRun terminated due to slowness.")
      return(NA)
    }
    set.seed(s)
    if (verbose == 2)
      cat("\nNew game, seed", s)
    # Pass custom randomWC function as the makeMoves function
    res <- runWheresCroc(randomWC, doPlot = FALSE, pause = 1, verbose = verbose == 2, returnMem = TRUE, mem = mem)
    mem <- res$mem
    moves <- c(moves, res$move)
  }
  
  if (verbose >= 1) {
    endTime <- Sys.time()
    cat("\nMean moves:", mean(moves))
    cat("\nSD moves:", sd(moves))
    cat("\nTime taken:", as.numeric(endTime) - as.numeric(startTime), "seconds.")
  }
  
  if (returnVec)
    return(moves)
  else
    return(mean(moves))
}


#' Run Where's Croc
#'
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park.
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game.
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' Note that the croc will move randomly, with a uniform distribution over moving to any adjacent waterholes
#' or staying still.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fields. The first is a vector of numbers called 'moves', where you will enter
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current
#' location. (3) A vector giving the positions of the two tourists
#' (elements 1 and 2) and yourself (element 3). If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a two column matrix giving the
#' edges paths between waterholes (edges) present (the numbers are from and to numbers for
#' the waterholes). All edges can be crossed both ways, so are only given once.
#' (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector
#' and any changes to the 'mem' field you wish to access later on.
#' @param doPlot A Boolean stating if you want the gameboard to be plotted each turn
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Designed to give time to plot the game.
#' @param verbose Set to FALSE to stop any print output
#' @param returnMem Should the info$mem field be returned? If so, the output is a list consisting of
#' the move field, giving the number of moves in the game, and the mem field consisting of the mem
#' object
#' @param mem If you returned a mem object from a previous run, it can be passed here. Its status
#' will be set to 1. Otherwise, a new mem list will be created with status set to 0. The idea is
#' to speed up multiple runs, such as the evaluation run of 500 games, by avoiding redoing
#' expensive initial setups of the transition matrix and routing information.
#' @return A string describing the outcome of the game.
#' @export
# Run Where's Croc
runWheresCroc <- function(makeMoves, doPlot = TRUE, showCroc = FALSE, pause = 1, verbose = TRUE, returnMem = FALSE, mem = NA) {
  # Initialize game positions
  positions <- c(sample(1:40, 1), sample(1:40, 1), sample(1:40, 1), sample(1:40, 1))  # Croc, BP1, BP2, Player
  points <- getPoints()
  edges <- getEdges()  # Retrieve edges using the getEdges function
  probs <- getProbs()
  move <- 0
  moveInfo <- list(moves = c(), mem = list(status = 0))
  
  if (!all(is.na(mem))) {
    moveInfo$mem <- mem
  }
  
  first <- TRUE
  gameResult <- "Incomplete"
  
  while (gameResult == "Incomplete") {
    move <- move + 1
    
    if (!first) {
      # Sample a new position for the crocodile (position 1)
      valid_indices <- which(edges[, 1] == positions[1])
      if (length(valid_indices) == 0) {
        stop("No valid edges found for the current position.")
      }
      
      new_position <- sample(edges[valid_indices, 2], 1)
      
      if (!is.na(new_position)) {
        positions[1] <- new_position
      } else {
        warning("No valid new position found.")
      }
      
      # Sample new positions for BP1 and BP2 (positions 2 and 3)
      for (bp in 2:3) {
        if (!is.na(positions[bp]) && positions[bp] > 0) {
          valid_indices <- which(edges[, 1] == positions[bp])
          if (length(valid_indices) > 0) {
            positions[bp] <- sample(edges[valid_indices, 2], 1)
          } else {
            positions[bp] <- NA
          }
        } else if (!is.na(positions[bp]) && positions[bp] < 0) {
          positions[bp] <- NA
        }
      }
      
      # Check and adjust for positions 2 and 3 if they are the same as the crocodile
      if (!is.na(positions[2]) && positions[2] == positions[1]) {
        positions[2] <- -positions[2]
      }
      if (!is.na(positions[3]) && positions[3] == positions[1]) {
        positions[3] <- -positions[3]
      }
    } else {
      first <- FALSE
    }
    
    if (doPlot) {
      plotGameboard(points, edges, move, positions, showCroc)
    }
    
    Sys.sleep(pause)
    
    readings <- getReadings(positions[1], probs)
    moveInfo <- makeMoves(moveInfo, readings, positions[2:4], edges, probs)
    
    if (length(moveInfo$moves) != 2) {
      stop("Error! The makeMoves function should return a vector of two elements.")
    }
    
    for (m in moveInfo$moves) {
      if (m == 0) {
        if (positions[1] == positions[4]) {
          gameResult <- paste("Congratulations - You found Croc at move", move)
          if (returnMem) {
            mem <- moveInfo$mem
            mem$status <- 1
            return(list(move = move, mem = mem, result = gameResult))
          }
          return(list(move = move, result = gameResult))
        }
      } else {
        # Corrected the way edges are checked
        if (m %in% edges[edges[, 1] == positions[4], 2]) {
          positions[4] <- m
        } else {
          warning("Invalid move.")
        }
      }
    }
  }
  
  if (returnMem) {
    return(list(move = move, mem = moveInfo$mem, result = gameResult))
  } else {
    return(list(move = move, result = gameResult))
  }
}

#' @keywords internal
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @keywords internal
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @keywords internal
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @keywords internal
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @keywords internal
plotGameboard <- function(points, edges, move, positions, showCroc) {
  plot(points[, 1], points[, 2], pch = 18, col = "blue", cex = 2, xlab = "X", ylab = "Y", main = paste("Where's Croc - Move", move))
  xFrom <- points[edges[, 1], 1]
  yFrom <- points[edges[, 1], 2]
  xTo <- points[edges[, 2], 1]
  yTo <- points[edges[, 2], 2]
  
  # Filter out any invalid edges
  validEdges <- which(!is.na(xFrom) & !is.na(yFrom) & !is.na(xTo) & !is.na(yTo))
  segments(xFrom[validEdges], yFrom[validEdges], xTo[validEdges], yTo[validEdges])
  
  for (bp in 2:3) {
    if (!is.na(positions[bp])) {
      if (positions[bp] > 0) {
        points(points[as.numeric(positions[bp]), 1], points[as.numeric(positions[bp]), 2], col = "orange", pch = 17, cex = 4)
      } else {
        points(points[-as.numeric(positions[bp]), 1], points[-as.numeric(positions[bp]), 2], col = "red", pch = 17, cex = 4)
      }
    }
  }
  
  points(points[as.numeric(positions[4]), 1], points[as.numeric(positions[4]), 2], col = "green", pch = 15, cex = 4)
  
  if (showCroc) {
    if (!is.na(positions[1])) {
      points(points[as.numeric(positions[1]), 1], points[as.numeric(positions[1]), 2], col = "red", pch = 15, cex = 4)
    }
  }
  
  text(points[, 1] + 0.4, points[, 2], labels = as.character(1:40))
}

result <- runWheresCroc(randomWC, doPlot = TRUE, showCroc = TRUE)



