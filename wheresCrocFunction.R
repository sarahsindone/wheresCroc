# Set the working directory to the location of your R script
setwd("C:/Users/sindo/OneDrive/Documents/R")

# Now you can source the WheresCroc.R file
source("WheresCroc.R", echo = TRUE)

# ------ Function to Calculate Transition Probability -----

calculateTransitionProbability <- function(node, edges) {
  # Calculate transition probability based on the number of neighbors
  neighbors <- getOptions(node, edges)
  return (1 / length(neighbors))  # Probability is inversely proportional to the number of neighbors
}

# ------ Function to Calculate Emission Vector -----

# Function to calculate emission probabilities for each node based on environmental readings
emissionVector <- function(readings, probabilityDistributions) {
  
  # Calculate probability density functions for each environmental variable
  salinity <- dnorm(readings[1], probabilityDistributions[["salinity"]][, 1], probabilityDistributions[["salinity"]][, 2], FALSE)
  phosphate <- dnorm(readings[2], probabilityDistributions[["phosphate"]][, 1], probabilityDistributions[["phosphate"]][, 2], FALSE)
  nitrogen <- dnorm(readings[3], probabilityDistributions[["nitrogen"]][, 1], probabilityDistributions[["nitrogen"]][, 2], FALSE)
  
  # Initialize an array to store emission probabilities for each node
  emission <- rep(0, 40)
  
  # Compute emission probabilities for each node
  for (i in 1:40) {
    emission[i] <- salinity[i] * phosphate[i] * nitrogen[i]
  }
  
  # Normalize the emission probabilities
  emission <- emission / sum(emission)
  
  # Return the calculated emission vector
  return (emission)
}

# ------ Function to Calculate Node Probability -----

# Function to calculate the probability of being in a specific node
nodeProbability <- function(node, previousProbabilities, edges, emissions) {
  
  # Get the neighboring nodes of the current node
  neighbors <- getOptions(node, edges)
  sumValue <- 0  # Initialize a variable to store the sum of probabilities
  
  # Compute the probability of reaching the node considering the previous state
  for (neighbor in neighbors) {
    # Calculate the transition probability from the neighbor to the current node
    transitionProbability <- calculateTransitionProbability(neighbor, edges)
    
    # Accumulate the sum of probabilities based on neighbors and transition probabilities
    sumValue <- sumValue + transitionProbability * previousProbabilities[neighbor]
  }
  
  # Calculate the new probability for the current node considering emissions
  newProbability <- sumValue * emissions[node]
  
  # Return the calculated new probability for the current node
  return(newProbability)
}

# ------ Hidden Markov Model Function -----

hiddenMarkovModel <- function(previousProbabilities, probabilityDistributions, sensorReadings, touristPositions, graphEdges) {
  tourist1 <- touristPositions[1]
  tourist2 <- touristPositions[2]
  
  newProbabilities <- rep(0, 40)
  
  # Check if tourists have been eaten this turn
  if (!is.na(tourist1) && tourist1 < 0) {
    crocodileNode <- -1 * tourist1
    newProbabilities[crocodileNode] <- 1
  } else if (!is.na(tourist2) && tourist2 < 0) {
    crocodileNode <- -1 * tourist2
    newProbabilities[crocodileNode] <- 1
  } else {
    emissions <- emissionVector(sensorReadings, probabilityDistributions)  # Fix the function name here
    
    # Compute probabilities for each node
    for (i in 1:length(newProbabilities)) {
      newProbabilities[i] <- nodeProbability(i, previousProbabilities, graphEdges, emissions)
    }
    
    currentNode <- touristPositions[3]
    newProbabilities[currentNode] <- 0  # Probability in the current node is set to 0
    # Normalize probabilities
    newProbabilities <- newProbabilities / sum(newProbabilities)
  }
  return (newProbabilities)
}

# ------ Breadth-First Search Function -----

# Function to perform Breadth-First Search on a graph
breadthFirstSearch <- function(startNode, goalNode, graphEdges) {
  visitedNodes <- c(startNode)  # Initialize a vector to store visited nodes, starting with the start node
  nodeQueue <- c(startNode)  # Initialize a queue with the start node
  parentsList <- rep(0, 40)  # Initialize a vector to store parent nodes, assuming there are 40 nodes
  parentsList[startNode] <- -1  # Set the parent of the start node to -1 since it has no parent
  
  # Continue the search until the queue is empty
  while (length(nodeQueue) != 0) {
    currentNode <- head(nodeQueue, n = 1)  # Extract the front of the queue
    nodeQueue <- setdiff(nodeQueue, currentNode)  # Remove the front node from the queue
    neighbors <- getOptions(currentNode, graphEdges)  # Get neighbors of the current node
    neighbors <- setdiff(neighbors, currentNode)  # Exclude the current node from neighbors
    neighbors <- setdiff(neighbors, visitedNodes)  # Exclude already visited nodes
    
    # Process neighbors of the current node
    for (neighbor in neighbors) {
      if (!(neighbor %in% visitedNodes)) {
        nodeQueue <- c(nodeQueue, neighbor)  # Enqueue the neighbor
        parentsList[neighbor] <- currentNode  # Set the parent of the neighbor to the current node
        visitedNodes <- c(visitedNodes, neighbor)  # Mark the neighbor as visited
      }
    }
  }
  
  currentNode <- goalNode  # Start backtracking from the goal node
  path <- numeric()  # Initialize a vector to store the path
  
  # Reconstruct the path by backtracking using parent information
  while (currentNode != -1) {
    if (parentsList[currentNode] != -1) {
      path <- c(currentNode, path)  # Add the current node to the path
    }
    currentNode <- parentsList[currentNode]  # Move to the parent node
  }
  
  # Return the path found using Breadth-First Search
  return (path)
}

# ----- Function to Initialize Probabilities -----

# Function to initialize probability values based on tourist positions
initializeProbabilities <- function(tourist1, tourist2) {
  initialProbabilities <- rep(0, 40)  # Initialize a vector to store initial probabilities for each node
  counter <- 0  # Initialize a counter to track the number of nodes with non-zero probability
  
  # Loop through all nodes
  for (node in 1:40) {
    # Check if both tourists are alive
    if (!is.na(tourist1) && !is.na(tourist2)) {
      # If a tourist is at the waterhole and alive, set probability to 0 for that node
      if (tourist1 == node || tourist2 == node) {
        initialProbabilities[node] <- 0
      } else {
        initialProbabilities[node] <- 1  # Set probability to 1 for nodes not occupied by tourists
        counter <- counter + 1  # Increment the counter
      }
    }
  }
  
  # Probabilities are equal at every possible node, normalize to ensure sum equals 1
  initialProbabilities <- initialProbabilities / counter
  
  # Return the initialized probability vector
  return (initialProbabilities)
}

# ----- Main function -----

# Function defining the main logic for crocodile movement
myFunction <- function(moveInformation, sensorReadings, touristPositions, graphEdges, probabilityDistributions) {
  currentLocation <- touristPositions[[3]]  # Current location of the crocodile
  gameStatus <- moveInformation[["mem"]][["status"]]  # Retrieve the game status information
  
  # Check if it's a new game or a continuation
  if (gameStatus == 0 || gameStatus == 1) {
    # Initialize probabilities for the initial state of the game
    moveInformation[["mem"]][["prev_f"]] <- initializeProbabilities(touristPositions[[1]], touristPositions[[2]])
  }
  
  previousProbabilities <- moveInformation[["mem"]][["prev_f"]]  # Retrieve previous probabilities
  newProbabilities <- hiddenMarkovModel(previousProbabilities, probabilityDistributions, sensorReadings, touristPositions, graphEdges)  # Calculate new probabilities
  goalNode <- which.max(newProbabilities)  # Determine the node with the highest probability
  
  # If the goal is in the neighboring node, move there
  neighbors <- getOptions(touristPositions[3], graphEdges)
  if (goalNode %in% neighbors) {
    moveInformation$moves <- c(goalNode, 0)
    return (moveInformation)
  }
  
  # Use BFS search to find the shortest path to the goal node
  pathToGoal <- breadthFirstSearch(touristPositions[3], goalNode, graphEdges)
  
  # Two nodes away from the goal
  if (length(pathToGoal) >= 2) {
    moveInformation$moves <- c(pathToGoal[1], pathToGoal[2])
  }
  
  # One node away from the goal
  if (length(pathToGoal) == 1) {
    moveInformation$moves <- c(pathToGoal[1], 0)
  }
  
  # At the goal node, search for the crocodile
  if (length(pathToGoal) == 0) {
    moveInformation$moves <- c(0, 0)  
  }
  
  moveInformation[['mem']][["prev_f"]] <- newProbabilities  # Update previous probabilities
  moveInformation[["mem"]][["status"]] <- 2  # Set the game status to continuation
  
  return(moveInformation)
}

# ----- Runnning the program -----
result <- runWheresCroc(myFunction, doPlot = T, showCroc = F, pause = 1, verbose = T, returnMem = F, mem = NA)