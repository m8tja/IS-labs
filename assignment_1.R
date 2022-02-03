#install.packages("GA")
#install.packages("combinat")
library(GA)
library(combinat)

exp <- c("10", "25", "100", "5", "3", "+" ,"-", "/", "*")
target <- 2512

#exp <- c("10", "3", "7", "2", "+" ,"-", "/", "*")
#target <- 305

#exp <- c("10", "70", "7", "2", "15", "3", "12","+" ,"-", "/", "*")
#target <- 1444

#exp <- c(c(1:10),"+" ,"-", "/", "*")
#target <- 3628800

expSize <- length(exp)
diffOperators <- 4
numbNumbers <- expSize - diffOperators
numbOperators <- numbNumbers - 1
equationLength <- (numbNumbers * 2) - 1


f <- function(x) {
  
  a <- exp[x]
  
  str = paste(a, collapse='')
  y <- eval(parse(text=str))
  
  fit <- 1 / (1 + abs(y - target))
  
  fit
} 


myInitPopulation <- function(object) {
  
  p <- matrix(nrow = object@popSize, ncol = object@upper)
  
  for (i in 1:nrow(p)) {
    
    t <- generateIndividual()
    
    p[i,] <- as.vector(t)
  }
  
  return(p)
}


generateIndividual <- function() {
  
  numbers <- c(sample(1:numbNumbers, numbNumbers))
  opIndex <- expSize - diffOperators + 1
  operators <- c(sample(opIndex:expSize, numbOperators, replace = T))
  
  t <- integer(equationLength)
  
  ix1 = 1
  ix2 = 1
  
  for (j in 1:equationLength) {
    
    if (j %% 2 != 0) {
      t[j] = numbers[ix1]
      ix1 = ix1 + 1
    }
    else {
      t[j] = operators[ix2]
      ix2 = ix2 + 1
    }
    
  }
  
  return(t)
}


# Mutation swap operands

myMutation_swapOperands <- function(object, parent) {
  
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  
  odd <- seq(1,n, by=2)
  
  m <- sample(odd, size = 2)
  
  mutate[m[1]] <- parent[m[2]]
  mutate[m[2]] <- parent[m[1]]
  
  #print(mutate)
  
  return(mutate)
}


# Mutation swap operators

myMutation_swapOperators <- function(object, parent) {
  
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  
  even <- seq(2,n, by=2)
  
  m <- sample(even, size = 2)
  
  mutate[m[1]] <- parent[m[2]]
  mutate[m[2]] <- parent[m[1]]
  
  #print(mutate)
  
  return(mutate)
}


myMutation_replaceSubTree <- function(object, parent) {
  
  mutate <- parent <- as.vector(object@population[parent,])
  
  n <- length(parent)
  
  myIndex <- c(3, 5, 7)
  ix <- sample(1:3, 1, replace=F)
  randValue <- myIndex[ix]
  
  size <- randValue
  
  sIx <- seq(1,n - 3, by=2)
  startIx <- sample(sIx, size = 1)
  
  if (size + startIx > equationLength) {
    size = equationLength - startIx
    
    if (size %% 2 == 0) {
      size <- size - 1
    }
  }
  
  subTree <- generateSubTree(size)
  
  ixST = 1
  
  for (i in startIx:startIx + size) {
    mutate[i] <- subTree[ixST]
    ixST <- ixST + 1
  }
  
  return(mutate)
}


generateSubTree <- function(size) {
  
  oSize <- floor(size / 2)
  nSize <- oSize + 1
  #opIndex <- expSize - numbOperators + 1 #
  opIndex <- expSize - diffOperators + 1
  
  numb <- sample(1:numbNumbers, nSize, replace=F)
  operator <- sample(opIndex:expSize, oSize, replace=T)
  
  equation <- integer(size)
  
  ixN = 1
  ixO = 1
  
  for (i in 1:size) {
    
    if (i %% 2 != 0) {
      
      equation[i] <- numb[ixN]
      ixN <- ixN + 1
    }
    else {
      
      equation[i] <- operator[ixO]
      ixO <- ixO + 1
    }
    
  }
  
  return(equation)
}


sp_crossover <- function(object, parents) {
  
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  
  if(crossOverPoint == 0) { 
    
    children[1:2,] <- parents[2:1,]
    fitnessChildren[1:2] <- fitness[2:1] 
  }
  else if(crossOverPoint == n) { 
    
    children <- parents
    fitnessChildren <- fitness 
  }
  else {
    
    overPoint <- 0
    for(i in 1:n) {
      
      if(i >= crossOverPoint) {
        overPoint <- 1
      }
      
      if(!(i %% 2) || !overPoint) {
        children[1,i]=parents[1,i]
        children[2,i]=parents[2,i]
      }
      else {
        children[1,i]=parents[2,i]
        children[2,i]=parents[1,i]
      }
    }
  }
  
  out <- list(children = children, fitness = fitnessChildren)
  
  return(out)
}

mix_crossover <- function(object, parents) {
  
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  parent1 <- parents[1,]
  parent2 <- parents[2,]
  
  child1 <- integer(equationLength)
  child2 <- integer(equationLength)
  
  for (i in 1:equationLength) {
    
    if (i %% 2 != 0) {
      
      child1[i] <- parent1[i]
      child2[i] <- parent2[i]
    }
    else if (i %% 2 == 0) {
      
      child1[i] <- parent2[i]
      child2[i] <- parent1[i]
    }
    
  }
  
  children <- matrix(c(child1,child2), 2, byrow=TRUE)
  out <- list(children = children, fitness = rep(NA,2))
  
  return(out)
}

uniform_crossover <- function(object, parents) {
  
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  parent1 <- parents[1,]
  parent2 <- parents[2,]
  
  child1 <- parent1
  child2 <- parent2
  
  for (i in 1:equationLength) {
    
    if (i %% 2 == 0) {
      
      flip1 <- sample(0:1, 1)
      flip2 <- sample(0:1, 1)
      
      if (flip1 == 1) {
        child1[i] <- parent2[i]
      }
      
      if (flip2 == 1) {
        child2[i] <- parent1[i]
      }
    }
  }
  
  children <- matrix(c(child1,child2), 2, byrow=TRUE)
  out <- list(children = children, fitness = rep(NA,2))
  
  return(out)
}

twoPoint_crossover <- function(object, parents) {
  
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 2)
  
  cop1 = min(crossOverPoint)
  cop2 = max(crossOverPoint)
  
  for(i in 1:n) {
    
    if(!(i %% 2) && i >= cop1 && i <= cop2) {
      
      children[1,i]=parents[2,i]
      children[2,i]=parents[1,i]
    } 
    else {
      
      children[1,i]=parents[1,i]
      children[2,i]=parents[2,i]
    }
  }
  
  out <- list(children = children, fitness = fitnessChildren)
  
  return(out)
}


randomSearch <- function() {
  
  fit <- 0
  startTime_rs <- proc.time()
  
  while (fit != 1) {
    
    individual <- generateIndividual()
    fit <- f(individual)
  }
  
  time_rs <- proc.time() - startTime_rs
  
}


research_f <- function(){
  time_brute <- 0
  time_ga <- 0
  
  for(it in 1:10){
    
    
    time_brute <- time_brute + randomSearch()
    
    
    startTime_ga <- proc.time()
    
    GA <- ga(
      type = "permutation", 
      fitness = f, 
      lower = 1, 
      upper = numbNumbers * 2 - 1,
      popSize = 1000, 
      maxiter = 400, 
      population = myInitPopulation, 
      mutation = myMutation_swapOperators,
      #mutation = myMutation_swapOperands,
      #mutation = myMutation_replaceSubTree,
      pmutation = 0.1, 
      pcrossover = 0.8,
      #crossover = mix_crossover,
      crossover = twoPoint_crossover,
      #crossover = sp_crossover,
      #crossover = uniform_crossover,
      elitism = TRUE,
      maxFitness=1,
      keepBest = TRUE)
    
    time_ga <- time_ga + (proc.time() - startTime_ga)
  }
  plot(GA)
  print(time_brute[3]/10)
  print(time_ga[3]/10)
}

research_f()


startTime_ga <- proc.time()

GA <- ga(
  type = "permutation", 
  fitness = f, 
  lower = 1, 
  upper = numbNumbers * 2 - 1,
  popSize = 10, 
  maxiter = 200, 
  population = myInitPopulation,
  #mutation = myMutation_swapOperands,
  mutation = myMutation_swapOperators,
  #mutation = myMutation_replaceSubTree,
  pmutation = 0.05, 
  pcrossover = 0.8,
  #crossover = sp_crossover,
  #crossover = mix_crossover,
  crossover = twoPoint_crossover,
  #crossover = uniform_crossover,
  elitism = 5,
  keepBest = TRUE)

time_ga <- proc.time() - startTime_ga
print(time_ga)

plot(GA)

exp[GA@bestSol[[length(GA@summary[,1])]][1,]]
summary(GA)
