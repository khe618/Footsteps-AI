states <- array(c(rep(0,51), rep(0,51)), dim = c(51,51,7))
states[,,1] <- rep(-1,(51^2))
states[,,7] <- rep(1,(51^2))
#states[k,j,i] = [AI's score + 1, opponent's score + 1, ball position + 4]
initialize_values <- function(){
  temp <- array(c(rep(0,51), rep(0,51)), dim = c(51,51,7))
  temp[,,1] <- rep(-1,(51^2))
  temp[,,7] <- rep(1,(51^2))
  for (i in 2:6){
    for (j in 1:51){
      for (k in 1:51){
        sum <- 0
        for (m in 0:(k-1)){ #m is AI's move
          for (n in 0:(j-1)){ #n is opponent's move
            if (m > n){
              sum <- sum + states[k-m, j-n, i+1]
            }
            else if (m < n){
              sum <- sum + states[k-m, j-n, i-1]
            }
            else{
              sum <- sum + states[k-m, j-n, i]
            }
          }
        }
        temp[k,j,i] <- sum / (k * j)
      }
    }
  }
  return(temp)
}
optimize_values <- function(){
  temp <- array(c(rep(0,51), rep(0,51)), dim = c(51,51,7))
  temp[,,1] <- rep(-1,(51^2))
  temp[,,7] <- rep(1,(51^2))
  for (i in 2:6){
    for (j in 1:51){
      for (k in 1:51){
        ev <- c()
        for (m in 0:(k-1)){ #m is AI's move
          sum <- 0
          prbs_sum <- sum(prbs[[i]][[k]][[j]])
          for (n in 0:(j-1)){ #n is opponent's move
            if (m > n){
              sum <- sum + states[k-m, j-n, i+1] * prbs[[i]][[k]][[j]][n+1]
            }
            else if (m < n){
              sum <- sum + states[k-m, j-n, i-1] * prbs[[i]][[k]][[j]][n+1]
            }
            else{
              sum <- sum + states[k-m, j-n, i] * prbs[[i]][[k]][[j]][n+1]
            }
          }
          #ev <- c(ev, (sum / j))
          ev <- c(ev, (sum/ prbs_sum))
        }
        #temp[k,j,i] <- max(ev)
        if (max(ev) == 1 || max(ev) == 0){
          temp[k,j,i] <- max(ev)
        }
        else{
          temp[k,j,i] <- sum(ev * (1000 ^ ev) ) / sum(1000 ^ ev)
        }
      }
    }
  }
  return(temp)
}
prbs <- list(rep(list(0), 51))
prbs <- list(rep(prbs, 51))
prbs <- rep(prbs, 7)
#prbs[[i]][[j]][[k]] = prbs[[ball position + 4]][[AI score + 1]][[Opponent score + 1]] 
for(i in 1:7){
  for(j in 1:51){
    for(k in 1:51){
      prbs[[i]][[j]][[k]] <- rep(1,k)
    }
  }
}
update_prbs <- function(i,j,k,l){#change 
  prbs[[i+4]][[j+1]][[k+1]][l+1] <<- prbs[[i+4]][[j+1]][[k+1]][l+1] + 1
}
select_move <- function(k, j, i){
  k <- k + 1
  j <- j + 1
  i <- i + 4
  ev <- c()
  for (m in 0:(k-1)){ #m is AI's move
    sum <- 0
    prbs_sum <- sum(prbs[[i]][[k]][[j]])
    for (n in 0:(j-1)){ #n is opponent's move
      if (m > n){
        sum <- sum + states[k-m, j-n, i+1] * prbs[[i]][[k]][[j]][n+1]
      }
      else if (m < n){
        sum <- sum + states[k-m, j-n, i-1] * prbs[[i]][[k]][[j]][n+1]
      }
      else{
        sum <- sum + states[k-m, j-n, i] * prbs[[i]][[k]][[j]][n+1]
      }
    }
    #ev <- c(ev, (sum / j))
    ev <- c(ev, (sum/ prbs_sum))
  }
  if (max(ev) == 1 || max(ev) == 0){
    max_moves <- which(ev == 1)
    if (length(max_moves) > 1 && max_moves[1] == 1){
      return (max_moves[2] - 1)
    }
    return(which.max(ev) - 1)
  }
  move_prbs <- (1000 ^ ev) / sum(1000 ^ ev)
  cum_prbs <- c(0)
  for(i in 1:length(move_prbs)){
    cum_prbs <- c(cum_prbs, (move_prbs[i] + cum_prbs[length(cum_prbs)]))
  }
  cum_prbs <- cum_prbs[-1]
  #return(cum_prbs)
  random <- runif(1, 0, cum_prbs[length(cum_prbs)])
  for (i in 1:length(cum_prbs)){
    if (cum_prbs[i] > random){
      return(i - 1)
    }
  }
  return(NULL)
}
self_simulate <- function(){
  ball_position <- 0
  p1_score <- 50
  p2_score <- 50
  #p1 wins when ball_position = 3, p2 wins when ball_position = -3
  moves <- list()
  limit <- 0
  while (ball_position > -3 && ball_position < 3 && (p1_score + p2_score) > 0 && limit < 40){
    p1_move <- select_move(p1_score, p2_score, ball_position)
    p2_move <- select_move(p2_score, p1_score, ball_position * -1)
    update_prbs(ball_position, p1_score, p2_score, p2_move)
    update_prbs((ball_position * -1), p2_score, p1_score, p1_move)
    moves[[length(moves) + 1]] <- c(ball_position, p1_score, p2_score, p1_move, p2_move)
    if (p1_move > p2_move){
      ball_position <- ball_position + 1
    }
    else if (p1_move < p2_move){
      ball_position <- ball_position - 1
    }
    p1_score <- p1_score - p1_move
    p2_score <- p2_score - p2_move
    limit <- limit + 1
  }
  return(moves)
}
s <- 0
for (i in 1:7){
  for (j in 1:51){
    for (k in 1:51){
      s <- s + sum(prbs[[i]][[j]][[k]])
    }
  }
}
for (q in 1:5000){
  self_simulate()
  print(q)
}
reevaluate <- function(states){
  temp <- array(c(rep(0,51), rep(0,51)), dim = c(51,51,7))
  while (sum((states - temp) ^ 2) > 0.1){
    temp <- states
    states <- optimize_values()
    print(1)
  }
  return(states)
}
for (j in 1:30){
  for (i in 1:10000){
    self_simulate()
  }
  for (i in 1:3){
    states <- optimize_values()
  }
  print(j)
}  