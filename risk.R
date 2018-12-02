library(ggplot2)
library(tidyr)
print.risk <- function(x) print(x$win.prob)
plot.risk <- function(x) {
  x$surv.prob %>%
    ggplot(aes(x=Var1, y=Freq, fill=Var2)) +
    geom_bar(stat= "identity")+
    ggtitle(x$win.prob)}



risk <- function(attacker, defender, attacker.bonus=0, defender.bonus=0, n=10000, defender.limit=2)
{
  attacker <- attacker - 1
  result <- list(survivors = numeric(n),
                 winner = numeric(n))
  class(result) <- "risk"
  
  # attacker.win <- 0
  # defender.win <- 0
  for(i in 1:n){
    attacker.tmp <- attacker
    defender.tmp <- defender
    while(attacker.tmp > 0 & defender.tmp > 0){
    attacker.dice <- sort(sample(1:6, min(3, attacker.tmp), replace = T), decreasing = T)[1:2]
    defender.dice <- sort( sample(1:6, min(attacker.tmp, defender.limit, defender.tmp), replace = T), decreasing = T)[1:2]
    attacker.dice[1] <- attacker.dice[1]+attacker.bonus
    defender.dice[1] <- defender.dice[1]+defender.bonus
    attacker.tmp <- attacker.tmp - sum(attacker.dice <= defender.dice, na.rm = T)
    defender.tmp <- defender.tmp - sum(attacker.dice > defender.dice, na.rm = T)
    }
    result$survivors[i] <- ifelse(attacker.tmp==0,
                                  -defender.tmp,
                                  attacker.tmp)
    
    result$winner[i] <- ifelse(attacker.tmp==0,
                                  "Defender",
                                  "Attacker")
    
    # ifelse(attacker.tmp == 0,
    #        defender.win <- defender.win+1,
    #        attacker.win <- attacker.win+1)
  }
  result$win.prob <- table(result$winner)/n
  result$surv.prob <- as.data.frame(table(result$survivors, result$winner)/n)
  return(result)
# return(data.frame(win = attacker.win/10000,lose = defender.win/n))
}

t <- risk(5, 3,0,1, n=10000, defender.limit = 2)
plot(t)
t$surv.prob
t
