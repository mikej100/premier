# Result of match is dependent on recent home wins this season, layered onto the
# baseline success rate of the team.
#
library(Rlab)
library(dplyr)
library(ggplot2)
library(usethis)

use_test()
# Additional factor is const times wins in last n games

# 100 teams
n <- 1000

# baseline success rate
# choose a success rate distribution
beta_shape <- c(2,4)
ggplot( data= tibble(x=0:1), aes(x)) +
  stat_function(fun = dbeta, n=100, args=beta_shape)

base_win = rbeta(n, beta_shape[1], beta_shape[2])
ggplot(tibble(x=base_win), aes(x=x)) + geom_density() 


# recent wins
# Same win prob for all teams
rw <- rbinom(n, 3, 0.33)
ggplot(tibble(rw=rw), aes(x=rw)) + geom_histogram()

# use the base_win for the team.
# The result if very different.
recent <- 3
rw <- rbinom(n, recent, base_win)/ recent
ggplot(tibble(rw=rw), aes(x=rw)) + geom_histogram()


# what is the difference between Bernoulli and Binom distributions?
ggplot(tibble(x=rbinom(n,1, .33)), aes(x)) + geom_histogram()
ggplot(tibble(x=rbern(n, .33)), aes(x)) + geom_histogram()

# I think rbern(n, p) is same as rbinom(n, 1, p)




#model
mod_recent_wins <- function (base, recent, recent_wins, alpha) {
  base + (1-base) * recent_wins/recent * alpha
}
alpha = 0.3
wp = base_win + alpha * rw
y = rbinom(n, 1,wp )
ggplot( data= tibble(x=0:1), aes(x)) +
  stat_function(fun = dbeta, n=100, args=c(1, .5) )
ggplot(tibble(x=mi), aes(x=x)) + geom_histogram()
rbinom(n, 1, mi)
mi > 1

mod_recent_1 <- function(base, delta) {
  base + ((1-base) * delta)
}
