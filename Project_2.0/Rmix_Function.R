# Funkcja rmix
rmix <- function(n, family_1, par_1 , family_2, par_2, p){
  
  index <- sample(c(1,2), n, prob = c(p, 1-p), replace = T)
  
  #ilość liczb, które będą pochodzić z rozkładu typu family_1
  sum1 <- length(which(index == 1))
  #ilość liczb, które będą pochodzić z rozkładu typu family_2
  sum2 <- length(which(index == 2))
  
  # norm -> rozkłąd normalny  t -> rozkład t-Studenta   binom -> rozkład dwumianowy   poisson -> rozkład Poissona 
  
  if(family_1 == "norm"){ 
    sample_1 <- rnorm(sum1, mean = par_1[1], sd = par_1[2])
  } else if(family_1 == "t"){
    sample_1 <- rt(sum1, dt = par_1[1])
  } else if(family_1 == "binom"){
    sample_1 <- rbinom(sum1, size = par_2[1], prob = par_2[2])
  } else if(family_1 == "poisson"){
    sample_1 <- rpois(sum1, lambda = par_1[1])
  }
  
  if(family_2 == "norm"){
    sample_2 <- rnorm(sum2, mean = par_2[1], sd = par_2[2])
  } else if(family_2 == "t"){
    sample_2 <- rt(sum2, df = par_2[1])
  } else if(family_2 == "binom"){
    sample_2 <- rbinom(sum2, size = par_2[1], prob = par_2[2])
  } else if(family_2 == "poisson"){
    sample_2 <- rpois(sum2, lambda = par_2[1]) 
  }
  c(sample_1, sample_2)
}