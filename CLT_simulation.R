set.seed(123)

M <- 5000  #Change to 10000 for better results
N <- 200

a <- -5; b <- -a
unif_var <- (b - a)^2/12

uniform <- lapply(1:N, function(n){
  muestras <- matrix(runif(n*M, min=a, max=b), ncol = M, nrow = n)
  medias_muestrales <- apply(muestras, MARGIN = 2, mean)
  medias_muestrales_normalizadas <- (medias_muestrales)*sqrt(n)/(sqrt(unif_var))
})

lambda <- 1/16
sigma_exp <- 1/lambda^2

exponential <- lapply(1:N, function(n){
  muestras <- matrix(rexp(n*M, rate = lambda), ncol = M, nrow = n)
  medias_muestrales <- apply(muestras, MARGIN = 2, mean)
  medias_muestrales_normalizadas <- (medias_muestrales - 1/lambda)*sqrt(n)/(sqrt(sigma_exp))
})

uniform <- as.data.frame(uniform)
exponential <- as.data.frame(exponential)

colnames(uniform) <- 1:N
colnames(exponential) <- 1:N

exponential$distribution <- "Exponential"
uniform$distribution <- "Uniform"

data <- rbind(exponential, uniform) 
write.csv(data, "CLT_simulation_data.csv", row.names = FALSE)