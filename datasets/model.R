# load packages
library(DT)
library(readr)
library(rethinking)
library(dplyr)

# read files
df = read.csv(file.path("datasets", "df.csv"))

# trim data frame
df = df[ , c("log_GNI","SIGI_s","MENA")]

# define model fit by map
m1.2 <- map(
  alist(
    log_GNI ~ dnorm(mu, sigma),
    mu <- a + bS*SIGI_s,
    a ~ dnorm(8,100),
    bS ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data <- df)


# define model fit by map
m1.4 <- map(
  alist(
    log_GNI ~ dnorm(mu, sigma),
    mu <- a + bS*SIGI_s + bSM*SIGI_s*MENA + bM*MENA,
    a ~ dnorm(8,100),
    bS ~ dnorm(0,1),
    bM ~ dnorm(0,1),
    bSM ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data <- df)
