# Load packages
library(readr)
library(shiny)

df = read.csv("df_shiny.csv")

m1.2 <- map(
  alist(
    log_GNI ~ dnorm(mu, sigma),
    mu <- a + bS*SIGI_s,
    a ~ dnorm(8,100),
    bS ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data <- df)

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

# Define a server for the Shiny app
function(input, output){

  # Fill in the spot we created for a plot
  output$plot <- renderPlot({
    if(input$select ==  "Without Interaction"){
      par(mfrow=c(1,2))

      S.seq <- seq( from=-1.5, to=3, length.out=30)

      mu <- link(m1.2, data=data.frame(SIGI_s=S.seq))
      mu.mean <- apply(mu, 2, mean)
      mu.PI <- apply(mu, 2, PI, prob=0.95)

      plot(log_GNI ~ SIGI_s, data=df,
           col=rangi2, ylab="log GNI per capita",
           xlab="Gender Equality (reversed SIGI_s)", xlim=c(3, -1.5))
      mtext("All nations", 3)
      lines(S.seq, mu.mean, col=rangi2)
      shade(mu.PI, S.seq, col=col.alpha(rangi2, 0.3))
    }

    else{
      par(mfrow=c(1,2))

      S.seq <- seq( from=-1.5, to=3, length.out=30)

      # compute counterfactual mean weight (mu)
      mu.NotMENA <- link(m1.4, data=data.frame(MENA=0, SIGI_s=S.seq))
      mu.NotMENA.mean <- apply(mu.NotMENA, 2, mean)
      mu.NotMENA.PI <- apply(mu.NotMENA, 2, PI, prob=0.95)

      mu.MENA <- link(m1.4, data=data.frame(MENA=1, SIGI_s=S.seq))
      mu.MENA.mean <- apply(mu.MENA, 2, mean)
      mu.MENA.PI <- apply(mu.MENA, 2, PI, prob=0.95)

      # plot non-MENA nations with regression
      d.M0 <- df[df$MENA==0,]
      plot(log_GNI ~ SIGI_s, data=d.M0,
           col=rangi2, ylab="log GNI per capita",
           xlab="Gender Equality (reversed SIGI_s)", xlim=c(3, -1.5))
      mtext("Non-MENA nations", 3)
      lines(S.seq, mu.NotMENA.mean, col=rangi2)
      shade(mu.NotMENA.PI, S.seq, col=col.alpha(rangi2, 0.3))

      # plot MENA nations with regression
      d.M1 <- df[df$MENA==1,]
      plot(log_GNI ~ SIGI_s, data=d.M1,
           col="darkgreen", ylab="log GNI per capita",
           xlab="Gender Equality (reversed SIGI_s)", xlim=c(3, -1.5))
      mtext("MENA nations", 3)
      lines(S.seq, mu.MENA.mean, col="darkgreen")
      shade(mu.MENA.PI, S.seq, col=col.alpha("darkgreen", 0.3))
    }

  })
}
