
library(shiny)
library(VGAM)


# discrete distribution functions - probability density functions
rbern <- function(n=n.default,bern.prob=0.5){ rbinom(n=n,size=1,prob=bern.prob) }
rbinom2 <- function(n=n.default,binom.size=10,binom.prob=0.5){ rbinom(n,size=binom.size,prob=binom.prob) }
drunif <- function(n=n.default,drunif.min=0,drunif.max=100,drunif.step=1){ sample(seq(drunif.min,drunif.max,by=drunif.step),size=n,rep=T) }
rgeom2 <- function(n=n.default,geom.prob=0.5){ rgeom(n,prob=geom.prob) }
rhyper2 <- function(n=n.default,hyper.M=10,hyper.N=20,hyper.K=10){ rhyper(nn=n,m=hyper.M,n=hyper.N-hyper.M,k=hyper.K) }
rnbinom2 <- function(n=n.default,nbin.size=10,nbin.prob=0.5){ rnbinom(n,size=nbin.size,prob=nbin.prob) }
rpois2 <- function(n=n.default,poi.lambda=10){ rpois(n,poi.lambda) }

# continuous distributions functions - probability density functions
rbeta2 <- function(n=n.default,beta.shape1=2,beta.shape2=2){ rbeta(n,shape1=beta.shape1,shape2=beta.shape2) }
rcauchy2 <- function(n=n.default,cau.location=0,cau.scale=1){ rcauchy(n,location=cau.location,scale=cau.scale) }
rchisq2 <- function(n=n.default,chisq.df=1){ rchisq(n,df=chisq.df) }
rexp2 <- function(n=n.default,exp.rate=1){ rexp(n=n,rate=exp.rate) }
rf2 <- function(n=n.default,F.df1=1,F.df2=15){ rf(n,df1=F.df1,df2=F.df2) }
rgamma2 <- function(n=n.default,gam.shape=1,gam.rate=1){ rgamma(n,shape=gam.shape,rate=gam.rate) }
rlaplace2 <- function(n=n.default,lap.location=0,lap.scale=1){ rlaplace(n,location=lap.location,scale=lap.scale) }
rlogis2 <- function(n=n.default,logi.location=0,logi.scale=1){ rlogis(n,location=logi.location,scale=logi.scale) }
rpareto2 <- function(n=n.default,pareto.location=1,pareto.shape=3){ rpareto(n,location=pareto.location,shape=pareto.shape) }
rweibull2 <- function(n=n.default,weib.shape=1,weib.scale=1){ rweibull(n,shape=weib.shape,scale=weib.scale) }
rt2 <- function(n=n.default,t.df=15){ rt(n=n,df=t.df) }



# Discrete distribution plotmath expressions:
expr.bern <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~p^x*(1-p)^{1-x})
                                     ~~~~displaystyle(atop(x~"="~list(0,1), paste(0<=p)<=1))
)))

expr.bin <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~bgroup("(",atop(n,x),")")~p^x*(1-p)^{n-x})
                                    ~~~~displaystyle(atop(x~"="~list(0,1,...,n), paste(0<=p)<=1))
)))

expr.dunif <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~frac(1,N))
                                      ~~~~displaystyle(x~"="~list(1,2,...,N))
)))

expr.geom <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~p*(1-p)^x)
                                     ~~~~displaystyle(atop(x~"="~list(1,2,...), paste(0<=p)<=1))
)))

expr.hgeom <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~frac(bgroup("(",atop(M,x),")")~bgroup("(",atop(N-M,K-x),")"),bgroup("(",atop(N,K),")")))
                                      ~~~~displaystyle(list(x~"="~list(0,1,...,K), atop(paste(M-(N-K)<=x)<=M, list(N,M,K)>=0)))
)))

expr.nbin <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~frac(Gamma(x+n),Gamma(n)*x*"!")~p^r*(1-p)^x)
                                     ~~~~displaystyle(atop(x~"="~list(0,1,...), paste(0<=p)<=1))
)))

expr.poi <- expression(italic(paste(displaystyle(P(X~"="~x)~"="~frac(e^{-lambda}*lambda^x,x*"!"))
                                    ~~~~displaystyle(atop(x~"="~list(0,1,...), paste(0<=lambda)<infinity))
)))


# Continuous distribution plotmath expressions:
expr.beta <- expression(italic(paste(displaystyle(f(x)~"="~frac(Gamma(alpha+beta),Gamma(alpha)*Gamma(beta))*x^{alpha-1}*(1-x)^{beta-1})
                    ~~~~displaystyle(list(paste(0<=x) <=1, atop(paste(0<alpha) <infinity, paste(0<beta) <infinity)))
                    )))
 
expr.cauchy <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,pi*sigma)~frac(1,1+bgroup("(",frac(x-theta,sigma),")")^2))
                    ~~~~displaystyle(list(paste(-infinity<x) <infinity, atop(paste(-infinity<theta) <infinity, sigma > 0)))
                    )))
 
expr.chisq <- expression(italic(paste(frac(1,2^{frac(nu,2)}*Gamma~bgroup("(",frac(nu,2),")"))*x^{frac(nu,2)-1}*e^{-frac(x,2)}
                    ~~~~displaystyle(atop(paste(0<=x) <infinity, nu~"="~list(1,2,...)))
                    )))
 
expr.exp <- expression(italic(paste(displaystyle(f(x)~"="~lambda*e^{-lambda*x})
                    ~~~~displaystyle(atop(paste(0<=x) <infinity,lambda>0))
                    )))
 
expr.F <- expression(italic(paste(displaystyle(f(x)~"="~frac(Gamma~bgroup("(",frac(nu[1]+nu[2],2),")"),Gamma~bgroup("(",frac(nu[1],2),")")~Gamma~bgroup("(",frac(nu[2],2),")"))
                    ~bgroup("(",frac(nu[1],nu[2]),")")^{frac(nu[1],2)}~frac(x^{frac(nu[1],2)-1},bgroup("(",1+frac(d[1],d[2])*x,")")^{frac(d[1]+d[2],2)}))
                    ~~~~displaystyle(atop(paste(0<=x) <infinity,list(d[1],d[2])~"="~list(1,2,...)))
                    )))
 
expr.gam <- expression(italic(paste(displaystyle(f(x)~"="~frac(beta^alpha,Gamma(alpha))*x^{alpha-1}*e^{-beta*x})
                    ~~~~displaystyle(list(paste(0<x) <infinity, atop(paste(0<alpha) <infinity, paste(0<beta) <infinity)))
                    )))
 
expr.lap <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,2*sigma)~e^{-frac(abs(x-mu),sigma)})
                    ~~~~displaystyle(list(paste(-infinity<x) <infinity, atop(paste(-infinity<mu) <infinity, sigma > 0)))
                    )))
 
expr.logi <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,beta)~frac(e^{-frac(x-mu,beta)},bgroup("[",1+e^{-frac(x-mu,beta)},"]")^2))
                    ~~~~displaystyle(list(paste(-infinity<x) <infinity, atop(paste(-infinity<mu) <infinity, beta > 0)))
                    )))
 
expr.lognorm <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,x*sigma*sqrt(2*pi))~e^{-frac((log(x)-mu)^2,2*sigma^2)})
                    ~~~~displaystyle(list(paste(0<x) <infinity, atop(paste(-infinity<log(mu)) <infinity, paste(0<sigma^scriptscriptstyle("2")) <infinity)))
                    )))
 
expr.norm <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,sqrt(2*pi*sigma^scriptscriptstyle("2")))~e^{frac(-1,2*sigma^{scriptscriptstyle("2")})*(x-mu)^scriptscriptstyle("2")})
                    ~~~~displaystyle(list(paste(-infinity<x) <infinity, atop(paste(-infinity<mu) <infinity, paste(0<sigma^scriptscriptstyle("2")) <infinity)))
                    )))
 
expr.pareto <- expression(italic(paste(displaystyle(f(x)~"="~frac(beta*alpha^beta,x^{beta+1}))
                    ~~~~displaystyle(atop(paste(alpha<x) <infinity, list(alpha,beta) > 0))
                    )))
 
expr.t <- expression(italic(paste(displaystyle(f(x)~"="~frac(Gamma~bgroup("(",frac(nu+1,2),")"),sqrt(nu*pi)~Gamma~bgroup("(",frac(nu,2),")"))~bgroup("(",1+frac(x^2,nu),")")^{-frac(nu+1,2)})
                    ~~~~displaystyle(atop(paste(-infinity<x) <infinity, nu > 0))
                    )))
 
expr.unif <- expression(italic(paste(displaystyle(f(x)~"="~frac(1,b-a)
                    ~~~~displaystyle(paste(-infinity<paste(a<=paste(x<=b))) <infinity))
                    )))
 
expr.weib <- expression(italic(paste(displaystyle(f(x)~"="~frac(k,lambda)~bgroup("(",frac(x,lambda),")")^{k-1}*e^(-x/lambda)^k)
                    ~~~~displaystyle(atop(paste(0<=x) <infinity, list(k,lambda) > 0))
                    )))
 
########################################################################################################

# server code starts here
 
shinyServer(function(input,output){
 
    output$distName <- renderUI({
      
      # discrete
      if(input$dist.type=="Discrete"){
        radioButtons("dist","Distribution:",
                     selected="bin",
                     list("Binomial"="bin",
                          "Bernoulli"="bern",
                          "Negative Binomial"="nbin",
                          "Poisson"="poi",
                          "Discrete Uniform"="dunif",
                          "Geometric"="geom",
                          "Hypergeometric"="hgeom"                      
                          ) 
                     )
        
      } else if(input$dist.type=="Continuous"){ # continuous
        radioButtons("dist","Distribution:",
                     selected="norm",
                     list("Normal"="norm",
                          "Log-Normal"="lognorm",
                          "Exponential"="exp",
                          "Uniform"="unif",
                          "Chi-squared"="chisq",
                          "Student's t"="t",
                          "F"="F",
                          "Logistic"="logi",
                          "Beta"="beta",
                          "Cauchy"="cauchy",                          
                          "Gamma"="gam",
                          "Weibull"="weib",
                          "Laplace"="lap",                   
                          "Pareto"="pareto"
                          )
        )
      }
    })
    
    
    dat <- reactive({
      
      dist <- switch(EXPR = input$dist,
                     
                     # discrete
                     bin=rbinom2,
                     bern=rbern, 
                     nbin=rnbinom2, 
                     poi=rpois2,
                     dunif=drunif,
                     geom=rgeom2, 
                     hgeom=rhyper2, 
                     
                     # continuous
                     norm=rnorm,
                     lognorm=rlnorm,
                     exp=rexp2,
                     unif=runif,
                     chisq=rchisq2,
                     t=rt2,
                     F=rf2,
                     logi=rlogis2,
                     beta=rbeta2, 
                     cauchy=rcauchy2,    
                     gam=rgamma2, 
                     weib=rweibull2,
                     lap=rlaplace2, 
                     pareto=rpareto2   
      )
      
      
      def.args <- switch(input$dist,
                         
                         # discrete
                         bin=c(input$binom.size,input$binom.prob),
                         bern=c(input$bern.prob),     
                         nbin=c(input$nbin.size,input$nbin.prob),
                         poi=c(input$poi.lambda),
                         dunif=c(input$drunif.min,input$drunif.max,input$drunif.step),
                         geom=c(input$geom.prob),
                         hgeom=c(input$hyper.M,input$hyper.N,input$hyper.K),                    
                         
                         # continuous
                         norm=c(input$mean,input$sd),
                         lognorm=c(input$meanlog,input$sdlog),
                         exp=c(input$exp.rate),
                         unif=c(input$min,input$max),
                         chisq=c(input$chisq.df),
                         t=c(input$t.df),
                         F=c(input$F.df1,input$F.df2),
                         logi=c(input$logi.location,input$logi.scale),
                         beta=c(input$beta.shape1,input$beta.shape2),
                         cauchy=c(input$cau.location,input$cau.scale),             
                         gam=c(input$gam.shape,input$gam.rate),
                         weib=c(input$weib.shape,input$weib.scale),
                         lap=c(input$lap.location,input$lap.scale),                  
                         pareto=c(input$pareto.location,input$pareto.shape)                      
                         
      )
      
      f <- formals(dist)
      f <- f[names(f)!="nn" & names(f)!="n"]
      
      if(any(input$dist==c("dunif","hgeom"))){ 
        len <- min(length(f),4-1); f <- f[1:len]
      } else { 
        len <- min(length(f),3-1); f <- f[1:len] 
      }
      
      argList <- list(n=input$n)
      
      for(i in 1:len) argList[[names(f)[i]]] <- def.args[i]
      return(list(do.call(dist,argList),names(f)))
    
    })
    
    
    output$dist1 <- renderUI({
      
      if(length(input$dist)){
        
        lab <- switch(input$dist,
                      
                      # discrete
                      bin="Size:",
                      bern="Probability:",  
                      nbin="Number of successes:",
                      poi="Mean and Variance:",
                      dunif="Discrete sequence minimum:", 
                      geom="Probability:", 
                      hgeom="M:", 
                      
                      # continuous
                      norm="Mean:",
                      lognorm="Mean(log):",
                      exp="Rate:",
                      unif="Minimum:",
                      chisq="Degrees of freedom:",
                      t="Degrees of freedom:",
                      F="Numerator degrees of freedom:",
                      logi="Location:",
                      beta="Alpha:", 
                      cauchy="Location:",    
                      gam="Shape:", 
                      weib="Shape:",
                      lap="Location:",
                      pareto="Location:"  
        
        )
        
        
        ini <- switch(input$dist,
                      
                      # discrete
                      bin=10,
                      bern=0.5,  
                      nbin=10, 
                      poi=10,
                      dunif=0, 
                      geom=0.5, 
                      hgeom=10,
                      
                      # continuous
                      norm=0,
                      lognorm=0,
                      exp=1,
                      unif=0,
                      chisq=1,
                      t=15,
                      F=1,
                      logi=0,
                      beta=2, 
                      cauchy=0,    
                      gam=1, 
                      weib=1 ,
                      lap=0,    
                      pareto=1
                      
        )
        
        numericInput(dat()[[2]][1],lab,ini)
      
      }
      
    })
    
    
    output$dist2 <- renderUI({
      
      if(length(input$dist)){
        
        lab <- switch(input$dist,
                      
                      # discrete
                      bin="Probability:",
                      nbin="Probability:",
                      dunif="Discrete sequence maximum:", 
                      hgeom="N:",
                      
                      # continuous
                      norm="Standard deviation:",
                      lognorm="Standard deviation(log)",
                      unif="Maximum:",
                      F="Denominator degrees of freedom:",
                      logi="Scale:",
                      beta="Beta:",
                      cauchy="Scale:", 
                      gam="Rate:", 
                      weib="Scale:",
                      lap="Scale:", 
                      pareto="Shape:"
                      
        )
        
        ini <- switch(input$dist,
                      
                      # discrete
                      bin=0.5, 
                      nbin=0.5,
                      dunif=100, 
                      hgeom=20, 
                      
                      # continuous
                      norm=1,
                      lognorm=1,
                      unif=1,
                      F=15,
                      logi=1,
                      beta=2, 
                      cauchy=1,  
                      gam=1, 
                      weib=1,
                      lap=1,    
                      pareto=3
        )
        
        
        if(any(input$dist==c("bin","nbin","dunif","hgeom",
                             "norm","lognorm","unif","F",
                             "logi","beta","cauchy","gam",
                             "weib","lap","pareto"
                            )
               )
           ){
          numericInput(dat()[[2]][2],lab,ini)
        }
        
      }
      
    })
    
     
    output$dist3 <- renderUI({
      
      if(length(input$dist)){
        
        lab <- switch(input$dist,
                      dunif="Step size:", 
                      hgeom="K:"
        )
        
        ini <- switch(input$dist,
                      dunif=1, 
                      hgeom=5
        )
        
        if(any(input$dist==c("dunif","hgeom"))){
          numericInput(dat()[[2]][3],lab,ini)
        }
        
      }
    
    })
     
    
    output$sampDens <- renderUI({
      
        if(input$dist.type=="Continuous"){ 
          checkboxInput("density","Plot density curve",FALSE)
        }
        
    })
    
    
    output$BW <- renderUI({
      
        if(length(input$density)){
          
            if(input$density) {
              numericInput("bw","Bandwidth:",1)
            }
        }
        
    })
     
    
    doPlot <- function(margins){
      
        if(length(input$dist)){
          
            d <- dat()[[1]]
            dist <- input$dist
            n <- input$n
            expr <- get(paste("expr",dist,sep="."))
            par(mar=margins)
            
            if(input$dist.type=="Discrete"){
                barplot(as.numeric(table(d))/input$n,
                        names.arg=names(table(d)),
                        main=expr,
                        xlab="Observations",
                        ylab="Density",
                        col="skyblue",
                        cex.main=1.5,
                        cex.axis=1.3,
                        cex.lab=1.3
                )
                legend("topright", inset = 0.025, 
                       legend=bquote(atop(mu==.(round(mean(dat()[[1]]))),sigma==.(round(sd(dat()[[1]]))))), 
                       bty = "n", cex = 2, text.col = 'black', text.font = 2)
                
            }
            
            if(input$dist.type=="Continuous"){
                hist(d,
                     main=expr,
                     xlab="Observations",
                     ylab="Density",
                     col="skyblue",
                     cex.main=1.5,
                     cex.axis=1.3,
                     cex.lab=1.3,
                     prob=T
                )
                legend("topright", inset = 0.025, 
                       legend=bquote(atop(mu==.(round(mean(dat()[[1]]))),sigma==.(round(sd(dat()[[1]]))))), 
                       bty = "n", cex = 2, text.col = 'black', text.font = 2)
                if(length(input$density)){
                  if(input$density & length(input$bw)){
                    lines(density(d,adjust=input$bw),lwd=2)
                  }
                }
                  
            }
        }
    }
     
    
    output$plot <- renderPlot({
      doPlot(margins=c(4,4,10,1))
    },
    height=750, 
    width=1000
    )
     
    
    output$dlCurPlot <- downloadHandler(
      filename = 'Plot.pdf',
      content = function(file){
        pdf(file = file, width=11, height=8.5)
        doPlot(margins=c(6,6,10,2))
        dev.off()
      }
    )
    
    
    output$dldat <- downloadHandler(
      filename = function() { 
        paste(input$dist, '.csv', sep='') 
      },
      content = function(file) {
        write.csv(data.frame(x=dat()[[1]]), file)
      }
    )
    
    
    output$summary <- renderPrint({
      summary(dat()[[1]])
    })
    
    
    output$table <- renderTable({
      data.frame(x=dat()[[1]])
    })
    
})
