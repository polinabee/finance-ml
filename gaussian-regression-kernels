set.seed(1)
n <- 100
n.samples <- 5
colours <- c('#dadaeb','#bcbddc','#9e9ac8','#756bb1','#54278f')

X <- matrix(seq(-5,5, length = n), ncol=1)

get_sigma <- function(mk,x){
  sigma  <- matrix(0, length(x),length(x))
  for (i in 1:n) {
    for (j in 1:n) {
      sigma[i,j] <- mk(x[i],x[j])  
    }
  }
  return(sigma)
}


### Defining the kernels ###
se.kernel <- function(h, l) { 
  MyKer <- function(a,b) {h^2 * exp(-(a-b)^2 / l^2)}
  return(MyKer)
}

rq.kernel <- function(h,l,alph){
  MyKer <- function(a,b) {h^2 * (1 + ((a-b)^2)/(alph*l^2))^-alph}
  return(MyKer)
}

k3.kernel <- function(l){
  MyKer <- function(a,b) {2 * exp((-sin(pi*(a-b))^2)/3)/(2*(l^2))}
  return(MyKer)
}

k4.kernel <- function(l){
  MyKer <- function(a,b){2 * exp((-(a-b)^2)/(2*l^2))+ 1.5*a*b}
  return(MyKer)
}


plot_kernel <- function(h, lambda, X, ker,alpha='n/a'){
  for (a in 1:length(alpha)){
    for (s in 1:length(lambda)){
      if (ker == 'SE'){
        mk <- se.kernel(h,lambda[s]) 
      }else if (ker == 'RQ'){
        mk <- rq.kernel(h,lambda[s],alpha[a])
      }else if (ker =='K3'){
        mk <- k3.kernel(lambda[s])
      }else if (ker == 'K4'){
        mk <- k4.kernel(lambda[s])
      }else{
        print('no kernel supplied')
      }
      sigma.x <- get_sigma(mk,X)
      values <- matrix(0,n, n.samples)
      for (i in 1:n.samples) {
        values[,i] <- mvrnorm(1, rep(0, n), sigma.x)
        }
      values <- cbind(x=X,as.data.frame(values))
    values <- melt(values,id="x")

    colnames(values) <- c('x', 'Function','value')
    levels(values$Function) <- c('f1','f2','f3','f4','f5')

    fig <- ggplot2::ggplot(values,aes(x=x,y=value)) +
      geom_line(aes(group=Function,colour=Function)) +
      annotate("rect", alpha = 0.2) + scale_y_continuous(name="f(x)") + scale_color_manual(values = colours) +
      xlab("x") + ggtitle(paste(ker,'kernel, lambda =',lambda[s],', alpha = ',alpha[a])) +
      theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank())
    print(fig)

    }
  }
}

######## PLOTTING ######

plot_kernel(h=1, lambda=c(0.1, 1, 10),X=X, ker='SE')
plot_kernel(h=1, lambda=c(0.1, 1, 10),X=X, ker='RQ',alpha=c(1,5,10))
plot_kernel(h=1, lambda=c(0.1, 1, 10),X=X, ker='RQ',alpha=c(100000))
plot_kernel(h=1, lambda=c(0.1, 1.5, 5),X=X, ker='K3')
plot_kernel(h=1, lambda=c(0.1, 1.5, 5),X=X, ker='K4')
