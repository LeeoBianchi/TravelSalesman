#Simulate positions for n=8 sities
pos <- read.table("optimaltransport.ascii",header=F)
p = nrow(pos)
set.seed(345)
par(mfrow=c(1,1))
plot(pos)
dev.copy2pdf(file="../doc/example_trav_sale.pdf")

#Calculate pairwise distances between cities
d = as.matrix((dist(pos,diag=TRUE,upper=TRUE)))
#Convert to vector in order to access many components at a time
d = as.vector(d)

#Perform neighbor search, simulated annealing
#Random inizialization
#We implement the fact that the distribution must start and finish at home town
theta = c(1, sample(2:p,p-1), 1)

#Convert sequential pairs into index in the d-vector
ind = (theta[-p]-1)*p+theta[-1]
#Calculate total distance of order
V = sum(d[ind])
Vseq = V
Numit=50000
for(i in 1:Numit)
{
  tau = 100/i
  #tau = 1/log(i+1)
  #tau <- (0.8 + i/Numit/10)*tau
  #we never change the position of index=1 (home)
  ind2 = sample(2:p,2,replace=F)
  theta2 = theta
  theta2[ind2[1]] = theta[ind2[2]]
  theta2[ind2[2]] = theta[ind2[1]]
  ind2 = (theta2[-p]-1)*p+theta2[-1]
  V2 = sum(d[ind2])
  prob = exp((V-V2)/tau)
  u = runif(1)
  if(u<prob)
  {
    theta = theta2
    V = V2
  }
  Vseq = c(Vseq,V)
}
#par(mfrow=c(2,1),mar=c(0.5,1,0.5,1))
plot(pos)
lines(pos[theta,1],pos[theta,2])
points(pos[1,1],pos[1,2], type = 'o', col = 'red')
#dev.copy2pdf(file="../doc/example_trav_sale_SA.pdf")
plot.ts(Vseq)
show(min(Vseq))


