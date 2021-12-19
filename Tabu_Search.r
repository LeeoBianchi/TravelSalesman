#Load positions
pos <- read.table("optimalTransport.ascii",header=F)
p = nrow(pos)
par(mfrow=c(1,1))
plot(pos)

set.seed(3453443)
#Calculate pairwise distances between cities
d = as.matrix((dist(pos,diag=TRUE,upper=TRUE)))
#Convert to vector in order to access many components at a time
d = as.vector(d)

#Random order of visits as initialization
#We implement the fact that the distribution must start and finish at home town
theta = c(1, sample(2:p,p-1), 1)
#Convert sequential pairs into index of the d-vector
ind = (theta[-p]-1)*p+theta[-1]
#Calculate total distance of order
V = sum(d[ind])
Vopt = V
Vseq = V

#Make table of neighbor search 
#first two columns are indices to be swapped
#We exclude the hometown from it, starting from 2 instead of 1
num = (p-1)*(p-2)/2
searchtab = matrix(0,nrow=num,ncol=2)
ind = 1
for(i1 in 2:(p-1))
for(i2 in (i1+1):p)
{
  searchtab[ind,1:2] = c(i1,i2)
  ind = ind+1
}
searchtab
#Perform neighbor search, changing best two components
more = TRUE
tabu = NULL
H = NULL
tau = 5
#while(more)
for(it in 1:10000)
{
  V2opt = V+1000  #Just to get some initial value to beat
  i1opt = NA
  for(i in 1:num)
  {
    if(is.na(pmatch(i,H)))
    {
      #Find indices to swap
     i1 = searchtab[i,1]
     i2 = searchtab[i,2]
     #Swap components, put into theta2
     theta2 = theta
     theta2[i1] = theta[i2]
     theta2[i2] = theta[i1]
     #Calculate value for new configuration
     ind2 = (theta2[-p]-1)*p+theta2[-1]  
     V2 = sum(d[ind2])
     #If best so far, store it
     if(V2<V2opt)
     {
       V2opt = V2      
       iopt = i
       i1opt = i1
       i2opt = i2
     }
    }
  }
  #Change to best configuration found
  theta2 = theta
  theta2[i1opt] = theta[i2opt]
  theta2[i2opt] = theta[i1opt]
  theta = theta2
  V = V2opt
  Vseq = c(Vseq,V)
  #Include the swap in TABU table
  H = c(H,iopt)
  #If table is too large, remove first element (oldest swap)
  if(length(H)>tau)
    H = H[-1]
  #Check if better than best so far
  if(V < Vopt)
  {
    theta.opt = theta
    Vopt = V
  }
}
#par(mfrow=c(2,1),mar=c(0.5,1,0.5,1))
plot(pos)
lines(pos[theta.opt,1],pos[theta.opt,2])
points(pos[1,1],pos[1,2], type = 'o', col = 'red')
dev.copy2pdf(file="../doc/travel_sale_opt1.pdf")
plot.ts(Vseq)
show(min(Vseq))
