GramSchmidt<-function(x){
  n<-nrow(x)
  v<-matrix(0,nrow(x),ncol(x))
  v[1,]<-x[1,]

  C<-matrix(0,nrow(x),ncol(x))
  for (i in 2:n){

    for (j in 1:(i-1)){
      C[i,]=C[i,]-sum(x[i,]*v[j,])/sum((v[j,])^2)*v[j,]

    }
    v[i,]=x[i,]+C[i,]
  }
  print("Orthogonal Basis")
  print(v)
  u<-matrix(0,nrow(x),ncol(x))
  for (i in 1:n){
    u[i,]<-v[i,]/sqrt(sum((v[i,])^2))


  }
  print("Orthonormal Basis")
  print(u)

}




