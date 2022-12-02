\name{GramSchmidt}
\alias{GramSchmidt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Find orthogonal and orthonormal basis of a set of vectors of inner space
}
\usage{
GramSchmidt(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is the matrix by putiing linearly independent set of vectors x1,x2,...xm together
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix represents the orthogonal  basis and another matrix represents and the orthonormal basis for the subspace of V

}

\author{
Shuo
}

\examples{
x<-rbind(c(1,0,1,0),c(1,1,1,1),c(-1,2,0,1))
 GramSchmidt(x)
            "Orthogonal Basis"

           1.0  0.0  1.0  0.0
           0.0  1.0  0.0  1.0
          -0.5  0.5  0.5 -0.5
           "Orthonormal Basis"

  0.7071068 0.0000000 0.7071068  0.0000000
  0.0000000 0.7071068 0.0000000  0.7071068
 -0.5000000 0.5000000 0.5000000 -0.5000000
}

