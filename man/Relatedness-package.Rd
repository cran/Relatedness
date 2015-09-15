\name{Relatedness-package}
\alias{Relatedness-package}
\alias{Relatedness}
\docType{package}
\title{
An Algorithm to Infer Relatedness
}
\description{
Inference of relatedness coefficients from a bi-allelic genotype matrix using a Maximum Likelihood estimation.
}
\details{

This package infers relatedness coefficients from a bi-allelic genotype matrix. The main function is 'RelCoef' which infers those coefficients. The arguments of this function are a genotype matrix for individuals and an allele frequencies matrix for each markers and each population. Alternatively, a parental genotype matrix and a crossing matrix can be used. Information about structure can also be taken into account via a ParentPop vector (for more details see the help of 'RelCoef'). The main matrix is write with C language, make sure you can use this code.
}
\author{
Fabien Laporte

Maintainer: Fabien Laporte <fabien.laporte@moulon.inra.fr>
}

\keyword{ Relatedness }
\examples{
require('Relatedness')
data(Genotype)
data(Frequencies)
data(Cross)
RelCoef(IndividualGenom=matrix(0,ncol=0,nrow=0),ParentalLineGenom=Genotype,
Freq=Frequencies,Crossing=Cross,ParentPop=rep(1,20),NbCores=2)
}
