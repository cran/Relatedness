\name{Relatedness-package}
\alias{Relatedness-package}
\alias{Relatedness}
\docType{package}
\title{
Maximum Likelihood Estimation of Relatedness using EM Algorithm
}
\description{
Inference of relatedness coefficients from a bi-allelic genotype matrix using a Maximum Likelihood estimation, Laporte, F., Charcosset, A. and Mary-Huard, T. (2017) <doi:10.1111/biom.12634>.
}
\details{

This package infers the relatedness distribution coefficients for all couple of individuals in a set from their genotype, provided in a bi-allelic genotype matrix. The main function is 'RelCoef' which infers those coefficients. The arguments of this function are a genotype matrix for individuals and an frequency matrix that displays the allelic frequency at each marker in each population. Alternatively, a parental genotype matrix and a crossing matrix can be used. Additional information about structure membership can also be provided via a ParentPop vector (for more details see the help of 'RelCoef'). The main matrix is writen with C language, make sure you can use this code.
}
\author{
Fabien Laporte, Tristan Mary-Huard

Maintainer: Fabien Laporte <fabien.laporte@inra.fr>
}

\keyword{ Relatedness }
\examples{
require('Relatedness')
data(Genotype)
data(Frequencies)
data(Cross)
RelCoef(IndividualGenom=matrix(0,ncol=0,nrow=0),ParentalLineGenom=Genotype,
Freq=Frequencies,Crossing=Cross,ParentPop=rep(1,20),Phased=TRUE,NbCores=2)
}
