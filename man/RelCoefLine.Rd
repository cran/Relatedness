\name{RelCoefLine}
\alias{RelCoefLine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Relatedness Coefficients Estimation for Lines
%%  ~~function to do ... ~~
}
\description{This function performs Maximum Likelihood estimation for the relatedness coefficients between lines based on a bi-allelic genotype matrix.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RelCoefLine(LineGenom = matrix(0,nrow=0,ncol=0), 
	    Freq = matrix(0,nrow=0,ncol=0), 
	    LinePop = rep(0,0), 
	    Combination = NULL, 
	    NbInit = 5, Prec = 10^(-4), NbCores = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LineGenom}{Genotype matrix of lines. Each line is described by 1 column. Each row corresponds to a marker. Entries of matrix Genotype should be either 0 or 1.
%%     ~~Describe \code{LineGenom} here~~
}
  \item{Freq}{Allelic frequencies for allele 1 at each markers and for all populations (one column per population, one line per marker).
%%     ~~Describe \code{Freq} here~~
}
  \item{LinePop}{A vector of numbers corresponding to population membership for the parental lines.
%%     ~~Describe \code{LinePop} here~~
}
  \item{Combination}{If provided, a list of vector with two components. The jth vector is composed with the number of the first hybrid and the number of the second hybrid of the jth couple to study.
%%     ~~Describe \code{Combination} here~~
}
  \item{NbInit}{Number of initial values for the EM algorithm.
%%     ~~Describe \code{NbInit} here~~
}
  \item{Prec}{Convergence precision parameter for the EM algorithm.
%%     ~~Describe \code{Prec} here~~
}
  \item{NbCores}{Number of cores used by the algorithm (Default is the number of cores available minus one). Only available for linux and Mac.
%%     ~~Describe \code{NbCores} here~~
}
}
\value{By default, relatedness coefficients are displayed for all couple of genotyped lines. In that case the function returns a matrix corresponding to the Simple Relatedness Coefficient, i.e. the probability that each couple of lines are related. Element (i,j) of the matrix corresponds to the estimated relatedness coefficient for the couple of lines i and j. Alternatively, if a list of couples is specified with argument Combination, the function returns a list of coefficients (each coefficient corresponding to an relatedness coefficient). In that case element i of the list corresponds to the estimated relatedness coefficient of the ith couple specified in Combination.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Fabien Laporte, 'UMR Genetique Quantitative et Evolution' INRA France.
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
require('Relatedness')
data(Genotype)
data(Frequencies)
data(Cross)
SimpleRelatedness <- RelCoefLine(LineGenom=Genotype,Freq=Frequencies,
LinePop=rep(1,8),NbCores=2)
print(SimpleRelatedness)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Relatedness }
