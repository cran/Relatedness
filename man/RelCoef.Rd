\name{RelCoef}
\alias{RelCoef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RelatednessCoefficients
%%  ~~function to do ... ~~
}
\description{This function performs Maximum Likelihood estimation for the relatedness coefficients between individuals based on a bi-allelic genotype matrix. Alternatively, a parental genotype matrix and a crossing matrix can be used. In that case information about structure can also be taken into account via a ParentPop vector.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RelCoef(IndividualGenom = matrix(0, nrow=0, ncol=0), 
        ParentalLineGenom = matrix(0, nrow=0, ncol=0), 
        Freq = matrix(0, nrow=0, ncol=0), 
        Crossing = matrix(0, nrow=0, ncol=0), ParentPop = rep(0,0),
        Combination = list(), Phased = FALSE, Details = FALSE,
        NbInit = 5, Prec = 10^(-4), NbCores = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{IndividualGenom}{Genotype matrix of individuals. Each individual is described by 2 columns. Each row corresponds to a marker. Entries of matrix IndividualGenom should be either 0 or 1. Either IndividualGenom or ParentalLineGenom has to be provided.
%%     ~~Describe \code{IndividualGenom} here~~
}
  \item{ParentalLineGenom}{Genotype matrix of parental lines. Each parental line is described by one column with rows corresponding to markers. Entries of ParentalLineGenome should be either 0 or 1.
%%     ~~Describe \code{ParentalLineGenom} here~~
}
  \item{Freq}{Allelic frequencies for all markers and all populations (one column per population, one line per marker).
%%     ~~Describe \code{Freq} here~~
}
  \item{Crossing}{Required when argument ParentalLineGenom is provided. A 2-column matrix where each row corresponds to a crossing between 2 parents. Parents should be numbered according to their order of appearance in the ParentalLineGenom matrix.
%%     ~~Describe \code{Crossing} here~~
}
  \item{ParentPop}{Only available if ParentalLineGenom is displayed. A vector of numbers corresponding to population membership for the parental lines.
%%     ~~Describe \code{ParentPop} here~~
}
  \item{Combination}{If provided, a list of couples for which the relatedness coefficients should be computed.
%%     ~~Describe \code{Combination} here~~
}
  \item{Phased}{A Boolean with value TRUE if observations are phased.
%%     ~~Describe \code{Phased} here~~
}
  \item{Details}{A Boolean variable. If TRUE, the relatedness mode graph is displayed.
%%     ~~Describe \code{Details} here~~
}
  \item{NbInit}{Number of initial values for the EM algorithm.
%%     ~~Describe \code{NbInit} here~~
}
  \item{Prec}{Convergence precision parameter for the EM algorithm.
%%     ~~Describe \code{Prec} here~~
}
  \item{NbCores}{Number of cores used by the algorithm (Default is the number of cores available minus one).
%%     ~~Describe \code{NbCores} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Argument IndividualGenom should be used if the available data consist in genotypic information only. By default data are assumed to be unphased and the function returns 9 relatedness coefficients. If data are phased, use argument Phased = TRUE to obtain the 15 relatedness coefficients. Note that in that case the ordering of the 2 columns per individual in IndividualGenome does matter. Alternatively, if the genotyped individuals are hybrids resulting from the crossing of parental lines (or combinations of parental gametes), it is possible to provide a ParentalLineGenom and a Crossing matrix directly. Additionally, the population membership of the parents can be provided via argument ParentPop. Whatever the arguments used to enter the genotypic data, the allelic frequencies of the markers have to be provided using argument Freq. Arguments NbInit and Prec are tuning parameters for the EM algorithm used for likelihood maximization. 

}
\value{
By default, relatedness coefficients are displayed for all couple of genotyped individuals (or hybrids). In that case the function returns a list of matrices, each corresponding to a specific relatedness coefficients (details about relatedness coefficients can be obtained by displaying the relatedness mode graph with argument Details). Element (i,j) of matrix k corresponds to the kth estimated relatedness coefficient for the couple of individuals i and j. Alternatively, if a list of couples is specified with argument Combination, the function returns a list of vectors (each vector corresponding to an relatedness coefficient). In that case element i of vector k corresponds to the kth relatedness coefficient of the ith couple specified in Combination.
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

\section{Warning }{In absence of population structure, some relatedness coefficients are not identifiable. Since an EM algorithm is run for each couple of individuals, the procedure can be time consuming for large panels.
}

\examples{
require('Relatedness')
data(Genotype)
data(Frequencies)
data(Cross)
RelCoef(IndividualGenom=matrix(0,ncol=0,nrow=0),ParentalLineGenom=Genotype,
Freq=Frequencies,Crossing=Cross,ParentPop=rep(1,20),NbCores=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Relatedness }
