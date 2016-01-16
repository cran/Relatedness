\name{RelComb}
\alias{RelComb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computation of Linear Combination of Relatedness Coefficients
%%  ~~function to do ... ~~
}
\description{Compute any relatedness synthetic criterion based on a linear combination of the relatedness coefficients.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RelComb(Combination, Delta, 
		 Crossing = matrix(0, nrow = 0, ncol = 0), 
		 ParentPop = rep(0, 0), 
		 ShowIdentifiable = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Combination}{A vector, with length identical to the length of \code{Delta} where the kth element corresponds to the coefficient for the kth relatedness coefficient, 
or alternatively a character in the following list: 
'simple relatedness', 
'double relatedness', 
'first inbreeding', 
'second inbreeding', 
'double inbreeding'.
%%     ~~Describe \code{Combination} here~~
}
  \item{Delta}{A list of matrices, each corresponding to a specific relatedness coefficients. Element (i,j) of matrix k corresponds to the kth estimated relatedness coefficient for the couple of individuals i and j.
%%     ~~Describe \code{Delta} here~~
}
  \item{Crossing}{A 2-column matrix where each row corresponds to a crossing between 2 parents. Parents should be numbered.
%%     ~~Describe \code{Crossing} here~~
}
  \item{ParentPop}{A vector of numbers corresponding to population membership for the parental lines.
%%     ~~Describe \code{ParentPop} here~~
}
  \item{ShowIdentifiable}{A boolean describing whether the combination should be displayed only for identifiable cases. Default value is TRUE.
%%     ~~Describe \code{ShowIdentifiable} here~~
}
}
\details{The function can be applied to a list of relatedness coefficients - as produced by the RelCoeff function - to compute any synthetic criterion based on a linear combination of the relatedness coefficients, for all couples. Additional information Crossing and ParentPop are required if they were used in the RelCoeff function to obtain the relatedness coefficients. The function automatically checks the identifiability of the combination to be evaluated. Several classical genetic criteria are implemented by default and can be computed using the Csuros argument. Alternatively, the user can provide a vector of coefficients to be applied through the Combination argument.
%%  ~~ If necessary, more details than the description above ~~
}
\value{If identifiability is satisfied for all pairs of individuals, all criteria are computed and returned in a matrix. If identifiability is not guaranteed for all pairs, the function will return a matrix with NA entries for the potentially non-estimable pairs. This by-default behavior can be bypassed if required by setting ShowIdentifiable to FALSE.
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
\references{
Csuros M (2014) Non-identifiability of identity coefficients at biallelic loci. \emph{Theoretical Population Biology} 92: 22-24.
}

\examples{
require('Relatedness')
data(Delta)
RelatednessComb <- RelComb(Combination='simple relatedness', Delta, ShowIdentifiable = TRUE)
print(RelatednessComb)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Relatedness }
\keyword{ Identifiability }% __ONLY ONE__ keyword per line
