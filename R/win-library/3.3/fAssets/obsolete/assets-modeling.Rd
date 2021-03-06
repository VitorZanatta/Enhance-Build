\name{assets-modeling}


\alias{assetsFit}
\alias{assetsSim}


\title{Modeling of Multivariate Asset Sets}


\description{
  
    Fits the parameters to a multivariate normal, skew normal, 
    or (skew) Student-t distribution and allows to simulate 
    artificial asset series.
    
}
    

\usage{   
assetsFit(x, method = c("st", "snorm", "norm"), title = NULL, 
    description = NULL, fixed.df = NA)
    
assetsSim(n, dim = 2, model = list(mu = rep(0, dim), Omega = diag(dim), 
    alpha = rep(0, dim), df = Inf), assetNames = NULL) 
}


\arguments{
  
    \item{x}{
        a numeric matrix of returns or any other rectangular object like
        a data.frame or a multivariate time series object which can be 
        transformed by the function \code{as.matrix} to an object of 
        class \code{matrix}.
        }
    \item{n}{
        integer value, the number of data records to be simulated. 
        }
    \item{method}{
        a character string, which type of distribution should be fitted?
        \code{method="st"} denotes a multivariate skew-Student-t distribution,
        \code{method="snorm"} a multivariate skew-Normal distribution, and
        \code{method="norm"} a multivariate Normel distribution.  
        By default a multivariate normal distribution will be fitted to the
        empirical market data.
        }
    \item{dim}{
        integer value, the dimension (number of columns) of the assets 
        set.
        }
    \item{model}{
        a list of model parameters: \cr
        \code{mu} a vector of mean values, one for each asset series, \cr
        \code{Omega} the covariance matrix of assets, \cr
        \code{alpha} the skewness vector, and \cr
        \code{df} the number of degrees of freedom which is a measure for
            the fatness of the tails (excess kurtosis). \cr
        For a symmetric distribution \code{alpha} is a vector of zeros.
        For the normal distributions \code{df} is not used and set to 
        infinity, \code{Inf}. Note that all assets have the same value 
        for \code{df}.
        }   
    \item{assetNames}{
        [assetsSim] - \cr
        a vector of character strings of length \code{dim} allowing
        for modifying the names of the individual assets.
        }
    \item{title}{
        a character string, assigning a title to an 
        \code{"fASSETS"} object.
        }
    \item{description}{
        a character string, assigning a brief description to an 
        \code{"fASSETS"} object.
        }
    \item{fixed.df}{
        either \code{NA}, the default, or a numeric value assigning the
        number of degrees of freedom to the model. In the case that 
        \code{fixed.df=NA} the value of \code{df} will be included in the
        optimization process, otherwise not.
        } 
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{
  
    \code{assetsFit()} 
    \cr
    returns a S4 object class of class \code{"fASSETS"}, with the 
    following slots:

    \item{@call}{
        the matched function call.
        }
    \item{@data}{
        the input data in form of a data.frame.
        }
    \item{@description}{
        allows for a brief project description.
        }
    \item{@fit}{
        the results as a list returned from the underlying
        fitting function. 
        }
    \item{@method}{
        the selected method to fit the distribution, one 
        of \code{"norm"}, \code{"snorm"}, \code{"st"}.
        }
    \item{@model}{
        the model parameters describing the fitted parameters in 
        form of a list, \code{model=list(mu, Omega, alpha, df}.
        }
    \item{@title}{
        a title string.
        }
    
    The \code{@fit} slot is a list with the following compontents:
    (Note, not all are documented here).
    
    \item{@fit$dp}{
        a list containing the direct parameters beta, Omega, alpha. 
        Here, beta is a matrix of regression coefficients with 
        \code{dim(beta)=c(nrow(X), ncol(y))}, \code{Omega} is a 
        covariance matrix of order \code{dim}, \code{alpha} is 
        a vector of shape parameters of length \code{dim}.  
        }
    \item{@fit$se}{
        a list containing the components beta, alpha, info. Here, 
        beta and alpha are the standard errors for the corresponding 
        point estimates; info is the observed information matrix 
        for the working parameter, as explained below.
        }  
    \item{fit@optim}{ 
        the list returned by the optimizer \code{optim}; see the 
        documentation of this function for explanation of its 
        components.  
        }
        
    Note that the \code{@fit$model} slot can be used as input to the 
    function \code{assetsSim} for simulating a similar portfolio of 
    assets compared with the original portfolio data, usually market
    assets.   
    
    \code{assetsSim()} 
    \cr
    returns a signal series (S4 time Series object) of simulated assets.

}


\details{

    The function \code{assetsFit} for the parameter estimation uses code 
    based on functions from the contributed packages \code{"mtvnorm"} and 
    \code{"sn"} for fitting data to a multivariate Normal, skew-Normal, 
    or skew-Student-t distribution. 

}


\references{

Azzalini A. (1985);
    \emph{A Class of Distributions Which Includes the Normal Ones},
    Scandinavian Journal of Statistics 12, 171--178. 

Azzalini A. (1986);
    \emph{Further Results on a Class of Distributions Which Includes 
        the Normal Ones},
    Statistica 46, 199--208. 

Azzalini A., Dalla Valle A. (1996);
    \emph{The Multivariate Skew-normal Distribution},
    Biometrika 83, 715--726. 

Azzalini A., Capitanio A. (1999);
    \emph{Statistical Applications of the Multivariate Skew-normal 
        Distribution},
    Journal Roy. Statist. Soc. B61, 579--602. 

Azzalini A., Capitanio A. (2003);
    \emph{Distributions Generated by Perturbation of Symmetry with 
        Emphasis on a Multivariate Skew-t Distribution},
    Journal Roy. Statist. Soc. B65, 367--389. 
  
Genz A., Bretz F. (1999);
    \emph{Numerical Computation of Multivariate t-Probabilities
        with Application to Power Calculation of Multiple Contrasts}, 
    Journal of Statistical Computation and Simulation 63, 361--378.

Genz A. (1992);
    \emph{Numerical Computation of Multivariate Normal Probabilities},
    Journal of Computational and Graphical Statistics 1, 141--149.
    
Genz A. (1993); 
    \emph{Comparison of Methods for the Computation of Multivariate
        Normal Probabilities},
    Computing Science and Statistics 25, 400--405.
    
Hothorn T., Bretz F., Genz A. (2001);
    \emph{On Multivariate t and Gauss Probabilities in R},
    R News 1/2, 27--29.
    
Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.

}


\author{

    Adelchi Azzalini for R's \code{sn} package, \cr
    Torsten Hothorn for R's \code{mtvnorm} package, \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP2005REC -
   # Load Swiss Pension Fund Data as Percentual Returns:
   LPP <- 100 * LPP2005REC[, 1:3]
   head(LPP)
   
## assetsFit -
   # Fit a Skew-Student-t Distribution: 
   fit <- assetsFit(LPP)
   # Show Model Slot:
   print(fit@model)
   
## assetsSim -
   # Simulate set with same statistical properties:
   set.seed(1953)
   LPP.SIM <- assetsSim(n=nrow(LPP), dim=ncol(LPP), model=fit@model)
   colnames(LPP.SIM) <- colnames(LPP)
   head(LPP.SIM) 
}

\keyword{models}

