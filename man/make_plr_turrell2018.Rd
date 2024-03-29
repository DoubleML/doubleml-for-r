% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{make_plr_turrell2018}
\alias{make_plr_turrell2018}
\title{Generates data from a partially linear regression model used in a blog
article by Turrell (2018).}
\usage{
make_plr_turrell2018(
  n_obs = 100,
  dim_x = 20,
  theta = 0.5,
  return_type = "DoubleMLData",
  nu = 0,
  gamma = 1
)
}
\arguments{
\item{n_obs}{(\code{integer(1)}) \cr
The number of observations to simulate.}

\item{dim_x}{(\code{integer(1)}) \cr
The number of covariates.}

\item{theta}{(\code{numeric(1)}) \cr
The value of the causal parameter.}

\item{return_type}{(\code{character(1)}) \cr
If \code{"DoubleMLData"}, returns a \code{DoubleMLData} object.
If \code{"data.frame"} returns a \code{data.frame()}.
If \code{"data.table"} returns a \code{data.table()}.
If \code{"matrix"} a named \code{list()} with entries \code{X}, \code{y} and \code{d} is returned.
Every entry in the list is a \code{matrix()} object.  Default is \code{"DoubleMLData"}.}

\item{nu}{(\code{numeric(1)}) \cr
The value of the parameter \eqn{\nu}. Default is \code{0}.}

\item{gamma}{(\code{numeric(1)}) \cr
The value of the parameter \eqn{\gamma}. Default is \code{1}.}
}
\value{
A data object according to the choice of \code{return_type}.
}
\description{
Generates data from a partially linear regression model used in a blog
article by Turrell (2018). The data generating process is defined as

\eqn{d_i = m_0(x_i' b) + v_i,}

\eqn{y_i = \theta d_i + g_0(x_i' b) + u_i,}

with \eqn{v_i \sim \mathcal{N}(0,1)}, \eqn{u_i \sim \mathcal{N}(0,1)}, and
covariates \eqn{x_i \sim \mathcal{N}(0, \Sigma)}, where  \eqn{\Sigma}
is a random symmetric, positive-definite matrix generated with
\code{\link[clusterGeneration:genPositiveDefMat]{clusterGeneration::genPositiveDefMat()}}. \eqn{b} is a vector with entries
\eqn{b_j=\frac{1}{j}} and the nuisance functions are given by

\eqn{m_0(x_i) = \frac{1}{2 \pi}
\frac{\sinh(\gamma)}{\cosh(\gamma) - \cos(x_i-\nu)},}

\eqn{g_0(x_i) = \sin(x_i)^2.}
}
\references{
Turrell, A. (2018), Econometrics in Python part I - Double
machine learning, Markov Wanderer: A blog on economics, science, coding and
data.
\url{https://aeturrell.com/blog/posts/econometrics-in-python-parti-ml/}.
}
