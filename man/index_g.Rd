% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agops-impact.R
\name{index_g}
\alias{index_g}
\alias{index.g}
\alias{index_g_zi}
\title{Egghe's g-index}
\usage{
index_g(x)

index.g(x) # same as index_g(x), deprecated alias

index_g_zi(x)
}
\arguments{
\item{x}{a non-negative numeric vector}
}
\value{
a single numeric value
}
\description{
Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
where \eqn{x_i \ge x_j \ge 0} for \eqn{i \le j},
the \dfn{\eqn{g}-index} (Egghe, 2006) for \eqn{x} is defined as
\deqn{G(x)=\max\{i=1,\dots,n: \sum_{j=1}^i x_i \ge i^2\}}{
G(x)=max{i=1,\dots,n: x_1+\dots+x_i \ge i^2}}
if \eqn{n \ge 1} and \eqn{x_1 \ge 1}, or \eqn{G(x)=0} otherwise.
}
\details{
\code{index.g} is a (deprecated) alias for \code{index_g}.

Note that \code{index_g} is not a zero-insensitive impact function,
see Examples section. \code{index_g_zi} is its zero-sensitive variant:
it assumes that the aggregated vector is padded with zeros.

If a non-increasingly sorted vector is given, the function has O(n) run-time.

For historical reasons, this function is also available via an alias,
\code{index.g} [but its usage is deprecated].
}
\examples{
sapply(list(c(9), c(9,0), c(9,0,0), c(9,0,0,0)), index_g) # not a zero-sensitive agop

}
\references{
Egghe L., Theory and practise of the g-index,
\emph{Scientometrics} 69(1), 2006, pp. 131-152.

Mesiar R., Gagolewski M., H-index and other Sugeno integrals:
   Some defects and their compensation,
 IEEE Transactions on Fuzzy Systems 24(6), 2016, pp. 1668-1672.
doi:10.1109/TFUZZ.2016.2516579

Gagolewski M., Mesiar R., Monotone measures and universal integrals
  in a uniform framework for the scientific impact assessment problem,
  Information Sciences 263, 2014, pp. 166-174. doi:10.1016/j.ins.2013.12.004

Gagolewski M., Data Fusion: Theory, Methods, and Applications,
   Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
   isbn:978-83-63159-20-7
}
\seealso{
Other impact_functions: 
\code{\link{index_h}()},
\code{\link{index_lp}()},
\code{\link{index_maxprod}()},
\code{\link{index_rp}()},
\code{\link{index_w}()},
\code{\link{pord_weakdom}()}
}
\concept{impact_functions}
