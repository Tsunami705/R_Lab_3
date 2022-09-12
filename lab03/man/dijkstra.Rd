\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~~ This is title 1 ~~
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    nodes_num = length(unique(graph[, 1]))
    dist <- vector(mode = "numeric", length = nodes_num)
    prev <- vector(mode = "numeric", length = nodes_num)
    for (i in 1:nodes_num) {
        dist[i] <- 1024
        prev[i] <- NA
    }
    Q <- 1:nodes_num
    dist[init_node] <- 0
    while (length(Q) != 0) {
        u <- which.min(dist[Q])
        b <- Q[u]
        Q = Q[-u]
        rest1 = which(graph[, 1] == b)
        rest2 = intersect(graph[rest1, 2], Q)
        for (j in rest2) {
            a = which(graph[, 1] == b & graph[, 2] == j)
            alt <- dist[b] + graph[a, 3]
            if (alt < dist[j]) {
                dist[j] <- alt
                prev[j] <- b
            }
        }
    }
    return(dist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
