\name{df2msatallele}
\alias{df2msatallele}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Reads tab delimited text files with input fragment sizes creates a R data base }
\description{
  Transform a typical GeneMarker Table with two columns per marker and many markers in a input data.frame in
  order to be reading in a fastReadFrag command, ie. a MsatAllele format.
}
\usage{
df2msatallele(placa.multiplex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{placa.multiplex}{ A matrix that include sample information plus two columns per marker }
}
\details{
  This function read a comma delimiter table including the follow information: 1st column-sample name;
  2nd column-Panel or test name (specific combination of loci in that sample); 3rd first allele size of marker 1; 
  4th second allele size of marker 1; 5th first allele size of marker 2; 6th second allele size of marker 2; and so
  on and so far. Use NA to code absent data.
}
\value{
 \item{SampleName}{A unique sample ID}
 \item{Panel}{A short description for the specific test or componations of markers} 
 \item{Marker}{Marker code} 
 \item{Size1}{First allele size for all markers} 
 \item{Size2}{Second allele size for all markers} 
}
\author{Genuar Núñez}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{ \code{\link{fastReadFrag}} }
\examples{

}

\keyword{ gero }
