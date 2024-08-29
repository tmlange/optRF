#' Simulated data of wheat yield and genomic markers
#'
#' Data set containing simulated data of wheat yield in g/m^2 of 250 wheat lines and 7,500 SNP markers
#' being coded as 0 for homozygous form of the major allele and 2 for homozygous form of the
#' minor allele.
#'
#' @docType data
#'
#' @usage data(SNPdata)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{yield}{Simulated wheat yield in g/m^2}
#'  \item{SNP_0001 to SNP_7500}{Simulated values for 7,500 single nucleotide polymorphism (SNP) markers}
#' }
#' @references This artificial data set was created for the optRF package.
#' @keywords datasets
#' @examples
#'
#' data(SNPdata)
#' SNPdata[1:5,1:5]
#'
"SNPdata"
