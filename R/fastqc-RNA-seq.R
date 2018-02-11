#' @title browse fastqc html output view RStduio Server
#' @section package options embedded in aves:
#'     options(mcapomorphy.wd = '/media/data/aves')
#'     options(mcapomorphy.url = 'http://222.30.49.71:8211')



#' @title return the url of a file on the workstation
#'
#' @param path string. full absolute path of the file
#'
#' @return string.
#'
#' @examples file_url(fastqc_path('SRR6148275_2'))
file_url <- function(path) {
	paste0(getOption('mcapomorphy.url'), '/file_show?path=', path)
}



#' @title return the full absolute path of the fastqc result
#'
#' @param name string. name of the sequence file, such as `'SRR6148275_2'`
#'
#' @return string.
#'
#' @examples fastqc_path('SRR6148275_2')
fastqc_path <- function(name) {
	paste0(getOption('mcapomorphy.wd'), '/oases/fastqc/', name, '_fastqc.html')
}



#' @title view fastqc result of original SRA sequence for single ends
#'
#' @param id string. sra accession, such as `'SRR029421'`
#'
#' @return `NULL`
#'
#' @examples
#' \donotrun{
#'     view_sra_single('SRR029421')
#' }
view_sra_single <- function(id) {
	id %>% fastqc_path %>% file_url %>% libzhuoer::browse_url();
}



#' @title view fastqc result of original SRA sequence for paired ends
#'
#' @param id string. sra accession, such as `'SRR6148275'`
#'
#' @return `NULL`
#'
#' @examples
#' \donotrun{
#'     view_sra_paired('SRR6148275')
#' }
view_sra_paired <- function(id) {
	id %>% paste0('_', 1:2) %>% fastqc_path %>% file_url %>% libzhuoer::browse_url();
}



#' @title compare fastqc result of original and trimmed sequence for single ends
#'
#' @param id string. sra accession, such as `'SRR029421'`
#'
#' @return `NULL`
#'
#' @examples
#' \donotrun{
#'     compare_sra_single('SRR029421')
#' }
compare_sra_single <- function(id) {
	id %>% paste0(c('', '-trim')) %>% fastqc_path %>% file_url %>% libzhuoer::browse_url();
}



#' @title compare fastqc result of original and trimmed sequence for paired ends
#'
#' @param id string. sra accession, such as `'SRR6148275'`
#'
#' @return `NULL`
#'
#' @examples
#' \donotrun{
#'     compare_sra_paired('SRR6148275')
#' }
compare_sra_paired <- function(id) {
	names <- id %>% paste0('_', 1:2) %>% plyr::llply(. %>% paste0(., c('', '-trim-paired'))) %>% unlist;
	names %>% fastqc_path %>% file_url %>% libzhuoer::browse_url(0.5);
}


