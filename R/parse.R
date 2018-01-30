#' @title parse NCBI genome webpage
#'
#' @param path string. path to the HTML file.
#'
#' @return data_frame
#'
#' 1. species
#'
#' 1. number
#'
#' 1. dna
#'
#' 1. rna
#'
#' 1. protein
#'
#' @examples
#'     parse_genome('data-raw/genome55342.html')
#'     parse_genome('data-raw/genome2793.html')
#'
#' @export
parse_genome <- function(path) {
	html <- xml2::read_html(path);

	species <- html %>% rvest::html_nodes('span.GenomeTitle') %>%
		rvest::html_text() %>% stringr::str_extract('\\w+ \\w+');

	number <- html %>% rvest::html_nodes('.refgenome_sensor b') %>%
		rvest::html_text() %>% stringr::str_subset('\\d+ genomes') %>%
		{if (length(.) > 0) stringr::str_extract(., '\\d+') %>% as.integer else 1L};

	get_url <- function(pattern) {
		html %>% rvest::html_nodes('span.shifted a') %>% rvest::html_attr('href') %>%
			stringr::str_subset(pattern) %>% {if (length(.) > 0) . else ''}
	}

	tibble::data_frame(species = species, number = number,
					   dna = get_url('genomic.fna'), rna = get_url('rna.fna'),
					   protein = get_url('protein.faa'));
}





#' @title parse wikipedia species webpage
#'
#' @param path string. path to the HTML file.

#'
#' @return data_frame
#'
#' 1. order
#'
#' 1. family
#'
#' 1. genus
#'
#' 1. species
#'
#' @examples parse_avibase('data-raw/Avibase-1FDDABDB0D4421F9.html')
#'
#' @export
parse_avibase <- function(path) {
	avibase <- path %>% xml2::read_html();

	species <- avibase %>% rvest::html_node('td.AVBContainerText p i') %>% rvest::html_text();
	v <- avibase %>% rvest::html_nodes('td.AVBContainerText script + p') %>%
		rvest::html_text() %>% {stringr::str_extract_all(., '[a-zA-Z]+')[[1]]};

	tibble::data_frame(order = v[1], family = v[2], genus = v[3], species = species);
}
