
#' @title convert gff file produced by augustus to fasta file storing protein
#'
#' @param input. string. path to input file
#' @param output string. path to output file
#'
#' @return `NULL`
#' @export
#'
augustus_gff_to_fasta <- function(input, output) {
	if (!file.exists(input)) return(NULL);

    gff <- readr::read_lines(input)

	fasta <- gff %>% stringr::str_subset('^#') %>%
    	stringr::str_replace('^# ', '') %>%
		paste0(collapse = '') %>%
		{stringr::str_extract_all(., '(?<=protein sequence = \\[)[^\\[\\]]+(?=\\])')[[1]]}
	names(fasta) <- seq_along(fasta);

    bioinfor::write_fasta(fasta, output);
}



#' @title convert hamstr output file to FASTA format
#'
#' @param input. character. path to input file, can be more than one.
#'   non-existent ones are ignored
#' @param output string. path to output file
#'
#' @return `NULL`
#' @export
#'
hamstr_out_to_fasta <- function(input, output) {
	content <- input %>% {.[file.exists(.)]} %>% lapply(readr::read_lines) %>% unlist;

	fasta <- tibble::data_frame(
		name = stringr::str_extract(content, '^\\w+'),
		seq  = stringr::str_replace(content, '^[\\w\\W]+\\|', '')
	) %>% plyr::ddply('name', . %>% slice(1));

    biozhuoer::write_fasta(fasta, output);
}

