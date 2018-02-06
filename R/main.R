#' @title wrapper to run AUGUSTUS
#'
#'
#' @param species string. an identifier for the species model to use, such as `'chicken'`
#' @param input string. path to the input file, which contains the gene sequence(s) in fasta format.
#' @param output string. path to the output file. if `NULL` output is writen into standard output
#' @param args character. additional parameters passed on to AUGUSTUS
#' @param ... other parameters passed on to [system2()], from `stdout` to the end
#'
#' @return result of [system2()] or `NULL` if `input` doesn't exist
#'
#' @examples
#' \donotrun {
#'     augustus('human', 'software/augustus/examples/example.fa');
#'     augustus('human', 'software/augustus/examples/hsackI10.fa.gz')
#'
#'     augustus('human', 'software/augustus/examples/example.fa', 'test.gff');
#' }
#'
#' @export
augustus <- function(species, input, output = NULL, args = '', ...) {
	if (!file.exists(input)) return(NULL);

	species %<>% paste0('--species=', .);
	output %<>% {if (is.null(.)) '' else paste0('--outfile=', .)};
	args %<>% c(species, input, output, .);

	system2('augustus', args, ...);
}
