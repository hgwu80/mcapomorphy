



#' @title download NCBI assembly webpage for a vector of given assessions
#'
#' @param assessions character.
#'
#' @return `NULL`
#' @export
fetch_assembly <- function(assessions) {
	fetch(assessions, 'https://www.ncbi.nlm.nih.gov/assembly/', 'data-raw/assembly-html/');
}


#' @title download NCBI genome webpage for a vector of given id
#'
#' @param assessions character.
#'
#' @return `NULL`
#' @export
fetch_genome <- function(id) {
	fetch(id, 'https://www.ncbi.nlm.nih.gov/genome/', 'data-raw/genome-html/');
}



#' @title download NCBI WGS TSA assembly webpage for a vector of given ids
#'
#' @param ids character.
#'
#' @return `NULL`
#' @export
fetch_TSA <- function(ids) {
	fetch(ids, 'https://www.ncbi.nlm.nih.gov/Traces/wgs/', 'data-raw/TSA-html/');
}




#" for debug `fetch()`
# srcs = assembly_id;
# prefix_url = 'https://www.ncbi.nlm.nih.gov/assembly/';
# dest_dir = 'data-raw/assembly-html/';
# mutate = function(x){x};

#' @title download many webpages parallelly
#' 
#' @param srcs character. would become the names of downloaded files.
#' @param prefix_url string. such as `'https://en.wikipedia.org/wiki/'`,
#'   `paste0(prefix_url, query)` gives the url, see `mutate` for `query`
#' @param dest_dir string. such as 'data-raw/wikipedia/'
#' @param mutate function. `mutate(src)` returns `query`.
#'
#' @return `NULL`
#' @export
fetch <- function(srcs, prefix_url, dest_dir, mutate = function(x){x}) {
	dest_dir %>% {if (!dir.exists(.)) dir.create(.)};
	remove_empty <- . %>% {if (file.size(.) == 0 ) file.remove(.)};
	i = 0L;
	
	while (TRUE) {
		dir(dest_dir, full.names = T) %>% plyr::l_ply(remove_empty);
		
	    done <- dest_dir %>% dir;
	    done = done[file.size(paste0(dest_dir, done)) > 10];
	    todo = setdiff(srcs, done);
	    
	    i = i + 1;
	    if (i >= max_iterate) {
	    	warning('reached max iterate, stoping');
	    	break;
	    }
	    
	    if (length(todo) == 0) break;
	    
	    parallel::mclapply(
	    	todo,
	    	function(src) {
	    		query <- mutate(src);
	
				url  <- paste0(prefix_url, query);
				dest <- paste0(dest_dir, src);
				
				message(url, dest)
				
				#tibble::data_frame(url, dest)
				tryCatch(download.file(url, dest), error = function(e){});
	    	}			   
	    );
	}
	
	NULL;
}
