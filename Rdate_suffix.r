
Rdate_suffix <- function(dest = "reports", check_dest = TRUE, use_date = TRUE, date = Sys.Date(), date_format = "%Y%m%d", alt_suffix = NULL) {
	if (current_date) {
		res0 <- format(date, date_format)
	} else {
		res0 <- alt_suffix
	} 
	if (check_dest) {
		dest_contents <- list.files(path = dest)
		dest_conflicts <- ifelse(grep(res0, dest_contents) > 0, grep(res0, dest_contents, value = TRUE), NULL)
		if (!is.null(dest_conflicts)) {
			
		}
	}
    
}