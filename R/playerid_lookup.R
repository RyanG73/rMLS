#' A function to find the identification number for any player
#'
#' This function allows me to test setup
#' @param playerid_lookup, no default, pulls all players
#' @keywords playerid, id, player, lookup
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{playerid_lookup()}
#' playerid_lookup()

playerid_lookup <- function(id=NULL,name=NULL){
    if (length(id) > 0) {
      players <- rMLS::players %>%
        dplyr::filter(player_id == id)
    return(players)
    } else {
      players <- rMLS::players %>%
        dplyr::filter(grepl(as.character(name), player_name))
    return(players)
    }
}
