get_recipe <- function(id, key){
    # make url
    url <- paste0("https://spoonacular-recipe-food-nutrition-v1.p.rapidapi.com/recipes/",id,"/summary")
    # set key
    names(key) <- "X-RapidAPI-Key"
    # get respond
    response <- httr::GET(url, add_headers(key))
    # parse response
    ds_contents <- httr::content(response, as="parsed")
    # return readable summary of recipe
    return(gsub("<[^>]+>", "",ds_contents$summary))
}
