#' This function gives the summary to a specific recipe-id gathered from get_recommendations.
#'
#' @param ingredients a list of characters, specifying the ingredients
#' @param no_results integer specifying the number of recommendations
#' @param key app credentials from rapidapi
#'
#' @return Returns a dataframe, which recommends recipes based on the specified ingredients.
#'

get_recommendations <- function(ingredients, no_results, key){
    if(no_results > 50){ stop("Don't burn your credit card!!!") }
    # set key
    names(key) <- "X-RapidAPI-Key"
    # collapse the ingredience for query
    ingreds <- paste0(ingredients,collapse = "%2C")
    # build query for API
    url <- paste0("https://spoonacular-recipe-food-nutrition-v1",
                  ".p.rapidapi.com/recipes/findByIngredients?",
                  "number=", as.character(no_results), "&ranking=1",
                  "&ignorePantry=false", "&ingredients=", ingreds)
    # make API call
    response <- httr::GET(url, httr::add_headers(key))
    # parse content
    ds_contents <- httr::content(response, as="parsed")
    # build result dataframe
    recipe_suggestions <- df <- data.frame(ID = integer(),
                                           Dish = character(),
                                           Picture = character(),
                                           Matched_Ingredients = character(),
                                           Missed_Ingredients = character(),
                                           Unused_Ingredients = character(),
                                           Likes = integer(),
                                           stringsAsFactors=FALSE)
    # fill dataframe with content
    for(i in 1:length(ds_contents)){
        recipe_suggestions[i,"Dish"] <- ds_contents[[i]]$title
        recipe_suggestions[i,"ID"] <- ds_contents[[i]]$id
        recipe_suggestions[i,"Picture"] <- ds_contents[[i]]$image
        recipe_suggestions[i,"Likes"] <- ds_contents[[i]]$likes
        recipe_suggestions[i,"Matched_Ingredients"] <-
            paste(sapply(ds_contents[[i]]$usedIngredients, function(x) x$name),collapse=", ")
        recipe_suggestions[i,"Missed_Ingredients"] <-
            paste(sapply(ds_contents[[i]]$missedIngredients, function(x) x$name),collapse=", ")
        recipe_suggestions[i,"Unused_Ingredients"] <-
            paste(sapply(ds_contents[[i]]$unusedIngredients, function(x) x$name),collapse=", ")
    }
    # return results
    return(recipe_suggestions)
}
