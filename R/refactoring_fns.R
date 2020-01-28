#' @export
get_author_label = function(node, mongo_key){
  label = paste0(xml2::xml_text(xml2::xml_find_all(node, mongo_key[1])), " ", xml2::xml_text(xml2::xml_find_all(node, mongo_key[2])))
  return(label)
}

#' @export
get_author_orcid = function(node){
  orcid = xml2::xml_text(xml2::xml_find_first(node, "./uri[@content-type='orcid']"))
  orcid = gsub("^(.*)orcid.org\\/", "", orcid)
  return(orcid)
}

#' @export
get_taxon_label = function(node, mongo_key)
{
  b=NULL
  x = sapply(mongo_key, function(i){
    label = xml2::xml_text(xml2::xml_find_all(node, i))
    if(length(label) > 0 && label!="" && label!=" "){
      b = c(b, label)
      b
    }
  })

  tolit = do.call(get_scientific_name_or_tcl, x)
  if (is.null(tolit)){
    tolit = xml2::xml_text(xml2::xml_find_first(node, "."))
  }
  label = tolit
  label = gsub("\"", "", label)
  label
}



#' @export
get_figure_label = function (node, mongo_key, fig_number)
{
  caption_xpath = paste0("//fig[@id='", fig_number, "']", "|//fig-group[@id='", fig_number, "']")
  label = xml2::xml_text(xml2::xml_find_first(node, caption_xpath))
  label
}


#' @export
process_author = function (node, mongo_key)
{
  label = get_author_label(node, mongo_key)
  orcid = get_author_orcid(node)
  mongo_key = c(author = "")
  df = data.frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = as.character(orcid), parent = NA, key = NA, publisher_id = NA, journal_id = NA,  stringsAsFactors = FALSE )

  return(df)
}


#' @export
process_tnu = function (node, mongo_key)
{
  label = get_taxon_label(node, mongo_key)
  mongo_key = c("tnu" = "")
  label = strip_trailing_whitespace(label)
  #label =  jsonlite::fromJSON(jsonlite::toJSON(label))
  label = escape_special(label)
  df = data.frame(label = label, mongo_key = mongo_key,
                             type = names(mongo_key), orcid = NA, parent = NA,
                             key = NA, publisher_id = NA, journal_id = NA,  stringsAsFactors = FALSE )
  return(df)
}


#' @export
process_figure = function (node, mongo_key, publisher_id, journal_id)
{
  id = xml2::xml_text(xml2::xml_find_all(node, "object-id[@content-type='arpha']"))
  if (length(id) > 0) {
    fig_id = identifier(id, prefix)
    fig_id = fig_id$uri
    id_literal = id
  }
  else {
    fig_id = NA
    id_literal = ""
  }
  fig_number = xml2::xml_attr(node, "id")
  label = get_figure_label(node, mongo_key, fig_number)
  label = gsub(id_literal, "", label)
  label = strip_trailing_whitespace(label)
  label = escape_special(label)
 # label =  jsonlite::fromJSON(jsonlite::toJSON(label))
  type = paste0(names(mongo_key), " ", fig_number)
  df = data.frame(label = label, mongo_key = mongo_key,
                           type = type, orcid = NA, parent = NA, key = fig_id, publisher_id = publisher_id, journal_id = journal_id,  stringsAsFactors = FALSE)
  return(df)
}

#' @export
process_treatment = function (node, mongo_key,publisher_id, journal_id)
{
  id = xml2::xml_text(xml2::xml_find_all(node, "tp:nomenclature/tp:taxon-name/object-id[@content-type='arpha']"))
  if (length(id) > 0) {
    treat_id = identifier(id, prefix)
    id_literal = id
    label = xml2::xml_text(xml2::xml_find_first(node, mongo_key))
    label = gsub(id_literal, "", label)
    label = strip_trailing_whitespace(label)
    label = escape_special(label)
 #   label =  jsonlite::fromJSON(jsonlite::toJSON(label))
    df = data.frame(label = label, mongo_key = mongo_key,
                             type = names(mongo_key), orcid = NA, parent = NA,
                             key = treat_id$uri, publisher_id = publisher_id, journal_id = journal_id,  stringsAsFactors = FALSE)
  }
  else {
    df = process_general_component(node, mongo_key)
  }
  return(df)
}

#' @export
process_general_component = function (node, mongo_key,  publisher_id, journal_id)
{
  label = xml2::xml_text(xml2::xml_find_first(node, mongo_key))

  id = xml2::xml_text(xml2::xml_find_all(node, ".//object-id[@content-type='arpha']"))
  if (length(id)>0){
    label = gsub(id, "", label)
  }
   label = strip_trailing_whitespace(label)
   label = escape_special(label) #escape special chars
 #  label =  jsonlite::fromJSON(jsonlite::toJSON(label))
  #label = escape_special_json(label)
  df = data.frame(label = label, mongo_key = mongo_key, type = names(mongo_key), orcid = NA, parent = NA, key = NA,  publisher_id = publisher_id, journal_id = journal_id,  stringsAsFactors = FALSE)
  return(df)
}





#' @export
process_schema_component = function(node, mongo_key, publisher_id, journal_id)
{

  if (is.author(mongo_key) == TRUE){
    df = process_author(node, mongo_key)
  } else if (is.tnu(mongo_key) == TRUE){
    df = process_tnu(node, mongo_key)
  } else if (is.figure(mongo_key) == TRUE){
    df = process_figure(node, mongo_key, publisher_id = publisher_id, journal_id = journal_id)
  } else if (is.treatment(mongo_key) == TRUE){
    df = process_treatment(node, mongo_key, publisher_id = publisher_id, journal_id = journal_id)
  } else{
    df = process_general_component(node, mongo_key, publisher_id = publisher_id, journal_id = journal_id)
  }

  return(df)
}


#' @export
escape_special_json = function(string){

  string =  gsub("\"", "\\\"", string , fixed = TRUE)
  string =  gsub("\r?\n|\r", " ", string)
  string =  gsub("\\“", "\\\\", string , fixed = TRUE)
  string =  gsub("\\”", "\\\\", string , fixed = TRUE)
  string =  gsub("[[\\]\\{\\}\\^$|#]", "\\\\", string, perl=TRUE)
  string =  gsub("\\n", " ", string, perl=TRUE)
  string =  gsub("\"N", "\"N", string , fixed = TRUE)
  string =  gsub("\"E", "\"E", string , fixed = TRUE)
  string =  gsub("\"S", "\"S", string , fixed = TRUE)
  string =  gsub("\"W", "\"W", string , fixed = TRUE)
  string = gsub("\\\\(?=[WNSE])(?=[a-zA-Z])", "", string,fixed = TRUE)
  string = gsub("\\s{2,}", " ", string)
  string = strip_trailing_whitespace(string)
  string  = gsub("\\s*$", "", string)

  return(string)
}

#' @export
escape_special = function(string){
  string =  gsub("\r?\n|\r", " ", string)
  string =  gsub("\\“", "\\\\", string , fixed = TRUE)
  string =  gsub("\\”", "\\\\", string , fixed = TRUE)
  string =  gsub("\"N", "\\\"N", string , fixed = TRUE)
  string =  gsub("\"E", "\\\"E", string , fixed = TRUE)
  string =  gsub("\"S", "\\\"S", string , fixed = TRUE)
  string =  gsub("\"W", "\\\"W", string , fixed = TRUE)
  string =  gsub("\"", "\\\"", string , fixed = TRUE)
  string =  gsub("\\\\", "\\", string , fixed = TRUE)
  string = gsub("\\]", "\\\\\\]", string)
  string = gsub("\\[", "\\\\\\[", string)
  string = gsub("–", "-", string)
  string = gsub("\\r\\n", " ", string)
  string = gsub("\\\\(?=[WNSE])(?=[a-zA-Z])", "", string,fixed = TRUE)
  string = gsub("\\s{2,}", " ", string)
  string = strip_trailing_whitespace(string)
  string  = gsub("\\s*$", "", string)

  return(string)
}


#' @export
get_or_set_mongoid= function (df, prefix)
{
  general_collection = mongolite::mongo("new_collection")
  if (is.null(df) == FALSE){
    #check whether the type is treatment and the id is not na


    if (!(is.na(df$key))){
      key = df$key
      save_to_mongo(key = key, value = df$label, type = df$type, orcid = df$orcid , parent = NA, publisher_id = df$publisher_id, journal_id = df$journal_id, collection = general_collection)
      key = toString(key)
      id = rdf4r::strip_angle(key)
      id = gsub("http:\\/\\/openbiodiv\\.net\\/", "", id)
    } else{
        if (!(is.na(df$orcid))) {
          query = sprintf("{\"%s\":\"%s\"}", "orcid", df$orcid)
          key = general_collection$find(query)$key

          #key = check_mongo_key_via_orcid(df$orcid, general_collection)
          if (is.null(key)){
            key = check_mongo_key(value = df$label, type = df$type, collection = general_collection, regex = FALSE)
          }
          id = get_or_set(key, df)
        }
        else {
          if (df$type == "taxonomicConcept"){
            key = check_mongo_key_via_parent(parent = df$parent, type = df$type, collection = general_collection)
            id = get_or_set(key, df)
          } else if (df$type == "tnu"){
            id = get_or_set(NULL, df)
          }
          else
          {

          nocheck = c("introduction", "abstract", "discussion", "bibliography", "keywords")
          if (df$type %in% nocheck){
            key = NULL
            id = get_or_set(key, df)
          }else{
          key = check_mongo_key(value = df$label, type = df$type, collection = general_collection, regex = FALSE)
          id = get_or_set(key, df)
           }
          }
        }
      }
   }else{
    id = NULL
  }

  return(id)
}

#' @export
get_or_set = function(key, df){
  if (is.null(key) == TRUE) {
    key = uuid::UUIDgenerate()
    #remove dashes and convert to uppercase
    #key = gsub("-", "", key)
    key = toupper(key)
    save_to_mongo(key = identifier(key, prefix)$uri, value = df$label, type = df$type,  orcid = df$orcid, parent = df$parent, publisher_id = df$publisher_id, journal_id = df$journal_id, collection = general_collection)
    id = key
  }else{
    if (!(is.na(df$orcid))){
      key = paste0("<",key,">")
      query = sprintf("{\"%s\":\"%s\"}", "key", key)
      update = sprintf("{\"$set\":{\"%s\":\"%s\"}}", "orcid", df$orcid)
      general_collection$update(query = query, update = update)
    }
    id = rdf4r::strip_angle(key)

    id = gsub("http:\\/\\/openbiodiv\\.net\\/", "", id)
  }
}



#' @export
#replace all double quotes with sngle quotes
double_quote_replacer = function(x){
  for (a in 1:length(x)){
    x[[a]]$text_value = gsub("(?<=[\\w\\s(])(\")(?=[\\.\\w\\s)]{3}[^\n])", "'", x[[a]]$text_value  ,perl=TRUE)
    x[[a]]$squote = gsub("(?<=[\\w\\s(])(\")(?=[\\.\\w\\s)]{3}[^\n])", "'", x[[a]]$squote  ,perl=TRUE)

  }
  x
}

#' @export
strip_trailing_whitespace = function(string){
  string = gsub("\\s*$", "", string)
  return(string)
}
