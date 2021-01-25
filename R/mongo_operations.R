#' @export
check_mongo_key = function(value, type, collection, regex)
{
  if (regex == TRUE){
    query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}}", "value", "$regex", value, "$options", "i")
  } else{
    query = sprintf("{\"value\": \"%s\", \"type\": \"%s\"}", value, type)
  }
      df = collection$find(query)
      key = NULL
      if (!(is.null(df))){
        df = df[which(df$value == value),]
        key = df[1,]$key
      }
      return(key)
}


#' @export
check_mongo_parent = function(key, value, type, collection)
{
  if (is.null(key)){
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "value", value, "type", type)

    #query = sprintf('{"$text":{"$search":"%s"}, "type": "%s"}', value, type)
    df = collection$find(query)
    parent = NULL
    if (!(is.null(df))){
      df <- df[which(df$value == value),]
      for (n in 1:nrow(df)){
        if (!(is.null(df[n,]$value)) && length(value)>0 ){
         if (df[n,]$value == value){
            parent = df[n,]$parent
        }
       }
      }
    }
  } else{
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "key", key, "type", type)
    parent = collection$find(query)$parent
  }
  return(parent)
}

#' @export
check_mongo_instCode = function(code, collection)
{
    query = sprintf("{\"%s\":\"%s\"}", "code", code)
    res = collection$find(query)
    return(res)
  }

#' @export
check_mongo_instName = function(name, collection)
{
    query = sprintf("{\"%s\":\"%s\"}", "name", name)
    res = collection$find(query)
    return(res)
  }



#' @export
check_mongo_key_via_parent = function(parent,type, collection)
{
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "parent", parent, "type", type)
    key = collection$find(query)$key
    return(key)
  }


#' @export
check_mongo_inst = function(tpKey, collection){
  query = sprintf("{\"%s\":\"%s\"}", "tpKey", tpKey)
  res = collection$find(query)
  return(res)
}


#' @export
check_mongo_key_via_orcid = function(orcid, collection)
{
  query = sprintf("{\"%s\":{\"%s\":\"%s\",\"%s\":\"%s\"}}", "orcid", "$regex", orcid, "$options", "i")
  collection$find(query)
  key = collection$find(query)$key
  return(key)
}




#' @export
save_to_mongo = function (key, value, type, orcid, parent, publisher_id, journal_id, plazi_doc, doi, article_id,
                          collection)
{
  if (!(is.na(plazi_doc))){
    if (plazi_doc == TRUE){
      d = data.frame(key = as.character(key), value = as.character(value),
                     type = as.character(type), orcid = as.character(orcid),
                     parent = as.character(parent), publisher_id = as.character(publisher_id),
                     journal_id = as.character(journal_id), plazi_doc = as.character(plazi_doc), doi = as.character(doi), article_id = as.character(article_id))
    }else{
      d = data.frame(key = as.character(key), value = as.character(value),
                     type = as.character(type), orcid = as.character(orcid),
                     parent = as.character(parent), publisher_id = as.character(publisher_id),
                     journal_id = as.character(journal_id), doi = as.character(doi), article_id = as.character(article_id))
    }
  } else{
    d = data.frame(key = as.character(key), value = as.character(value),
                   type = as.character(type), orcid = as.character(orcid),
                   parent = as.character(parent), publisher_id = as.character(publisher_id),
                   journal_id = as.character(journal_id), doi = as.character(doi), article_id = as.character(article_id))
  }


  collection$insert(d)
}



#' @export
save_orcid_to_mongo = function(key, value, type, parent, orcid, collection)
{
  d = data.frame(
    key = as.character(key),
    value = as.character(value),
    type = as.character(type),
    parent = as.character(parent),
    orcid = as.character(orcid)
  )
  collection$insert(d)
}


#' @export
check_code_name_combo = function(code, name, collection)
{
    query = sprintf("{\"%s\":\"%s\",\"%s\":\"%s\"}", "code", code, "name", name)
    key = collection$find(query)
    return(key)
  }

#' @export
#'
update_parent =  function(key, parent, collection = general_collection){
  query = sprintf("{\"%s\":\"%s\"}", "key", key)
  update = sprintf("{\"$set\":{\"%s\":\"%s\"}}", "parent", parent)
  collection$update(query = query, update = update)
}
