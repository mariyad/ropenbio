
#' Maps ScNames from xmls to ScNames from GBIF
#' @export
gbif_taxonomy_mapping = function(scName, collection = checklistCol)
{

  query_can = sprintf("{\"%s\":\"%s\"}", "canonicalName", scName)

  query_sc = sprintf("{\"%s\":\"%s\"}", "scientificName", scName)


  res = collection$find(query_can)
  if (nrow(res)==0)
    res = collection$find(query_sc)


  if (nrow(res)==0 || nrow(res)>1){
    gbif_key = NULL
  }else{
    gbif_key = res$tcId
  }

  if(!(is.null(gbif_key))){
    gbif_id = strip_angle(gbif_key)
    gbif_id = gsub("http:\\/\\/openbiodiv\\.net\\/", "", gbif_id)
    gbif_id = paste0(gbif_id, "-scName")
    gbif_id = identifier(gbif_id, prefix)
  }else{
    gbif_id = NULL
  }

  return(gbif_id)
}


#' Serialization of BOLD record ids/BINS/Genbank ids in different article sections (a separate function like institution_serializer)
#' @export
 bold_genbank_serializer = function(tt, atoms, identifiers){
  nid = identifiers$nid
  
  #BOLD id
  sapply(atoms$bold_id, function(n){
    
    bold_uri_label = n$text_value
    bold_id_label = stringr::str_extract(bold_uri_label, "(?<==).*")
    
    if (grepl("bin", bold_uri_label) || grepl("clusteruri", bold_uri_label)){
      df_type = "bin"
      semantic_type = BOLDBin
    } else{
      df_type = "bold-id"
      semantic_type = BOLDRecord
    }
    
    bold_df = data.frame(label =  bold_id_label , mongo_key = NA, type = df_type, orcid = NA, parent = NA, key = NA, stringsAsFactors = FALSE)
    bold_id = identifier(get_or_set_mongoid(bold_df, prefix), prefix)
    tt$add_triple(nid, mentions_id , bold_id)
    tt$add_triple(bold_id, rdf_type , semantic_type)
    tt$add_triple(bold_id, rdf_type , ResourceIdentifier)
    tt$add_triple(bold_id, identifier_scheme , boldsystems)
    tt$add_triple(bold_id, rdfs_label , literal(bold_id_label))
    tt$add_triple(bold_id, has_url, literal(bold_uri_label, xsd_type = xsd_uri))
    
  })
  
  #GenBank id
  sapply(atoms$genbank_id, function(n){
    
    genbank_label = n$text_value
    genbank_df = data.frame(label =  genbank_label , mongo_key = NA, type = "genbank-id", orcid = NA, parent = NA, key = NA, stringsAsFactors = FALSE)
    genbank_id = identifier(get_or_set_mongoid(genbank_df, prefix), prefix)
    tt$add_triple(nid, mentions_id , genbank_id)
    tt$add_triple(genbank_id, rdf_type , GenBankAccession)
    tt$add_triple(genbank_id, rdf_type , ResourceIdentifier) #this could go in the ontology (e.g. GenBankAccession subclassOf ResourceIdentifier) but for now stays here until we have updated the ontology
    tt$add_triple(genbank_id, identifier_scheme , genbank)
    tt$add_triple(genbank_id, rdfs_label , literal(genbank_label))
    
    
  })
  
  return(tt)
  
}

#' DEPRECATED
#' Crosslinker extracts external identifiers within the xml which enables crosslinking between OpenBiodiv and other systems
#' Can add more types of identifiers to be extracted
#' @export
crosslinker <- function(file)
{
    suppressWarnings({
      xml = xml2::read_xml(file)

      doc = XML::xmlParse(xml, isHTML = FALSE)
      nodeset = XML::getNodeSet(doc, "//article-meta")
      #xpath to find all bold systems links within article!
      #crosslinks with BOLD systems
      nodeset = bold_processor(xml, nodeset)
      #crosslinks with GenBank
      nodeset = genbank_processor(xml, nodeset)

      new_xml = XML::saveXML(doc)

      return(new_xml)
    })
  }


#' DEPRECATED
#' @export
bold_processor = function(xml, nodeset){
  bold_results = xml2::xml_find_all(xml, "//*[starts-with(@xlink:href, 'http://www.boldsystems.org/')] | //*[starts-with(@xlink:href, 'http://boldsystems.org/')]")
  if(!(length(bold_results)==0))
  {
    nodeset = bold_xml_modifier(nodeset = nodeset, results = bold_results)
  }
  return(nodeset)
}


#' DEPRECATED
#' @export
genbank_processor =  function(xml, nodeset){
  genbank_results = xml2::xml_find_all(xml, "//ext-link[@ext-link-type='gen']")
  if(!(length(genbank_results)==0))
  {
    nodeset = genbank_xml_modifier(nodeset = nodeset, results = genbank_results)
  }
  return(nodeset)
}


#' DEPRECATED
#' @export
bold_string_cleaner = function(node_string){
  node_string = sub('.*http', '', node_string)
  #handles cases where the node value is either a link or just an id
  #if we have 2 closing tags => the value is just an id
  #1 closing tag means the value contained a link and sub('.*http','', r) matched and replaced the second 'http' (within the value)
  if (stringi::stri_count_regex(node_string, '>') > 1){
    string = sub('\">?.*', '', node_string)
  }
  else {
    string = sub('<.*', '', node_string)
  }
  #adds the http part to url_parse the string => break it down into parts and only take the query part
  string = paste0("http", string)
  result = xml2::url_parse(string)
  query = result$query
  return(query)
}


#' DEPRECATED
#' @export
bold_xml_modifier = function(nodeset, results){

  #creates parent nodes for the bold-ids or bins section
  parent_id = XML::newXMLNode("bold-ids", parent = nodeset)
  parent_bin = XML::newXMLNode("bins", parent = nodeset)
  empty_bin = TRUE
  empty_bold_id = TRUE
  for (r in results)
  {
    string = bold_string_cleaner(r)
    #if the query part contains "bin" or "clusteruri" the uri contains a bin
    #else - bold-id (which can be anything, like record-id or process-id)
    if((sub('=.*', '', string) == "bin") || (sub('=.*', '', string) == "clusteruri" ))
    {
      empty_bin = FALSE
      tag = "bin"
      new_node = XML::newXMLNode(tag, parent = parent_bin)
    } else
    {
      empty_bold_id = FALSE
      tag = "bold-id"
      new_node = XML::newXMLNode(tag, parent = parent_id)
    }
    value = sub('.*=', '', string)
    XML::xmlValue(new_node) = value
  }

  #remove the empty nodesets
  if (empty_bin == TRUE)
  {
    XML::removeNodes(parent_bin)
  }
  if (empty_bold_id == TRUE)
  {
    XML::removeNodes(parent_id)
  }

  return(nodeset)
}


#' DEPRECATED
#' @export
genbank_xml_modifier = function(nodeset, results){
  #creates parent nodes for the bold-ids or bins section
  parent_gen = XML::newXMLNode("genbank-ids", parent = nodeset)
  for (r in results)
  {
    node_string = xml2::xml_text(r)
    tag = "genbank-id"
    new_node = XML::newXMLNode(tag, parent = parent_gen)
    XML::xmlValue(new_node) = node_string
  }
  return(nodeset)
}


#' DEPRECATED
#' @export
table_formatter = function(xml){

 # doc = XML::xmlParse(xml, isHTML = FALSE)
#  nodeset = XML::getNodeSet(doc, "//table")
    nd = xml2::xml_find_all(xml, "//table")

    for (n in 1:length(nd)){

    children = xml2::xml_children(nd[n])

    #parent_gen = XML::newXMLNode("genbank-ids", parent = nodeset)
     # xml_string = toString(nodeset[n])
    xml_string = toString(nd[n])

    #scrap all <th> for now - need complex regexes to make those work
    xml_string = gsub("<tr>\\s+<th(.*?)<\\/th>", "", xml_string)
    xml_string = gsub("<th(.*?)<\\/th>\\s+<\\/tr>", "", xml_string)
    xml_string = gsub("  <th(.*?)<\\/th>", "", xml_string)
  #  xml_string = gsub("\n", "", xml_string)
    #xml_string = gsub("<tr>", "", xml_string)
    xml_string = gsub("\\s+", " ", xml_string)
    xml_string = gsub("<tr>", "", xml_string)
    xml_string = gsub("<td(.*?)>", "", xml_string)
    xml_string = gsub("<\\/td>\\s+<\\/tr>", "   ;   ", xml_string)
    xml_string = gsub("<\\/td>", " ||| ", xml_string)
    xml_string = gsub("<(.)*?>", "", xml_string)

    child = children[[1]]
    sib = xml2::xml_add_sibling(child, "table-contents")
    xml2::xml_set_text(sib, xml_string)
    }

  return(xml)
}


