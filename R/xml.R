#' XML Node Schema
#'
#' Defines the XML schema of a node.
#'
#' @field schema_name the name of the schema
#' @field file_pattern regular expression pattern matching the file-name extension
#' @field extension the file-name extension
#' @field prefix the prefix that these documents have in the URI
#' @field atoms named character vector of xpath locations
#' @field constructor an RDF constructor function that can be called on
#'   a list of atoms extractor from a node in the schema
#' @field injector
#' @field atom_types
#' @field atom_lang
#' @field mongo_key
#' @field components a list of \code{XmlSchema} object containing the nested
#'   components in the node
#'
#'
#' @examples
#' taxonx = XmlSchema$new(schema_name = "taxonx", file_pattern = ".*\\.xml")
#'
#' @export
XmlSchema =
  R6::R6Class("xml_schema",
              public = list(
                schema_name = NULL,
                xpath = NA,
                file_pattern = NULL,
                extension = NULL,
                prefix = NULL,
                atoms = NULL,
                atom_types = NULL,
                atom_lang = NULL,
                constructor = NULL,
                injector = NULL,
                mongo_key = NULL,
                components = NULL,

                initialize =
                  function(schema_name = NA, xpath = NA, file_pattern = NA, extension = NA, prefix = NA, atoms = NA, atom_types = NULL, atom_lang = NA, constructor = NULL, injector = NULL, mongo_key = NULL, components = NULL)
                  {
                    self$schema_name = schema_name
                    self$xpath = xpath
                    self$file_pattern = file_pattern
                    self$extension = extension
                    self$prefix = prefix
                    self$atoms = atoms
                    self$atom_lang = atom_lang
                    self$atom_types = atom_types
                    self$constructor = constructor
                    self$injector = injector
                    self$mongo_key = mongo_key
                    self$components = components
                  }
              )
  )








#' RDF-ization of a Single XML File
#'
#' Converts an XML file to RDF and submits it to a triple store. Writes
#' the serialization in a file in the file system.
#'
#' @param filename locator, e.g. file path or URL, of the XML resource
#' @param xml_schema
#'        XML schema of the XML resource; one of
#'        taxpub taxonx or plazi_int
#' @param access_options endpoint to database to which to submit
#' @param serialization_dir directory where to dump intermediate serializations
#' @param serialization_format default is "turtle"
#' @param reprocess \code{logical}
#'
#' @return \code{logical}. TRUE if everything went OK. FALSE if there was a problem
#'
#'
#' @export
xml2rdf = function(filename, xml_schema, access_options, serialization_dir, reprocess, dry, grbio, taxon_discovery)
{
  # generate lookup functions


      inst_collection = mongolite::mongo(collection = "institutions", db = "test")
      checklistCol = mongolite::mongo(collection = "checklist", db = "openbiodiv")

  tryCatch(
    {
      xml = xml2::read_xml(filename)

      if (processing_status(xml)==FALSE){

     # xml_string = crosslinker(filename)
    #  xml = xml2::as_xml_document(xml_string)

      # taxon_discovery = "/home/mid/R_wd/openbidiv/tests/status_vocab_abbrev/taxon_discovery.txt"

      prefix = c(openbiodiv = "http://openbiodiv.net/")

      triples = ResourceDescriptionFramework$new()
      xml_schema = material_schema

      root_ident = root(node=xml, xml_schema = xml_schema, xml=xml, prefix = prefix, blank = FALSE)
      print(root_ident)
      processing_xml = xml
      xml2::write_xml(xml, filename)
      triples$set_context(root_ident)

      #set publisher id as a 'global variable' for mongo purposes
      publisher_name = xml2::xml_text(xml2::xml_find_all(processing_xml, xml_schema$atoms['publisher']))
      df = data.frame(label = publisher_name, mongo_key = c(publisher = NA), type = "publisher", orcid = NA, parent = NA, key = NA, publisher_id = NA, journal_id = NA, stringsAsFactors = FALSE)
      print(df)
      publisher_id = get_or_set_mongoid(df, prefix )
      publisher_id = paste0("<http://openbiodiv.net/",publisher_id,">")

      #set journal id as a 'global variable' for mongo purposes
      journal_name = xml2::xml_text(xml2::xml_find_all(processing_xml, xml_schema$atoms['journal']))
      df = data.frame(label = journal_name, mongo_key = c(journal = NA), type = "journal", orcid = NA, parent = NA, key = NA, publisher_id = NA, journal_id = NA, stringsAsFactors = FALSE)
      journal_id = get_or_set_mongoid(df, prefix )
      journal_id = paste0("<http://openbiodiv.net/",journal_id,">")



      #finds all institution codes and names and saves them in mongodb collection
      extract_inst_identifiers(processing_xml, root_id = root_ident, prefix = prefix, collection = inst_collection, grbio = grbio)
      #TODO: add into proj directory
      new_taxons = scan(taxon_discovery, character(), quote = "", sep="\n")

      print(filename)

      triples = node_extractor_en(
        node = processing_xml,
        xml_schema = xml_schema,
        reprocess = reprocess,
        triples = triples,
        prefix = prefix,
        new_taxons = new_taxons,
        dry = dry,
        filename = filename,
        root_id = root_ident,
        publisher_id = publisher_id,
        journal_id = journal_id
      )


      serialization = triples$serialize()
      #cat(serialization, file = "~/diptera.trig")
      save_serialization(serialization, serialization_dir)
      xml2::write_xml(processing_xml, filename)
      xml_collection = mongolite::mongo(collection = "xmls", db = "test")
      xml_str = toString(processing_xml)

      d = data.frame(
        xml = xml_str,
        filename = as.character(filename)
      )
      xml_collection$insert(d)

     # cat(fil, ename, file = "/opt/data/obkms/processed/obkms-processed.txt", append = TRUE)
    #  cat("\n", file = "/opt/data/obkms/processed/obkms-processed.txt", append = TRUE)


      return(TRUE)
    }
      },
    error = function(e)
    {
      warning(e)
      if (processing_status(xml)==FALSE){
      skipped = paste(filename, "\n")
      cat(skipped, file="/home/backend/OpenBiodiv/skipped.txt", append = TRUE)
      }
      return(FALSE)
    })
}

#' @export
general_collection =  mongolite::mongo(collection = "new_collection", db = "test")

#' @export
create_new_file = function(serialization_dir){
  time = Sys.time()
  time = gsub(":|\\s", "-", time)
  file = paste0(serialization_dir, "/",time, ".trig")
  file.create(file)
  file
}


#' @export
save_serialization = function(serialization,serialization_dir){
  df <- file.info(list.files(serialization_dir, full.names = T))
  #if there are no files in the serialization dir, create new one and write to it
  if (nrow(df) == 0){
    file = create_new_file(serialization_dir)
    cat(serialization, file = file, append = TRUE)
  }else{
    #if there are files, find the last modified one and write to it if its less than 200 Mb, or otherwise create a new one
    last_modified = rownames(df)[which.max(df$mtime)]
    file_size = file.info(last_modified)$size
    if(file_size < 200000000){
      #keep appending to file
      cat(serialization, file = last_modified, append = TRUE)
    }else{
      file = create_new_file(serialization_dir)
      #open new file and start appending to it
      cat(serialization,file = file,append = TRUE )
    }
  }
}





#' NOT USED ANYMORE
#' Get the OBKMS Id of an XML node, if not set, set it.
#'
#' Does not do any database lookups.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#'
#' @return the local id (not an identifier object)
#'
#' @export
get_or_set_obkms_id = function (node)
{
  if (is.na(xml2::xml_attr(node, "obkms_id")))
  {
    xml2::xml_attr(node, "obkms_id") = uuid::UUIDgenerate()
  }

  xml2::xml_attr(node, "obkms_id")
}


#' NOT USED ANYMORE
#' Set the OBKMS ID of an XML node according to new id format
#'
#' Does not do any database lookups.
#'
#' @param schema_name the name of the xml_schema
#' @param mongo_name the names of mongo_key
#' @return the local id (not an identifier object)
#'
#' @export
set_obkms = function(schema_name, mongo_name)
{
  random = uuid::UUIDgenerate()
  id = paste0(schema_name,"/", mongo_name, "/",random)
}






#' Find (Re-)Processing Status
#'
#' @param node \code{XML2} object
#'
#' @return \code{logical} TRUE if the node has been processed, FALSE otherwise
#' @export
processing_status = function(node)
{
  if (is.na(xml2::xml_attr(node, "obkms_process"))) {
    return (FALSE)
  }
  else {
    return (as.logical(xml2::xml_attr(node, "obkms_process")))
  }
}









#' Finds the Atoms in a XML Node
#'
#' @param xml the XML node
#' @param xpath the atom locations as a named character vector
#' @param atom_types the type (explicitly stated, not as xpath) of the atom
#' @param atom_lang the language (as xpath), if the xpath fails, will set to
#'   en (if string)
#'
#' @return list
#'
#' @export
find_literals = function (xml, xml_schema)
{
  rr = vector(mode = "list", length = length(xml_schema$atoms))
  names(rr) = names(xml_schema$atoms)
  for (nn in names(xml_schema$atoms)) {
    nodes = xml2::xml_find_all(xml, xml_schema$atoms[nn])
    #literals = paste(xml2::xml_text(nodes), collapse = " ")
    literals = xml2::xml_text(nodes)
   # literals = gsub("(?<=[a-z0-9])(?=[A-Z])", " ", literals,
                #    perl = TRUE)
    languages = tryCatch(xml2::xml_text(xml2::xml_find_all(xml,
                                                           xml_schema$atom_lang[nn])), error = function(e) {
                                                             NA
                                                           })
    rr[[nn]] = lapply(seq(along.with = literals), function(i) {
      literal(literals[i], xsd_type = xml_schema$atom_types[[nn]],
              lang = languages[i])
    })
  }
  return(rr)
}


#'
#find_identifiers = function(node, xml_schema)
#{
#  lapply(xml_schmea$, function(p)
#  {
#    xml2::xml_text(xml2::xml_find_all(xml, p))
#  })
#
#}





#' Get the Parent OBKS Id for an XML Node
#'
#' Does not do any database lookups. Recursive looks for the parent node until
#' it finds one with ID.
#'
#' @param node the XML node from which to take the ID; cannot be missing!
#' @param fullname if TRUE, returns a URI with the OBKMS base prefix
#'
#' @export
parent_id = function (node, fullname = FALSE )
{
  obkms_id = NA
  path = xml2::xml_path( node )
  # will repeat while we don't have an id and we aren't at the top
  while ( path != "/" && is.na( obkms_id ) ) {
    node = xml2::xml_parent( node )
    obkms_id = xml2::xml_attr( node, "obkms_id")
    path = xml2::xml_path( node )
  }

  if ( fullname )
  {
    return (  paste0( strip_angle( obkms$prefixes$`_base`) , obkms_id ) )
  }

  else return ( obkms_id )
}



#' Get the Root (Article) OBKMS Id for an XML Node
#'
#' Does not do any database lookups - only looks at XML
#'
#' @param node
#'
#' @export
root = function (node, xml_schema, xml, prefix = NA, blank = FALSE)
{
  #look for "new style" article id:
  #new_xpath = "//article/front/article-meta/article-id[@pub-id-type='other']"
  new_xpath = "//article/front/article-meta/uri[@content-type='arpha']"
  arpha_id = xml2::xml_text(xml2::xml_find_first(node, new_xpath))
  #if (grepl("urn:lsid:arphahub.com", arpha_id)==FALSE){
  #  arpha_id = NA
  #}
  root_node = xml2::xml_find_all(node, xpath = "/*")


  if(is.na(arpha_id)){
    id = identifier_new(node=root_node, xml=xml, mongo_key = xml_schema$mongo_key, prefix=prefix, blank = FALSE, publisher_id = NA, journal_id = NA)

    }else{
    #arpha_id = stringr::str_extract(arpha_id, "(?:.(?!\\/)){36}$") #extract uuid
    xml2::xml_attr(root_node, "obkms_id") = arpha_id #save to xml
    id = identifier(id = arpha_id, prefix = prefix)       #build identifier
  }

  xml2::xml_attr(node, "obkms_process") = "TRUE"


  title = xml2::xml_text(xml2::xml_find_first(xml, xml_schema$atoms['title']))
  doi = xml2::xml_text(xml2::xml_find_first(xml, xml_schema$atoms['doi']))
  save_to_mongo(key = toString(id$uri), value =  title, type = "article", orcid = NA, parent = doi, publisher_id = NA, journal_id = NA, collection = general_collection)
  id
}
















#' Strips all XML tags
#'
#' @param xmldoc
#'
#' @return a vector of the document as plain text
#' @export
remove_all_tags = function(xmldoc) {
  sapply(xml_find_all(xmldoc, xpath = paste0("//", "p")), function(p) {
    xml_text(p) = paste(xml_text(p), "\n")
  })
  unescape_html(do.call(pasteconstr(" "), xml_find_all(xmldoc, "//text()")))
}

#' Unescpae HTML Characters
#'
#' @param str input string
#' @return string without the escape characters
#' @export
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"

  )))
}





#' Simpler version of find literals
#'
#' Finds the Atoms in a XML Node
#'
#' @param xml the XML node
#' @param xpath the atom locations as a named character vector
#'
#' @return list
#'
#' @export
find_atoms =
  function(xml, xpath) {
    lapply(xpath, function(p)
    {
      xml2::xml_text(xml2::xml_find_all(xml, p))
    })
  }




#' Standard injector. Sets the id from the XPATH
#'
#' @param obkms_id
#' @param xml_node
#'
#' @return
#' @export
standard_injector = function(obkms_id, xml_node)
{
  xml2::xml_attr(xml_node, "obkms_id") = obkms_id
}

