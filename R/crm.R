#' Calls CRExplorer
#'
#' For more details, see the documentation for CRExplorer at
#'
#' @param data_files A character vector with a list of filenames. If provided, data_path must be NULL.
#' @param data_path A character vector with a directory. If provided, data_files must be NULL.
#' @param cre_path Path to the CitedReferencesExplorerScript.jar file.
#' @param type Type of bibliometric records, can be "WOS", "Scopus" or "CrossRef". Defaults to "WOS".
#' @param cited_year_range Vector with minimum and maximum publication year for cited references to be included. Defaults to c(0,2100).
#' @param include_cited_wo_year Logical. Whether to include cited references without publication year. Defaults to c(0,2100).
#' @param citing_year_range Vector with minimum and maximum publication year for citing references to be included.
#' @param include_citing_wo_year Logical. Whether to include citing references without publication year.
#' @param sampling Type of sampling (useful when there are many references). Can be "Random" or "None" or "Systematic" or "Cluster". Defaults to "None" (i.e. use all references).
#' @param offset Used only when sampling is "Systematic".
#' @param import_max Maximum number of references to import. Defaults to 10,000.
#' @param save_files If NULL (default), the data will not be saved to disk. If you provide a path, it will save the results to the given folder.
#'
#' @return
#' @export
#'
#' @examples
run_CRE <- function (
  data_files = NULL,
  data_path = NULL,
  cre_path = NULL,
  type = "WOS",
  cited_year_range = c(0, 2100),
  include_cited_wo_year = T,
  citing_year_range = c(0, 2100),
  include_citing_wo_year = T,
  sampling = "None",
  offset = 3,
  import_max = 10000,
  save_files = NULL # if not NULL, should be a path
) {

  # check CRE installation
  if (is.null(cre_path)) {
    cre_path = system.file("java", "CitedReferencesExplorerScript.jar", package = "CRMilestones", mustWork = T)
  } else if (!file.exists(cre_path)) {
    stop(paste0("CRExplorer installation not found at ", cre_path, ".\n If you need to install it, go to https://andreas-thor.github.io/cre/."))
  }

  # make string for list of input files
  if (!is.null(data_files) & is.null(data_path)) {
    files_list = data_files
  } else if (is.null(data_files) & !is.null(data_path)) {
    files_list = list.files(data_files)
  } else {
    stop("One of data_files or data_path must be NULL.")
  }

  files_list_str = paste0('"', paste(files_list, collapse = '", "'), '"')

  # define export paths
  fn_graph = file.path(tempdir(), "graph.csv")
  fn_cited = file.path(tempdir(), "cited.csv")

  # prepare CRE script and run it
  cre_script = CRM_base_CRE_jar_script_string %>%
    str_replace("_FILE_LIST_", files_list_str) %>%
    str_replace("_TYPE_", type) %>%
    str_replace("_MIN_RPY_", as.character(cited_year_range[1])) %>%
    str_replace("_MAX_RPY_", as.character(cited_year_range[2])) %>%
    str_replace("_NO_RPY_", ifelse(include_cited_wo_year, "true", "false")) %>%
    str_replace("_MIN_PY_", as.character(citing_year_range[1])) %>%
    str_replace("_MAX_PY_", as.character(citing_year_range[2])) %>%
    str_replace("_NO_PY_", ifelse(include_citing_wo_year, "true", "false")) %>%
    str_replace("_SAMPLING_", sampling) %>%
    str_replace("_OFFSET_", as.character(offset)) %>%
    str_replace("_MAXCR_", as.character(import_max)) %>%
    str_replace("_EXPORT_GRAPH_", fn_graph) %>%
    str_replace("_EXPORT_CITED_", fn_cited)

  crs_file = file.path(tempdir(), "script.crs")
  readr::write_lines(cre_script, path = crs_file)

  # run external call
  jar_call = sprintf('java -jar "%s" "%s"', cre_path, crs_file)
  system(jar_call)

  # read files
  data_graph = read_csv(fn_graph, col_types = cols()) %>%
    select(-AVG) %>%
    `colnames<-`(c("Year", "# Cited References", "Deviation from 5-year median"))

  data_cited = read_csv(fn_cited, col_types = cols()) %>%
    select(ID, CR, N_CR, AU, RPY, J_N, DOI) %>%
    `colnames<-`(c("ID", "Reference", "Times Cited", "Author", "Year", "Journal", "DOI"))

  # save files if desired
  if (!is.null(save_files)) {
    write_tsv(data_graph, file.path(save_files, "CRE_data_graph.tsv"))
    write_tsv(data_cited, file.path(save_files, "CRE_data_cited_refs.tsv"))
  }

  # make rpys graph
  df = data_graph %>%
    pivot_longer(cols = -Year)

  p = ggplot(df) +
    aes(x = Year, y = value, color = name) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "", y = "# References", color = "")

  # return data frames
  list(rpys_graph = p, rpys_data = data_graph, references = data_cited)
}

identify_milestones <- function (CR_data, n_milestones = 10, n_refs = 1) {


  list(pubs = pubs, peak_years = peak_years, graph = graph)
}
