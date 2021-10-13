#' Process and disambiguate references using CRExplorer
#'
#' *data_files* or *data_path* serve to specify the files to be read and
#' *save_files* indicate whether to save the output to disk. The rest of
#' the arguments are passed to CRExplorer.
#'
#' For more details about how the arguments are used, see the documentation
#' for CRExplorer at https://andreas-thor.github.io/cre/manual.pdf
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
#' @return A list containing two data frames, one with the references (citing and cited) and one with the data underlying the RPYS graph
#' @importFrom magrittr %>%
#' @export
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
    files_list = paste0(data_path,
                        "/", list.files(data_path))
  } else {
    stop("One of data_files or data_path must be NULL.")
  }

  files_list_str = paste0('"', paste(files_list, collapse = '", "'), '"')

  # define export paths
  fn_graph = file.path(normalizePath(tempdir(), winslash = "/"), "graph.csv")
  fn_cited = file.path(normalizePath(tempdir(), winslash = "/"), "cited.csv")

  # prepare CRE script and run it
  cre_script = CRM_base_CRE_jar_script_string %>%
    stringr::str_replace("_FILE_LIST_", files_list_str) %>%
    stringr::str_replace("_TYPE_", type) %>%
    stringr::str_replace("_MIN_RPY_", as.character(cited_year_range[1])) %>%
    stringr::str_replace("_MAX_RPY_", as.character(cited_year_range[2])) %>%
    stringr::str_replace("_NO_RPY_",
                         ifelse(include_cited_wo_year, "true", "false")) %>%
    stringr::str_replace("_MIN_PY_", as.character(citing_year_range[1])) %>%
    stringr::str_replace("_MAX_PY_", as.character(citing_year_range[2])) %>%
    stringr::str_replace("_NO_PY_",
                         ifelse(include_citing_wo_year, "true", "false")) %>%
    stringr::str_replace("_SAMPLING_", sampling) %>%
    stringr::str_replace("_OFFSET_", as.character(offset)) %>%
    stringr::str_replace("_MAXCR_", as.character(import_max)) %>%
    stringr::str_replace("_EXPORT_GRAPH_", fn_graph) %>%
    stringr::str_replace("_EXPORT_CITED_", fn_cited)

  crs_file = file.path(normalizePath(tempdir(), winslash = "/"), "script.crs")
  readr::write_lines(cre_script, file = crs_file)

  # run external call
  if (Sys.info()[1] == "Windows") {
	  jar_call = sprintf("\"%s\" -jar \"%s\" \"%s\"", JAVA_PATH, cre_path, crs_file) %>%
      stringr::str_replace_all("/","\\\\")
  } else {
	  jar_call = sprintf("java -jar \"%s\" \"%s\"", cre_path, crs_file)
  }

  system(jar_call)

  # read files
  data_graph = readr::read_csv(fn_graph, col_types = readr::cols()) %>%
    dplyr::select(-AVG) %>%
    `colnames<-`(c("Year", "# Cited References", "Deviation from 5-year median"))

  data_cited = suppressWarnings(suppressMessages(readr::read_csv(fn_cited,
                                                               col_types = readr::cols(), na = c("", "null")))) %>%
  dplyr::select(1, 8, 9, 10, 3, 18, 25, 26, 27,
                28, 41) %>% `colnames<-`(c("ID_Citing", "Title_Citing",
                                              "Year_Citing", "Journal_Citing", "Author_Citing", "DOI_Citing",
                                              "ID_Cited", "Reference_Cited", "Year_Cited", "Times_Cited",
                                              "DOI_Cited"))

  # save files if desired
  if (!is.null(save_files)) {
    readr::write_tsv(data_graph, file.path(normalizePath(save_files, winslash = "/"), "CRE_data_graph.tsv"))
    readr::write_tsv(data_cited, file.path(normalizePath(save_files, winslash = "/"), "CRE_cited_citing_refs.tsv"))
  }

  # return data frames
  list(references = data_cited, graph_data = data_graph)
}


#' Identify milestone years and papers
#'
#' Given a data frame of cited references, it runs a multi-RPYS analysis
#' to find the milestones. Details of the algorithm can be found in the
#' following references:
#'
#' Comins & Hussey (2015). Compressing multiple scales of impact detection by Reference Publication Year Spectroscopy. Journal of Informetrics. DOI:10.1016/j.joi.2015.03.003
#' Comins & Leydesdorff (2017). Citation algorithms for identifying research milestones driving biomedical innovation. Scientometrics. DOI:10.1007/s11192-016-2238-1
#'
#' @param CR_data A list containing the bibliometric data frame, as returned by *run_CRE()*
#' @param n_milestones How many milestone years will be selected (default is 10).
#' @param n_refs How many references from each year will be selected (default is 1).
#' @param min_times_cited References cited strictly less than this argument will be excluded from the analysis (default is 2)
#' @param cited_year_range Passed to the plotting function (the analysis will take up the full range)
#'
#' @return A list containing a data frame with the milestone papers, a vector of the milestone years and two options of RPYS figures (ggplot objects)
#' @importFrom magrittr %>%
#' @export
identify_milestones <- function (CR_data, n_milestones = 10, n_refs = 1, min_times_cited = 2, cited_year_range = NULL) {

  # if (is.null(cited_year_range)) {
  #   max_py = min(max(CR_data$references$Year_Cited),
  #                lubridate::year(lubridate::now()))
  #   cited_year_range = c(1900, max_py)
  # }

  # Find peak years
  df = CR_data$references %>%
    dplyr::filter(Times_Cited >= min_times_cited) %>%
    dplyr::count(Year_Citing, Year_Cited) %>%
    dplyr::group_by(Year_Citing) %>%
    dplyr::mutate(
      dt = detrend(n),
      r = rank(dt, ties.method = "min"),
      r = 100 + r - max(r)
    )

  dfs = df %>% dplyr::group_by(Year_Cited) %>%
    dplyr::summarise(M = mean(r, na.rm = T))

  peaks = dfs %>%
    dplyr::slice_max(order_by = M, n = n_milestones) %>%
    dplyr::pull(Year_Cited)

  df = df %>% dplyr::mutate(Peak = Year_Cited %in% peaks)

  p1 = plot_multi_rpys(CR_data, cited_year_range = cited_year_range) +
    ggplot2::geom_vline(xintercept = peaks - 0.5, size = 1, alpha = 0.4, color = "red") +
    ggplot2::geom_vline(xintercept = peaks + 0.5, size = 1, alpha = 0.4, color = "red") +
    ggplot2::theme(legend.position = "none")

  p2 = plot_rpys(CR_data, cited_year_range = cited_year_range) +
    ggplot2::geom_vline(xintercept = peaks - 0.5, size = 1, alpha = 0.4, color = "red") +
    ggplot2::geom_vline(xintercept = peaks + 0.5, size = 1, alpha = 0.4, color = "red")

  p = cowplot::plot_grid(plotlist = list(p1, p2),
                         ncol = 1, align = "h", axis = "tblr")

  # Find top references of each peak year
  df = CR_data$references %>%
    dplyr::filter(Year_Cited %in% peaks) %>%
    dplyr::group_by(Year_Cited) %>%
    dplyr::distinct(ID_Cited, .keep_all = T) %>%
    dplyr::slice_max(order_by = Times_Cited, n = n_refs) %>%
    dplyr::select(ID_Cited, Reference_Cited, Year_Cited, Times_Cited, DOI_Cited)

  list(pubs = df, rpys = p1, rpys_stacked = p, peak_years = peaks)
}



#' Plots a single RPYS plot
#'
#' The plot shows the frequency of cited references (CR) in each year, as well as
#' the detrended value (#CR - median of the #CR of 5 years around (±2)).
#'
#' @param CR_data A list containing the bibliometric data frame, as returned by *run_CRE()*
#' @param cited_year_range The range of years to plot. Default is NULL, which the function transforms to "from 1900 to the most recent year in the dataset".
#'
#' @return A ggplot.
#' @importFrom magrittr %>%
#' @export
plot_rpys <- function (CR_data, cited_year_range = NULL) {

  df = CR_data$references %>%
    dplyr::count(Year_Cited) %>%
    dplyr::rename(`# References` = n) %>%
    dplyr::mutate(`Deviation from 5-yr Median` = detrend(`# References`)) %>%
    tidyr::pivot_longer(-Year_Cited)

  if (is.null(cited_year_range)) {
    max_py = min(max(df$Year_Cited), lubridate::year(lubridate::now()))
    cited_year_range = c(1900, max_py)
  }

  p = ggplot2::ggplot(df) +
    ggplot2::aes(x = Year_Cited, y = value, color = name) +
    ggplot2::geom_line() +
    ggplot2::xlim(cited_year_range) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "", y = "# Cited References", color = "")

  p
}


#' Plots a multi-RPYS plot
#'
#' The plot shows a matrix where the rows indicate the citing year and columns the cited year. The color indicates the frequency of the cited year for that citing year (i.e. colors are relative to each row). However, to be comparable across citing years, the color indicates the ranking, not the absolute values of the deviations from the median of the 5-years around it.
#'
#' In other words, a simple RPYS analysis is done for each citing year, the de-trended data is calculated (i.e. deviation from the local median) and the de-trended data is then ranked. This is the same procedure followed for identifying milestones.
#'
#' @param CR_data A list containing the bibliometric data frame, as returned by *run_CRE()*
#' @param cited_year_range The range of years to plot. Default is NULL, which the function transforms to "from 1900 to the most recent year in the dataset".
#'
#' @return A ggplot.
#' @importFrom magrittr %>%
#' @export
plot_multi_rpys <- function (CR_data, cited_year_range = NULL) {

  df = CR_data$references %>%
    dplyr::count(Year_Citing, Year_Cited) %>%
    dplyr::group_by(Year_Citing) %>%
    dplyr::mutate(
      dt = detrend(n),
      r = rank(dt, ties.method = "min"),
      r = 100 + r - max(r)
    )

  if (is.null(cited_year_range)) {
    max_py = min(max(df$Year_Cited), lubridate::year(lubridate::now()))
    cited_year_range = c(1950, max_py)
  }

  p = ggplot2::ggplot(df) +
    ggplot2::aes(x = Year_Cited, y = Year_Citing, fill = r) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#f2f0df", mid = "#98db81", high = "#08004f", midpoint = 60) +
    ggplot2::scale_x_continuous(limits = cited_year_range, breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = "Cited References", y = "Citing Documents", fill = "Ranked Absolute Deviation")

  p
}

#' De-trends a numeric vector
#'
#' Given a numeric vector, it calculates a rolling median and then from each element, it subtracts the local median. The window for the median is centered on the element, so if the size is 5, it takes the element itself and the elements up to ±2 positions from it. If the size is even, it's rounded down (e.g. window_size = 6 is the same as window_size = 5).
#'
#' @param x A numeric vector.
#' @param window_size A length for the rolling window to calculate the median.
#'
#' @return A numeric vector, de-trended.
#' @export
detrend <- function (x, window_size = 5) {
  half_size = floor((window_size - 1) / 2)
  m = numeric(length(x))
  for (i in 1:length(x)) {
    lower_i = max(1, i - half_size)
    upper_i = min(length(x), i + half_size)
    m[i] = stats::median(x[lower_i:upper_i])
  }
  x - m
}
