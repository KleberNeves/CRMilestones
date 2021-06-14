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

  data_cited = suppressWarnings(suppressMessages(
    read_csv(fn_cited, col_types = cols(), na = c("", "null"))
  )) %>%
    select(ID, TI, PY, SO, AU, DI, ID_1, CR, RPY, N_CR, DOI) %>%
    `colnames<-`(c("ID_Citing", "Title_Citing", "Year_Citing", "Journal_Citing",
                   "Author_Citing", "DOI_Citingr", "ID_Cited", "Reference_Cited",
                   "Year_Cited", "Times_Cited", "DOI_Cited"))

  # save files if desired
  if (!is.null(save_files)) {
    write_tsv(data_graph, file.path(save_files, "CRE_data_graph.tsv"))
    write_tsv(data_cited, file.path(save_files, "CRE_cited_citing_refs.tsv"))
  }

  # return data frames
  list(references = data_cited, graph_data = data_graph)
}

identify_milestones <- function (CR_data, n_milestones = 10, n_refs = 1, min_times_cited = 2, cited_year_range = NULL) {

  if (is.null(cited_year_range)) {
    max_py = min(max(CR_data$references$Year_Cited),
                 lubridate::year(lubridate::now()))
    cited_year_range = c(1900, max_py)
  }

  # Find peak years
  df = CR_data$references %>%
    filter(Times_Cited >= min_times_cited) %>%
    count(Year_Citing, Year_Cited) %>%
    group_by(Year_Citing) %>%
    mutate(
      dt = detrend(n),
      r = rank(dt, ties.method = "min"),
      r = 100 + r - max(r)
    )

  dfs = df %>% group_by(Year_Cited) %>%
    summarise(M = mean(r), SE = sd(r) / n(), D = M / SE)

  peaks = dfs %>% slice_max(order_by = M, n = n_milestones) %>% pull(Year_Cited)

  df = df %>% mutate(Peak = Year_Cited %in% peaks)

  p1 = plot_multi_rpys(CR_data, cited_year_range = cited_year_range) +
    geom_vline(xintercept = peaks - 0.5, size = 1, alpha = 0.4, color = "red") +
    geom_vline(xintercept = peaks + 0.5, size = 1, alpha = 0.4, color = "red") +
    theme(legend.position = "none")

  p2 = plot_rpys(CR_data, cited_year_range = cited_year_range) +
    geom_vline(xintercept = peaks - 0.5, size = 1, alpha = 0.4, color = "red") +
    geom_vline(xintercept = peaks + 0.5, size = 1, alpha = 0.4, color = "red")

  p = cowplot::plot_grid(plotlist = list(p1, p2),
                         ncol = 1, align = "h", axis = "tblr")

  # Find top references of each peak year
  df = CR_data$references %>%
    filter(Year_Cited %in% peaks) %>%
    group_by(Year_Cited) %>%
    distinct(ID_Cited, .keep_all = T) %>%
    slice_max(order_by = Times_Cited, n = n_refs) %>%
    select(ID_Cited, Reference_Cited, Year_Cited, Times_Cited, DOI_Cited)

  list(pubs = df, rpys = p1, rpys_stacked = p, peak_years = peaks)
}

plot_rpys <- function (CR_data, cited_year_range = NULL) {

  df = CR_data$references %>%
    count(Year_Cited) %>%
    rename(`# References` = n) %>%
    mutate(`Deviation from 5-yr Median` = detrend(`# References`)) %>%
    pivot_longer(-Year_Cited)

  if (is.null(cited_year_range)) {
    max_py = min(max(df$Year_Cited), lubridate::year(lubridate::now()))
    cited_year_range = c(1900, max_py)
  }

  p = ggplot(df) +
    aes(x = Year_Cited, y = value, color = name) +
    geom_line() +
    xlim(cited_year_range) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "", y = "# Cited References", color = "")

  p
}

plot_multi_rpys <- function (CR_data, cited_year_range = NULL) {

  df = CR_data$references %>%
    count(Year_Citing, Year_Cited) %>%
    group_by(Year_Citing) %>%
    mutate(
      dt = detrend(n),
      r = rank(dt, ties.method = "min"),
      r = 100 + r - max(r)
    )

  if (is.null(cited_year_range)) {
    max_py = min(max(df$Year_Cited), lubridate::year(lubridate::now()))
    cited_year_range = c(1950, max_py)
  }

  p = ggplot(df) +
    aes(x = Year_Cited, y = Year_Citing, fill = r) +
    geom_tile() +
    scale_fill_gradient2(low = "#f2f0df", mid = "#98db81", high = "#08004f", midpoint = 60) +
    scale_x_continuous(limits = cited_year_range, breaks = scales::pretty_breaks(n = 8)) +
    scale_y_reverse() +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid = element_blank()) +
    labs(x = "Cited References", y = "Citing Documents", fill = "Ranked Absolute Deviation")

  p
}

detrend <- function (x, window_size = 5) {
  half_size = floor((window_size - 1) / 2)
  m = numeric(length(x))
  for (i in 1:length(x)) {
    lower_i = max(1, i - half_size)
    upper_i = min(length(x), i + half_size)
    m[i] = median(x[lower_i:upper_i])
  }
  x - m
}
