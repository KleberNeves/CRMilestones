run_CRE <- function (
  data_path = "...",
  cre_path = CREXPLORER_PATH,
  type = "",
  cited_year_range = c(1500, 2022), # default to c(0, today + 1?)
  citing_year_range = c(1500, 2022),
  sampling = "None",# "Random", "None", "Systematic", "Cluster",
  offset = 3, # Required if sampling == "Systematic"
  import_max = 10000,
  save_files = NULL # if not NULL, should be a path
) {

  # check CRE installation

  # run CRE import

  # run CRE export

  # read files

  # save files if desired

  # return data frames
  list(rpys = rpys, references = references)
}

identify_milestones <- function (CR_data, n_milestones = 10, n_refs = 1) {


  list(pubs = pubs, peak_years = peak_years, graph = graph)
}

install_CRE <- function () {

  # shows clickable download link and brief instructions to set the path, recommends vignette

}
