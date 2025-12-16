#'
#'
#' @keywords internal
#'
#' 
skip_if_no_satscan <- function(ss_path = "/Applications/SaTScan.app/Contents/app/satscan") {
  testthat::skip_if_not(file.exists(ss_path), message = "SaTScan is not installed or not found")
}

#'
#' Extract results from SaTScan-text-based output 
#' 
#' @param file SaTScan-text-based output result given as "main" to be parsed
#' 
#' @keywords internal
#' 
#' 
parse_clusters <- function(file) {

  ## Subset SaTScan-text-based output file ----
  txt <- file$main

  ## Find line indices for cluster starts and coordinates
  cluster_start <- stringr::str_which(txt, "^\\d+\\.Location IDs included\\.:")
  coords <- stringr::str_which(txt, "^\\s+Coordinates / radius")

  ## Extract the name of the survey area ----
  area_name <- stringr::str_which(txt, "^[Case File]+\\:")

  ## Guard: align pairs if counts differ
  n <- min(length(cluster_start), length(coords))
  if (n == 0) return(tibble::tibble())

  out <- vector("list", n)

  for (j in seq_len(n)) {
    idx <- cluster_start[j]
    coord <- coords[j]

    ## Collect all lines containing IDs up to the coordinates line
    id_block <- txt[idx:(coord - 1)]
    ids_vec <- stringr::str_extract_all(id_block, "[0-9]+") |> unlist()

    ## Drop the leading cluster number (e.g., "1", "2") if present
    location_ids <- if (length(ids_vec) > 1) {
      paste(ids_vec[-1], collapse = ",")
    } else {
      NA_character_
    }

    ## Build tibble for this cluster (offsets are stable in SaTScan output)
    out[[j]] <- tibble::tibble(

      ### Summary metadata (from fixed lines in your example) ----
      survey_area = as.character(stringr::str_extract_all(basename(txt[[area_name]]), "^[^.]+")),
      nr_EAs = as.integer(stringr::str_extract(txt[18], "\\d+")),
      total_children = as.integer(stringr::str_extract(txt[19], "\\d+")),
      total_cases = as.integer(stringr::str_extract(txt[20], "\\d+")),
      `%_cases` = as.double(stringr::str_extract(txt[21], "\\d+\\.?\\d*")),

      ### Cluster-specific ----
      location_ids = location_ids,
      geo = stringr::str_extract(txt[coord],
        "\\d+\\.\\d+\\s+\\w\\,\\s+\\d+\\.\\d+\\s+\\w"),
      radius = stringr::str_extract(txt[coord],"[0-9]+\\.[0-9]+\\s*km"),
      span = stringr::str_extract(txt[coord + 1], "[0-9]+\\.[0-9]+\\s*km"),
      children = as.integer(stringr::str_extract(txt[coord + 2], "\\d+")),
      n_cases = as.integer(stringr::str_extract(txt[coord + 3], "\\d+")),
      expected_cases = as.double(stringr::str_extract(txt[coord + 4], "[0-9]+\\.[0-9]+")),
      observedExpected = as.double(stringr::str_extract(txt[coord + 5], "[0-9]+\\.[0-9]+")),
      relative_risk = as.double(stringr::str_extract(txt[coord + 6], "[0-9]+\\.[0-9]+")),
      `%_cases_in_area` = as.double(stringr::str_extract(txt[coord + 7], "[0-9]+\\.?[0-9]*")),
      log_lik_ratio = as.double(stringr::str_extract(txt[coord + 8], "[0-9]+\\.[0-9]+")),
      pvalue = as.double(stringr::str_extract(txt[coord + 9], "[0-9]+\\.?[0-9]*")),

      ### Check if IPC AMN reqs for survey disaggregation is met ----
      ipc_amn = ifelse(
        length(strsplit(location_ids, ",\\s*")[[1]]) >= 5 & !is.na(children) & children >= 100,
        "yes", "no"
      )
    ) 
  }

  ## Return binded results ----
  dplyr::bind_rows(out)
}