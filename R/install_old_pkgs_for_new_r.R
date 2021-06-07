#' @title Install R packages for new major R version
#'
#' @description Install all non-based packages installed for previous version of R
#' for new version of R.
#'
#' @param dir_old character. Absolute path to old R version's library (i.e. folder
#' where packages were installed).
#'
#' @details Installs \code{caret} package with
#'
#' @return A character vector of all packages that were not successfully installed.
#'
#' @export
install_old_pkgs_for_new_r <- function(dir_base = "C:/Users/migue/OneDrive/Documents/R/win-library",
                                       type = "win.binary"){

  # preparation
  # -----------------

  # check old dir exists
  if(!dir.exists(dir_base)){
    stop("dir_base does not exist")
  }

  version_vec <- sort(as.numeric(list.dirs(dir_base, full.names = FALSE, recursive = FALSE)))
  n_version <- length(version_vec)
  dir_old <- file.path(dir_base, version_vec[n_version - 1])
  if(!dir.exists(dir_old)){
    stop("calculated dir_old does not exist")
  }
  dir_new <- file.path(dir_base, version_vec[n_version ])
  if(!dir.exists(dir_new)){
    stop("calculated dir_new does not exist")
  }

  # send installation messages to text file to ignore them
  #sink(file = file.path(tempdir(), "install_old_pkgs_for_new_r-output.txt"),
  #     type = "output")
  #on.exit(sink())

  # initial details
  dir_new <- .libPaths()[1]
  pkg_vec_old <- list.dirs(dir_old, recursive = FALSE, full.names = FALSE)
  pkg_vec_new <- list.dirs(dir_new, recursive = FALSE, full.names = FALSE)
  pkg_vec_install <- setdiff(pkg_vec_old, pkg_vec_new)
  n_pkg_init <- length(pkg_vec_install)
  n_pkg <- n_pkg_init
  n_pkg_new_init <- length(pkg_vec_new)
  n_pkg_old_init <- length(pkg_vec_old)

  print(paste0(n_pkg, " packages to install"))

  # caret
  # --------------

  if('caret' %in% pkg_vec_install){
    print("installing caret")
    suppressMessages(suppressWarnings(invisible(try(
      install.packages(
        'caret',
        dependencies = c('Depends', "Imports", "LinkingTo", "Suggests"),
        type = type,
        quiet = TRUE
        )))))
    pkg_vec_new <- list.dirs(dir_new, recursive = FALSE, full.names = FALSE)
    pkg_vec_install <- setdiff(pkg_vec_install, pkg_vec_new)
    n_installed <- n_pkg - length(pkg_vec_install)
    print("installed ", n_installed, " packages when attempting install of caret")
  }

  # BioConductor
  # ----------------

  bioc_ind <- 'BiocManager' %in% pkg_vec_new
  if(!bioc_ind) install.packages("BiocManager")

  pkg_vec_install <- intersect(pkg_vec_install, suppressMessages(BiocManager::available()))
  n_pkg <- length(pkg_vec_install)
  print(paste0("installing ", n_pkg, " packages from BioConductor"))
  k <- 1
  n_round_install <- 0
  BiocManager::install(
    pkg_vec_install,
    quiet = TRUE,
    type = type,
    ask = FALSE
  )


  # CRAN
  # -------------------

  pkg_vec_new <- list.dirs(dir_new, recursive = FALSE, full.names = FALSE)
  pkg_vec_install <- setdiff(pkg_vec_old, pkg_vec_new)
  pkg_vec_install <- intersect(pkg_vec_install, available.packages())
  n_pkg <- length(pkg_vec_install)
  print(paste0("installing ", n_pkg, " packages from CRAN"))
  install.packages(
    pkg_vec_install,
    dependencies = TRUE,
    type = type,
    quiet = TRUE
  )


  pkg_vec_old <- list.dirs(dir_old, recursive = FALSE, full.names = FALSE)
  pkg_vec_new <- list.dirs(dir_new, recursive = FALSE, full.names = FALSE)
  pkg_vec_install <- setdiff(pkg_vec_old, pkg_vec_new)

  n_pkg_installed <- length(pkg_vec_new) - n_pkg_new_init
  print(paste0("installed ", n_pkg_installed, " packages"))

  pkg_vec_install <- setdiff(pkg_vec_old, pkg_vec_new)
  n_pkg_remaining <- length(pkg_vec_install)
  print(paste0(n_pkg_remaining, " packages not successfully installed"))
  pkg_vec_install
}
