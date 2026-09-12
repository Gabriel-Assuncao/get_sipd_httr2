#' Download, label, deflate and create survey design object for POF microdata
#' @description Core function of package. With this function only, the user can download a POF microdata from a year and get a sample design object ready to use with \code{survey} package functions.
#' @import dplyr httr2 magrittr projmgr readr readxl survey tibble timeDate utils
#' @param year The year of the data to be downloaded. Must be a number equal to 2008 or 2017 or 2024. Vector not accepted.
#' @param selected Logical value. If \code{TRUE}, the specific questionnaire for selected resident will be used. If \code{FALSE}, the basic questionnaire for household and residents will be used.
#' @param anthropometry Logical value. If \code{TRUE}, the specific questionnaire for the anthropometry module of the selected resident will be used. If \code{FALSE}, the questionnaire defined by the argument \code{selected} of this function will be used. This argument will be used only if \code{year} is equal to 2017 or 2024.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variables will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design} or \code{svyrep.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param reload Logical value. If \code{TRUE}, will re-download the files even if they already exist in the save directory. If \code{FALSE}, will be checked if the files already exist in the save directory and the download will not be performed repeatedly, be careful with coinciding names of microdata files.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from POF and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{read_pof} for reading POF microdata.\cr \link[POFIBGE]{pof_labeller} for labeling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_deflator} for adding deflator variables to POF microdata.\cr \link[POFIBGE]{pof_design} for creating POF survey design object.\cr \link[POFIBGE]{pof_example} for getting the path of the POF toy example files.
#' @examples
#' \donttest{
#' pof.svy <- get_pof_httr2(year=2017, selected=FALSE, anthropometry=FALSE, vars=c("V0407","V0408"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, savedir=tempdir())
#' # Calculating proportion of people's purchase of goods or services
#' if (!is.null(pof.svy)) survey::svymean(x=~V0408, design=pof.svy, na.rm=TRUE)
#' pof.svy2 <- get_pof_httr2(year=2017, selected=TRUE, anthropometry=FALSE, vars=c("V4104","V4105"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, savedir=tempdir())
#' # Calculating proportion of reasons for non-routine trips indicated by people
#' if (!is.null(pof.svy2)) survey::svymean(x=~V4104, design=pof.svy2, na.rm=TRUE)
#' pof.svy3 <- get_pof_httr2(year=2017, selected=FALSE, anthropometry=TRUE, vars=c("V7102","V7104"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, savedir=tempdir())
#' # Calculating proportion of people who followed some type of diet
#' if (!is.null(pof.svy3)) survey::svymean(x=~V7104, design=pof.svy3, na.rm=TRUE)}
#' @export

get_pof_httr2 <- function(year, selected = FALSE, anthropometry = FALSE, vars = NULL,
                     labels = TRUE, deflator = TRUE, design = TRUE, reload = TRUE, savedir = tempdir())
{
  message("The POFIBGE package was archived due to the impossibility of restructuring the files related to the survey microdata.\n")
  return(NULL)
  message("The get_pof function is under development and will be available soon in package POFIBGE.\n")
  return(NULL)
  if (year != 2008 & year != 2017 & year != 2024) {
    message("Year must be equal to 2008 or 2017 or 2024.\n")
    return(NULL)
  }
  if (!(selected %in% c(TRUE, FALSE))) {
    selected <- FALSE
    message("Invalid value provided for selected argument, so default value FALSE was set to this argument.\n")
  }
  if (!(anthropometry %in% c(TRUE, FALSE))) {
    anthropometry <- FALSE
    message("Invalid value provided for anthropometry argument, so default value FALSE was set to this argument.\n")
  }
  if (!(labels %in% c(TRUE, FALSE))) {
    labels <- TRUE
    message("Invalid value provided for labels argument, so default value TRUE was set to this argument.\n")
  }
  if (!(deflator %in% c(TRUE, FALSE))) {
    deflator <- TRUE
    message("Invalid value provided for deflator argument, so default value TRUE was set to this argument.\n")
  }
  if (!(design %in% c(TRUE, FALSE))) {
    design <- TRUE
    message("Invalid value provided for design argument, so default value TRUE was set to this argument.\n")
  }
  if (!(reload %in% c(TRUE, FALSE))) {
    reload <- TRUE
    message("Invalid value provided for reload argument, so default value TRUE was set to this argument.\n")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", savedir), "'.\n")
  }
  if (savedir != tempdir()) {
    printpath <- TRUE
  }
  else {
    printpath <- FALSE
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  if (year == 2008) {
    pofyear <- "2008_2009"
  }
  else if (year == 2017) {
    pofyear <- "2017_2018"
  }
  else if (year == 2024) {
    pofyear <- "2024_2025"
  }
  else {
    pofyear <- ""
  }
  ftpdir <- paste0("https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_", pofyear, "/Microdados/")
  if (!projmgr::check_internet()) {
    message("The internet connection is unavailable.\n")
    return(NULL)
  }
  if (httr2::req_perform(req=httr2::request(base_url=ftpdir) |> httr2::req_timeout(seconds=60)) |> httr2::resp_is_error()) {
    message("The microdata server is unavailable.\n")
    return(NULL)
  }
  restime <- getOption("timeout")
  on.exit(options(timeout=restime))
  options(timeout=max(600, restime))
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", httr2::req_perform(req=httr2::request(base_url=ftpdata)) |> httr2::resp_body_string()), "\n")), "<a href=[[:punct:]]")), ".zip"))
  dataname <- datayear[which(startsWith(datayear, paste0("POF_", year)))]
  if (length(dataname) == 0) {
    message("Data unavailable for selected year.\n")
    return(NULL)
  }
  else if (length(dataname) > 1) {
    message("There is more than one file available for the requested microdata, please contact the package maintainer.\n")
    return(NULL)
  }
  else {
    dataname <- paste0(dataname, ".zip")
  }
  if (reload == FALSE & file.exists(paste0(savedir, "/", dataname))) {
    message("The reload argument was defined as FALSE and the file of microdata was already downloaded, so the download process will not execute again.\n")
  }
  else {
    httr2::req_perform(req=httr2::request(base_url=paste0(ftpdata, dataname)) |> httr2::req_progress(), path=paste0(savedir, "/", dataname), verbosity=1)
    if (suppressWarnings(class(try(utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir), silent=TRUE)) == "try-error")) {
      message("The directory defined to save the downloaded data is denied permission to overwrite the existing files, please clear or change this directory.\n")
      return(NULL)
    }
    if (reload == FALSE) {
      message("The definition of FALSE for the reload argument will be ignored, since the file of microdata was not downloaded yet.\n")
    }
  }
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", httr2::req_perform(req=httr2::request(base_url=paste0(ftpdir, "Documentacao/"))) |> httr2::resp_body_string()), "\n")), "<a href=[[:punct:]]")), ".zip"))
  inputzip <- paste0(docfiles[which(startsWith(docfiles, "Dicionario_e_input"))], ".zip")
  if (reload == FALSE & file.exists(paste0(savedir, "/Dicionario_e_input.zip"))) {
    message("The reload argument was defined as FALSE and the file of dictionary and input was already downloaded, so the download process will not execute again.\n")
  }
  else {
    httr2::req_perform(req=httr2::request(base_url=paste0(ftpdir, "Documentacao/", inputzip)) |> httr2::req_progress(), path=paste0(savedir, "/Dicionario_e_input.zip"), verbosity=1)
    if (reload == FALSE) {
      message("The definition of FALSE for the reload argument will be ignored, since the file of dictionary and input was not downloaded yet.\n")
    }
  }
  utils::unzip(zipfile=paste0(savedir, "/Dicionario_e_input.zip"), exdir=savedir)
  microdataname <- dir(savedir, pattern=paste0("^POF_", year, ".*\\.txt$"), ignore.case=FALSE)
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$mtime),])[length(microdatafile)]
  inputname <- dir(savedir, pattern=paste0("^input_POF_", year, ".*\\.txt$"), ignore.case=FALSE)
  inputfile <- paste0(savedir, "/", inputname)
  inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$mtime),])[length(inputfile)]
  data_pof <- POFIBGE::read_pof(microdata=microdatafile, input_txt=inputfile, vars=vars)
  if (anthropometry == TRUE & (year == 2017 | year == 2024) & c("W001") %in% names(data_pof)) {
    data_pof <- data_pof[(data_pof$W001 == "1" & !is.na(data_pof$W001)),]
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0028", "V00281", "V00282", "V00283", "V0029", "V00291", "V00292", "V00293"))]
    if (selected == TRUE) {
      message("The definition of TRUE for the selected argument will be ignored, since the anthropometry argument was also defined as TRUE.\n")
    }
  }
  else if (selected == TRUE | (selected == FALSE & anthropometry == TRUE) & c("M001") %in% names(data_pof)) {
    data_pof <- data_pof[(data_pof$M001 == "1" & !is.na(data_pof$M001)),]
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0028", "V00281", "V00282", "V00283", "V0030", "V00301", "V00302", "V00303"))]
    if (selected == FALSE) {
      message("The selected argument was defined as true for the use of the anthropometry module, since the year is different from 2017 or 2024.\n")
    }
  }
  else {
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0029", "V00291", "V00292", "V00293", "V0030", "V00301", "V00302", "V00303"))]
  }
  if (labels == TRUE) {
    if (exists("pof_labeller", where="package:POFIBGE", mode="function")) {
      dicname <- dir(savedir, pattern=paste0("^dicionario_POF_microdados_", year, ".*\\.xls$"), ignore.case=FALSE)
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$mtime),])[length(dicfile)]
      data_pof <- POFIBGE::pof_labeller(data_pof=data_pof, dictionary.file=dicfile)
    }
    else {
      message("Labeller function is unavailable in package POFIBGE.\n")
    }
  }
  if (deflator == TRUE) {
    if (exists("pof_deflator", where="package:POFIBGE", mode="function")) {
      ftpdef <- ("https://ftp.ibge.gov.br/Orcamentos_Familiares/Documentacao_Geral/")
      deffiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", httr2::req_perform(req=httr2::request(base_url=ftpdef)) |> httr2::resp_body_string()), "\n")), "<a href=[[:punct:]]")), ".zip"))
      defzip <- paste0(deffiles[which(startsWith(deffiles, "Deflatores"))], ".zip")
      if (reload == FALSE & file.exists(paste0(savedir, "/Deflatores.zip"))) {
        message("The reload argument was defined as FALSE and the file of deflator was already downloaded, so the download process will not execute again.\n")
      }
      else {
        httr2::req_perform(req=httr2::request(base_url=paste0(ftpdef, defzip)) |> httr2::req_progress(), path=paste0(savedir, "/Deflatores.zip"), verbosity=1)
        if (reload == FALSE) {
          message("The definition of FALSE for the reload argument will be ignored, since the file of deflator was not downloaded yet.\n")
        }
      }
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^deflator_POF.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$mtime),])[length(deffile)]
      data_pof <- POFIBGE::pof_deflator(data_pof=data_pof, deflator.file=deffile)
    }
    else {
      message("Deflator function is unavailable in package POFIBGE.\n")
    }
  }
  if (design == TRUE) {
    if (exists("pof_design", where="package:POFIBGE", mode="function")) {
      data_pof <- POFIBGE::pof_design(data_pof=data_pof)
    }
    else {
      message("Sample design function is unavailable in package POFIBGE.\n")
    }
  }
  if (printpath == TRUE) {
    message("Paths of files downloaded in this function at the save directory provided are:")
    message(paste0(list.files(path=savedir, pattern="POF", full.names=TRUE), collapse="\n"), "\n")
  }
  return(data_pof)
}
