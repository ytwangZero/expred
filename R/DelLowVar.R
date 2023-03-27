#' @title Delete variables with low variance
#' @description Whether to delete variables with low variance. The default option is "yes".
#'     If skipped, it may result in failure to build models.
#' @usage DelLowVar(eSet = eSet)
#' @param eSet An R6 class object.
#' @details
#' @return An R6 class object containing the variable(s) with acceptable variance.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' eSet = DelLowVar(eSet = eSet)
#' @author Bin Wang
DelLowVar <- function(eSet){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  #check the loading data
  if(length(eSet$Expo$Data) == 0 |
     length(eSet$Expo$Voca) == 0){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelLowVar(eSet = eSet)"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #save running log
    message("Error: Exposome data or vocabulary file is missing! Please add them, or the following functions cann't run. ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: Exposome data or vocabulary file is missing! Please add them, or the following functions cann't run. ", NowTime))

    eSet$ExcecutionLog %>%
      as_tibble() %>%
      purrr::set_names("running log") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

    eSet %>%
      save(file = str_c(eSet$FileDirOut,"/eSet.Rdata"))

    tictoc::toc()

    print(Flag_Error)

    return(eSet)

  }else{

  #get the Vars names
  if(nrow(eSet$Expo$Data) > 0){
     quiet(names(eSet$Expo$Data) %>%
           stringr::str_extract_all("^[EC].*",
                           simplify = T) %>%
           as_tibble() %>%
           dplyr::filter(V1 != "") %>%
           .$V1 -> VarsEBC)

     names(eSet$Expo$Data) %>%
       stringr::str_extract_all("^[SYCEG].*",
                       simplify = T) %>%
       as_tibble() %>%
       dplyr::filter(V1 != "") %>%
       .$V1 -> Vars.all

     #get the names of the deleted features
     eSet$Expo$Data %>%
       dplyr::select(all_of(VarsEBC)) %>%
       caret::nearZeroVar(saveMetrics = T,
                          names = T) %>%
         dplyr::filter(nzv == "TRUE") %>%
         rownames() -> eSet$VarsDel

     #get the kept and deleted data
     eSet$Expo$Data %>%
       dplyr::select(-eSet$VarsDel) -> eSet$Expo$Data
     eSet$Expo$Voca%>%
       dplyr::filter(!SerialNo %in% eSet$VarsDel) -> eSet$Expo$Voca

     ddpcr::quiet(
       eSet$Expo$Data %>%
       vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoData.csv"),
                            delim = ",")
     )

     ddpcr::quiet(
     eSet$Expo$Voca %>%
       vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoVoca.csv"),
                          delim = ",")
     )


    message("Delete ", length(eSet$VarsDel)," from ",
            ncol(eSet$Expo$Data)+length(eSet$VarsDel), " features (remainning: ",
            ncol(eSet$Expo$Data), ") with zero or near zero variance. ",
            NowTime, "\n")

     eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Delete ", length(eSet$VarsDel)," from ",
                                                       ncol(eSet$Expo$Data)+length(eSet$VarsDel), " features (remainning: ",
                                                       ncol(eSet$Expo$Data), ") with zero or near zero variance. ",
                                                       NowTime))
  }else{
   message("Error: The exposome data is empty! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: The exposome data is empty! ", NowTime))
  }

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelLowVar(eSet = eSet)"))

  eSet$RCommandLog %>%
    as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #save log
  eSet$ExcecutionLog %>%
    as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
  }
}




