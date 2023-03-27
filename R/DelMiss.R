#' @title Delete variables with missing values
#' @description Whether to delete missing variables with low variance. The default option is "yes".
#'     If skipped, it may result in failure during modeling.
#' @usage DelMiss(eSet = eSet)
#' @param eSet An R6 class object.
#' @details
#' @return An R6 class object containing the variable(s) without missing values.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' eSet = DelLowVar(eSet = eSet)
#' eSet = DelMiss(eSet = eSet)
#' @author Bin Wang
DelMiss <- function(eSet){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  #check the loading data
  if(length(eSet$Expo$Data) == 0 |
     length(eSet$Expo$Voca) == 0){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelMiss(eSet = eSet)"))

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

    #get the missing information
    eSet$Expo$Data %>%
      naniar::miss_var_summary() %>%  #to be improved for the vars with chr type
      dplyr::filter(n_miss > 0) %>%
      purrr::set_names("vars",
                       "number_miss",
                       "percent_miss") -> temp1

    #update the kept and delete dexposome datasets
    if(length(eSet$VarsDel) > 0){
      cbind(eSet$VarsDel,
            temp1$vars) -> eSet$VarsDel
    }else{
      temp1$vars -> eSet$VarsDel
    }


    eSet$Expo$Data  %>%
      dplyr::select(-temp1$vars) -> eSet$Expo$Data

    eSet$Expo$Voca  %>%
      dplyr::filter(!SerialNo %in% temp1$vars) ->  eSet$Expo$Voca

    ddpcr::quiet(
      eSet$Expo$Data %>%
        vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoData.csv"),
                           delim = ",")
    )

    ddpcr::quiet(
      eSet$Expo$Voca  %>%
        vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoVoca.csv"),
                           delim = ",")
    )

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelMiss(eSet = eSet)"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #print message and save log
    message("Delete ", nrow(temp1)," from ", ncol(eSet$Expo$Data)+nrow(temp1),
            " features (remainning:", ncol(eSet$Expo$Data),") with missing values > 1.", " ",
            NowTime, "\n")

    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Delete ", nrow(temp1)," in ",
                                                      ncol(eSet$Expo$Data),
                                                      " features with missing values > 1. ",
                                                      NowTime))
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


