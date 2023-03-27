#' @title Transform variable distribution
#' @description Transform variable distribution
#' @usage TransDistr(eSet = eSet,Vars = c("C2", "C3"), Method = "log2")
#' @param eSet An R6 class object
#' @param Vars Variables to be imputed. Available options include:
#'     "all.e", all exposure variables; "all.c", all covariates;
#'     "all.ce", combination of All E and All C. Users can also choose available variables,e.g."E1,E2,E3".
#' @param Method chr. Methods used for imputation. Available options include "lod" or "cart".
#'     For "lod" method, limit of detection (LOD) should be included in the "Vocabulary" file.
#' @details
#' @return An R6 class object containing the variable(s) after transforming distribution.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' eSet = DelLowVar(eSet = eSet)
#' eSet = DelMiss(eSet = eSet)
#' eSet = TransType(eSet = eSet,Vars = c("Y1", "C1","E203","E204","E207","E209"),TypeTo = "factor")
#' eSet = TransScale(eSet = eSet,Group = T,Vars = "all.e",Method = "normal",
#'                   Direct = "positive",RangeLow = 0,RangeUpper = 1)
#' eSet = TransDistr(eSet = eSet,Vars = c("C2", "C3"), Method = "log2")
#' @author Bin Wang
TransDistr <- function(eSet,
                       Vars,
                       Method #trans Method: ln, log2, log10
){
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  #defind data
  switch (Vars[1],
          "all.c" = {
            eSet$Expo$Voca %>%
              dplyr::filter(str_detect(SerialNo_Raw, "[C]")) %>%
              .$SerialNo -> Vars
          },
          "all.e" = {
            eSet$Expo$Voca %>%
              dplyr::filter(str_detect(SerialNo_Raw, "[E]")) %>%
              .$SerialNo -> Vars
          },
          "all.ce" = {
            eSet$Expo$Voca %>%
              dplyr::filter(str_detect(SerialNo_Raw, "[CE]")) %>%
              .$SerialNo -> Vars
          }
  )

  #check the loading data
  if(length(eSet$Expo$Data) == 0 |
     length(eSet$Expo$Voca) == 0){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransDistr(eSet = eSet, Vars = Target variables, ",
                                                       "Method = ",Method,")"))

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

    # Define data -----------------------------------------------------------------------------
    eSet$Expo$Data  -> df.all

    # func_trans.distribution ---------------------------------------------------------------------------
    df.all %>%
      dplyr::select(all_of(Vars)) %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select(where(~ min(.x, na.rm = TRUE) > 0)) %>%
      colnames() -> Vars

    if(length(Vars) > 0){
      switch(Method,
             "ln" = {
               df.all %>%  #dplyr::across the function with the vector of target variables
                 dplyr::select(all_of(Vars)) %>%
                 dplyr::mutate(dplyr::across(where(is.numeric),
                                             log)) -> df.temp
               df.temp %>%
                 cbind(df.all %>%
                         dplyr::select(-all_of(Vars))) %>%
                 as_tibble() %>%
                 dplyr::select(names(df.all)) -> eSet$Expo$Data
             },

             "log2" = {
               eSet$Expo$Data %>%  #dplyr::across the function with the vector of target variables
                 dplyr::select(all_of(Vars)) %>%
                 dplyr::mutate(dplyr::across(where(is.numeric),
                                             log2)) -> df.temp
               df.temp %>%
                 cbind(df.all %>%
                         dplyr::select(-all_of(Vars))) %>%
                 as_tibble() %>%
                 dplyr::select(names(df.all)) -> eSet$Expo$Data
             },

             "log10" = {
               df.all %>%  #dplyr::across the function with the vector of target variables
                 dplyr::select(all_of(Vars)) %>%
                 dplyr::mutate(dplyr::across(where(is.numeric),
                                             log10)) -> df.temp
               df.temp %>%
                 cbind(df.all %>%
                         dplyr::select(-all_of(Vars))) %>%
                 as_tibble() %>%
                 dplyr::select(names(df.all)) -> eSet$Expo$Data
             }
      )
    }else{
      message("Error: The distributions of these variables can not be transformed!", NowTime, "\n")
    }

    #execute task --------------------------------------------------------------------------------
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
    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransDistr(eSet = eSet, Vars = Target variables, ",
                                                       "Method = ",Method,")"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #print message and save log
    message("Complete transform -> Distribution! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> distribution. ", NowTime))

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






