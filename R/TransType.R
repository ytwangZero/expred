#' @title Transform data type
#' @description Transform data type
#' @usage TransType(eSet = eSet,Vars,TypeTo)
#' @param eSet An R6 class object
#' @param Vars Variables to be imputed. Available options include:
#'     "all.e", all exposure variables; "all.c", all covariates;
#'     "all.ce", combination of All E and All C. Users can also choose available variables,e.g."E1,E2,E3".
#' @param TypeTo chr. Indicate the type of the chosen variables to be transformed into.
#'     Available options include "integer", "numeric", "character", "factor", "logical", and "date".
#' @details
#' @return An R6 class object containing the variable(s) after transforming data type.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' eSet = DelLowVar(eSet = eSet)
#' eSet = DelMiss(eSet = eSet)
#' eSet = TransType(eSet = eSet,Vars = c("Y1", "C1","E203","E204","E207","E209"),TypeTo = "factor")
#' @author Bin Wang

TransType <- function(eSet,
                      Vars,
                      TypeTo #integer, numeric, character, factor, logical, date
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
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransType(eSet = eSet, ",
                                                       "Vars = Target variables, ",
                                                       "TypeTo = ",TypeTo, ")"))

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

    # func_data.type -----------------------------------------------------------------------------
    switch(TypeTo,
           "integer" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as.integer)) -> eSet$Expo$Data
           },
           "numeric" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as.numeric)) -> eSet$Expo$Data
           },
           "character" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as.character)) -> eSet$Expo$Data
           },
           "factor" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as.factor)) -> eSet$Expo$Data
           },
           "logical" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as.logical)) -> eSet$Expo$Data
           },
           "date" = {
             df.all %>%
               dplyr::mutate(dplyr::across(all_of(Vars), as_date)) -> eSet$Expo$Data
           }
    )

    #save data --------------------------------------------------------------------------------
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
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransType(eSet = eSet, ",
                                                       "Vars = Target variables, ",
                                                       "TypeTo = ",TypeTo, ")"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #print message and save log
    message("Complete transform -> ", TypeTo, ". ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> ", TypeTo, ". ", NowTime))

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





