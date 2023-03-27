#' @title Scale variables
#' @description Scale variables
#' @usage TransScale(eSet = eSet,Group = T,Vars = "all.e",Method = "normal",
#'                   Direct = "positive",RangeLow = 0,RangeUpper = 1)
#' @param eSet An R6 class object.
#' @param Group lgl. T (or TRUE) and F (or FALSE). Whether to separate dataset into train and test data for processing data.
#' @param Vars Variables to be imputed. Available options include:
#'     "all.e", all exposure variables; "all.c", all covariates;
#'     "all.ce", combination of All E and All C. Users can also choose available variables,e.g."E1,E2,E3".
#' @param Method chr. Scaling methods. Available options include "normal" and "range".
#' @param Direct chr. Direction to be transformed, Available options include "positive" and "negative".
#' @param RangeLow num. Lower limit for range method.
#' @param RangeUpper num. Upper limit for range method. It should be greater than the lower limit.
#' @details
#' @return An R6 class object containing the variable(s) after scaling data.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' eSet = DelLowVar(eSet = eSet)
#' eSet = DelMiss(eSet = eSet)
#' eSet = TransType(eSet = eSet,Vars = c("Y1", "C1","E203","E204","E207","E209"),TypeTo = "factor")
#' eSet = TransScale(eSet = eSet,Group = T,Vars = "all.e",Method = "normal",
#'                   Direct = "positive",RangeLow = 0,RangeUpper = 1)
#' @author Bin Wang
TransScale <- function(eSet,
                       Group = T,
                       Vars,
                       Method = "normal", #scale Method: normal, range
                       Direct = NULL, #positive negative
                       RangeLow = NULL, #scale range low value
                       RangeUpper = NULL #scale range high value
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
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransScale(eSet = eSet, Group = ",Group,", ",
                                                       "VarsX = Target X variables, Method = ",Method, ", ",
                                                       "Direct = ",Direct,", ",
                                                       "RangeLow = ",RangeLow,", ",
                                                       "RangeUpper = ",RangeUpper,")"))

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

    eSet$Expo$Data %>%
      dplyr::filter(Group == "train") -> df.train

    eSet$Expo$Data %>%
      dplyr::filter(Group == "test") -> df.test

    # Define Functions
    FuncScaleNormal <- function(Var, df){
      df %>%
        dplyr::select(all_of(Var)) %>%
        scale() %>%
        as_tibble() -> temp
    }


    FuncScaleRange = function(Var, df){
      df %>%
        dplyr::select(all_of(Var)) %>%
        .[[Var]] %>%
        as_tibble() -> temp

      range(temp$value, na.rm = TRUE) -> rng

      switch(Direct,
             "positive" = {
               temp  %>%
                 dplyr::mutate(V2 = (RangeUpper-RangeLow)*(value-rng[1])/
                                 (rng[2]-rng[1])+RangeLow) %>%
                 dplyr::select(V2) %>%
                 purrr::set_names(Var) -> temp1
             },

             "negative" = {
               temp  %>%
                 dplyr::mutate(value = (RangeUpper-RangeLow)*(rng[2]-value)/
                                 (rng[2]-rng[1])+RangeLow) %>%
                 purrr::set_names(Var) -> temp1
             }
      )
      return(temp1)
    }


    # execute -----------------------------------------------------------------
    switch(Method,
           "normal" = {
             df.all %>%
               dplyr::select(all_of(Vars)) %>%
               dplyr::select(where(is.numeric)) %>%
               names() -> Vars

             if(!Group){ #calculate all dataset
               if(length(Vars) > 0){
                 purrr::map_dfc(Vars,
                                FuncScaleNormal,
                                df = df.all) -> df.temp

                 df.all %>%
                   dplyr::select(-all_of(Vars)) %>%
                   cbind(df.temp) %>%
                   as_tibble() %>%
                   dplyr::select(names(df.all)) -> eSet$Expo$Data
               }else{
                 message(stringr::str_c("Error: These group of Variables cann't be scaled!",
                                        lubridate::now()), "\n")
               }
             }else{ #calculate the train and test dataset separately
               if(length(Vars) > 0){
                 map_dfc(Vars,
                         FuncScaleNormal,
                         df = df.train) -> df.temp1

                 map_dfc(Vars,
                         FuncScaleNormal,
                         df = df.test) -> df.temp2

                 df.all %>%
                   dplyr::select(-all_of(Vars)) %>%
                   cbind(rbind(df.temp1, df.temp2)) %>%
                   tibble::as_tibble() %>%
                   dplyr::select(names(df.all)) -> eSet$Expo$Data
               }else{
                 message(stringr::str_c("Error: These group of Variables cann't be scaled!",
                                        lubridate::now()), "\n")
               }
             }
           },

           "range" = {
             df.all %>%
               dplyr::select(all_of(Vars)) %>%
               dplyr::select(where(is.numeric)) %>%
               names() -> Vars

             if(!Group){
               if(length(Vars) > 0){
                 purrr::map_dfc(Vars,
                                FuncScaleRange,
                                df = df.all) %>%
                   cbind(eSet$Expo$Data %>%
                           dplyr::select(-all_of(Vars))
                   ) %>%
                   as_tibble() %>%
                   dplyr::select(names(df.all)) -> eSet$Expo$Data
               }else{
                 message(stringr::str_c("Error: These group of Variables cann't be ranged!",
                                        NowTime), "\n")
               }
             }else{
               if(length(Vars) > 0){
                 purrr::map_dfc(Vars,
                                FuncScaleRange,
                                df = df.train) -> df.temp1

                 purrr::map_dfc(Vars,
                                FuncScaleRange,
                                df = df.test) -> df.temp2

                 df.all %>%
                   dplyr::select(-all_of(Vars)) %>%
                   cbind(rbind(df.temp1, df.temp2)) %>%
                   as_tibble() %>%
                   dplyr::select(names(df.all)) -> eSet$Expo$Data
               }else{
                 message(stringr::str_c("Error: These group of Variables cann't be ranged!",
                                        NowTime), "\n")
               }
             }
           }
    )

    #save data
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
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransScale(eSet = eSet, Group = ",Group,", ",
                                                       "VarsX = Target X variables, Method = ",Method, ", ",
                                                       "Direct = ",Direct,", ",
                                                       "RangeLow = ",RangeLow,", ",
                                                       "RangeUpper = ",RangeUpper,")"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #print message and save log
    message("Complete transform -> scale! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> scale! ", NowTime))

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






