#' @title Transform factor variables into dummy ones
#' @description Transform factor variables into dummy ones
#' @usage TransDummy(eSet = eSet,Vars = "default")
#' @param eSet An R6 class object
#' @param Vars Variables to be imputed. Available options include:
#'     "all.e", all exposure variables; "all.c", all covariates;
#'     "all.ce", combination of All E and All C. Users can also choose available variables,e.g."E1,E2,E3".
#' @details
#' @return An R6 class object containing the variable(s) after transforming the factor
#'     variables into dummy ones.
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
#' eSet = TransDummy(eSet = eSet,Vars = "default")
#' @author Bin Wang

TransDummy <- function(eSet,
                       Vars = "default"
){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  # Define data -----------------------------------------------------------------------------
  eSet$Expo$Data  -> df.all

  #reshape data
  if(all(Vars == "default")){
    df.all %>%
      dplyr::select(!contains("Y")) %>%
      dplyr::select(where(is.factor)) %>%
      names() -> Vars
  }else{
    df.all %>%
      dplyr::mutate(across(all_of(Vars), as.factor)) ->  eSet$Expo$Data
  }

  #check the loading data
  if(length(eSet$Expo$Data) == 0 |
     length(eSet$Expo$Voca) == 0 |
     !all(Vars %in% eSet$Expo$Voca$SerialNo_Raw)){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("\n",
                                                       "eSet <- TransDummy(eSet = eSet, \n",
                                                       "Vars = ",str_c("c('",str_c(Vars, collapse = "','"),"')"), "') \n"))
    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #save running log
    message("Error: It may due to 1) Exposome data or vocabulary file is missing! 2) Some input variables don't exist in the present data. ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: It may due to 1) Exposome data or vocabulary file is missing! 2) Some input variables don't exist in the present data. ", NowTime))

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


    # FuncDummySingle -------------------------------------------------------------------------------
    FuncDummySingle <- function(Var){
      df.all %>%
        dplyr::select(all_of(Var)) %>%
        modelr::model_matrix(as.formula(stringr::str_c("~", Var))) %>%
        dplyr::select(-1) %>%
        purrr::set_names(stringr::str_c(Var, ".", 2:length(unique(df.all[[Var]])))) -> temp

      return(temp)
    }



    if(length(Vars) > 0){
      furrr::future_map_dfc(Vars, FuncDummySingle) %>%
        cbind(df.all %>%
                dplyr::select(-all_of(Vars))) %>%
        as_tibble() -> temp1

      temp1 %>%
        names() %>%
        as_tibble() %>%
        dplyr::filter(stringr::str_detect(value, "^E")) %>%
        dplyr::mutate(value = as.numeric(stringr::str_remove(value, "E"))) %>%
        dplyr::arrange(value) %>%
        dplyr::mutate(value = stringr::str_c("E", value)) %>%
        .$value -> VarsE

      temp1 %>%
        names() %>%
        as_tibble() %>%
        dplyr::filter(stringr::str_detect(value, "^B")) %>%
        dplyr::mutate(value = as.numeric(stringr::str_remove(value, "E"))) %>%
        dplyr::arrange(value) %>%
        dplyr::mutate(value = stringr::str_c("B", value)) %>%
        .$value -> VarsB

      temp1 %>%
        names() %>%
        as_tibble() %>%
        dplyr::filter(stringr::str_detect(value, "^C")) %>%
        dplyr::mutate(value = as.numeric(stringr::str_remove(value, "C"))) %>%
        dplyr::arrange(value) %>%
        dplyr::mutate(value = stringr::str_c("C", value)) %>%
        .$value -> VarsC


      temp1 %>%
        names() %>%
        as_tibble() %>%
        dplyr::filter(stringr::str_detect(value, "^Y")) %>%
        dplyr::mutate(value = as.numeric(stringr::str_remove(value, "Y"))) %>%
        dplyr::arrange(value) %>%
        dplyr::mutate(value = str_c("Y", value)) %>%
        .$value -> VarsY

      if("Group" %in% names(temp1)){ #some module may have no "group" variable
        temp1 %>%
          dplyr::select("SampleID",
                        "Group",
                        all_of(VarsY),
                        all_of(VarsC),
                        all_of(VarsE),
                        all_of(VarsB)) -> eSet$Expo$Data
      }else{
        temp1 %>%
          dplyr::select("SampleID",
                        all_of(VarsY),
                        all_of(VarsC),
                        all_of(VarsE),
                        all_of(VarsB)) -> eSet$Expo$Data
      }

      #save expo data
      ddpcr::quiet(
        eSet$Expo$Data %>%
          vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoData.csv"),
                             delim = ",")
      )

      #reshape vocabulary
      tibble(SerialNo = names(eSet$Expo$Data)) %>%
        dplyr::slice(-c(1:2)) %>%
        dplyr::mutate(SerialNo_Raw = stringr::str_remove(SerialNo, "\\..*")) %>%
        dplyr::left_join(eSet$Expo$Voca,
                         by = c("SerialNo_Raw" = "SerialNo")) %>%
        dplyr::select(SerialNo, everything()) -> eSet$Expo$Voca

      #save expo vocabulary
      ddpcr::quiet(
        eSet$Expo$Voca %>%
          vroom::vroom_write(stringr::str_c(eSet$FileDirOut, "/ExpoVoca.csv"),
                             delim = ",")
      )

      message("Complete transform -> dummy! ", NowTime, "\n")
      eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> dummy! ", NowTime))
    }else{
      message("No factor Variables need transforming -> dummy! ", NowTime, "\n")
      eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("No factor Variables need transforming -> dummy! ", NowTime))
    }

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("\n",
                                                       "eSet <- TransDummy(eSet = eSet, \n",
                                                       "Vars = ",str_c("c('",str_c(Vars, collapse = "','"),"')"), "') \n"))

    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #save data and log
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


