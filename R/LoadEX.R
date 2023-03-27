#' @title  Load data file for multigroups module
#' @description Upload data file for multigroups module.
#' @usage LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' @param eSet An R6 class object
#' @param UseExample chr. Method of uploading data. If "default", user should upload their own data files,
#'                   or use "example#1" provided by this module.
#' @param FileDirExpo chr. Input file directory, e.g. "D:/test/eg_data.xlsx". It should be
#'    noted that the slash symbol is "/", not "\".
#' @param FileDirVoca chr. Input file vocabulary, e.g. "D:/test/eg_voca.xlsx". It should be
#'    noted that the slash symbol is "/", not "\".
#' @details
#'
#' @return An R6 class object containing the input data.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSet,UseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' @author Guohuan Zhang, Yuting Wang, Bin Wang (corresponding author)
LoadEX <- function(eSet,
                   UseExample = "default",
                   FileDirExpo = NULL ,
                   FileDirVoca = NULL
){
  tictoc::tic()

  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  if(UseExample != "example#1"){
    #check the file existance
    file.exists(stringr::str_c(getwd(), "/input_", eSet$PID, "/", FileDirExpo)) -> flag01
    file.exists(stringr::str_c(getwd(), "/input_", eSet$PID, "/", FileDirVoca)) -> flag02
  }else{
    flag01 = T
    flag02 = T
  }

  if(!all(flag01, flag02)){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("\n",
                                                       "eSet <- LoadCros(eSet = eSet, \n",
                                                       "FileDirExpo = '",FileDirExpo,"', \n",
                                                       "FileDirVoca = '",FileDirVoca,"') \n"))
    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #save running log
    message("Error: Exposome data or vocabulary file doesn't exist! Please provide the right directory. ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: Exposome data or vocabulary file doesn't exist! Please provide the right directory. ", NowTime))

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

    #find the detailed FileDirExpo
    switch(UseExample,
           "default" = {
             stringr::str_c("input_", eSet$PID, "/", FileDirExpo) -> FileDirExpo
             stringr::str_c("input_", eSet$PID, "/", FileDirVoca) -> FileDirVoca
           },

           "example#1" = {
             stringr::str_c("data/eg_mulom_data.xlsx") -> FileDirExpo
             stringr::str_c("data/eg_mulom_voca.xlsx") -> FileDirVoca
           }
    )

    #read data ---------------------------------------------------------------
    ddpcr::quiet(
      if(stringr::str_sub(FileDirExpo,-4,-1) == "xlsx"){
        readxl::read_xlsx(FileDirExpo) -> eSet$Expo$Data
      }else{
        vroom::vroom(FileDirExpo,
                     show_col_types = F) -> eSet$Expo$Data
      })

    ddpcr::quiet(
      if(stringr::str_sub(FileDirVoca,-4,-1) == "xlsx"){
        readxl::read_xlsx(FileDirVoca) -> eSet$Expo$Voca
      }else{
        vroom::vroom(FileDirVoca,
                     show_col_types = F) -> eSet$Expo$Voca
      }
    )

    eSet$Expo$Data %>%
      names() -> VarsName

    #check the name of Exposome file-----------------------------------------------------------------
    if(!VarsName[1] %in% "SampleID"){
      message("Error: Please rename the sample column to 'SampleID'")
      flag1 = F
    }else{
      flag1 = T
    }

    if(!VarsName[2] %>% str_detect("^Group")){
      message("Error: Please rename the outcome column with 'Y' as beginning")
      flag2 = F
    }else{
      flag2 = T
    }

    if(!VarsName[3] %>% str_detect("^Y")){
      message("Error: Please rename the outcome column with 'Y' as beginning")
      flag3 = F
    }else{
      flag3 = T
    }

    if(!VarsName[length(VarsName)] %>% str_detect("^E")){
      message("Error: Please rename the Exposome variable columns with 'X' as beginning")
      flag4 = F
    }else{
      flag4 = T
    }

    if(all(c(flag1,flag2, flag3,flag4))){
      message("Complete loading Exposome file! ", NowTime, "\n")
      eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete loading Exposome file!", NowTime))

      eSet$Expo$Voca %>%
        dplyr::mutate(SerialNo_Raw = SerialNo) %>%
        dplyr::select(SerialNo, SerialNo_Raw, everything()) -> eSet$Expo$Voca

    }else{
      message("Error: Fail to load Exposome file! ", NowTime, "\n")
      eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: Fail to load Exposome file!", NowTime))
      eSet$Expo$Voca  <- NULL
      eSet$Expo$Data <- NULL
    }

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("\n",
                                                       "eSet <- LoadCros(eSet = eSet, \n",
                                                       "FileDirExpo = '",FileDirExpo,"', \n",
                                                       "FileDirVoca = '",FileDirVoca,"') \n"))
    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #save exposome data and voca
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



