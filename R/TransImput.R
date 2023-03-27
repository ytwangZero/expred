#' @title Missing data imputation.
#' @description Missing data imputation.
#' @usage TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' @param eSet An R6 class object.
#' @param Group lgl. Whether to separate dataset into train and test data for processing data.
#' @param Vars Variables to be imputed. Available options include:
#'     "all.e", all exposure variables; "all.c", all covariates;
#'     "all.ce", combination of All E and All C. Users can also choose available variables,e.g."E1,E2,E3".
#' @param Method Methods used for imputation. Available options include "lod" or "cart" methods.
#'     For "lod" method, limit of detection (LOD) should be included in the "Vocabulary" file.
#' @details
#' @return An R6 class object containing variable(s) with imputation.
#' @export
#' @examples eSet = InitEX(PID = "EX", FileDirIn = "default", FileDirOut = "default")
#' eSet = LoadEX(eSet = eSetUseExample = "default",FileDirExpo = "examdata.xlsx",FileDirVoca = "examvoca.xlsx")
#' eSet = TransImput(eSet = eSet,Group = T,Vars = "all.e",Method = "lod")
#' @author Bin Wang
TransImput <- function(eSet,
                       Group = T, #T F
                       Vars,
                       Method = NULL
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
          "all.cb" = {
            eSet$Expo$Voca %>%
              dplyr::filter(str_detect(SerialNo_Raw, "[CE]")) %>%
              .$SerialNo -> Vars
          }
  )

  #check the loading data
  if(length(eSet$Expo$Data) == 0 |
     length(eSet$Expo$Voca) == 0){

    #save R command log
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransImput(eSet = eSet, Vars = Target X variables, ",
                                                       "Group = ",Group, ", ",
                                                       "Vars = Target variables, ",
                                                       "Method = ",Method,
                                                       ")"))

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


    # func_imputation ----------------------------------------------------------------
    switch(Method,
           "cart" = {
             if(!Group){
               df.all %>%
                 dplyr::select(all_of(Vars)) %>%
                 mice::mice(m = 5,
                            seed = 1,
                            method = "cart",
                            print = FALSE) %>%
                 complete() %>%
                 as_tibble() -> df.temp

               df.all %>%
                 dplyr::select(-all_of(Vars)) %>%
                 cbind(df.temp) %>%
                 as_tibble() %>%
                 dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
             }else{
               df.train %>%
                 dplyr::select(all_of(Vars)) %>%
                 mice::mice(m = 5,
                            seed = 1,
                            method = "cart",
                            print = FALSE) %>%
                 complete() %>%
                 as_tibble() -> df.temp1

               df.test %>%
                 dplyr::select(all_of(Vars)) %>%
                 mice::mice(m = 5,
                            seed = 1,
                            method = "cart",
                            print = FALSE) %>%
                 complete() %>%
                 as_tibble() -> df.temp2

               df.all %>%
                 dplyr::select(-all_of(Vars)) %>%
                 cbind(rbind(df.temp1, df.temp2)) %>% #combine the train and test dataset
                 as_tibble() %>%
                 dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
             }

           },

           "lod" = {
             eSet$Expo$Voca %>%
               dplyr::select(SerialNo, Lod) %>%
               dplyr::filter(SerialNo %in% Vars) %>%
               dplyr::filter(!Lod %in% c(NA)) -> df.Lod

             if(nrow(df.Lod) > 0){
               if(!Group){
                 df.all %>%
                   dplyr::select(SampleID, df.Lod$SerialNo) %>%
                   tidyr::pivot_longer(-SampleID,
                                       names_to = "SerialNo",
                                       values_to = "values") %>%
                   dplyr::left_join(df.Lod,
                                    by = "SerialNo") %>%
                   distinct(SampleID,
                            SerialNo,
                            .keep_all = T) %>%
                   dplyr::mutate("ValueLod" = case_when(values >= Lod &
                                                          !is.na(values) ~ values,
                                                        values < Lod |
                                                          is.na(values) ~ Lod/sqrt(2),
                                                        T ~ Lod/sqrt(2))
                   ) %>%
                   dplyr::select(SampleID,
                                 SerialNo,
                                 ValueLod) %>%
                   tidyr::pivot_wider(SampleID,
                                      names_from = SerialNo,
                                      values_from = ValueLod)  -> df.temp

                 df.all %>%
                   dplyr::select(-df.Lod$SerialNo) %>%
                   left_join(df.temp,
                             by = "SampleID") %>%
                   dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
               }else{
                 df.train %>%
                   dplyr::select(SampleID, df.Lod$SerialNo) %>%
                   dplyr::mutate(across(-SampleID, as.numeric)) %>%
                   tidyr::pivot_longer(-SampleID,
                                       names_to = "SerialNo",
                                       values_to = "values") %>%
                   dplyr::left_join(df.Lod,
                                    by = "SerialNo") %>%
                   distinct(SampleID,
                            SerialNo,
                            .keep_all = T) %>%
                   dplyr::mutate("ValueLod" = case_when(values >= Lod &
                                                          !is.na(values) ~ values,
                                                        values < Lod |
                                                          is.na(values) ~ Lod/sqrt(2),
                                                        T ~ Lod/sqrt(2))
                   ) %>%
                   dplyr::select(SampleID,
                                 SerialNo,
                                 ValueLod) %>%
                   tidyr::pivot_wider(SampleID,
                                      names_from = SerialNo,
                                      values_from = ValueLod)  -> df.temp1

                 df.test %>%
                   dplyr::select(SampleID, df.Lod$SerialNo) %>%
                   dplyr::mutate(across(-SampleID, as.numeric)) %>%
                   tidyr::pivot_longer(-SampleID,
                                       names_to = "SerialNo",
                                       values_to = "values") %>%
                   dplyr::left_join(df.Lod,
                                    by = "SerialNo") %>%
                   distinct(SampleID,
                            SerialNo,
                            .keep_all = T) %>%
                   dplyr::mutate("ValueLod" = case_when(values >= Lod &
                                                          !is.na(values) ~ values,
                                                        values < Lod |
                                                          is.na(values) ~ Lod/sqrt(2),
                                                        T ~ Lod/sqrt(2))
                   ) %>%
                   dplyr::select(SampleID,
                                 SerialNo,
                                 ValueLod) %>%
                   tidyr::pivot_wider(SampleID,
                                      names_from = SerialNo,
                                      values_from = ValueLod)  -> df.temp2


                 df.all %>%
                   dplyr::select(-df.Lod$SerialNo) %>%
                   left_join(rbind(df.temp1,df.temp2), #combine the train and test dataset
                             by = "SampleID") %>%
                   dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
               }
             }else{
               message("Please add the LOD data! ", NowTime, "\n")
             }
           }
    )


    # execute--------------------------------------------------------------------------------
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
    eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransImput(eSet = eSet, Vars = Target X variables, ",
                                                       "Group = ",Group, ", ",
                                                       "Vars = Target variables, ",
                                                       "Method = ",Method,
                                                       ")"))
    eSet$RCommandLog %>%
      as_tibble() %>%
      purrr::set_names("R commands") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

    #print message and save log
    message("Complete imputation by ", Method, " method! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete imputation by ", Method, " method! ", NowTime))

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






