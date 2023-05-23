#### Functions####
punotc <- function(pc, puc, pu) {
  answer <- (pu - puc * pc) / (1 - pc)
  return(answer)
}

VoI_naive <- function(pu, puc, pc, punotc) {
  answer <- abs(pu - puc) * pc + abs(pu - punotc) * (1 - pc)
  return(answer)
}

VoI_log <- function(pu, puc, pc, punotc) {
  l_puc_pu <- puc * log(puc / pu) + (1 - puc) * log((1 - puc) / (1 - pu))
  l_punotc_pu <- punotc * log(punotc / pu) + (1 - punotc) * log((1 - punotc) / (1 - pu))
  answer <- l_puc_pu * pc + l_punotc_pu * (1 - pc)
  return(answer)
}

VoI_quadratic <- function(pu, puc, pc, punotc) {
  q_puc_pu <- (pu - puc)^2
  q_punotc_pu <- (pu - punotc)^2
  answer <- q_puc_pu * pc + q_punotc_pu * (1 - pc)
  return(answer)
}

get_userIds <- function(prob_sheet) {
  userIds <- names(prob_sheet)[!grepl("ID", names(prob_sheet))]
  probs <- c("P(c)", "P(U|c)", "P(U)", "punotc", "VoI")
  userIds <- mgsub(paste0("_", probs), "", userIds)
  userIds <- unique(userIds)
  return(userIds)
}

get_VoI <- function(prob_sheet_id, prob_sheet) {
  # filter out columns with formulas
  # section not needed in cases where formulas aren't present, though including shouldn't break code
  colsToTrash <- names(prob_sheet)[grep("VoI", names(prob_sheet), ignore.case = TRUE)]
  colsToTrash <- c(colsToTrash, names(prob_sheet)[grep("¬", names(prob_sheet))])
  colsToTrash <- c(colsToTrash, names(prob_sheet)[grep("avg", names(prob_sheet), ignore.case = TRUE)])
  colsToTrash <- c(colsToTrash, names(prob_sheet)[grep("med", names(prob_sheet), ignore.case = TRUE)])
  colsToTrash <- c(colsToTrash, names(prob_sheet)[grep("geo", names(prob_sheet), ignore.case = TRUE)])
  colsToTrash <- c(colsToTrash, names(prob_sheet[grepl("...[0-9][0-9]", names(prob_sheet))]))
  colsToTrash <- unique(colsToTrash)

  prob_sheet <- prob_sheet %>% select(!all_of(colsToTrash))

  # get user IDs from column names
  userIds <- get_userIds(prob_sheet)

  prob_sheet_og <- prob_sheet

  # add P(U|¬C)
  for (i in 1:length(userIds)) {
    prob_sheet <- prob_sheet %>%
      mutate(XX_punotc = punotc(
        pc = as.numeric(get(paste0(userIds[i], "_P(c)"))),
        puc = as.numeric(get(paste0(userIds[i], "_P(U|c)"))),
        pu = as.numeric(get(paste0(userIds[i], "_P(U)")))
      ))
    newColName <- paste0(userIds[i], "_punotc")
    names(prob_sheet)[names(prob_sheet) == "XX_punotc"] <- newColName
  }

  # add VoI
  for (i in 1:length(userIds)) {
    prob_sheet <- prob_sheet %>%
      mutate(XX_VoI = VoI_naive(
        pu = as.numeric(get(paste0(userIds[i], "_P(U)"))),
        puc = as.numeric(get(paste0(userIds[i], "_P(U|c)"))),
        pc = as.numeric(get(paste0(userIds[i], "_P(c)"))),
        punotc = as.numeric(get(paste0(userIds[i], "_punotc")))
      ))
    newColName <- paste0(userIds[i], "_VoI")
    names(prob_sheet)[names(prob_sheet) == "XX_VoI"] <- newColName
  }

  # rearrange columns to match original google sheet

  suffixes <- c("P(c)", "P(U|c)", "P(U)", "punotc", "VoI")
  rearrange <- c("ID")

  for (i in 1:length(userIds)) {
    for (j in 1:length(suffixes)) {
      rearrange <- c(rearrange, paste0(userIds[i], "_", suffixes[j]))
    }
  }

  prob_sheet <- select(prob_sheet, all_of(rearrange))
  return(prob_sheet)
}

VoD_naive <- function(pu_a, pu_b, puc_a, puc_b, pc_a, pc_b, punotc_a, punotc_b) {
  initDis <- abs(pu_a - pu_b)
  expDis <- abs(puc_a - puc_b) * ((pc_a + pc_b) / 2) + abs(punotc_a - punotc_b) * (((1 - pc_a) + (1 - pc_b)) / 2)
  answer <- initDis - expDis
  return(answer)
}

VoD_log_mean <- function(pu_a, pu_b, puc_a, puc_b, pc_a, pc_b, punotc_a, punotc_b) {
  initDis <- pu_a * log(pu_a / pu_b) + (1 - pu_a) * log((1 - pu_a) / (1 - pu_b)) +
    pu_b * log(pu_b / pu_a) + (1 - pu_b) * log((1 - pu_b) / (1 - pu_a))
  expDis <- (puc_a * log(puc_a / puc_b) + (1 - puc_a) * log((1 - puc_a) / (1 - puc_b)) +
    puc_b * log(puc_b / puc_a) + (1 - puc_b) * log((1 - puc_b) / (1 - puc_a))) *
    ((pc_a + pc_b) / 2) +
    (punotc_a * log(punotc_a / punotc_b) + (1 - punotc_a) * log((1 - punotc_a) / (1 - punotc_b)) +
      punotc_b * log(punotc_b / punotc_a) + (1 - punotc_b) * log((1 - punotc_b) / (1 - punotc_a))) *
      (punotc_a + punotc_b) / 2
  answer <- initDis * log(initDis / expDis) + (1 - initDis) * log((1 - initDis) / (1 - expDis)) + expDis * log(expDis / initDis) + (1 - expDis) * log((1 - expDis) / (1 - initDis))
  return(answer)
}

VoD_log_geomean <- function(pu_a, pu_b, puc_a, puc_b, pc_a, pc_b, punotc_a, punotc_b) {
  initDis <- pu_a * log(pu_a / pu_b) + (1 - pu_a) * log((1 - pu_a) / (1 - pu_b)) +
    pu_b * log(pu_b / pu_a) + (1 - pu_b) * log((1 - pu_b) / (1 - pu_a))
  expDis <- (puc_a * log(puc_a / puc_b) + (1 - puc_a) * log((1 - puc_a) / (1 - puc_b)) +
    puc_b * log(puc_b / puc_a) + (1 - puc_b) * log((1 - puc_b) / (1 - puc_a))) *
    sqrt(pc_a * pc_b) +
    (punotc_a * log(punotc_a / punotc_b) + (1 - punotc_a) * log((1 - punotc_a) / (1 - punotc_b)) +
      punotc_b * log(punotc_b / punotc_a) + (1 - punotc_b) * log((1 - punotc_b) / (1 - punotc_a))) *
      sqrt(punotc_a * punotc_b)
  answer <- initDis * log(initDis / expDis) + (1 - initDis) * log((1 - initDis) / (1 - expDis)) + expDis * log(expDis / initDis) + (1 - expDis) * log((1 - expDis) / (1 - initDis))
  return(answer)
}

VoD_quadratic_mean <- function(pu_a, pu_b, puc_a, puc_b, pc_a, pc_b, punotc_a, punotc_b) {
  initDis <- (pu_a - pu_b)^2
  expDis <- ((puc_a - puc_b)^2 * (pc_a + pc_b) / 2) + ((punotc_a - punotc_b)^2 * ((1 - pc_a) + (1 - pc_b)) / 2)
  answer <- initDis - expDis
  return(answer)
}

VoD_quadratic_geomean <- function(pu_a, pu_b, puc_a, puc_b, pc_a, pc_b, punotc_a, punotc_b) {
  initDis <- (pu_a - pu_b)^2
  expDis <- ((puc_a - puc_b)^2 * sqrt(pc_a * pc_b)) + ((punotc_a - punotc_b)^2 * sqrt((1 - pc_a) * (1 - pc_b)))
  answer <- initDis - expDis
  return(answer)
}

get_VoD <- function(prob_sheet, userIDs, comparison_model_prefixes) {
  pSheet_final <- tibble(ID = prob_sheet$ID)

  for (i in 1:length(userIDs)) {
    for (j in 1:length(comparison_model_prefixes)) {
      id1 <- userIDs[i]
      id2 <- paste0(comparison_model_prefixes[j], id1)
      sheet1_id <- prob_sheet %>%
        select(ID, starts_with(id1))
      sheet2_id <- prob_sheet %>%
        select(ID, starts_with(id2))
      sheet_id <- full_join(sheet1_id, sheet2_id, by = "ID")

      sheet_id <- sheet_id %>%
        rowwise() %>%
        mutate(XX_VoD_naive = VoD_naive(
          pu_a = as.numeric(get(paste0(id1, "_PU"))),
          pu_b = as.numeric(get(paste0(id2, "_PU"))),
          puc_a = as.numeric(get(paste0(id1, "_PUc"))),
          puc_b = as.numeric(get(paste0(id2, "_PUc"))),
          pc_a = as.numeric(get(paste0(id1, "_Pc"))),
          pc_b = as.numeric(get(paste0(id2, "_Pc"))),
          punotc_a = as.numeric(get(paste0(id1, "_punotc"))),
          punotc_b = as.numeric(get(paste0(id2, "_punotc")))
        )) %>%
        mutate(XX_VoD_log_mean = VoD_log_mean(
          pu_a = as.numeric(get(paste0(id1, "_PU"))),
          pu_b = as.numeric(get(paste0(id2, "_PU"))),
          puc_a = as.numeric(get(paste0(id1, "_PUc"))),
          puc_b = as.numeric(get(paste0(id2, "_PUc"))),
          pc_a = as.numeric(get(paste0(id1, "_Pc"))),
          pc_b = as.numeric(get(paste0(id2, "_Pc"))),
          punotc_a = as.numeric(get(paste0(id1, "_punotc"))),
          punotc_b = as.numeric(get(paste0(id2, "_punotc")))
        )) %>%
        mutate(XX_VoD_log_geomean = VoD_log_geomean(
          pu_a = as.numeric(get(paste0(id1, "_PU"))),
          pu_b = as.numeric(get(paste0(id2, "_PU"))),
          puc_a = as.numeric(get(paste0(id1, "_PUc"))),
          puc_b = as.numeric(get(paste0(id2, "_PUc"))),
          pc_a = as.numeric(get(paste0(id1, "_Pc"))),
          pc_b = as.numeric(get(paste0(id2, "_Pc"))),
          punotc_a = as.numeric(get(paste0(id1, "_punotc"))),
          punotc_b = as.numeric(get(paste0(id2, "_punotc")))
        )) %>%
        mutate(XX_VoD_quadratic_mean = VoD_quadratic_mean(
          pu_a = as.numeric(get(paste0(id1, "_PU"))),
          pu_b = as.numeric(get(paste0(id2, "_PU"))),
          puc_a = as.numeric(get(paste0(id1, "_PUc"))),
          puc_b = as.numeric(get(paste0(id2, "_PUc"))),
          pc_a = as.numeric(get(paste0(id1, "_Pc"))),
          pc_b = as.numeric(get(paste0(id2, "_Pc"))),
          punotc_a = as.numeric(get(paste0(id1, "_punotc"))),
          punotc_b = as.numeric(get(paste0(id2, "_punotc")))
        )) %>%
        mutate(XX_VoD_quadratic_geomean = VoD_quadratic_geomean(
          pu_a = as.numeric(get(paste0(id1, "_PU"))),
          pu_b = as.numeric(get(paste0(id2, "_PU"))),
          puc_a = as.numeric(get(paste0(id1, "_PUc"))),
          puc_b = as.numeric(get(paste0(id2, "_PUc"))),
          pc_a = as.numeric(get(paste0(id1, "_Pc"))),
          pc_b = as.numeric(get(paste0(id2, "_Pc"))),
          punotc_a = as.numeric(get(paste0(id1, "_punotc"))),
          punotc_b = as.numeric(get(paste0(id2, "_punotc")))
        ))
      names(sheet_id) <- gsub("XX", paste0(id1, "_", id2, ""), names(sheet_id))
      pSheet_final <- full_join(pSheet_final, sheet_id, by = "ID")
    }
  }
  return(pSheet_final)
}

summaryResults <- function(pSheet_final) {
  summaryResults_final <- pSheet_final %>%
    select("ID", all_of(names(pSheet_final)[
      grep(paste(c("VoI", "VoD"), collapse = "|"), names(pSheet_final))
    ])) %>%
    select(!starts_with("S_"))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveMean = mean(c_across(ends_with("VoI_naive")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveMedian = median(c_across(ends_with("VoI_naive")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveGeo = geoMeanCalc(c_across(ends_with("VoI_naive")))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logMean = mean(c_across(ends_with("VoI_log")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logMedian = median(c_across(ends_with("VoI_log")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logGeo = geoMeanCalc(c_across(ends_with("VoI_log")))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticMean = mean(c_across(ends_with("VoI_quadratic")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticMedian = median(c_across(ends_with("VoI_quadratic")), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticGeo = geoMeanCalc(c_across(ends_with("VoI_quadratic")))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveMean = mean(abs(c_across(ends_with("VoD_naive"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveMedian = median(abs(c_across(ends_with("VoD_naive"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveGeo = geoMeanCalc(abs(c_across(ends_with("VoD_naive"))))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanMean = mean(abs(c_across(ends_with("VoD_log_mean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanMedian = median(abs(c_across(ends_with("VoD_log_mean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanGeo = geoMeanCalc(abs(c_across(ends_with("VoD_log_mean"))))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanMean = mean(abs(c_across(ends_with("VoD_log_geomean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanMedian = median(abs(c_across(ends_with("VoD_log_geomean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanGeo = geoMeanCalc(abs(c_across(ends_with("VoD_log_geomean"))))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanMean = mean(abs(c_across(ends_with("VoD_quadratic_mean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanMedian = median(abs(c_across(ends_with("VoD_quadratic_mean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanGeo = geoMeanCalc(abs(c_across(ends_with("VoD_quadratic_mean"))))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanMean = mean(abs(c_across(ends_with("VoD_quadratic_geomean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanMedian = median(abs(c_across(ends_with("VoD_quadratic_geomean"))), na.rm = TRUE)) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanGeo = geoMeanCalc(abs(c_across(ends_with("VoD_quadratic_geomean"))))) %>%
    ungroup()

  summaryResults_final <- summaryResults_final %>% select(ID, all_of(
    names(summaryResults_final)[grepl(paste(c("Mean", "Median", "Geo"), collapse = "|"), names(summaryResults_final))]
  ))

  return(summaryResults_final)
}

summaryResults_component <- function(pSheet_final) {
  summaryResults_final <- pSheet_final %>%
    select("ID", all_of(names(pSheet_final)[
      grep(paste(c("Pc", "PUc", "PU", "punotc"), collapse = "|"), names(pSheet_final))
    ])) %>%
    select(!starts_with("S_"))

  userIds <- names(summaryResults_final %>% select(!starts_with("E_")) %>% select(!ID))
  userIds <- unique(substr(userIds, 1, 2))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(Pc_mean = mean(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PUc_mean = mean(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PU_mean = mean(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(punotc_mean = mean(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(Pc_median = median(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PUc_median = median(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PU_median = median(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(punotc_median = median(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(Pc_geomean = geoMeanCalc(c_across(ends_with("_Pc"))))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PUc_geomean = geoMeanCalc(c_across(ends_with("_PUc"))))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(PU_geomean = geoMeanCalc(c_across(ends_with("_PU"))))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(punotc_geomean = geoMeanCalc(c_across(ends_with("_punotc"))))

  summaryResults_users <- summaryResults_final %>%
    select(grep(paste(userIds, collapse = "|"), names(summaryResults_final), value = TRUE)) %>%
    select(!starts_with("E_"))

  summaryResults_experts <- summaryResults_final %>%
    select(grep(paste(userIds, collapse = "|"), names(summaryResults_final), value = TRUE)) %>%
    select(starts_with("E_"))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(Pc_mean_a = mean(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PUc_mean_a = mean(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PU_mean_a = mean(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(punotc_mean_a = mean(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(Pc_median_a = median(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PUc_median_a = median(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PU_median_a = median(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(punotc_median_a = median(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(Pc_geomean_a = geoMeanCalc(c_across(ends_with("_Pc"))))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PUc_geomean_a = geoMeanCalc(c_across(ends_with("_PUc"))))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(PU_geomean_a = geoMeanCalc(c_across(ends_with("_PU"))))

  summaryResults_users <- summaryResults_users %>%
    rowwise() %>%
    mutate(punotc_geomean_a = geoMeanCalc(c_across(ends_with("_punotc"))))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(Pc_mean_b = mean(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PUc_mean_b = mean(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PU_mean_b = mean(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(punotc_mean_b = mean(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(Pc_median_b = median(c_across(ends_with("_Pc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PUc_median_b = median(c_across(ends_with("_PUc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PU_median_b = median(c_across(ends_with("_PU")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(punotc_median_b = median(c_across(ends_with("_punotc")), na.rm = TRUE))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(Pc_geomean_b = geoMeanCalc(c_across(ends_with("_Pc"))))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PUc_geomean_b = geoMeanCalc(c_across(ends_with("_PUc"))))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(PU_geomean_b = geoMeanCalc(c_across(ends_with("_PU"))))

  summaryResults_experts <- summaryResults_experts %>%
    rowwise() %>%
    mutate(punotc_geomean_b = geoMeanCalc(c_across(ends_with("_punotc"))))

  summaryResults_final <- tibble(
    summaryResults_final,
    summaryResults_users %>%
      select(starts_with("Pc"), starts_with("PUc"), starts_with("PU"), starts_with("punotc"))
  )

  summaryResults_final <- tibble(
    summaryResults_final,
    summaryResults_experts %>%
      select(starts_with("Pc"), starts_with("PUc"), starts_with("PU"), starts_with("punotc"))
  )

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveMean = VoI_naive(PU_mean, PUc_mean, Pc_mean, punotc_mean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveMedian = VoI_naive(PU_median, PUc_median, Pc_median, punotc_median))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_naiveGeo = VoI_naive(PU_geomean, PUc_geomean, Pc_geomean, punotc_geomean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logMean = VoI_log(PU_mean, PUc_mean, Pc_mean, punotc_mean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logMedian = VoI_log(PU_median, PUc_median, Pc_median, punotc_median))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_logGeo = VoI_log(PU_geomean, PUc_geomean, Pc_geomean, punotc_geomean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticMean = VoI_quadratic(PU_mean, PUc_mean, Pc_mean, punotc_mean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticMedian = VoI_quadratic(PU_median, PUc_median, Pc_median, punotc_median))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoI_quadraticGeo = VoI_quadratic(PU_geomean, PUc_geomean, Pc_geomean, punotc_geomean))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveMean = VoD_naive(
      pu_a = PU_mean_a,
      pu_b = PU_mean_b,
      puc_a = PUc_mean_a,
      puc_b = PUc_mean_b,
      pc_a = Pc_mean_a,
      pc_b = Pc_mean_b,
      punotc_a = punotc_mean_a,
      punotc_b = punotc_mean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveMedian = VoD_naive(
      pu_a = PU_median_a,
      pu_b = PU_median_b,
      puc_a = PUc_median_a,
      puc_b = PUc_median_b,
      pc_a = Pc_median_a,
      pc_b = Pc_median_b,
      punotc_a = punotc_median_a,
      punotc_b = punotc_median_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_naiveGeo = VoD_naive(
      pu_a = PU_geomean_a,
      pu_b = PU_geomean_b,
      puc_a = PUc_geomean_a,
      puc_b = PUc_geomean_b,
      pc_a = Pc_geomean_a,
      pc_b = Pc_geomean_b,
      punotc_a = punotc_geomean_a,
      punotc_b = punotc_geomean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanMean = VoD_log_mean(
      pu_a = PU_mean_a,
      pu_b = PU_mean_b,
      puc_a = PUc_mean_a,
      puc_b = PUc_mean_b,
      pc_a = Pc_mean_a,
      pc_b = Pc_mean_b,
      punotc_a = punotc_mean_a,
      punotc_b = punotc_mean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanMedian = VoD_log_mean(
      pu_a = PU_median_a,
      pu_b = PU_median_b,
      puc_a = PUc_median_a,
      puc_b = PUc_median_b,
      pc_a = Pc_median_a,
      pc_b = Pc_median_b,
      punotc_a = punotc_median_a,
      punotc_b = punotc_median_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_meanGeo = VoD_log_mean(
      pu_a = PU_geomean_a,
      pu_b = PU_geomean_b,
      puc_a = PUc_geomean_a,
      puc_b = PUc_geomean_b,
      pc_a = Pc_geomean_a,
      pc_b = Pc_geomean_b,
      punotc_a = punotc_geomean_a,
      punotc_b = punotc_geomean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanMean = VoD_log_geomean(
      pu_a = PU_mean_a,
      pu_b = PU_mean_b,
      puc_a = PUc_mean_a,
      puc_b = PUc_mean_b,
      pc_a = Pc_mean_a,
      pc_b = Pc_mean_b,
      punotc_a = punotc_mean_a,
      punotc_b = punotc_mean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanMedian = VoD_log_geomean(
      pu_a = PU_median_a,
      pu_b = PU_median_b,
      puc_a = PUc_median_a,
      puc_b = PUc_median_b,
      pc_a = Pc_median_a,
      pc_b = Pc_median_b,
      punotc_a = punotc_median_a,
      punotc_b = punotc_median_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_log_geomeanGeo = VoD_log_geomean(
      pu_a = PU_geomean_a,
      pu_b = PU_geomean_b,
      puc_a = PUc_geomean_a,
      puc_b = PUc_geomean_b,
      pc_a = Pc_geomean_a,
      pc_b = Pc_geomean_b,
      punotc_a = punotc_geomean_a,
      punotc_b = punotc_geomean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanMean = VoD_quadratic_mean(
      pu_a = PU_mean_a,
      pu_b = PU_mean_b,
      puc_a = PUc_mean_a,
      puc_b = PUc_mean_b,
      pc_a = Pc_mean_a,
      pc_b = Pc_mean_b,
      punotc_a = punotc_mean_a,
      punotc_b = punotc_mean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanMedian = VoD_quadratic_mean(
      pu_a = PU_median_a,
      pu_b = PU_median_b,
      puc_a = PUc_median_a,
      puc_b = PUc_median_b,
      pc_a = Pc_median_a,
      pc_b = Pc_median_b,
      punotc_a = punotc_median_a,
      punotc_b = punotc_median_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_meanGeo = VoD_quadratic_mean(
      pu_a = PU_geomean_a,
      pu_b = PU_geomean_b,
      puc_a = PUc_geomean_a,
      puc_b = PUc_geomean_b,
      pc_a = Pc_geomean_a,
      pc_b = Pc_geomean_b,
      punotc_a = punotc_geomean_a,
      punotc_b = punotc_geomean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanMean = VoD_quadratic_geomean(
      pu_a = PU_mean_a,
      pu_b = PU_mean_b,
      puc_a = PUc_mean_a,
      puc_b = PUc_mean_b,
      pc_a = Pc_mean_a,
      pc_b = Pc_mean_b,
      punotc_a = punotc_mean_a,
      punotc_b = punotc_mean_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanMedian = VoD_quadratic_geomean(
      pu_a = PU_median_a,
      pu_b = PU_median_b,
      puc_a = PUc_median_a,
      puc_b = PUc_median_b,
      pc_a = Pc_median_a,
      pc_b = Pc_median_b,
      punotc_a = punotc_median_a,
      punotc_b = punotc_median_b
    ))

  summaryResults_final <- summaryResults_final %>%
    rowwise() %>%
    mutate(VoD_quadratic_geomeanGeo = VoD_quadratic_geomean(
      pu_a = PU_geomean_a,
      pu_b = PU_geomean_b,
      puc_a = PUc_geomean_a,
      puc_b = PUc_geomean_b,
      pc_a = Pc_geomean_a,
      pc_b = Pc_geomean_b,
      punotc_a = punotc_geomean_a,
      punotc_b = punotc_geomean_b
    ))

  summaryResults_final <- summaryResults_final %>% select(ID, all_of(
    names(summaryResults_final)[grepl(paste(c("Mean", "Median", "Geo"), collapse = "|"), names(summaryResults_final))]
  ))

  return(summaryResults_final)
}

raw_data_to_results <- function(ids) {
  userIDs <- c()

  for (i in 1:length(ids)) {
    # get user id
    userID <- substr(gs4_get(ids[i])$name, 1, 2)
    userIDs <- c(userIDs, userID)

    # load sheets and clean tables
    initial_probs <- read_sheet(ids[i], sheet = "Initial probabilities")
    names(initial_probs) <- c("Question", "Answer", "Notes")
    initial_probs <- initial_probs[!apply(apply(initial_probs, 2, is.na), 1, all), ]

    check_initial <- unlist(initial_probs %>% select(Answer))

    questions <- read_sheet(ids[i], sheet = "Questions")
    names(questions) <- as.character(unlist(questions[1, ]))
    questions <- questions[2:nrow(questions), ]
    names(questions) <- c("OpFinished", "QID", "QResDate", "Question", "Pc", "PUc", "PU", "Pc_expert", "PUc_expert", "PU_expert", "Pc_super", "PUc_super", "PU_super", "Notes")

    check_questions <- unlist(questions %>% select(Pc, PUc, PU, Pc_expert, PUc_expert, PU_expert, Pc_super, PUc_super, PU_super))

    check <- as.numeric(c(check_initial, check_questions))
    if (any(check[!is.na(check)] > 1)) {
      scale <- 100
    } else {
      scale <- 1
    }

    # get P(U) for different groups
    PU <- (initial_probs %>% filter(grepl("Unincentivized", Question)))$Answer / scale
    PU_expert <- (initial_probs %>% filter(grepl("EXPERT", Question)))$Answer / scale
    PU_super <- (initial_probs %>% filter(grepl("SUPERFORECASTER", Question)))$Answer / scale

    # get ID, P(c), P(U|c)
    Pc <- lapply(questions$Pc, function(x) ifelse(is.null(x), NA, x))
    Pc <- unlist(Pc)
    Pc <- gsub("^,", ".", Pc)
    Pc <- gsub("\\.\\.", ".", Pc)
    Pc <- as.numeric(Pc) / scale

    PUc <- lapply(questions$PUc, function(x) ifelse(is.null(x), NA, x))
    PUc <- unlist(PUc)
    PUc <- gsub("^,", ".", PUc)
    PUc <- gsub("\\.\\.", ".", PUc)
    PUc <- as.numeric(PUc) / scale

    user_sheet <- tibble(
      ID = questions$QID,
      XX_Pc = Pc,
      XX_PUc = PUc,
      XX_PU = rep(PU, length(Pc))
    )

    # Calc P(U|¬C), VoI
    user_sheet <- user_sheet %>%
      mutate(XX_punotc = punotc(XX_Pc, XX_PUc, XX_PU)) %>%
      mutate(XX_VoI_naive = VoI_naive(XX_PU, XX_PUc, XX_Pc, XX_punotc)) %>%
      mutate(XX_VoI_log = VoI_log(XX_PU, XX_PUc, XX_Pc, XX_punotc)) %>%
      mutate(XX_VoI_quadratic = VoI_quadratic(XX_PU, XX_PUc, XX_Pc, XX_punotc))

    # Repeat for expert models
    Pc_expert <- lapply(questions$Pc_expert, function(x) ifelse(is.null(x), NA, x))
    Pc_expert <- unlist(Pc_expert)
    Pc_expert <- gsub("^,", ".", Pc_expert)
    Pc_expert <- gsub("\\.\\.", ".", Pc_expert)
    Pc_expert <- as.numeric(Pc_expert) / scale

    PUc_expert <- lapply(questions$PUc_expert, function(x) ifelse(is.null(x), NA, x))
    PUc_expert <- unlist(PUc_expert)
    PUc_expert <- gsub("^,", ".", PUc_expert)
    PUc_expert <- gsub("\\.\\.", ".", PUc_expert)
    PUc_expert <- as.numeric(PUc_expert) / scale

    E_user_sheet <- tibble(
      E_XX_Pc = Pc_expert,
      E_XX_PUc = PUc_expert,
      E_XX_PU = rep(PU_expert, length(Pc_expert))
    )

    user_sheet <- tibble(user_sheet, E_user_sheet)

    # Calc P(U|¬C), VoI
    user_sheet <- user_sheet %>%
      mutate(E_XX_punotc = punotc(E_XX_Pc, E_XX_PUc, E_XX_PU)) %>%
      mutate(E_XX_VoI_naive = VoI_naive(E_XX_PU, E_XX_PUc, E_XX_Pc, E_XX_punotc)) %>%
      mutate(E_XX_VoI_log = VoI_log(E_XX_PU, E_XX_PUc, E_XX_Pc, E_XX_punotc)) %>%
      mutate(E_XX_VoI_quadratic = VoI_quadratic(E_XX_PU, E_XX_PUc, E_XX_Pc, E_XX_punotc))

    # Repeat for super models
    Pc_super <- lapply(questions$Pc_super, function(x) ifelse(is.null(x), NA, x))
    Pc_super <- unlist(Pc_super)
    Pc_super <- gsub("^,", ".", Pc_super)
    Pc_super <- gsub("\\.\\.", ".", Pc_super)
    Pc_super <- as.numeric(Pc_super) / scale

    PUc_super <- lapply(questions$PUc_super, function(x) ifelse(is.null(x), NA, x))
    PUc_super <- unlist(PUc_super)
    PUc_super <- gsub("^,", ".", PUc_super)
    PUc_super <- gsub("\\.\\.", ".", PUc_super)
    PUc_super <- as.numeric(PUc_super) / scale

    S_user_sheet <- tibble(
      S_XX_Pc = Pc_super,
      S_XX_PUc = PUc_super,
      S_XX_PU = rep(PU_super, length(Pc_super))
    )

    user_sheet <- tibble(user_sheet, S_user_sheet)

    # Calc P(U|¬C), VoI
    user_sheet <- user_sheet %>%
      mutate(S_XX_punotc = punotc(S_XX_Pc, S_XX_PUc, S_XX_PU)) %>%
      mutate(S_XX_VoI_naive = VoI_naive(S_XX_PU, S_XX_PUc, S_XX_Pc, S_XX_punotc)) %>%
      mutate(S_XX_VoI_log = VoI_log(S_XX_PU, S_XX_PUc, S_XX_Pc, S_XX_punotc)) %>%
      mutate(S_XX_VoI_quadratic = VoI_quadratic(S_XX_PU, S_XX_PUc, S_XX_Pc, S_XX_punotc))

    names(user_sheet) <- gsub("XX", userID, names(user_sheet))

    # join user to individual results sheet
    if (i == 1) {
      prob_sheet <- user_sheet
    } else {
      prob_sheet <- full_join(prob_sheet, user_sheet, by = "ID")
    }
  }

  # get VoD for individual results sheet for a series of comparison models. In this case, we'll just be comparing to the expert group we defined above.
  comparison_model_prefixes <- "E_"
  pSheet_final <- get_VoD(prob_sheet, userIDs, comparison_model_prefixes)

  # join in super model P's and VoI
  other_cols <- prob_sheet %>% select(all_of(names(prob_sheet)[!names(prob_sheet) %in% names(pSheet_final)]))
  pSheet_final <- tibble(pSheet_final, other_cols)

  return(pSheet_final)
}

geoMeanCalc <- function(x) {
  x[x == 0] <- as.numeric(quantile(x[x != 0], 0.05, na.rm = TRUE))
  geoMean <- exp(mean(log(x), na.rm = TRUE))
  return(geoMean)
}

getProbComposite_p8 <- function(var_name) {
  newcols <- aggregate(eval(parse(text = var_name)) ~ Camp + C_id, mean, data = p8_composite)
  names(newcols) <- c("Camp", "C_id", var_name)
  return(newcols)
}

bindComposite_p8 <- function(composite_sheet, var_name, newcols) {
  concerned <- newcols %>% filter(Camp == "Concerned")
  composite_sheet[[paste0(var_name, "_concerned")]] <- concerned[var_name]
  names(composite_sheet)[length(composite_sheet)] <- paste0(var_name, "_concerned")
  skeptical <- newcols %>% filter(Camp == "Skeptical")
  composite_sheet[[paste0(var_name, "_skeptical")]] <- skeptical[var_name]
  names(composite_sheet)[length(composite_sheet)] <- paste0(var_name, "_skeptical")
  return(composite_sheet)
}
