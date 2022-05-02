#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bv_load_data <- function() {


  # This is vial-level data can be matched to
  #  - st_adulabels_ext
  #  - st_pedlabels_ext
  # 1:1 using labelid

  # data_bv has 1 row per labelid
  data_bv <- load_all('SAS_Blood_Viability') |>
    mutate(ppt_type = recode(ppt_type, 'ADULT' = 'ADU'))

  data_bv_split <- split(data_bv, data_bv$ppt_type)

  data_adu <- load_all('st_adulabels_ext')

  data_ped <- load_all('st_pedlabels_ext')

  data_bv_adu <- data_bv_split |>
    getElement('ADU') |>
    mutate(labelid = as.character(labelid)) |>
    left_join(data_adu, by = c('labelid' = 'labelID')) |>
    mutate(MTP_PARTIALVOL = as.logical(MTP_PARTIALVOL),
           MTP_ALQQUALITYCODE = as.integer(MTP_ALQQUALITYCODE),
           MTP_SHIPFROZEN = as.logical(MTP_SHIPFROZEN),
           protocol = as.integer(protocol))

  data_bv_ped <- data_bv_split |>
    getElement('PED') |>
    left_join(data_ped, by = c('labelid' = 'labelID')) |>
    mutate(labelid = as.character(labelid),
           vialLabel = as.character(vialLabel))

  # create anchor with d_visit_pcaa and d_visit_pcab
  #     count number of days since jan 1 2017
  #     if timepoint is 24 hour timepoint use B instead of A
  #     if timepoint %in% c(24 hr post, 24 hr Rest 3), use B; ow use A
  #          diff b/t anchor and d_mencyc_pcaa
  #          diff b/t anchor and d_lastfluvac_pcaa

  not_all_missing <- function(x) !all(is.na(x))

  data_preselect <- bind_rows(adu = data_bv_adu,
                              ped = data_bv_ped) |>
    select(where(not_all_missing)) |>
    # has all missing values.
    mutate(
      outcome = case_when(
        type %in% c('PAX', 'PBMC QC') ~ low_rin,
        type %in% c('PBMC SHIPPED') ~ low_viability
      ),
      .before = 1
    ) |>
    mutate(
      d_visit_pca = if_else(
        timepoint %in% c('24 hr post',
                         '24 hr post_v2', # correct?
                         '24 hr Rest 3'),
        true = d_visit_pcab,
        false =  d_visit_pcaa
      ),
      across(starts_with('d_'),
             ~ as.numeric(difftime(.x, as.POSIXct('2017-01-01')))),
      d_diff_mencyc = d_visit_pca - d_mencyc_pcaa,
      d_diff_lastfluvac = d_visit_pca - d_lastfluvac_pcaa,
      across(starts_with("t_"), .fns = hrs_since_midnight)
    ) |>
    rename(vial_type = type) |>  # for clarity
    select(-d_visit_pcaa, -d_visit_pcab)

  # Put a - sign in front of the variables we want to drop,
  # and leave a comment with a quick explanation if needed.
  # for example, see line 99, where I put a '-' in front of
  # bid, indicating it would be dropped, and said I was dropping
  # it because it is an identifier.

  # the first four variables are placed at the front of the data
  # for convenience. The others are listed in alphabetical order.

  # if you don't see a variable here, it probably got removed above
  # due to having only missing values.

  data_out <- data_preselect |>
    select(
      outcome,
      ppt_type,
      vial_type,
      aablack_psca,
      addnlmsrs,
      -addnlmsrs_spc, # just text
      alcogr24_pcaa,
      alcogr24_pcab,
      aliquotamnt,
      aliquotCode,
      aliquotDescription,
      aliquotGUID,
      aliquotmsrmnt,
      allex_pcaa,
      anestallerg_pcaa,
      anestreceiv_pcaa,
      asExpected,
      asian_psca,
      assignmentcode,
      assignmentdescription,
      assignmentGUID,
      barcodeID_pcaa,
      barcodeID_pcab,
      -bid, # identifier
      biotindays_pcaa,
      biotingr3_pcaa,
      blddraw_pcaa,
      bleedeasy_pcaa,
      BLOincubate,
      bloodhow,
      BLOsample_comnt,
      BLOsample_comp,
      BLOsample_stat,
      bruising_pcaa,
      bruising_pcab,
      caffgr24_pcaa,
      caffgr24_pcab,
      caffhrs_pcaa,
      caffhrs_pcab,
      calculatedAge,
      cauc_psca,
      clean_pcaa,
      codedSiteID,
      coll_staffid,
      collected,
      collectionQtr,
      -collForm,
      -collForm_barcodeID,
      -collForm_comments,
      -collForm_d_visit,
      -collForm_recordthread,
      -collForm_staffID,
      colorCode,
      colorDescription,
      colorGUID,
      -comments,
      d_fusample_pcaa,
      d_fusample_pcab,
      d_lastfluvac_pcaa,
      d_mencyc_pcaa,
      d_visit_pca, # this was derived above
      deviation,
      -deviationComments,
      dffcltdraw,
      domainCode,
      domainDescription,
      domainguid,
      drinkgr10_pcaa,
      drinkgr10_pcab,
      drinkhrs_pcaa,
      drinkshake_pcaa,
      drinkshake_pcab,
      drinkwater_pcaa,
      eatgr10_pcaa,
      eatgr10_pcab,
      eathrs_pcaa,
      exdays_pcaa,
      expectedSample,
      fainting_pcaa,
      feelwell_pcaa,
      freezetime,
      -fuaddcom_pcaa,
      -fuaddcom_pcab,
      fuoth_pcaa,
      fuoth_spc_pcaa,
      fushow_pcaa,
      fushow_pcab,
      fusignsymp_pcaa,
      fusignsymp_pcab,
      fustaffid_pcaa,
      fustaffid_pcab,
      fwMatch,
      haex_pcaa,
      health_pcaa,
      hemolysis,
      Htcmavg_hwwt,
      incubatetime,
      infoCorrect,
      -labelGUID, # identifier
      -labelid, # identifier,
      lastfluvacna_pcaa,
      latexallerg_pcaa,
      latino_psca,
      leakage_pcaa,
      -Live_cell_concentration, # part of the outcome
      -low_rin, # part of the outcome
      -low_viability, # part of the outcome
      manifestGUID,
      meetprereq_pcaa,
      meetprereq_pcab,
      mencycdays_pcaa,
      mencycna_pcaa,
      MTP_ALQQUALITYCODE,
      MTP_EXPECTEDVOL,
      -MTP_OGLABELID, # identifier
      MTP_PARTIALVOL,
      MTP_RECEIVEDDate,
      -MTP_REPOLABELID,
      MTP_SHIPFROZEN,
      MTP_SHIPNUM,
      -MTP_SMPBID, # identifier
      MTP_SMPTYPE,
      multiple_pcaa,
      natamer_psca,
      -notes_pcaa,
      -notes_pcab,
      nsaiddays_pcaa,
      nsaidgr24_pcab,
      nsaidgr7_pcaa,
      numbness_pcaa,
      numbness_pcab,
      painproc_pcaa,
      painproc_pcab,
      -participantGUID, # identifier
      -pid, # identifier
      -processTimesComments,
      protocol,
      raceoth_psca,
      randomGroupCode,
      randomGroupDescription,
      randomGroupGUID,
      -recid,
      reconcileStatus,
      -recordthread_pcaa,
      -recordthread_pcab,
      -RIN, # dropped b/c it is the outcome
      samemeal_pcab,
      sampleGroupCode,
      sampleGroupDescription,
      sampleGroupGUID,
      sampleGroupOrder,
      sampleNumber,
      sampleprocessGroupCode,
      sampleprocessGroupDescription,
      sampleprocessGroupGUID,
      sampleprocessGroupOrder,
      sampletime,
      sampleTypeCode,
      sampleTypeDescription,
      sampleTypeGUID,
      sampleTypeOrder,
      schedulecode,
      scheduleDescription,
      scheduleGUID,
      sedconex_pcaa,
      sedex_pcaa,
      sex_psca,
      siteGUID,
      siteID,
      siteName,
      skinirritat_pcaa,
      skinirritat_pcab,
      spintime,
      staffID,
      -staffID_pcaa,
      -staffID_pcab,
      stopreas_pcab,
      stpnewmed_pcaa,
      strtnewmed_pcaa,
      study,
      supine30,
      supinemin,
      swelling_pcaa,
      swelling_pcab,
      t_BLOfreeze,
      t_BLOincubate,
      t_BLOspin,
      t_liedown,
      t_strt24rst_pcab,
      t_strtprerst_pcaa,
      t_syrstp,
      t_syrstrt,
      testWeight,
      timepoint,
      timepointDescription,
      timepointGUID,
      timepointOrder,
      timepointVersion,
      uploadGUID,
      verified,
      Viability,
      -vialLabel,
      visitCode,
      visitDescription,
      visitGUID,
      visitstatus,
      wtkg_pcaa,
      wtkg_pcab
    )



}
