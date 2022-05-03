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
  # for example, see line 114, where I put a '-' in front of
  # bid, indicating it would be dropped, and said I was dropping
  # it because it is an identifier.

  # the first four variables are placed at the front of the data
  # for convenience. The others are listed in alphabetical order.

  # if you don't see a variable here, it probably got removed above
  # due to having only missing values.

  data_out <- data_preselect |>
    select(
      outcome,   #grouping
      ppt_type,  #grouping
      vial_type, #grouping
      
      aablack_psca,
      addnlmsrs,
      -addnlmsrs_spc, # just text
      alcogr24_pcaa,
      alcogr24_pcab,
      aliquotamnt,
      -aliquotCode, #whenever you have a code/description/guid set of variables like this, the values always correspond and you only need to use one
      aliquotDescription, #grouping
      -aliquotGUID,
      aliquotmsrmnt,
      allex_pcaa,
      anestallerg_pcaa,
      anestreceiv_pcaa,
      asExpected,
      asian_psca,
      assignmentcode,
      -assignmentdescription,
      -assignmentGUID,
      -barcodeID_pcaa,#Form ID
      -barcodeID_pcab,#Form ID
      -bid, # identifier
      biotindays_pcaa,
      biotingr3_pcaa,
      blddraw_pcaa,
      bleedeasy_pcaa,
      BLOincubate,
      bloodhow,
      -BLOsample_comnt, #comment
      BLOsample_comp, #significant
      BLOsample_stat, #significant
      bruising_pcaa,
      bruising_pcab,
      caffgr24_pcaa, #if you use a pcab / pcaa pair then you should probably use one based on the timepoint (24 hr post and 24 hr rest get pcab)
      caffgr24_pcab,
      caffhrs_pcaa,
      caffhrs_pcab,
      calculatedAge,
      cauc_psca,
      clean_pcaa,
      -codedSiteID,#masked copy of siteid
      coll_staffid,
      collected,
      collectionQtr,
      -collForm,
      -collForm_barcodeID,
      -collForm_comments,
      -collForm_d_visit,
      -collForm_recordthread,
      -collForm_staffID,
      colorCode, #I think this corresponds to sample type?
      -colorDescription,
      -colorGUID,
      -comments,
      d_fusample_pcaa, #combine if used
      d_fusample_pcab,
      d_lastfluvac_pcaa,
      d_mencyc_pcaa,
      d_visit_pca, # this was derived above
      deviation,
      -deviationComments,
      dffcltdraw,
      domainCode,
      -domainDescription,
      -domainguid,
      drinkgr10_pcaa,#combine if used
      drinkgr10_pcab,
      drinkhrs_pcaa,
      drinkshake_pcaa,#combine if used
      drinkshake_pcab,
      drinkwater_pcaa,
      eatgr10_pcaa,#combine if used
      eatgr10_pcab,
      eathrs_pcaa,
      exdays_pcaa,
      -expectedSample,#always 1
      fainting_pcaa,
      feelwell_pcaa,
      freezetime,
      -fuaddcom_pcaa,#combine if used
      -fuaddcom_pcab,
      fuoth_pcaa,
      fuoth_spc_pcaa,
      fushow_pcaa,#combine if used
      fushow_pcab,
      fusignsymp_pcaa,#combine if used
      fusignsymp_pcab,
      fustaffid_pcaa,#combine if used
      fustaffid_pcab,
      fwMatch,
      haex_pcaa,
      health_pcaa,
      hemolysis,
      Htcmavg_hwwt,
      incubatetime,
      -infoCorrect,#if this isn't always 1 then it would probably just be a proxy for date?
      -labelGUID, # identifier
      -labelid, # identifier,
      lastfluvacna_pcaa,
      latexallerg_pcaa,
      latino_psca,
      leakage_pcaa,
      -Live_cell_concentration, # part of the outcome
      -low_rin, # part of the outcome
      -low_viability, # part of the outcome
      -manifestGUID,#proxy for shipment
      meetprereq_pcaa,#combine if used
      meetprereq_pcab,
      mencycdays_pcaa,
      mencycna_pcaa,
      MTP_ALQQUALITYCODE,
      -MTP_EXPECTEDVOL,#always same for same aliquot type
      -MTP_OGLABELID, # identifier
      -MTP_PARTIALVOL,#always same for same aliquot type
      MTP_RECEIVEDDate,#combine all date variables to get things like shipping time?
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
      nsaidgr24_pcab,#combine if used
      nsaidgr7_pcaa,
      numbness_pcaa,#combine if used
      numbness_pcab,
      painproc_pcaa,#combine if used
      painproc_pcab,
      -participantGUID, # identifier
      -pid, # identifier
      -processTimesComments,
      protocol,
      raceoth_psca,
      randomGroupCode,
      -randomGroupDescription,
      -randomGroupGUID,
      -recid,
      reconcileStatus,#they should all be reconciled
      -recordthread_pcaa,
      -recordthread_pcab,
      -RIN, # dropped b/c it is the outcome
      samemeal_pcab,
      -sampleGroupCode,#always same for same aliquot type
      -sampleGroupDescription,
      -sampleGroupGUID,
      -sampleGroupOrder,
      -sampleNumber,
      -sampleprocessGroupCode,#always same for same aliquot type
      -sampleprocessGroupDescription,
      -sampleprocessGroupGUID,
      -sampleprocessGroupOrder,
      sampletime,
      -sampleTypeCode,#always same for same aliquot type
      -sampleTypeDescription,
      -sampleTypeGUID,
      -sampleTypeOrder,
      schedulecode,#important
      -scheduleDescription,
      -scheduleGUID,
      sedconex_pcaa,
      sedex_pcaa,
      sex_psca,
      -siteGUID,
      siteID,
      -siteName,
      skinirritat_pcaa,#combine if used
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
      swelling_pcaa,#combine if used
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
      -timepoint,
      -timepointDescription,
      -timepointGUID,
      timepointOrder,#probably make your own timepoint variable that matches on time
      -timepointVersion,
      -uploadGUID,
      verified,
      Viability,
      -vialLabel,
      visitCode,
      -visitDescription,
      -visitGUID,
      visitstatus,
      wtkg_pcaa,#combine if used?  If there is a big difference that could be interesting, but probably irrelevant to this
      wtkg_pcab
    )



}
