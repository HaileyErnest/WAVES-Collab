check_input_ref <- function(lst_ref,
                            my_tz) {

  lst_ref <- lst_ref[sapply(
    lst_ref,
    \(x) {
      !all(simplify_is_null(x))
    }
  )]
  lst_chk <-
    vector(mode = "list",
           length = length(lst_ref)) |>
    setNames(names(lst_ref))

  for (i in seq_along(lst_ref)) {

    le_ref <- names(lst_ref)[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_chk[[le_ref]] <- switch(
        le_ref,
        "do"  = {
          rlang::try_fetch(
            check_input_do(
              fpa_do = lst_ref$do,
              my_tz  = my_tz
            ),
            waves_chk_do_name = \(cnd) {cnd},
            waves_chk_do_site = \(cnd) {cnd},
            waves_chk_do_obs  = \(cnd) {cnd},
            waves_chk_do_dttm = \(cnd) {cnd},
            error = \(cnd) {
              cli::cli_abort(
                message = c(
                  "Unexpected Error.",
                  "i" = "Share with WAVES teams."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            }
          )
        },
        "img" = {}, # TODO
        "pal" = {
          rlang::try_fetch(
            check_input_pal(
              vct_fpa_palp = lst_ref$pal$palp,
              vct_fpa_palv = lst_ref$pal$palv,
              spo_pat      = lst_ref$pal$spo_pat
            ),
            waves_chk_ap_miss  = \(cnd) {cnd},
            waves_chk_ap_id_pt = \(cnd) {cnd},
            error = \(cnd) {
              cli::cli_abort(
                message = c(
                  "Unexpected Error.",
                  "i" = "Share with WAVES teams."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            }
          )
        },
        "pass" = {} # TODO
      )
    } else {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_chk[[le_ref]] <- lst_ref[[le_ref]]$process_meta_function(
      #   lst_param <- lst_ref[[le_ref]][
      #     !names(lst_ref[[le_ref]]) %in% c("process_meta_function",
      #                                      "process_file_function")
      #   ]
      # )
    }
  }

  vct_error <- sapply(lst_chk, inherits, what = "error")

  if (any(vct_error)) {
    lst_error <- lst_chk[vct_error]
    vct_pretty_nm <- recode_values(
      names(lst_error),
      "do"   ~ "Direct Observation",
      "img"  ~ "Still-Images",
      "pal"  ~ "activPAL",
      "pass" ~ "ActiPass"
    )
    lst_message <- vector(mode = "list",
                          length = length(lst_error))

    for (i in seq_along(lst_error)) {
      lst_message[[i]] <- c(
        ">" = paste0("Reference ", vct_pretty_nm[i], ":"),
        "x" = lst_error[[i]]$message,
        lst_error[[i]]$body
      )
    }
    cli::cli_abort(
      message = c(
        "Input from the following reference's had user errors. See below.",
        unlist(lst_message),
        "i" = "NOTE: Disregard below text about debugging."
      ),
      call    = rlang::caller_env()
    )
    lapply(lst_chk[vct_error], conditionMessage)

  }

  # If activPAL data is provided alongside direct observation data, check if
  # "(?<obs>)" was present in site_pid_observation pattern.
  # If not, we can't do much checking until the merging step. We are assuming
  # all direct observation sessions occur within the activPAL file, and can only
  # check that once merging the data.
  if ("observation" %in% names(lst_chk$pal)) {
    chk_obs <- !any(
      unique(lst_chk$pal$observation) %in% unique(lst_chk$do$observation)
    )
    chk_spo <- any(
      lst_chk$do |>
        count(site, pid, observation) |>
        unite(col = spo,
              site, pid, observation,
              sep = "_") |>
        pull(spo) !=
        paste(
          lst_chk$do$site[1],
          lst_chk$pal$pid,
          lst_chk$pal$observation,
          sep = "_"
        )
    )

    # TODO: Figure if chk_obs or chk_spo is better. chk_spo is better
    # for giving more informative error, but still need to test with UWM
    # data.
    if (chk_obs) {
      cli::cli_abort(
        message = c(
          "Observations found from activPAL filenames do not match.",
          "WAVES team still working on providing which files do not match DO data.",
          "i" = "Please double-check both direct observation and activPAL data to ensure unique site, pid, and observation combinations match.",
          "i" = "NOTE: Disregard below text about debugging."
        ),
        call = rlang::caller_env()
      )
    }

  }

  return(lst_chk)

}
check_input_do <- function(fpa_do,
                           my_tz) {

  df <- fread(fpa_do)
  names(df) <-
    names(df) |>
    tolower()

  # Prior guiding documents had variable named as "date_time".
  if ("date_time" %in% names(df)) names(df)[names(df) == "date_time"] <- "datetime"

  vct_nm <- c(
    "site", "pid", "observation", "datetime", "date", "time", "domain_do",
    "posture_do", "intensity3_do", "intensity4_do", "steps_do", "sedtype_do"
  )
  chk_names <- !vct_nm %in% names(df)

  if (any(chk_names)) {
    vct_miss <- vct_nm[chk_names]
    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "The following variables are missing or named incorrectly:",
        indent_cli_message(vct_miss),
        "i" = "Please provide the variable or rename it.",
        "i" = "If it is a variable whos name ends with '_do' and that variable was not collected at all in your study, still provide as an empty column."
      ),
      class = "waves_chk_do_name",
      call = rlang::caller_env()
    )
  }

  df <-
    df |>
    mutate(
      date =
        date |>
        parse_date_time(orders = c("ymd", "mdy", "dmy"),
                        tz     = my_tz) |>
        as_date(),
      time =
        time |>
        parse_date_time(orders = c("HMS", "IMSp"),
                        tz = my_tz) |>
        format("%H:%M:%S")
    )

  # Site, Pid, Observation
  df_spo <- count(df, site, pid, observation)
  chk_site <- uniqueN(df_spo$site) > 1

  if (chk_site) {
    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "More than one value for {.var site} variable provided:",
        "i" = "Values provided:",
        indent_cli_message(unique(df_spo$site)),
        "i" = "There should only be one unique value for {.var site} variable."
      ),
      class = "waves_chk_do_site",
      call = rlang::caller_env()
    )
  }

  # From experience, observation sessions per participants are less than 5. Can
  # change this number if a site has more than 5
  chk_obs <- uniqueN(df_spo$observation) >= 5

  if (chk_obs) {
    vct_obs <- unique(df_spo$observation)

    if (length(vct_obs) > 10) {
      # Pretty printing, don't overload console.
      vct_obs <- c(
        vct_obs[1:10],
        paste0("...plus ", length(vct_obs) - 10, " unique values.")
      )
    }

    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "Five or more observation session values found.",
        "i" = "Unique observation sessions found:",
        indent_cli_message(vct_obs),
        "i" = "There are typically less than 5 sessions per participant in direct observation studies.",
        "i" = "Please double-check your DO file."
      ),
      class = "waves_chk_do_obs",
      call = rlang::caller_env()
    )
  }

  # Check if datetime is in ISO 8601 format of YYYY-mm-ddTHH:MM:SSZ.
  if (!is.POSIXct(df$datetime[1])) {
    df$datetime <-
      paste0(df$date, "_", df$time) |>
      ymd_hms(tz = my_tz) |>
      with_tz(tzone = "UTC")
  } else if (!my_tz %in% c("UTC", "GMT", "Etc/GMT+0")) {
    # Check if date + time together is = to datetime. It shouldn't be since
    # time is suppose to be in local time unless lst_yaml$my_tz is UTC as well.
    chk_dttm <-
      df |>
      summarise(
        chk = as.character(datetime[1]) == ymd_hms(paste(date[1], time[1]), tz = my_tz),
        .by = c(site, pid, observation)
      ) |>
      pull(chk) |>
      any()

    if (chk_dttm) {
      cli::cli_abort(
        message = c(
          "User error with supplied Direct Observation data.",
          "x" = "{.var datetime} values have the same value as {.var date} and {.var time} variables combined.",
          "i" = "{.var datetime} should only be in UTC time, while date and time variables are in local time.",
          "i" = "Please double-check your DO file."
        ),
        class = "waves_chk_do_dttm",
        call = rlang::caller_env()
      )
    }

  }

  df

}
check_input_pal <- function(vct_fpa_palp,
                            vct_fpa_palv,
                            spo_pat) {
  # Extract ids and match files
  mtx_palp <-
    basename(vct_fpa_palp) |>
    stri_match(regex = spo_pat[1])
  colnames(mtx_palp)[1] <- "spo"
  df_palp <- as_tibble(mtx_palp)

  mtx_palv <-
    basename(vct_fpa_palv) |>
    stri_match(regex = spo_pat[1])
  colnames(mtx_palv)[1] <- "spo"
  df_palv <- as_tibble(mtx_palv)

  if ("obs" %in% names(df_palp)) {
    names(df_palp)[names(df_palp) == "obs"] <- "observation"
    names(df_palv)[names(df_palv) == "obs"] <- "observation"
  }

  # If the site_pid_observation from a filename is not extracted properly, then
  # the entire row will be empty. Just use the first column to check then.
  if (anyNA(c(df_palp$spo, df_palv$spo))) {

    vct_err_epoch <- basename(vct_fpa_palp)[is.na(df_palp$spo)]
    vct_err_event <- basename(vct_fpa_palv)[is.na(df_palv$spo)]
    spo_pat_pretty <-
      sub(
        x = spo_pat,
        pattern = "\\^",
        replacement = ""
      ) |>
      gsub(
        x = _,
        pattern = "\\\\d",
        replacement = "0"
      )
    cli::cli_abort(
      message = c(
        "User error with supplied activPAL data.",
        "The following activPAl files do not start with supplied {.envvar id_pt} '{id_pt_pretty}':",
        "i" = "1 second epoch files:",
        indent_cli_message(vct_err_epoch),
        "i" = "Event files:",
        indent_cli_message(vct_err_event)
      ),
      class = "waves_chk_ap_id_pt",
      call = rlang::caller_env()
    )

  }

  vct_nomatch_epoch <- setdiff(df_palv$spo, df_palp$spo)
  vct_nomatch_event <- setdiff(df_palp$spo, df_palv$spo)

  if (length(vct_nomatch_epoch) != 0 || length(vct_nomatch_event) != 0) {
    cli::cli_abort(
      message = c(
        "User error with supplied activPAL data.",
        "Not all activPAL files have a matching event OR matching 1sec file.",
        "i" = "Missing epoch files:",
        indent_cli_message(vct_nomatch_epoch),
        "i" = "Missing event files:",
        indent_cli_message(vct_nomatch_event),
        "i" = "Please either create missing files or remove extra files."
      ),
      class = "waves_chk_ap_miss",
      call = rlang::caller_env()
    )
  } else {
    df_palp |>
      mutate(
        fpa_epoch = vct_fpa_palp,
        fpa_event = vct_fpa_palv
      )
  }
}
