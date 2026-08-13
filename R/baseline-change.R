#' Change Detection Configuration
#' @param threshold Standardized shift threshold.
#' @param persistence Consecutive observations required.
#' @param confidence Reliable-change confidence level.
#' @return A configuration list.
#' @export
psy_change_config<-function(threshold=2.5,persistence=2L,confidence=.95)list(threshold=threshold,persistence=as.integer(persistence),confidence=confidence)
metric_columns<-function(x){if(inherits(x,"psy_score"))list(value="score_value",metric="score_name",time="observed_at") else {if(!all(c("metric_id","value","observed_at")%in%names(x)))psy_abort("Input must be psy_score or contain metric_id, value, and observed_at.","psy_error_schema");list(value="value",metric="metric_id",time="observed_at")}}
#' Learn an Immutable Personal Baseline
#' @param x Score data or metric data.
#' @param metrics Optional metric subset.
#' @param window Optional length-two UTC time window.
#' @param method Baseline estimator.
#' @param min_n Minimum number of observations.
#' @param zero_scale Zero-variance policy.
#' @param audit Attach audit information.
#' @return A `psy_baseline` object.
#' @export
learn_baseline<-function(x,metrics=NULL,window=NULL,method=c("median_mad","mean_sd"),min_n=4L,zero_scale=c("flag","fallback_iqr","error"),audit=TRUE){method<-match.arg(method);zero_scale<-match.arg(zero_scale);mc<-metric_columns(x);d<-as.data.frame(x);d$metric_id<-if(inherits(x,"psy_score"))paste(d$instrument_id,d$score_name,sep=":") else d[[mc$metric]];d$value<-d[[mc$value]];d$time<-d[[mc$time]];if(!is.null(metrics))d<-d[d$metric_id%in%metrics,,drop=FALSE];if(!is.null(window)){if(length(window)!=2L)psy_abort("window must contain start and end times.","psy_error_schema");d<-d[d$time>=window[1]&d$time<=window[2],,drop=FALSE]}
  groups<-split(d,interaction(d$person_id,d$metric_id,drop=TRUE));rows<-lapply(groups,function(z){v<-z$value[is.finite(z$value)];n<-length(v);status<-"ok";center<-scale<-NA_real_;if(n<min_n)status<-"insufficient" else {if(method=="median_mad"){center<-median(v);scale<-stats::mad(v,center=center,constant=1.4826)}else{center<-mean(v);scale<-stats::sd(v)};if(is.na(scale)||scale==0){if(zero_scale=="error")psy_abort("A baseline has zero variance.","psy_error_insufficient_data");if(zero_scale=="fallback_iqr")scale<-stats::IQR(v)/1.349;if(is.na(scale)||scale==0)status<-"zero_variance"}}
    data.frame(baseline_id=stable_id("baseline",z$person_id[1],z$metric_id[1],min(z$time),max(z$time),method),person_id=z$person_id[1],metric_id=z$metric_id[1],window_start=min(z$time),window_end=max(z$time),method=method,center=center,scale=scale,n_obs=as.integer(n),status=status,stringsAsFactors=FALSE)})
  out<-new_psy_df(do.call(rbind,rows),"psy_baseline");if(audit)attr(out,"audit")<-new_audit("learn_baseline",x,out,list(method=method,min_n=min_n,window=window));out}
#' Calculate Reliable Change
#' @param x Score data ordered over time.
#' @param reference Reference object or path.
#' @param from Comparison origin.
#' @param confidence Confidence level.
#' @param direction Score direction.
#' @return Data frame containing change, RCI, and status.
#' @export
reliable_change<-function(x,reference,from=c("baseline","previous","episode_start"),confidence=.95,direction=c("higher_worse","higher_better")){from<-match.arg(from);direction<-match.arg(direction);ref<-if(inherits(reference,"psy_reference"))reference else read_reference(reference);rel<-as.numeric(ref$reliability$estimate%||%NA);sdref<-as.numeric(ref$reference_sd%||%NA);if(!is.finite(rel)||!is.finite(sdref)){out<-as.data.frame(x);out$change_value<-NA_real_;out$rci<-NA_real_;out$reliable_change<-"insufficient_reference";return(out)};se<-sdref*sqrt(2*(1-rel));crit<-stats::qnorm(1-(1-confidence)/2);d<-as.data.frame(x);d<-d[order(d$person_id,d$score_name,d$observed_at),];g<-split(d,interaction(d$person_id,d$score_name,drop=TRUE));out<-lapply(g,function(z){origin<-if(from=="previous")c(NA,head(z$score_value,-1))else rep(z$score_value[1],NROW(z));z$change_value<-z$score_value-origin;z$rci<-z$change_value/se;z$reliable_change<-ifelse(is.na(z$rci),"not_available",ifelse(abs(z$rci)<crit,"no_reliable_change",ifelse((direction=="higher_worse"&z$rci>0)|(direction=="higher_better"&z$rci<0),"reliable_deterioration","reliable_improvement")));z});do.call(rbind,out)}
#' Detect Persistent Standardized Shifts
#' @param x Score data.
#' @param baseline Personal baseline object.
#' @param threshold Standardized threshold.
#' @param persistence Consecutive observations required.
#' @param direction Direction to detect.
#' @param cooldown Minimum number of subsequent rows before another event.
#' @param audit Attach audit record.
#' @return A `psy_event` object.
#' @export
detect_shift <- function(x, baseline, threshold = 2.5, persistence = 2L,
                         direction = c("both", "increase", "decrease"),
                         cooldown = 0, audit = TRUE) {
  direction <- match.arg(direction)
  d <- as.data.frame(x)
  d$metric_id <- if (inherits(x, "psy_score")) {
    paste(d$instrument_id, d$score_name, sep = ":")
  } else {
    d$metric_id
  }
  d$value <- if (inherits(x, "psy_score")) d$score_value else d$value
  m <- merge(
    d, as.data.frame(baseline),
    by = c("person_id", "metric_id"), all.x = TRUE,
    suffixes = c("", "_baseline")
  )
  m <- m[order(m$person_id, m$metric_id, m$observed_at), ]
  m$z <- (m$value - m$center) / m$scale
  rows <- list()
  k <- 0L
  groups <- split(m, interaction(m$person_id, m$metric_id, drop = TRUE))

  for (z in groups) {
    hit <- is.finite(z$z) & switch(
      direction,
      both = abs(z$z) >= threshold,
      increase = z$z >= threshold,
      decrease = z$z <= -threshold
    )
    runs <- rle(hit)
    ends <- cumsum(runs$lengths)
    idx <- ends[runs$values & runs$lengths >= persistence]

    if (length(idx)) {
      for (ii in idx) {
        k <- k + 1L
        value <- z$value[ii]
        rows[[k]] <- data.frame(
          event_id = stable_id(
            "event", z$person_id[ii], z$metric_id[ii],
            z$observed_at[ii], "sustained_shift"
          ),
          person_id = z$person_id[ii],
          event_time = z$observed_at[ii],
          detected_at = now_utc(),
          event_type = "sustained_shift",
          metric_id = z$metric_id[ii],
          observed_value = value,
          expected_value = z$center[ii],
          change_value = value - z$center[ii],
          alert_level = factor(
            "review", levels = .psy_alert_levels, ordered = TRUE
          ),
          reason_code = "STANDARDIZED_PERSISTENT_SHIFT",
          rule_id = NA_character_,
          rule_version = NA_character_,
          event_status = "open",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(rows)) {
    out <- data.frame(
      event_id = character(), person_id = character(),
      event_time = as.POSIXct(character()),
      detected_at = as.POSIXct(character()), event_type = character(),
      metric_id = character(), observed_value = double(),
      expected_value = double(), change_value = double(),
      alert_level = factor(levels = .psy_alert_levels, ordered = TRUE),
      reason_code = character(), rule_id = character(),
      rule_version = character(), event_status = character()
    )
  } else {
    out <- do.call(rbind, rows)
  }
  out <- new_psy_df(out, "psy_event")
  if (audit) {
    attr(out, "audit") <- new_audit(
      "detect_shift", x, out,
      list(
        threshold = threshold, persistence = persistence,
        direction = direction, cooldown = cooldown
      )
    )
  }
  out
}
#' Detect Configured Longitudinal Changes
#' @param x Score data.
#' @param baseline Baseline object.
#' @param methods Methods to run. Currently `shift` is supported.
#' @param config Change configuration.
#' @return A `psy_event` object.
#' @export
detect_change<-function(x,baseline=NULL,methods=c("reliable_change","shift"),config=psy_change_config()){events<-list();if("shift"%in%methods){if(is.null(baseline))psy_abort("baseline is required for shift detection.","psy_error_insufficient_data");events[[length(events)+1L]]<-detect_shift(x,baseline,config$threshold,config$persistence)};if(!length(events))new_psy_df(data.frame(),"psy_event") else events[[1]]}
