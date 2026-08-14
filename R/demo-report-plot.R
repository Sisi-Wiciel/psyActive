#' Generate Synthetic Demonstration Observations
#' @param n_people Number of synthetic people.
#' @param n_assessments Assessments per person.
#' @param seed Random seed.
#' @return A `psy_observation` object. All content is fictional and demo-only.
#' @export
psy_demo_data<-function(n_people=3L,n_assessments=8L,seed=20260813L){set.seed(seed);people<-sprintf("demo-p%02d",seq_len(n_people));rows<-expand.grid(person_id=people,assessment=seq_len(n_assessments),item=seq_len(9L),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE);rows$assessment_id<-sprintf("%s-a%02d",rows$person_id,rows$assessment);rows$when<-as.POSIXct("2026-01-01 09:00:00",tz="UTC")+(rows$assessment-1)*7*86400;trend<-ifelse(rows$person_id==people[1]&rows$assessment>5,1,0);rows$value<-pmin(3,pmax(0,round(stats::rnorm(NROW(rows),1+trend,.7))));rows$item_id<-paste0("dms_",rows$item);as_psy_observation(rows,mapping=list(person_id="person_id",assessment_id="assessment_id",observed_at="when",item_id="item_id",value_num="value",instrument_id="demo_mood_9",instrument_version="1.0.0",language="zh-CN",record_id="assessment_id",rater_type="self",source_type="clinic",setting="adult_outpatient_demo"),timezone="UTC",source_system="psyActive-demo")}
#' Plot a Longitudinal Score Trajectory
#' @param x Score data.
#' @param person_id Person identifier.
#' @param metric Score name or full `instrument:score` metric.
#' @param baseline Optional baseline object.
#' @param events Optional event data.
#' @param reference Optional reference.
#' @param timezone Display timezone.
#' @return A ggplot object.
#' @export
plot_trajectory<-function(x,person_id,metric,baseline=NULL,events=NULL,reference=NULL,timezone=NULL){if(!requireNamespace("ggplot2",quietly=TRUE))psy_abort("Package 'ggplot2' is required for plotting.","psy_error_dependency");d<-as.data.frame(x);d$metric_id<-paste(d$instrument_id,d$score_name,sep=":");d<-d[d$person_id==person_id&(d$score_name==metric|d$metric_id==metric),,drop=FALSE];if(!NROW(d))psy_abort("No matching trajectory data.","psy_error_insufficient_data");observed_at<-score_value<-NULL;p<-ggplot2::ggplot(d,ggplot2::aes(x=observed_at,y=score_value))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(x="Time",y="Score",title=paste("Trajectory:",person_id),subtitle="Decision support only; not a diagnosis")+ggplot2::theme_minimal();if(!is.null(baseline)){b<-baseline[baseline$person_id==person_id&baseline$metric_id%in%unique(d$metric_id),,drop=FALSE];if(NROW(b))p<-p+ggplot2::geom_hline(yintercept=b$center[1],linetype=2,color="steelblue")};p}
escape_html<-function(x){x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);x<-gsub(">","&gt;",x,fixed=TRUE);x}
html_table<-function(x){if(is.null(x)||!NROW(x))return("<p>No records.</p>");x<-as.data.frame(x);head<-paste0("<tr>",paste0("<th>",escape_html(names(x)),"</th>",collapse=""),"</tr>");rows<-apply(x,1,function(r)paste0("<tr>",paste0("<td>",escape_html(as.character(r)),"</td>",collapse=""),"</tr>"));paste0("<table>",head,paste(rows,collapse=""),"</table>")}

reserve_report_base <- function(output_dir, type, stamp) {
  root <- paste0("psyactive-", type, "-", stamp)
  index <- 0L
  repeat {
    suffix <- if (index == 0L) "" else sprintf("-%03d", index)
    base <- paste0(root, suffix)
    artifacts <- file.path(
      output_dir,
      paste0(base, c(".html", ".manifest.json", ".audit.csv"))
    )
    lock <- file.path(output_dir, paste0(".", base, ".lock"))
    if (!any(file.exists(artifacts)) &&
        dir.create(lock, showWarnings = FALSE)) {
      return(list(base = base, lock = lock))
    }
    if (!dir.exists(lock) && !any(file.exists(artifacts))) {
      psy_abort("Could not reserve a report filename.", "psy_error_io")
    }
    index <- index + 1L
    if (index > 99999L) {
      psy_abort("Could not reserve a unique report filename.", "psy_error_io")
    }
  }
}

filter_report_data <- function(data, person_id) {
  filter_person <- function(z) {
    if (is.null(z) || is.null(person_id) || !"person_id" %in% names(z)) {
      return(z)
    }
    z[z$person_id %in% person_id, , drop = FALSE]
  }
  filtered <- lapply(data, filter_person)
  reviews <- filtered$reviews
  if (is.null(person_id) || is.null(reviews) || "person_id" %in% names(reviews)) {
    return(filtered)
  }

  if (!is.data.frame(reviews) || !"event_id" %in% names(reviews)) {
    filtered$reviews <- if (is.data.frame(reviews)) {
      reviews[FALSE, , drop = FALSE]
    } else {
      data.frame()
    }
    return(filtered)
  }

  events <- data$events
  can_link <- is.data.frame(events) && "person_id" %in% names(events) &&
    "event_id" %in% names(events)
  allowed_event_ids <- if (can_link) {
    event_ids <- as.character(events$event_id)
    selected <- events$person_id %in% person_id
    selected_ids <- event_ids[selected & !is.na(event_ids) & nzchar(event_ids)]
    outside_ids <- event_ids[!selected & !is.na(event_ids) & nzchar(event_ids)]
    setdiff(unique(selected_ids), unique(outside_ids))
  } else {
    character()
  }
  filtered$reviews <- reviews[
    !is.na(reviews$event_id) & as.character(reviews$event_id) %in% allowed_event_ids,
    , drop = FALSE
  ]
  filtered
}

report_audit_input <- function(data) {
  lapply(data, function(z) {
    if (!is.data.frame(z)) return(z)
    columns <- lapply(z, identity)
    names(columns) <- names(z)
    columns
  })
}

#' Render an Auditable HTML Report
#' @param data Named list containing scores, quality, baseline, events, and reviews.
#' @param person_id Optional person filter.
#' @param type Patient or cohort report.
#' @param audience Clinician or researcher.
#' @param language Chinese or English.
#' @param format Currently HTML; PDF requires an external workflow.
#' @param output_dir Output directory.
#' @param template Reserved for a custom template.
#' @param include_audit Write an audit CSV. Patient-filtered reports contain a
#'   report-scope audit derived only from the filtered sections; upstream audit
#'   rows are included only for unfiltered reports.
#' @param quiet Suppress completion message.
#' @return Paths to report, manifest, and optionally audit files.
#' @details With a person filter, reviews that do not carry `person_id` are
#' included only when their `event_id` unambiguously links to an event for the
#' selected person. Output names receive a numeric suffix when another report
#' uses the same UTC timestamp. The manifest contains the report SHA-256 and,
#' when requested, the audit CSV SHA-256. A report-scope audit row hashes the
#' exact filtered sections used for rendering. To avoid carrying cohort-level
#' provenance into a patient report, attached upstream audit rows are copied
#' only when `person_id` is `NULL`.
#' @export
render_psy_report <- function(data, person_id = NULL,
                              type = c("patient", "cohort"),
                              audience = c("clinician", "researcher"),
                              language = c("zh-CN", "en"),
                              format = c("html", "pdf"),
                              output_dir = tempdir(), template = NULL,
                              include_audit = TRUE, quiet = TRUE) {
  type <- match.arg(type)
  audience <- match.arg(audience)
  language <- match.arg(language)
  format <- match.arg(format)
  if (format == "pdf") {
    psy_abort(
      paste(
        "PDF rendering requires a separately configured Quarto/LaTeX",
        "workflow; HTML is guaranteed by the core package."
      ),
      "psy_error_dependency"
    )
  }
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    psy_abort("Could not create the report output directory.", "psy_error_io")
  }

  d <- filter_report_data(data, person_id)
  report_sections <- d[c("scores", "quality", "baseline", "events", "reviews")]
  generated_at <- now_utc()
  stamp <- format(generated_at, "%Y%m%dT%H%M%SZ", tz = "UTC")
  reservation <- reserve_report_base(output_dir, type, stamp)
  on.exit(unlink(reservation$lock, recursive = TRUE, force = TRUE), add = TRUE)
  base <- reservation$base
  report <- file.path(output_dir, paste0(base, ".html"))
  manifest <- file.path(output_dir, paste0(base, ".manifest.json"))
  audit_path <- file.path(output_dir, paste0(base, ".audit.csv"))

  title <- if (language == "zh-CN") {
    "psyActive \u7cbe\u795e\u79d1\u4e3b\u52a8\u5065\u5eb7\u62a5\u544a"
  } else {
    "psyActive Mental Health Monitoring Report"
  }
  warning <- if (language == "zh-CN") {
    paste0(
      "\u672c\u62a5\u544a\u4ec5\u7528\u4e8e\u79d1\u7814\u4e0e\u4e34\u5e8a\u8f85\u52a9\uff0c\u4e0d\u8fdb\u884c\u81ea\u52a8\u8bca\u65ad\u3001\u7ed9\u836f\u6216\u81ea\u6740\u98ce\u9669\u5206\u7ea7\uff1b",
      "\u5371\u673a\u76f8\u5173\u4fe1\u606f\u5fc5\u987b\u7531\u5408\u683c\u4eba\u5458\u4eba\u5de5\u590d\u6838\u3002"
    )
  } else {
    paste(
      "Decision support only: no automated diagnosis, prescribing, or",
      "suicide-risk classification. Crisis-related information requires",
      "qualified human review."
    )
  }
  sections <- paste0(
    "<h2>",
    c(
      "Scores", "Data quality", "Personal baselines",
      "Events requiring workflow review", "Human reviews"
    ),
    "</h2>",
    mapply(
      html_table,
      report_sections,
      USE.NAMES = FALSE
    ),
    collapse = ""
  )
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", title,
    "</title><style>body{font-family:sans-serif;max-width:1100px;",
    "margin:2em auto;line-height:1.5}table{border-collapse:collapse;",
    "width:100%;font-size:90%}th,td{border:1px solid #ccc;padding:.35em}",
    "th{background:#eaf2f8}.warning{background:#fce4d6;",
    "border-left:5px solid #c00;padding:1em}</style></head><body><h1>",
    title, "</h1><div class='warning'>", warning,
    "</div><p>Generated: ",
    format(generated_at, tz = "UTC", usetz = TRUE),
    "</p>", sections,
    "</body></html>"
  )
  writeLines(html, report, useBytes = TRUE)

  paths <- c(report = report, manifest = manifest)
  report_hash <- digest::digest(file = report, algo = "sha256")
  audit_hash <- NULL
  if (include_audit) {
    attached <- if (is.null(person_id)) {
      Filter(
        Negate(is.null),
        lapply(data, function(z) attr(z, "audit"))
      )
    } else {
      list()
    }
    report_audit <- new_audit(
      "render_psy_report",
      report_audit_input(report_sections),
      output = list(report_hash = report_hash),
      config = list(
        person_id = person_id, type = type, audience = audience,
        language = language, format = format
      )
    )
    audits <- do.call(rbind, c(attached, list(report_audit)))
    utils::write.csv(
      audits, audit_path, row.names = FALSE, fileEncoding = "UTF-8"
    )
    audit_hash <- digest::digest(file = audit_path, algo = "sha256")
    paths <- c(paths, audit = audit_path)
  }

  man <- list(
    package = "psyActive",
    version = as.character(utils::packageVersion("psyActive")),
    schema_version = .psy_schema_version,
    generated_at = format(generated_at, tz = "UTC", usetz = TRUE),
    type = type,
    audience = audience,
    language = language,
    person_id = person_id,
    report_hash = report_hash,
    safety = "decision_support_only"
  )
  if (!is.null(audit_hash)) man$audit_hash <- audit_hash
  jsonlite::write_json(man, manifest, pretty = TRUE, auto_unbox = TRUE)

  if (!quiet) message("Report written to ", report)
  paths
}
