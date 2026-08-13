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
plot_trajectory<-function(x,person_id,metric,baseline=NULL,events=NULL,reference=NULL,timezone=NULL){if(!requireNamespace("ggplot2",quietly=TRUE))psy_abort("Package 'ggplot2' is required for plotting.","psy_error_dependency");d<-as.data.frame(x);d$metric_id<-paste(d$instrument_id,d$score_name,sep=":");d<-d[d$person_id==person_id&(d$score_name==metric|d$metric_id==metric),,drop=FALSE];if(!NROW(d))psy_abort("No matching trajectory data.","psy_error_insufficient_data");p<-ggplot2::ggplot(d,ggplot2::aes(x=.data$observed_at,y=.data$score_value))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(x="Time",y="Score",title=paste("Trajectory:",person_id),subtitle="Decision support only; not a diagnosis")+ggplot2::theme_minimal();if(!is.null(baseline)){b<-baseline[baseline$person_id==person_id&baseline$metric_id%in%unique(d$metric_id),,drop=FALSE];if(NROW(b))p<-p+ggplot2::geom_hline(yintercept=b$center[1],linetype=2,color="steelblue")};p}
escape_html<-function(x){x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);x<-gsub(">","&gt;",x,fixed=TRUE);x}
html_table<-function(x){if(is.null(x)||!NROW(x))return("<p>No records.</p>");x<-as.data.frame(x);head<-paste0("<tr>",paste0("<th>",escape_html(names(x)),"</th>",collapse=""),"</tr>");rows<-apply(x,1,function(r)paste0("<tr>",paste0("<td>",escape_html(as.character(r)),"</td>",collapse=""),"</tr>"));paste0("<table>",head,paste(rows,collapse=""),"</table>")}
#' Render an Auditable HTML Report
#' @param data Named list containing scores, quality, baseline, events, and reviews.
#' @param person_id Optional person filter.
#' @param type Patient or cohort report.
#' @param audience Clinician or researcher.
#' @param language Chinese or English.
#' @param format Currently HTML; PDF requires an external workflow.
#' @param output_dir Output directory.
#' @param template Reserved for a custom template.
#' @param include_audit Write an audit CSV.
#' @param quiet Suppress completion message.
#' @return Paths to report, manifest, and optionally audit files.
#' @export
render_psy_report<-function(data,person_id=NULL,type=c("patient","cohort"),audience=c("clinician","researcher"),language=c("zh-CN","en"),format=c("html","pdf"),output_dir=tempdir(),template=NULL,include_audit=TRUE,quiet=TRUE){type<-match.arg(type);audience<-match.arg(audience);language<-match.arg(language);format<-match.arg(format);if(format=="pdf")psy_abort("PDF rendering requires a separately configured Quarto/LaTeX workflow; HTML is guaranteed by the core package.","psy_error_dependency");dir.create(output_dir,recursive=TRUE,showWarnings=FALSE);filter_person<-function(z)if(is.null(z)||is.null(person_id)||!"person_id"%in%names(z))z else z[z$person_id%in%person_id,,drop=FALSE];d<-lapply(data,filter_person);stamp<-format(now_utc(),"%Y%m%dT%H%M%SZ",tz="UTC");base<-paste0("psyactive-",type,"-",stamp);report<-file.path(output_dir,paste0(base,".html"));manifest<-file.path(output_dir,paste0(base,".manifest.json"));audit_path<-file.path(output_dir,paste0(base,".audit.csv"));title<-if(language=="zh-CN")"psyActive 精神科主动健康报告" else "psyActive Mental Health Monitoring Report";warning<-if(language=="zh-CN")"本报告仅用于科研与临床辅助，不进行自动诊断、给药或自杀风险分级；危机相关信息必须由合格人员人工复核。" else "Decision support only: no automated diagnosis, prescribing, or suicide-risk classification. Crisis-related information requires qualified human review.";sections<-paste0("<h2>",c("Scores","Data quality","Personal baselines","Events requiring workflow review","Human reviews"),"</h2>",mapply(html_table,d[c("scores","quality","baseline","events","reviews")],USE.NAMES=FALSE),collapse="");html<-paste0("<!doctype html><html><head><meta charset='utf-8'><title>",title,"</title><style>body{font-family:sans-serif;max-width:1100px;margin:2em auto;line-height:1.5}table{border-collapse:collapse;width:100%;font-size:90%}th,td{border:1px solid #ccc;padding:.35em}th{background:#eaf2f8}.warning{background:#fce4d6;border-left:5px solid #c00;padding:1em}</style></head><body><h1>",title,"</h1><div class='warning'>",warning,"</div><p>Generated: ",now_utc()," UTC</p>",sections,"</body></html>");writeLines(html,report,useBytes=TRUE);man<-list(package="psyActive",version=as.character(utils::packageVersion("psyActive")),schema_version=.psy_schema_version,generated_at=format(now_utc(),tz="UTC",usetz=TRUE),type=type,audience=audience,language=language,person_id=person_id,report_hash=digest::digest(file=report,algo="sha256"),safety="decision_support_only");jsonlite::write_json(man,manifest,pretty=TRUE,auto_unbox=TRUE);paths<-c(report=report,manifest=manifest);if(include_audit){audits<-do.call(rbind,Filter(Negate(is.null),lapply(data,function(z)attr(z,"audit"))));if(is.null(audits))audits<-data.frame(note="No attached audit records");utils::write.csv(audits,audit_path,row.names=FALSE,fileEncoding="UTF-8");paths<-c(paths,audit=audit_path)};if(!quiet)message("Report written to ",report);paths}
