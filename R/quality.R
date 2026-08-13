#' Quality Assessment Configuration
#' @param completion_min Minimum assessment completion fraction.
#' @param timing_min_seconds Optional minimum response duration.
#' @param timing_max_seconds Optional maximum response duration.
#' @return A configuration list.
#' @export
psy_quality_config<-function(completion_min=.8,timing_min_seconds=NULL,timing_max_seconds=NULL)list(completion_min=completion_min,timing_min_seconds=timing_min_seconds,timing_max_seconds=timing_max_seconds)
#' Assess Psychiatric Data Quality
#' @param x Observations.
#' @param registry Optional registry.
#' @param checks Checks to run.
#' @param config Quality configuration.
#' @return A `psy_quality` object.
#' @export
assess_quality<-function(x,registry=NULL,checks=c("schema","range","duplicates","time_order","completion","straightlining","timing"),config=psy_quality_config()){
  qs<-list();k<-0L;if("schema"%in%checks){k<-k+1L;qs[[k]]<-validate_observation(x,registry,if(is.null(registry))"schema" else "registered")}
  if("duplicates"%in%checks){dup<-duplicated(x[c("source_system","assessment_id","item_id")])|duplicated(x[c("source_system","assessment_id","item_id")],fromLast=TRUE);if(any(dup)){k<-k+1L;qs[[k]]<-make_quality("duplicates","error",paste(which(dup),collapse=","),message="Duplicate observation keys detected.",suggestion="Resolve source duplicates.")}}
  if("time_order"%in%checks){ord<-order(x$person_id,x$observed_at);if(!identical(ord,seq_len(NROW(x)))){k<-k+1L;qs[[k]]<-make_quality("time_order","info",message="Rows are not ordered by person and observation time.",suggestion="Sort before longitudinal analysis.")}}
  if("completion"%in%checks){g<-split(x,x$assessment_id);for(nm in names(g)){z<-g[[nm]];rate<-mean(!is.na(z$value_num)|!is.na(z$value_raw));if(rate<config$completion_min){k<-k+1L;qs[[k]]<-make_quality("completion","warning",row_id=nm,message=sprintf("Assessment completion is %.1f%%.",100*rate),suggestion="Review missingness before interpretation.")}}}
  if("straightlining"%in%checks){g<-split(x,x$assessment_id);for(nm in names(g)){z<-g[[nm]]$value_num;z<-z[!is.na(z)];if(length(z)>=4L&&length(unique(z))==1L){k<-k+1L;qs[[k]]<-make_quality("straightlining","info",row_id=nm,message="All available item values are identical.",suggestion="Treat as a data-quality signal, not a clinical event.")}}}
  append_quality(qs)
}
#' Summarize Quality Findings
#' @param x A quality object.
#' @param by Columns to group by.
#' @return Summary data frame.
#' @export
summarize_quality<-function(x,by=c("check_id","severity")){if(!NROW(x))return(data.frame(check_id=character(),severity=character(),n=integer()));if(!all(by%in%names(x)))psy_abort("Unknown quality summary column.","psy_error_schema");key<-interaction(x[by],drop=TRUE,lex.order=TRUE);out<-do.call(rbind,lapply(split(x,key),function(z)cbind(z[1,by,drop=FALSE],n=NROW(z))));rownames(out)<-NULL;out}
#' Plot Quality Findings
#' @param x Quality object.
#' @param type Plot type.
#' @return A ggplot object.
#' @export
plot_quality<-function(x,type=c("overview","missingness","compliance")){type<-match.arg(type);if(!requireNamespace("ggplot2",quietly=TRUE))psy_abort("Package 'ggplot2' is required for plotting.","psy_error_dependency");d<-summarize_quality(x);ggplot2::ggplot(d,ggplot2::aes(x=.data$check_id,y=.data$n,fill=.data$severity))+ggplot2::geom_col()+ggplot2::labs(x="Check",y="Findings",title="psyActive data quality")+ggplot2::theme_minimal()+ggplot2::coord_flip()}
