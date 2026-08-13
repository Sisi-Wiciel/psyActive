validate_ruleset_def<-function(x){req<-c("schema_version","ruleset_id","version","rules");miss<-setdiff(req,names(x));if(length(miss))psy_abort(paste("Ruleset is missing:",paste(miss,collapse=", ")),"psy_error_schema");allowed<-c("eq","in","gte","lte","exists");walk<-function(node){if(!is.null(node$operator)&&!node$operator%in%allowed)psy_abort(sprintf("Unsupported rule operator: %s",node$operator),"psy_error_schema");for(nm in intersect(names(node),c("all","any","not"))){kids<-node[[nm]];if(nm=="not")kids<-list(kids);lapply(kids,walk)}};for(r in x$rules)walk(r$when);invisible(TRUE)}
#' Read a Declarative Ruleset
#' @param path YAML or JSON path.
#' @param validate Validate operator and field structure.
#' @return A `psy_ruleset` list.
#' @export
read_ruleset<-function(path,validate=TRUE){x<-read_definition(path);if(validate)validate_ruleset_def(x);class(x)<-c("psy_ruleset","list");attr(x,"source_path")<-normalizePath(path);x}
#' Validate a Ruleset
#' @param x Ruleset object.
#' @param registry Optional registry.
#' @param strict Stop on errors.
#' @return A `psy_quality` object.
#' @export
validate_ruleset<-function(x,registry=NULL,strict=TRUE){err<-tryCatch({validate_ruleset_def(x);NULL},error=function(e)e);if(is.null(err))return(new_quality());q<-new_quality(make_quality("ruleset_schema","error",message=conditionMessage(err),suggestion="Use only documented declarative operators."));if(strict)stop(err);q}
condition_eval <- function(node, row) {
  if (!is.null(node$all)) {
    return(all(vapply(node$all, condition_eval, logical(1), row = row)))
  }
  if (!is.null(node$any)) {
    return(any(vapply(node$any, condition_eval, logical(1), row = row)))
  }
  if (!is.null(node$not)) return(!condition_eval(node$not, row))

  field <- node$field
  if (is.null(field) || !field %in% names(row)) return(FALSE)
  lhs <- row[[field]][1]
  rhs <- node$value

  switch(
    node$operator,
    eq = identical(as.character(lhs), as.character(rhs)),
    `in` = as.character(lhs) %in% as.character(unlist(rhs)),
    gte = !is.na(lhs) && as.numeric(lhs) >= as.numeric(rhs),
    lte = !is.na(lhs) && as.numeric(lhs) <= as.numeric(rhs),
    exists = !is.null(lhs) && !is.na(lhs),
    FALSE
  )
}
#' Apply Transparent Workflow Rules
#' @param events Event data.
#' @param scores Optional score data.
#' @param quality Optional quality data.
#' @param ruleset Ruleset object or path.
#' @param as_of Evaluation time.
#' @param audit Attach audit record.
#' @return Prioritized `psy_event` object.
#' @export
apply_rules<-function(events,scores=NULL,quality=NULL,ruleset,as_of=Sys.time(),audit=TRUE){if(is.character(ruleset))ruleset<-read_ruleset(ruleset);validate_ruleset_def(ruleset);out<-as.data.frame(events);if(!NROW(out)){out<-new_psy_df(out,"psy_event");return(out)};for(i in seq_len(NROW(out)))for(r in ruleset$rules)if(condition_eval(r$when,out[i,,drop=FALSE])){out$alert_level[i]<-as.character(r$then$alert_level%||%out$alert_level[i]);out$reason_code[i]<-r$then$reason_code%||%out$reason_code[i];out$rule_id[i]<-r$rule_id;out$rule_version[i]<-ruleset$version};out$alert_level<-factor(as.character(out$alert_level),levels=.psy_alert_levels,ordered=TRUE);out<-new_psy_df(out,"psy_event");if(audit)attr(out,"audit")<-new_audit("apply_rules",events,out,list(ruleset_id=ruleset$ruleset_id,version=ruleset$version,as_of=as_of));out}
#' Record a Human Review
#' @param event One event row or event identifier.
#' @param reviewer_id Pseudonymous local reviewer identifier.
#' @param disposition Review disposition.
#' @param action_code Optional institution-defined action code.
#' @param rationale_code Optional rationale code.
#' @param next_review_at Optional next review time.
#' @param reviewed_at Review time.
#' @return A one-row `psy_review` object.
#' @export
record_review<-function(event,reviewer_id,disposition,action_code=NULL,rationale_code=NULL,next_review_at=NULL,reviewed_at=Sys.time()){allowed<-c("confirmed","not_concerning","insufficient_data","duplicate","escalated");if(!disposition%in%allowed)psy_abort(paste("disposition must be one of",paste(allowed,collapse=", ")),"psy_error_schema");eid<-if(is.data.frame(event))event$event_id[1] else as.character(event)[1];if(is.na(eid)||!nzchar(eid))psy_abort("A valid event_id is required.","psy_error_schema");t<-as_utc(reviewed_at,"UTC");nxt<-if(is.null(next_review_at))as.POSIXct(NA,tz="UTC") else as_utc(next_review_at,"UTC");out<-data.frame(review_id=stable_id("review",eid,reviewer_id,t,disposition),event_id=eid,reviewer_id=as.character(reviewer_id),reviewed_at=t,disposition=disposition,action_code=action_code%||%NA_character_,rationale_code=rationale_code%||%NA_character_,next_review_at=nxt,stringsAsFactors=FALSE);new_psy_df(out,"psy_review",audit=new_audit("record_review",event,out,list(reviewer_id=reviewer_id,disposition=disposition)))}
#' Derive Current Event Status
#' @param events Event data.
#' @param reviews Review history.
#' @return Event data with derived status and latest-review fields.
#' @export
event_status<-function(events,reviews){out<-as.data.frame(events);out$event_status<-out$event_status%||%"open";out$latest_disposition<-NA_character_;out$latest_reviewed_at<-as.POSIXct(NA,tz="UTC");if(!NROW(reviews))return(new_psy_df(out,"psy_event"));for(i in seq_len(NROW(out))){z<-reviews[reviews$event_id==out$event_id[i],,drop=FALSE];if(NROW(z)){z<-z[order(z$reviewed_at),];last<-z[NROW(z),];out$latest_disposition[i]<-last$disposition;out$latest_reviewed_at[i]<-last$reviewed_at;out$event_status[i]<-if(last$disposition=="escalated")"escalated" else "reviewed"}};new_psy_df(out,"psy_event")}
