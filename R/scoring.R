score_one <- function(vals, spec) {
  ok<-!is.na(vals);answered<-sum(ok);total<-length(vals);min_answered<-as.integer(spec$min_answered%||%total)
  if(answered<min_answered)return(list(value=NA_real_,status="insufficient",answered=answered,missing=total-answered,prorated=FALSE))
  op<-spec$operation
  if(op=="sum")value<-sum(vals[ok])
  else if(op=="mean")value<-mean(vals[ok])
  else if(op=="weighted_sum"){w<-as.numeric(unlist(spec$weights));if(length(w)!=total)psy_abort("weighted_sum requires one weight per item.","psy_error_scoring");value<-sum(vals[ok]*w[ok])}
  else if(op=="count_if"){cmp<-spec$operator%||%"gte";cut<-as.numeric(spec$value);value<-sum(switch(cmp,eq=vals[ok]==cut,gte=vals[ok]>=cut,lte=vals[ok]<=cut,psy_abort("Unsupported count_if operator.","psy_error_scoring")))}
  else psy_abort(sprintf("Unsupported operation: %s",op),"psy_error_scoring")
  prorated<-FALSE
  if(answered<total&&isTRUE(spec$prorate)){if(op=="sum")value<-value*total/answered;prorated<-TRUE}
  list(value=as.numeric(value),status=if(prorated)"prorated" else "complete",answered=answered,missing=total-answered,prorated=prorated)
}
#' Score a Registered Instrument
#' @param x Standard observations.
#' @param instrument Instrument object, path, or registered ID.
#' @param registry Registry object.
#' @param scores Optional score-name subset.
#' @param on_invalid Invalid-value policy.
#' @param audit Attach an audit record.
#' @return A `psy_score` data frame.
#' @export
score_instrument<-function(x,instrument,registry=psy_registry(),scores=NULL,on_invalid=c("error","exclude","keep_na"),audit=TRUE){
  on_invalid<-match.arg(on_invalid);inst<-get_instrument(instrument,registry);key<-x$instrument_id==inst$instrument_id&x$instrument_version==inst$version
  dat<-x[key,,drop=FALSE];if(!NROW(dat))psy_abort("No observations match the requested instrument and version.","psy_error_scoring")
  item_defs<-setNames(inst$items,vapply(inst$items,function(z)z$item_id,character(1)));invalid<-logical(NROW(dat))
  for(i in seq_len(NROW(dat))){def<-item_defs[[dat$item_id[i]]];if(is.null(def)){invalid[i]<-TRUE;next};allowed<-as.numeric(unlist(def$allowed_values));if(!is.na(dat$value_num[i])&&length(allowed)&&!dat$value_num[i]%in%allowed)invalid[i]<-TRUE}
  q<-new_quality();if(any(invalid)){q<-new_quality(make_quality("range","error",paste(which(invalid),collapse=","),"value_num","Values fall outside the instrument definition.","Correct values or choose an explicit invalid-value policy."));if(on_invalid=="error")psy_abort("Invalid instrument values were found.","psy_error_scoring",problems=q);if(on_invalid=="exclude")dat<-dat[!invalid,,drop=FALSE] else dat$value_num[invalid]<-NA_real_}
  defs<-inst$scores;if(!is.null(scores))defs<-Filter(function(z)(z$score_name%||%"")%in%scores,defs);if(!length(defs))psy_abort("No requested score definitions were found.","psy_error_scoring")
  gkey<-interaction(dat$person_id,dat$assessment_id,dat$instrument_id,dat$instrument_version,drop=TRUE,lex.order=TRUE);groups<-split(dat,gkey);rows<-list();expl<-list();k<-0L;e<-0L
  for(g in groups)for(spec in defs){ids<-as.character(unlist(spec$items));sub<-g[match(ids,g$item_id),,drop=FALSE];vals<-sub$value_num
    for(j in seq_along(ids)){def<-item_defs[[ids[j]]];if(!is.null(def$reverse)){mn<-as.numeric(def$reverse$min);mx<-as.numeric(def$reverse$max);vals[j]<-ifelse(is.na(vals[j]),NA_real_,mn+mx-vals[j])}}
    z<-score_one(vals,spec);k<-k+1L;computed<-now_utc();sid<-stable_id("score",g$person_id[1],g$assessment_id[1],inst$instrument_id,inst$version,spec$score_name)
    rows[[k]]<-data.frame(score_id=sid,person_id=g$person_id[1],assessment_id=g$assessment_id[1],observed_at=min(g$observed_at),instrument_id=inst$instrument_id,instrument_version=inst$version,score_name=spec$score_name,score_value=z$value,score_min=as.numeric(spec$theoretical_min%||%NA),score_max=as.numeric(spec$theoretical_max%||%NA),answered_n=as.integer(z$answered),missing_n=as.integer(z$missing),prorated=z$prorated,score_status=z$status,scoring_version=inst$version,input_hash=stable_hash(g$observation_id),computed_at=computed,stringsAsFactors=FALSE)
    for(j in seq_along(ids)){e<-e+1L;expl[[e]]<-data.frame(score_id=sid,item_id=ids[j],value_raw=sub$value_raw[j],value_num=sub$value_num[j],transformed_value=vals[j],reversed=!is.null(item_defs[[ids[j]]]$reverse),weight=if(!is.null(spec$weights))as.numeric(unlist(spec$weights))[j] else 1,missing=is.na(vals[j]),operation=spec$operation,scoring_version=inst$version,stringsAsFactors=FALSE)}
  }
  out<-new_psy_df(do.call(rbind,rows),"psy_score",q);attr(out,"explanation")<-do.call(rbind,expl);if(audit)attr(out,"audit")<-new_audit("score_instrument",x,out,list(instrument=inst$instrument_id,version=inst$version,on_invalid=on_invalid),NROW(q));out
}
#' Explain Score Inputs and Transformations
#' @param x A score object.
#' @param score_id Optional score identifier.
#' @return Item-level scoring provenance.
#' @export
explain_score<-function(x,score_id=NULL){z<-attr(x,"explanation");if(is.null(z))return(data.frame());if(!is.null(score_id))z<-z[z$score_id%in%score_id,,drop=FALSE];z}
#' Interpret Scores Using an Explicit Reference
#' @param x Score object.
#' @param reference Reference object, path, or registered ID.
#' @param registry Registry object.
#' @param mismatch Mismatch policy.
#' @return Updated `psy_score` with interpretation columns.
#' @export
interpret_score<-function(x,reference,registry=psy_registry(),mismatch=c("error","warn","allow")){mismatch<-match.arg(mismatch);ref<-get_reference(reference,registry);bad<-any(x$instrument_id!=ref$instrument_id|x$instrument_version!=ref$instrument_version);if(bad){msg<-"Reference instrument/version does not match the score object.";if(mismatch=="error")psy_abort(msg,"psy_error_reference");if(mismatch=="warn")psy_warn(msg,"psy_warning_reference_mismatch")}
  x$reference_id<-ref$reference_id;x$severity_band<-NA_character_;bands<-ref$severity_bands%||%list();for(b in bands){hit<-!is.na(x$score_value)&x$score_value>=as.numeric(b$lower)&x$score_value<=as.numeric(b$upper);x$severity_band[hit]<-b$label};x$mcid<-as.numeric(ref$mcid$value%||%NA);x$interpretation_status<-ifelse(is.na(x$score_value),"not_available",ifelse(is.na(x$severity_band),"unclassified","classified"));class(x)<-c("psy_score","data.frame");attr(x,"reference")<-ref;x}
