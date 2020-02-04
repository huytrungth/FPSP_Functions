.libPaths("C:/Data/R_Library")
package_load = function(needed){
  installed <- needed %in% installed.packages()[, 'Package']
  if (length(needed[!installed]) > 0)
    install.packages(needed[!installed], dependencies = TRUE)
  
  lapply(needed, require, character.only = TRUE)
}

duplicate.df = function(df, field){
  row.names(df) = 1:nrow(df)
  field.values = df[ ,field, drop=T]
  c.dpc = field.values[duplicated(field.values)]
  names(field.values) =row.names(df)
  if(length(c.dpc)>0){
    for(c in c.dpc){
      rs = na.omit(as.numeric(which(c==field.values)))
      rs = rs[rs!=max(rs)]
      df = df[-rs, ]
    }
    warn= paste("WARNING: Duplicates in", field,"- the last row was used for:",stri_join_list(stri_extract_all_boundaries(c.dpc), sep="", collapse=", "))
  } else {
    df = df
    warn ="none"
  }
  return(list(df,warn))
}

pri_matrix = function(pri_rop, pri_ss,pri_tn,tn_selected,lu_Pricode,lu_technique){
  pri_ss = merge(pri_ss, lu_technique[, c("SurveyTechID", "SurveyTechPri")], by="SurveyTechID", all.x=T, all.y=F)
  pri_ss = pri_ss[pri_ss$SurveyTechPri %in% pri_tn, ]
  matrix = pri_rop[ ,c("CoupeID", "ProposedHarvestDate")]
  names(matrix) = c("COUPE_ID","PHD")
  mtx = as.data.frame(matrix(nrow=nrow(matrix), ncol = length(pri_tn)))
  names(mtx) = sort(pri_tn)
  row.names(mtx) = row.names(matrix)
  matrix = merge(matrix,mtx, by= "row.names")
  matrix$Row.names=NULL
  pri_coupes = matrix$COUPE_ID
  for(c in pri_coupes){
    cdf = pri_ss[pri_ss$CoupeID==c, c("CoupeID", "SurveyTechPri", "FieldSurveyStatusID", "NoSurveyClassID")]
    if(nrow(cdf)>0){
      ctn = cdf$SurveyTechPri
      for(tn in ctn){
        stt = cdf[cdf$SurveyTechPri ==tn, "FieldSurveyStatusID"]
        if(length(stt)>0){
          status_rank = c(5,4,3,6,1,2)
          stt_index = match(stt,status_rank)
          stt = status_rank[min(stt_index)]
        }
        if(stt==1|stt==6){
          substt = cdf[cdf$SurveyTechPri ==tn, "NoSurveyClassID"]
          pri_code = lu_Pricode[lu_Pricode$FieldSurveyStatusSubclassID==substt, "PriCodeValue",drop=T][1]
        } else {
          pri_code = lu_Pricode[lu_Pricode$FieldSurveyID==stt, "PriCodeValue"]
        }
        matrix[matrix$COUPE_ID==c, tn] = pri_code
      }
    }
  }
  matrix[is.na(matrix)] = 0
  tn.exclude = pri_tn[!pri_tn %in% tn_selected]
  if(length(tn.exclude)>0){
    for(tn in tn.exclude){
      tn.val = matrix[ ,tn,drop=T]
      matrix[which(tn.val==0),tn] = 2
    }
  }
  return(matrix)
}
