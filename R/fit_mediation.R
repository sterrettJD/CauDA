#' Run mediation analysis for a given feature pair.
#' @description Should not be run on its own. Typically, the main workhorse function will handle this for you.
#' @param y The outcome variable 
#' @param x_dir The putative direct effect variable .
#' @param x_indir The putative indirect effect variable.
#' @param f A formula object. Used to assess differential associations.
#' @param metadata Additional information such as group or covariate values. 
#' @return To be determined. Currently a table of mediation output summary
#' @export
#' @import mediation
#' @importFrom stats glm update.formula
#' @examples
#' mediation_df <- data.frame(z = sample(LETTERS[1:2], 32, replace = TRUE), mtcars)
#' fit_mediation(y = rnorm(32), 
#' x_dir = rnorm(32), 
#' x_indir = rnorm(32), 
#' f = ~z, 
#' metadata = mediation_df)
#'   
fit_mediation <- function(y, x_dir, x_indir, f, metadata){
  requireNamespace("mediation")
  
  #Collect all information in an object. 
  mediation_df <- data.frame(y = y, x_dir = x_dir, x_indir = x_indir, metadata)
  
  #OPTIONAL: Prepare a result container. Currently not used
  res_vec = vector("numeric", length = 6L)
  #Structure:
  #p dir, B dir, p indir, B indir, p joint, B joint, p mediate, B mediate 
  
  #We'll fit three models.
  
  #Update the formulas first to include the appropriate putative direct & indirect effects 
  
  ####NOTE: Currenlty I'm including all covariates in all models, but I'm not sure if that makes sense. 
  #We should think about this & perhaps allow for effect-specific covariates.
  #similarly, we should probably allow different effects to have different link functions. 
  dir_formula   = update.formula(old = f, y             ~ x_dir  * 1 * (.))
  indir_formula = update.formula(old = f, x_indir       ~ x_dir  * 1 * (.))
  joint_formula = update.formula(old = f, y ~  (x_indir + x_dir) * 1 * (.))
  
  #Fit the models
  dir_fit   <- glm(formula = dir_formula,   data = mediation_df)
  indir_fit <- glm(formula = indir_formula, data = mediation_df)
  joint_fit <- glm(formula = joint_formula, data = mediation_df)
  
  #Assess mediation
  mediation_fit <- mediation::mediate(model.m  = indir_fit, 
                                      model.y  = joint_fit, 
                                      treat    = 'x_dir', 
                                      mediator = 'x_indir', 
                                      data     = mediation_df)
  
  summ_med = enframe_mediation(res_mediate = mediation_fit)
  
  return(summ_med)
}

#' Wrangle output from mediation analysis to table.
#' @description Make a table out of a mediate object.
#' @param res_mediate The outcome from our mediation call
#' @return A structured table summarizing the mediation call.
#'    
enframe_mediation <- function(res_mediate){
  return(matrix(
    unlist(res_mediate[c("d0","d0.ci","d0.p",      
                           "d1","d1.ci","d1.p",     
                           "z0","z0.ci","z0.p",
                           "z1","z1.ci","z1.p",
                           
                           "tau.coef","tau.ci","tau.p",
                           
                           "n0","n0.ci","n0.p",
                           "n1","n1.ci","n1.p",
                           
                           "d.avg","d.avg.ci","d.avg.p",
                           "z.avg","z.avg.ci","z.avg.p",
                           "n.avg","n.avg.ci","n.avg.p")]), ncol = 4, byrow = T, 
    dimnames = list(
      c("ACME (control)", "ACME (treated)", 
        "ADE (control", "ADE (treated)", 
        "Total Effect", 
        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
        "ACME (average)", "ADE (average)", "Prop. Mediated (average)"), 
      c("Estimate", "95% CI Lower", "95% CI Upper", "p-value")
    )
  )
  )
  
}
