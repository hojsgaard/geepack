#' Growth curves of pigs in a 3x3 factorial experiment
#' 
#' The \code{dietox} data frame has 861 rows and 7 columns.
#'  
#' @format This data frame contains the following columns: \describe{
#'     \item{Weight}{a numeric vector} \item{Feed}{a numeric vector}
#'     \item{Time}{a numeric vector} \item{Pig}{a numeric vector} \item{Evit}{a
#'     numeric vector} \item{Cu}{a numeric vector} \item{Litter}{a numeric
#'     vector} }
#' @source Lauridsen, C., Højsgaard, S.,Sørensen, M.T. C. (1999) Influence
#'     of Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and
#'     Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
#' @keywords datasets
#' @examples
#' 
#' data(dietox)
#' dietox$Cu     <- as.factor(dietox$Cu)
#' gee01 <- geeglm (Weight ~ Time + Cu + Cu * Time, id =Pig, data = dietox,
#'          family=gaussian,corstr="ex")
#' 
#' mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
#' gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
#' summary(gee1)
#' anova(gee1)
#' 
#' 
"dietox"



#' Ordinal Data from Koch
#' 
#' The \code{koch} data frame has 288 rows and 4 columns.
#' 
#' @format This data frame contains the following columns: \describe{
#'     \item{trt}{a numeric vector} \item{day}{a numeric vector} \item{y}{an
#'     ordered factor with levels: \code{1} < \code{2} < \code{3}} \item{id}{a
#'     numeric vector} }
#' @keywords datasets
#' @examples
#' 
#' data(koch)
#' fit <- ordgee(ordered(y) ~ trt + as.factor(day), id=id, data=koch, corstr="exch")
#' summary(fit)
#' 
"koch"





#' Ohio Children Wheeze Status
#' 
#' The \code{ohio} data frame has 2148 rows and 4 columns. The dataset is a
#' subset of the six-city study, a longitudinal study of the health effects of
#' air pollution.
#' 
#' 
#' @format This data frame contains the following columns: \describe{
#'     \item{resp}{an indicator of wheeze status (1=yes, 0=no)} \item{id}{a
#'     numeric vector for subject id} \item{age}{a numeric vector of age, 0 is 9
#'     years old} \item{smoke}{an indicator of maternal smoking at the first
#'     year of the study} }
#' @references Fitzmaurice, G.M. and Laird, N.M. (1993) A likelihood-based
#'     method for analyzing longitudinal binary responses, \emph{Biometrika}
#'     \bold{80}: 141--151.
#' @keywords datasets
#' @examples
#' 
#' data(ohio)
#' fit <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
#'              family=binomial, corstr="exch", scale.fix=TRUE)
#' summary(fit)
#' fit.ar1 <- geese(resp ~ age + smoke + age:smoke, id=id, data=ohio,
#'                  family=binomial, corstr="ar1", scale.fix=TRUE)
#' summary(fit.ar1)
#' 
"ohio"





#' Clustered Ordinal Respiratory Disorder
#' 
#' The \code{respdis} data frame has 111 rows and 3 columns. The study described
#' in Miller et. al. (1993) is a randomized clinical trial of a new treatment of
#' respiratory disorder. The study was conducted in 111 patients who were
#' randomly assigned to one of two treatments (active, placebo). At each of four
#' visits during the follow-up period, the response status of each patients was
#' classified on an ordinal scale.
#' 
#' 
#' @format This data frame contains the following columns:
#' \describe{
#'    \item{y1, y2, y3, y4}{ordered factor measured at 4 visits for the response with
#'     levels, \code{1} < \code{2} < \code{3}, 1 = poor, 2 = good, and 3 =
#'     excellent}
#'    \item{trt}{a factor for treatment with levels, 1 = active, 0 =
#'     placebo.} }
#' @references Miller, M.E., David, C.S., and Landis, R.J. (1993) The analysis
#'     of longitudinal polytomous data: Generalized estimating equation and
#'     connections with weighted least squares, \emph{Biometrics} \bold{49}:
#'     1033-1048.
#' @keywords datasets
#' @examples
#' 
#' data(respdis)
#' resp.l <- reshape(respdis, varying = list(c("y1", "y2", "y3", "y4")),
#'                   v.names = "resp", direction = "long")
#' resp.l <- resp.l[order(resp.l$id, resp.l$time),]
#' fit <- ordgee(ordered(resp) ~ trt, id = id, data = resp.l, int.const = FALSE)
#' summary(fit)
#' 
#' z <- model.matrix( ~ trt - 1, data = respdis)
#' ind <- rep(1:111, 4*3/2 * 2^2)
#' zmat <- z[ind,,drop=FALSE]
#' fit <- ordgee(ordered(resp) ~ trt, id = id, data = resp.l, int.const = FALSE,
#'               z = zmat, corstr = "exchangeable")
#' summary(fit)
#' 
"respdis"





#' Data from a clinical trial comparing two treatments for a respiratory
#' illness
#' 
#' The data are from a clinical trial of patients with respiratory illness,
#' where 111 patients from two different clinics were randomized to receive
#' either placebo or an active treatment. Patients were examined at baseline
#' and at four visits during treatment. At each examination, respiratory status
#' (categorized as 1 = good, 0 = poor) was determined.
#' 
#' @name respiratory
#' @aliases respiratory respiratoryWide
#' @docType data
#' @format A data frame with 111 observations on the following 7 variables.
#'     \describe{ \item{center}{a numeric vector} \item{id}{a numeric vector}
#'     \item{age}{a numeric vector} \item{baseline}{a numeric vector}
#'     \item{active}{a numeric vector} \item{center2}{a numeric vector}
#'     \item{female}{a numeric vector} }
#' @keywords datasets
#' @examples
#' 
#' data(respiratory)
#' ## maybe str(respiratory) ; plot(respiratory) ...
#' 
"respiratory"





#' Epiliptic Seizures
#' 
#' The \code{seizure} data frame has 59 rows and 7 columns. The dataset has the
#' number of epiliptic seizures in each of four two-week intervals, and in a
#' baseline eight-week inverval, for treatment and control groups with a total
#' of 59 individuals.
#' 
#' @format This data frame contains the following columns: \describe{
#'     \item{y1}{the number of epiliptic seizures in the 1st 2-week interval}
#'     \item{y2}{the number of epiliptic seizures in the 2nd 2-week interval}
#'     \item{y3}{the number of epiliptic seizures in the 3rd 2-week interval}
#'     \item{y4}{the number of epiliptic seizures in the 4th 2-week interval}
#'     \item{trt}{an indicator of treatment} \item{base}{the number of epilitic
#'     seizures in a baseline 8-week interval} \item{age}{a numeric vector of
#'     subject age} }
#' @references Diggle, P.J., Liang, K.Y., and Zeger, S.L. (1994) Analysis of
#'     Longitudinal Data. Clarendon Press.
#' @source Thall, P.F. and Vail S.C. (1990) Some covariance models for
#'     longitudinal count data with overdispersion. \emph{Biometrics} \bold{46}:
#'     657--671.
#' @keywords datasets
#' @examples
#' 
#' data(seizure)
#' ## Diggle, Liang, and Zeger (1994) pp166-168, compare Table 8.10
#' seiz.l <- reshape(seizure,
#'                   varying=list(c("base","y1", "y2", "y3", "y4")),
#'                   v.names="y", times=0:4, direction="long")
#' seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
#' seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)
#' seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)
#' m1 <- geese(y ~ offset(log(t)) + x + trt + x:trt, id = id,
#'             data=seiz.l, corstr="exch", family=poisson)
#' summary(m1)
#' m2 <- geese(y ~ offset(log(t)) + x + trt + x:trt, id = id,
#'             data = seiz.l, subset = id!=49,
#'             corstr = "exch", family=poisson)
#' summary(m2)
#' 
#' ## Thall and Vail (1990)
#' seiz.l <- reshape(seizure, varying=list(c("y1","y2","y3","y4")),
#'                   v.names="y", direction="long")
#' seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
#' seiz.l$lbase <- log(seiz.l$base / 4)
#' seiz.l$lage <- log(seiz.l$age)
#' seiz.l$v4 <- ifelse(seiz.l$time == 4, 1, 0)
#' m3 <- geese(y ~ lbase + trt + lbase:trt + lage + v4, 
#'             sformula = ~ as.factor(time) - 1, id = id,
#'             data = seiz.l, corstr = "exchangeable", family=poisson)
#' ## compare to Model 13 in Table 4, noticeable difference
#' summary(m3)
#' 
#' ## set up a design matrix for the correlation
#' z <- model.matrix(~ age, data = seizure)  # data is not seiz.l
#' ## just to illustrate the scale link and correlation link
#' m4 <- geese(y ~ lbase + trt + lbase:trt + lage + v4,
#'             sformula = ~ as.factor(time)-1, id = id,
#'             data = seiz.l, corstr = "ar1", family = poisson,
#'             zcor = z, cor.link = "fisherz", sca.link = "log")
#' summary(m4)
#' 
"seizure"





#' Growth of Sitka Spruce Trees
#' 
#' Impact of ozone on the growth of sitka spruce trees.
#' 
#' @format A dataframe \describe{ \item{size:}{size of the tree measured in
#'     \eqn{log(height*diamter^2)}} \item{time:}{days after the 1st january,
#'     1988} \item{tree:}{id number of a tree} \item{treat:}{ozone: grown under
#'     ozone environment, control: ozone free} }
#' @keywords datasets
#' @examples
#' 
#' data(sitka89)
#' 
"sitka89"

#' Log-size of 79 Sitka spruce trees
#' 
#' The \code{spruce} data frame has 1027 rows and 6 columns. The data consists
#' of measurements on 79 sitka spruce trees over two growing seasons. The trees
#' were grown in four controlled environment chambers, of which the first two,
#' containing 27 trees each, were treated with introduced ozone at 70 ppb whilst
#' the remaining two, containing 12 and 13 trees, were controls.
#' 
#' 
#' @format This data frame contains the following columns: \describe{
#'     \item{chamber}{a numeric vector of chamber numbers} \item{ozone}{a factor
#'     with levels \code{enriched} and \code{normal}} \item{id}{a numeric vector
#'     of tree id} \item{time}{a numeric vector of the time when the
#'     measurements were taken, measured in days since Jan. 1, 1988}
#'     \item{wave}{a numeric vector of the measurement number} \item{logsize}{a
#'     numeric vector of the log-size} }
#' @source Diggle, P.J., Liang, K.Y., and Zeger, S.L. (1994) Analysis of
#'     Longitudinal Data, Clarendon Press.
#' @keywords datasets
#' @examples
#' 
#' data(spruce)
#' spruce$contr <- ifelse(spruce$ozone=="enriched", 0, 1)
#' sitka88 <- spruce[spruce$wave <= 5,]
#' sitka89 <- spruce[spruce$wave > 5,]
#' fit.88 <- geese(logsize ~ as.factor(wave) + contr +
#'                           I(time/100*contr) - 1,
#'                 id=id, data=sitka88, corstr="ar1")
#' summary(fit.88)
#' 
#' fit.89 <- geese(logsize ~ as.factor(wave) + contr - 1,
#'                 id=id, data=sitka89, corstr="ar1")
#' summary(fit.89)
#' 
"spruce"





#' Internal geese functions
#' 
#' Internal functions called by other functions.
#' 
#' 
#' @aliases anova.geeglm anovageePrim2 anova.geeglmlist plot.geeglm
#' print.geeglm eprint print.summary.geeglm residuals.geeglm summary.geeglm
#' @keywords internal



#' Internal geese functions
#' 
#' Internal functions called by other functions.
#' 
#' These are not to be called directly by the user.
#' 
#' @aliases crossutri genZodds
#' @keywords internal




