#' FJC federal judge biographies
#'
#' A dataset of federal judges with key dates as well as the name of the
#' nominating president
#'
#' @format ## 'fed_judges' A data frame with 4607 rows and 12 columns:
#' \describe{
#'   \item{action}{category of action, e.g., commission}
#'   \item{date}{date of action}
#'   \item{title}{title of judicial office}
#'   \item{court}{the name of the court}
#'   \item{nominator}{the nominating court}
#'   \item{nomination_date}{date nominated for position}
#'   \item{termination_date}{date the judges term ended}
#'   \itme{senior_date}{date the judge entered senior status}
#'   \item{judge_names}{the judges name}
#'   \item{}{the url of the judge biography}
#' }
#' @source <https://www.fjc.gov/history/judges>
"fed_judges"
