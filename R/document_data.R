#' ids data
#'
#' @description The data are perturbed data, based on scores on Test 14 (“naming
#' antonyms”) and Test 7 (“naming categories”) of the intelligence test
#' IDS-2 (Grob & Hagmann-von Arx, 2018a; Grob et al., 2018b).
#' The data are provided as supplementary material to Timmerman et al. (2021).
#'
#' @docType data
#'
#' @usage data(ids_data)
#'
#' @format A dataframe with three columns:
#' \describe{
#'   \item{\code{age}}{age in years}
#'   \item{\code{y7}}{raw test score on Test 7}
#'   \item{\code{y14}}{raw test score on Test 14}
#' }
#'
#' @keywords datasets
#'
#' @references \insertRef{grob2018ids}{normref}
#' @references \insertRef{grob2018idsNL}{normref}
#' @references \insertRef{timmerman2021tutorial}{normref}
#'
#' @source  \url{https://osf.io/p75a6}
#'
"ids_data"

#' The ids_kn_data are simlulated data for demonstration purposes
#'
#' @description The data are simulated data for demonstration purposes,
#' akin to Test 7 (“naming categories”) of the intelligence test IDS-2 (Grob & Hagmann-von Arx, 2018).
#' It consists of the binary scores on 34 items (KN_1,...,KN_34). The raw test score is the sum of the 34 item scores.
#' The data are provided as supplementary material to Heister et al. (2024).
#'
#' @docType data
#'
#' @usage data(ids_kn_data)
#'
#' @format A dataframe with 36 columns:
#' \describe{
#'   \item{\code{KN_1}}{binary score on item 1}
#'   \item{\code{KN_2}}{binary score on item 2}
#'   ...
#'   \item{\code{KN_34}}{binary score on item 34}
#'   \item{\code{rawscore}}{raw test score as the unweighted sum of the scores on item 1 to item 34}
#'   \item{\code{age_years}}{age in year}
#' }
#'
#' @keywords datasets
#'
#' @references \insertRef{grob2018ids}{normref}
#' @references \insertRef{heister2024item}{normref}
#'
#'
#' @source \url{https://osf.io/dc5k9/files/osfstorage}
#'
"ids_kn_data"



#' cotapp data
#'
#' @description The data are perturbed variants of the scores on
#' the raw speed of block 1 (\code{rt}) and
#' the raw number of errors of block 7 (\code{error})
#' of the normative sample of the cotapp test (Rommelse et al., 2018).
#' More information on the cotapp test: \url{https://www.boom.nl/productgroep/101-45_COTAPP}
#'
#' @docType data
#'
#' @usage data(cotapp_data)
#'
#' @format A dataframe with three columns:
#' \describe{
#'   \item{\code{age}}{age in years}
#'   \item{\code{rt}}{reaction time: scores on the raw speed of block 1}
#'   \item{\code{error}}{number of errors of block 7}
#' }
#'
#' @keywords datasets
#'
#'
#' @references \insertRef{rommelse2020cognitieve}{normref}
#'
"cotapp_data"


#' These fictional reliability data are for demonstration purposes.
#'
#' @description Dataframe with the vectors age and rel, with the ages evaluated, and rel the (fictional) test reliability per age.
#'
#' @docType data
#'
#' @usage data(ids_rel_data)
#'
#' @format A dataframe with two columns:
#' \describe{
#'   \item{\code{age}}{age in years}
#'   \item{\code{rel}}{reliability}
#' }
#'
#' @keywords datasets
#'
#' @source  constructed by authors
#'
"ids_rel_data"




