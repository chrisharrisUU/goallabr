#' Within Subjects Confidence Intervals.
#'
#' \code{within_ci} returns lower and upper bounds of confidence intervals
#' per factor for data with at least one within subject factor.
#'
#' @param data: Dataframe in long format
#' @param subject: Name of single column in data containing participant identifiers
#' @param value: Name of single column in data containing values of DV
#' @param conf: Confidence interval to be calculated
#' @param ws_factors: names of (multiple) columns in data containing within-subject factors
#' @param bs_factors: names of (multiple) columns in data containing between-subject factors
#' @return Returns length of Confidence Intervals per factor
#
#' @examples
#' # One within factor, no between factors
#' within_ci(data = data_1w,
#'            subject = subject,
#'            value = DV,
#'            ws_factors = within1)
#'
#' # Multiple within and between factors
#' within_ci(data = data_2w2b,
#'            subject = subject,
#'            value = DV,
#'            ws_factors = c(within1, within2),
#'            bs_factors = c(between1, between2))
#'
#' # Multiple within and between factors with graph
#'  data_2w2b %>%
#'    within_ci(data = .,
#'              subject = subject,
#'              value = DV,
#'              ws_factors = c(within1, within2),
#'              bs_factors = c(between1, between2)) %>%
#'   ggplot(aes(x = within1,
#'              y = sample_mean,
#'              group = within2)) +
#'   geom_bar(stat = "identity",
#'            position = position_dodge(width = 1),
#'            color = "black",
#'            aes(fill = within2)) +
#'   geom_errorbar(position = position_dodge(width = 1),
#'                 width = .1,
#'                 aes(ymin = sample_mean - CI,
#'                     ymax = sample_mean + CI)) +
#'   scale_fill_manual(values = c("white", "grey")) +
#'   labs(y = "DV",
#'        title = "Example bar plot - two within two between") +
#'   theme_classic() +
#'   facet_grid(between1 ~ between2)
#'
#' @importFrom dplyr %>%
#' @export
within_ci <- function(data,
                      subject,
                      value,
                      conf = .95,
                      ws_factors = c(),
                      bs_factors = c()) {

  # data: Dataframe in long format
  # subject: Name of single column in .data containing participant identifiers
  # value: Name of single column in .data containing values of DV
  # conf: Confidence interval to be calculated
  # ws_factors: names of (multiple) columns in .data containing within-subject factors
  # bs_factors: names of (multiple) columns in .data containing between-subject factors

  ### Preparations----
  # Quote all column names handed into function
  subject <- rlang::enquo(subject)
  value <- rlang::enquo(value)
  ws_factors <- rlang::enexprs(ws_factors)
  bs_factors <- rlang::enexprs(bs_factors)
  all_grouping <- dplyr::vars(!!!bs_factors, !!!ws_factors) # All within and between factors

  # Determine total number of levels for all within subject variables
  nwithin_levels <- data %>%
    dplyr::select(!!!ws_factors) %>%
    table() %>%
    dim() %>%
    prod()

  # Check that no duplicates and, importantly, all within-factors are declared
  check <- data %>%
    dplyr::group_by(!!subject) %>%
    dplyr::summarize(n = dplyr::n())
  if (any(check$n > nwithin_levels)) {
    warning("Probably you are not declaring all within-factors or you may have duplicates in your data!")
    return(NULL)
  }

  ### Normalize data----
  # (i.e., get rid of between-subject variance)
  # Exact calculation is dependent on existance of between-subject factors
  if (rlang::is_quosure(bs_factors)) {
    # With between-subject factors
    norm_data <- data %>%
      dplyr::group_by(!!subject) %>%
      dplyr::mutate(mean_pp = mean(!!value)) %>%
      dplyr::group_by(!!!bs_factors) %>%
      dplyr::mutate(grand_mean = mean(!!value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(norm_mean = !!value - .data$mean_pp + .data$grand_mean)
  } else {
    # Without between-subject factors
    norm_data <- data %>%
      dplyr::group_by(!!subject) %>%
      dplyr::mutate(mean_pp = mean(!!value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(grand_mean = mean(!!value)) %>%
      dplyr::mutate(norm_mean = !!value - .data$mean_pp + .data$grand_mean)
  }

  ### Calculate all measures of interest----
  # Calculate correction factor
  # (square root is necessary due to multiplication with sd's instead of variances)
  correction_factor <- sqrt(nwithin_levels/(nwithin_levels - 1))

  # Get sample size, mean and standard deviation for each condition
  sum_data <- norm_data %>%
    dplyr::group_by_at(all_grouping) %>% # Group by each within and between factor
    dplyr::summarise(sample_mean = mean(!!value),
                     normalized_mean = mean(.data$norm_mean),
                     sample_sd = stats::sd(.data$norm_mean),
                     sample_N = dplyr::n()) %>%
    dplyr::mutate(corrected_sd = .data$sample_sd * correction_factor) %>%
    dplyr::mutate(sample_se = .data$corrected_sd/sqrt(.data$sample_N)) %>%
    dplyr::mutate(CI_mult = stats::qt(conf/2 + .5, .data$sample_N - 1)) %>%
    dplyr::mutate(CI = .data$sample_se * .data$CI_mult)

  ### Return output----
  sum_data
}
