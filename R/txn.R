read_fidelity_txn <- function(filepath){

  fidelity_txn <- readr::read_file(filepath)

  # clip header
  fidelity_txn_clip_top <- stringr::str_locate(fidelity_txn, '^\n\n')[2]
  if (is.na(fidelity_txn_clip_top)){
    logger::log_error('{filepath} does not begin with whitespace'); stop()
  }

  # clip footer
  fidelity_txn_clip_bottom <- stringr::str_locate(
    fidelity_txn,
    stringr::regex('\n{3}"The data and information.*\nDate downloaded [0-9]{2}/[0-9]{2}/[0-9]{4} .*(am|pm)$', dotall=T)
  )[1]
  if (is.na(fidelity_txn_clip_bottom)){
    logger::log_error('{filepath} does not begin with whitespace'); stop()
  }

  fidelity_txn <- fidelity_txn |>
    stringr::str_sub(fidelity_txn_clip_top + 1, fidelity_txn_clip_bottom - 2)

  fidelity_txn <- read.table(text = fidelity_txn, sep = ',', header = T) |>
    dplyr::rename_with(~snakecase::to_snake_case(.x))

  if (!identical(colnames(fidelity_txn), c('run_date','action','symbol','description','type','quantity','price','commission','fees','accrued_interest','amount','cash_balance','settlement_date'))){
    logger::log_error('{filepath} column names do not match format'); stop()
  }

  fidelity_txn |>
    dplyr::mutate(
      across(c(run_date, settlement_date), ~lubridate::mdy(.x)),
      across(c(quantity, price, commission, fees, accrued_interest, amount, cash_balance), ~suppressWarnings(as.double(.x))),
      across(where(is.character), ~dplyr::na_if(stringr::str_trim(.x), '')),
      description = na_if(description, 'No Description')
    )

}

standardize_fidelity_txn <- function(fidelity_txn){

  fidelity_txn |>
    dplyr::select(run_date, action, amount) |>
    dplyr::rename(date = run_date, description = action)

}
