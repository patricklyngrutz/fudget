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

  read.table(text = fidelity_txn, sep = ',', header = T) |>
    dplyr::rename_with(~snakecase::to_snake_case(.x))

}
