expect_write_tail_validated <- function(.f, args) {
  bad_filename <- utils::modifyList(args, list(filename = 1))
  expect_error(
    rlang::exec(.f, !!!bad_filename),
    class = "rlang_error"
  )

  bad_overwrite <- utils::modifyList(args, list(overwrite = "yes"))
  expect_error(
    rlang::exec(.f, !!!bad_overwrite),
    class = "rlang_error"
  )
}
