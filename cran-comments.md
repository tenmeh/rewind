# CRAN comments

## Submission

This is an update of a package that is on CRAN. It moves the version from
0.2.0 to 0.2.1.

The changes correct two faults and add two arguments. `NEWS.md` gives the
full list.

* `rewind_enable()` waited a fixed 2 seconds for the browser to finish a
  restore. That is enough on a fast connection. On a slow one the wait can
  be too short, and a partial state then becomes a history entry that the
  user never made. The wait is now the `restore_timeout` argument.
* A data frame of the user with the same four column names as the value of
  a `fileInput()` was dropped from the history, with no message. The test
  now examines the column types as well as the names.
* `rewind_buttons()` takes a `button_class` argument, so an application can
  give the buttons a Bootstrap variant or a size.
* The arrows on the buttons are now SVG and not HTML entities. They thus
  have the same shape on each system.

## Test environments

* local Windows 11, R 4.6.1 (release), with `--as-cran`,
  `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE` and the PDF manual
* GitHub Actions with `--as-cran`: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)

The tarball was checked with the current release of R, not R-devel, because
this machine has only the release version. R-devel is covered by the
Ubuntu (devel) job above, which also runs `R CMD check --as-cran`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Notes for the reviewer

Examples for the session-scoped functions are wrapped in `if (interactive())`.
These functions require a live Shiny session (`shiny::getDefaultReactiveDomain()`)
and error informatively without one, so there is no meaningful non-interactive
example to run. The functions with no session requirement
(`rewind_buttons()`, `rewind_ui()`, `rewind_dependency()`) have examples that
execute during check.

One test (`tests/testthat/test-app-smoke.R`) drives the bundled demo app in a
headless browser via shinytest2. It calls `skip_on_cran()` and additionally
skips itself, rather than failing, if a browser cannot be started, so it does
not run on CRAN's check machines and does not require Chrome to be present.

The package writes nothing to the filesystem outside of `tempdir()`, opens no
connections, and starts no processes. All state is held per Shiny session in
`session$userData`.
