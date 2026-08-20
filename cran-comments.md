# CRAN comments

## Submission

This is a new submission. The package has not been on CRAN before; the
version is 0.2.0 because 0.1.0 was released publicly on GitHub only, and
the code has changed materially since.

## Test environments

* local Windows 11, R 4.6.1 (release), with `--as-cran`,
  `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE` and the PDF manual
* GitHub Actions with `--as-cran`: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)

The tarball was checked with the current release of R, not R-devel, because
this machine has only the release version. R-devel is covered by the
Ubuntu (devel) job above, which also runs `R CMD check --as-cran`.

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer: 'Tanmay Chanda <tanmaychanda96@gmail.com>'
  New submission

The only note is the standard first-submission note.

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
