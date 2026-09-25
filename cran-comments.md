# CRAN comments

## Submission

This is an update of a package that is on CRAN. It moves the version from
0.2.0 to 0.3.0. Version 0.2.1 was released on GitHub only, so this update
holds its changes as well. `NEWS.md` gives the full list.

New:

* `rewind_diff()` shows what changed between two steps: the value before, the
  value after, and a short description.
* `rewind_enable()` takes a `restore_timeout` argument. The wait for the
  browser to finish a restore was fixed at 2 seconds, which can be too short
  on a slow connection.
* `rewind_buttons()` takes a `button_class` argument.

Fixed:

* Undo, redo and the history rail did nothing when `rewind_enable()` was
  called inside a `moduleServer()`, which the documents say is supported.
* The history rail scrolled the page or sidebar that holds it on every
  change.
* The history rail showed times in UTC, not in the viewer's time zone.
* `NA` and `Inf` in `coalesce_ms`, `restore_timeout` and `hold_ms` were
  either accepted or stopped with an unclear message. They are now refused
  with a message that names the argument.
* A data frame of the user with the same column names as a `fileInput()`
  value was dropped from the history with no message.

## Test environments

* local Windows 11, R 4.6.1 (release), `R CMD check --as-cran` on the built
  tarball, with `_R_CHECK_CRAN_INCOMING_=TRUE`,
  `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE`, the PDF manual, and pandoc. Run once
  without `NOT_CRAN` and once with it.
* GitHub Actions with `--as-cran`: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)

R-devel is covered by the Ubuntu (devel) job, which runs
`R CMD check --as-cran`. This machine has the release version of R only.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Notes for the reviewer

Examples for the session-scoped functions are wrapped in `if (interactive())`.
These functions need a live Shiny session (`shiny::getDefaultReactiveDomain()`)
and stop with a clear message without one, so there is no useful
non-interactive example. The functions with no session requirement
(`rewind_buttons()`, `rewind_ui()`, `rewind_dependency()`) have examples that
run during the check.

Four test files drive a small Shiny application in a headless browser through
shinytest2: `test-app-smoke.R`, `test-module-browser.R`, `test-rail-scroll.R`
and `test-rail-time.R`. Each calls `skip_on_cran()`, and also skips rather
than fails when a browser cannot be started. They therefore do not run on the
CRAN check machines and do not need Chrome.

The package code writes nothing to the filesystem, opens no connections and
starts no processes. All state is held per Shiny session in
`session$userData`.
