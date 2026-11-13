# CRAN comments

## Submission

This is an update of a package that is on CRAN. It moves the version from
0.3.0 to 0.4.0. `NEWS.md` gives the full list of changes.

One change of behaviour:

* By default, `rewind` now captures only the inputs that the page can put
  back. A plot's hover, click and brush, and the values that DT keeps, have
  no element in the page to restore them. They were recorded before, so each
  move of the mouse over a plot became a step that an undo could not reverse.
  A list of inputs that the user gives is used as before.

New:

* `rewind_status()` lists what `rewind` captures, and why each other input
  is left out.
* `rewind_exclude()` leaves inputs out of the history, also from inside a
  module.
* `rewind_unrecorded()` runs a block of code whose changes are not a step,
  such as code that restores a saved view.
* `rewind_enable()` takes a `labels` argument: the names that a person reads,
  in the history rail and in `rewind_diff()`.
* `rewind_example()` runs the demo application.

Fixed:

* Every argument is checked, with a message that names it. Several wrong
  values were accepted before, and did the wrong thing with no message.
* Password fields are never kept in the history.
* An input that changes all the time no longer holds back every step.
* The help of `rewind_pause()` suggested a use that cannot work. It now says
  so, and points to `rewind_unrecorded()`.

## Test environments

* local Windows 11, R 4.6.1 (release), `R CMD check --as-cran` on the built
  tarball, with `_R_CHECK_CRAN_INCOMING_=TRUE`,
  `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE`, the PDF manual, and pandoc. Run once
  without `NOT_CRAN` and once with it.
* win-builder: Windows Server 2022, R-devel (2026-11-12 r90881 ucrt),
  Status OK
* GitHub Actions with `--as-cran`: macOS (release), Windows (release),
  Ubuntu (devel, release, and the four versions before release), and Ubuntu
  with R 4.1, the minimum in `DESCRIPTION`.

R-devel was checked on win-builder (Windows) and in the Ubuntu (devel) job
(Linux).

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Notes for the reviewer

Examples for the session-scoped functions are wrapped in `if (interactive())`.
These functions need a live Shiny session (`shiny::getDefaultReactiveDomain()`)
and stop with a clear message without one, so there is no useful
non-interactive example. `rewind_example()` starts a Shiny application, so
its example is wrapped in the same way. The functions with no session
requirement (`rewind_buttons()`, `rewind_ui()`, `rewind_dependency()`) have
examples that run during the check.

Seven test files drive a small Shiny application in a headless browser
through shinytest2: `test-app-smoke.R`, `test-module-browser.R`,
`test-rail-scroll.R`, `test-rail-time.R`, `test-restorable.R`,
`test-text-inputs.R` and `test-unrecorded.R`. Each of those tests calls
`skip_on_cran()`, and also skips rather than fails when a browser cannot be
started. They therefore do not run on the CRAN check machines and do not need
Chrome.

The package code writes nothing to the filesystem, opens no connections and
starts no processes. All state is held per Shiny session in
`session$userData`. `rewind_example()` starts the demo application with
`shiny::runApp()` only when the user calls it.
