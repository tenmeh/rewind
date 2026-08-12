# CRAN comments

## Submission

This is a new release.

## Test environments

* local Windows 11, R 4.6.1
* GitHub Actions: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer: 'Tanmay Chanda <jadevenom2430@gmail.com>'
  New submission

The only note is the standard first-submission note.

## Notes for the reviewer

Examples for the session-scoped functions are wrapped in `if (interactive())`.
These functions require a live Shiny session (`shiny::getDefaultReactiveDomain()`)
and error informatively without one, so there is no meaningful non-interactive
example to run. The functions with no session requirement
(`rewind_buttons()`, `rewind_ui()`, `rewind_dependency()`) have examples that
execute during check.

The package writes nothing to the filesystem, opens no connections, and starts
no processes. All state is held per Shiny session in `session$userData`.
