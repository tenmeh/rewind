# rewind 0.2.0

* `rewind_disable()` fully tears down a session's undo/redo history:
  destroys every observer `rewind_enable()` created and resets the buttons
  and rail in the browser. `rewind_pause()`/`rewind_resume()` remain for
  temporarily suspending capture; this is for turning it off for good,
  mid-session.
* `fileInput()` is now excluded from capture. Its value points at a
  server-side temp file that Shiny deletes on the next upload, so restoring
  an old snapshot would have pointed at a path that no longer exists.
* Fixed: calling `rewind_pause()` before the very first reactive flush (for
  example, immediately after `rewind_enable()`, before any input has
  changed) permanently stopped capture from ever starting, even after a
  matching `rewind_resume()`.
* `rewind_buttons(undo_label = NULL, redo_label = NULL)` now sets
  `aria-label` on the resulting icon-only buttons, so they have a correct
  accessible name.
* `rewind_enable()` inside a `moduleServer()` is now confirmed to work and
  is covered by tests; previously documented as untested.

# rewind 0.1.0

First release.

* `rewind_enable()` captures session inputs into an undo/redo history, with
  time-based coalescing so a slider drag is one step rather than forty.
* `rewind_track()` extends the history to server-side `reactiveValues`.
* `rewind_step()` groups several changes into a single labelled entry.
* `rewind_buttons()` and `rewind_ui()` provide drop-in undo/redo controls and a
  scrubbable history rail.
* Keyboard shortcuts (`Ctrl`/`Cmd` + `Z`, `Ctrl`/`Cmd` + `Shift` + `Z`,
  `Ctrl` + `Y`) that stand down while the user is typing in a text field.
* Restore goes through each input's registered Shiny binding rather than a
  hard-coded `update*Input()` table, so third-party inputs work without
  special-casing.
