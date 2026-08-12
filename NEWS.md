# rewind 0.1.0

First release.

* `rewind_enable()` captures session inputs into an undo/redo history, with
  time-based coalescing so a slider drag is one step rather than forty.
* `rewind_track()` extends the history to server-side `reactiveValues`.
* `rewind_step()` groups several changes into a single labelled entry.
* `rewind_buttons()` and `rewind_ui()` provide drop-in undo/redo controls and a
  scrubable history rail.
* Keyboard shortcuts (`Ctrl`/`Cmd` + `Z`, `Ctrl`/`Cmd` + `Shift` + `Z`,
  `Ctrl` + `Y`) that stand down while the user is typing in a text field.
* Restore goes through each input's registered Shiny binding rather than a
  hard-coded `update*Input()` table, so third-party inputs work without
  special-casing.
