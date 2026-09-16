# rewind 0.2.2

* The history rail no longer scrolls the page or the sidebar that holds it.
  It kept the current step in view with `scrollIntoView()`, which scrolls
  every scrollable ancestor of the element and not only the nearest one. In
  a layout that puts `rewind_ui()` in a sidebar, every change to the history
  scrolled that sidebar, so the application appeared to scroll by itself.
  The rail now sets its own `scrollTop`, and moves nothing else.

# rewind 0.2.1

* `rewind_enable()` takes a `restore_timeout` argument, in seconds. It sets
  how long `rewind` waits for the browser to finish a restore. The value
  was fixed at 2 seconds before, which is enough on a fast connection but
  not always on a slow one. If the limit is too short, capture starts again
  while the browser is still applying the restore, and a partial state
  becomes a history entry that the user never made.
* `rewind` no longer drops a data frame of the user that has the same four
  column names as the value of a `fileInput()`. It now examines the column
  types as well as the names. Such a value was dropped from the history
  before, with no message.
* `rewind_buttons()` takes a `button_class` argument. It adds CSS classes to
  the two buttons. Use it for a Bootstrap variant such as `"btn-primary"` or
  `"btn-outline-secondary"`, and for a size such as `"btn-sm"`. The `class`
  argument continues to hold classes for the container element only, so
  there was no way to reach the buttons before.
* The arrows on the buttons are now SVG. They were the HTML entities
  `&#8630;` and `&#8631;`, which come from the font of the browser and thus
  have a different weight and size on each system. The SVG arrows use
  `currentColor` and a size in `em`. They thus follow the colour and the
  size of the button, including with `btn-sm`, `btn-lg`, an outline variant
  and a dark theme.
* The redo button now says "Redo (Ctrl+Shift+Z or Ctrl+Y)" when the pointer
  rests on it. It said only "Redo (Ctrl+Shift+Z)" before. `Ctrl` + `Y` has
  always done a redo, but the button did not say so.
* The description of the `shortcuts` argument of `rewind_enable()` now
  lists the three shortcuts, and says which one belongs to which system.
* The demo application now names the redo shortcut in its hint.

# rewind 0.2.0

* `rewind_disable()` stops undo and redo for a session completely. It
  destroys each observer that `rewind_enable()` made. It also clears the
  buttons and the rail in the browser. Use `rewind_pause()` and
  `rewind_resume()` to stop capture for a short time. Use
  `rewind_disable()` to stop it for the rest of the session.
* `rewind` no longer captures `fileInput()`. Its value points to a
  temporary file on the server. Shiny deletes that file at the next upload.
  An old snapshot would thus point to a file that does not exist.
* Fixed: a call to `rewind_pause()` before the first reactive flush stopped
  capture for the rest of the session. A later `rewind_resume()` did not
  start it again. This occurred, for example, with a call to
  `rewind_pause()` immediately after `rewind_enable()`, before any input
  changed.
* `rewind_buttons(undo_label = NULL, redo_label = NULL)` now sets
  `aria-label` on the buttons. A button with an icon and no text thus has a
  correct accessible name.
* `rewind_enable()` inside a `moduleServer()` now has tests. The documents
  said "untested" before.

# rewind 0.1.0

First release.

* `rewind_enable()` captures the session inputs into an undo and redo
  history. It groups the changes by time. One movement of a slider is thus
  one step, and not forty steps.
* `rewind_track()` adds the `reactiveValues` on the server to the history.
* `rewind_step()` puts several changes into one entry with a label.
* `rewind_buttons()` and `rewind_ui()` give ready-made undo and redo
  controls, and a scrubbable history rail.
* Keyboard shortcuts: `Ctrl`/`Cmd` + `Z`, `Ctrl`/`Cmd` + `Shift` + `Z`, and
  `Ctrl` + `Y`. They do nothing while the user types in a text field.
* A restore uses the Shiny binding of each input. It does not use a fixed
  `update*Input()` table. Inputs from other packages thus operate with no
  extra code.
