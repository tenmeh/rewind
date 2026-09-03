/* rewind - undo and redo for Shiny applications
 *
 * The important function in this file is applyValue(). It does not keep a
 * table of "sliderInput -> updateSliderInput". It uses the input binding
 * that Shiny registered on the element. Each Shiny input gives a setValue()
 * or a receiveMessage() function through that binding. Correct inputs from
 * other packages do the same. A restore thus operates on widgets that this
 * package does not know.
 */
(function () {
  "use strict";

  var state = { entries: [], canUndo: false, canRedo: false, index: 0 };

  // ---------------------------------------------------------------- config

  function config() {
    var el = document.querySelector("script[data-rewind-config]");
    if (!el) return { shortcuts: true };
    try {
      return JSON.parse(el.textContent) || { shortcuts: true };
    } catch (e) {
      return { shortcuts: true };
    }
  }

  // -------------------------------------------------------------- restore

  function applyValue(id, value) {
    var el = document.getElementById(id);
    if (!el) return false;

    var $el = window.jQuery(el);
    var binding = $el.data("shiny-input-binding");
    if (!binding) return false;

    try {
      if (typeof binding.setValue === "function") {
        binding.setValue(el, value);
      } else if (typeof binding.receiveMessage === "function") {
        // Some bindings have no setValue function. But each binding
        // accepts the message that update*Input() sends.
        binding.receiveMessage(el, { value: value });
      } else {
        return false;
      }
    } catch (e) {
      if (window.console && console.warn) {
        console.warn("[rewind] could not restore input '" + id + "'", e);
      }
      return false;
    }

    // Make the binding send the new value to the server. The server then
    // has the same values as the screen.
    try {
      $el.trigger("change");
    } catch (e) {
      /* The binding can use a different event. The value is set already. */
    }
    return true;
  }

  // --------------------------------------------------------------- render

  function renderButtons() {
    var undos = document.querySelectorAll(".rewind-undo");
    var redos = document.querySelectorAll(".rewind-redo");
    var i;
    for (i = 0; i < undos.length; i++) undos[i].disabled = !state.canUndo;
    for (i = 0; i < redos.length; i++) redos[i].disabled = !state.canRedo;
  }

  function renderRail() {
    var rails = document.querySelectorAll(".rewind-rail");
    if (!rails.length) return;

    for (var r = 0; r < rails.length; r++) {
      var rail = rails[r];
      rail.textContent = "";

      for (var i = 0; i < state.entries.length; i++) {
        var e = state.entries[i];

        var li = document.createElement("li");
        li.className = "rewind-step" + (e.current ? " is-current" : "");
        li.setAttribute("data-index", e.index);
        li.setAttribute("tabindex", "0");
        li.setAttribute("role", "button");
        if (e.current) li.setAttribute("aria-current", "step");

        var label = document.createElement("span");
        label.className = "rewind-step-label";
        // Use textContent and not innerHTML. The labels come from the
        // input names. The browser must not read them as markup.
        label.textContent = e.label;

        var time = document.createElement("span");
        time.className = "rewind-step-time";
        time.textContent = e.time;

        li.appendChild(label);
        li.appendChild(time);
        rail.appendChild(li);
      }

      // Keep the current step in view, but move the rail and nothing else.
      //
      // Do not use scrollIntoView() here. It scrolls every scrollable
      // ancestor of the element, and not only the nearest one. When the
      // rail sits in a sidebar, that also scrolls the sidebar and the
      // page. The application then appears to scroll by itself each time
      // the history changes, which reads as a fault. The option
      // block: "nearest" limits how far each ancestor moves. It does not
      // stop them moving.
      var current = rail.querySelector(".is-current");
      if (current) {
        // Measure with getBoundingClientRect and not offsetTop. The rail
        // is not a positioned element, so it is not the offsetParent of
        // the step, and offsetTop would be relative to something else.
        var top = current.getBoundingClientRect().top -
          rail.getBoundingClientRect().top + rail.scrollTop;
        var bottom = top + current.offsetHeight;

        if (top < rail.scrollTop) {
          rail.scrollTop = top;
        } else if (bottom > rail.scrollTop + rail.clientHeight) {
          rail.scrollTop = bottom - rail.clientHeight;
        }
      }
    }
  }

  // --------------------------------------------------------------- events

  function send(name, payload) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    payload = payload || {};
    payload.at = Date.now();
    Shiny.setInputValue(name, payload, { priority: "event" });
  }

  function closest(node, selector) {
    while (node && node.nodeType === 1) {
      if (node.matches && node.matches(selector)) return node;
      node = node.parentNode;
    }
    return null;
  }

  function onActivate(target) {
    if (closest(target, ".rewind-undo")) {
      send("rewind_undo");
      return true;
    }
    if (closest(target, ".rewind-redo")) {
      send("rewind_redo");
      return true;
    }
    var step = closest(target, ".rewind-step");
    if (step) {
      send("rewind_jump", {
        index: parseInt(step.getAttribute("data-index"), 10)
      });
      return true;
    }
    return false;
  }

  document.addEventListener("click", function (ev) {
    onActivate(ev.target);
  });

  function isTextEntry(el) {
    if (!el) return false;
    if (el.isContentEditable) return true;

    var tag = el.tagName;
    if (tag === "TEXTAREA") return true;
    if (tag !== "INPUT") return false;

    var type = (el.type || "text").toLowerCase();
    return (
      ["text", "search", "url", "tel", "email", "password", "number"].indexOf(
        type
      ) !== -1
    );
  }

  document.addEventListener("keydown", function (ev) {
    // The user pressed Enter or Space on a history step that has focus.
    if (ev.key === "Enter" || ev.key === " ") {
      if (closest(ev.target, ".rewind-step")) {
        ev.preventDefault();
        onActivate(ev.target);
      }
      return;
    }

    if (!config().shortcuts) return;
    if (!(ev.ctrlKey || ev.metaKey)) return;

    var key = (ev.key || "").toLowerCase();
    if (key !== "z" && key !== "y") return;

    // Do nothing while the user types. The text undo of the browser must
    // continue to operate.
    if (isTextEntry(document.activeElement)) return;

    ev.preventDefault();
    if (key === "y" || ev.shiftKey) {
      send("rewind_redo");
    } else {
      send("rewind_undo");
    }
  });

  // ----------------------------------------------------------------- wire

  function install() {
    Shiny.addCustomMessageHandler("rewind:restore", function (msg) {
      var inputs = msg.inputs || {};
      Object.keys(inputs).forEach(function (id) {
        applyValue(id, inputs[id]);
      });
    });

    Shiny.addCustomMessageHandler("rewind:history", function (msg) {
      state = {
        entries: msg.entries || [],
        canUndo: !!msg.canUndo,
        canRedo: !!msg.canRedo,
        index: msg.index || 0
      };
      renderButtons();
      renderRail();
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    install();
  } else {
    document.addEventListener("shiny:connected", install, { once: true });
  }
})();
