/* rewind — undo/redo for Shiny applications
 *
 * The interesting part of this file is applyValue(). Rather than maintaining a
 * table of "sliderInput -> updateSliderInput", we go through the input binding
 * that Shiny itself registered against the element. Every Shiny input, and
 * every well-behaved third-party input, exposes either setValue() or
 * receiveMessage() through that binding, so restoring works for widgets this
 * package has never heard of.
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
        // Not every binding implements setValue, but the update*Input()
        // message path is universal.
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

    // Nudge the binding into reporting the new value back to the server, so
    // the server's view of the session matches what is now on screen.
    try {
      $el.trigger("change");
    } catch (e) {
      /* the binding may use a different event; the value is set regardless */
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
        // textContent, not innerHTML: labels are derived from input names and
        // must never be interpreted as markup.
        label.textContent = e.label;

        var time = document.createElement("span");
        time.className = "rewind-step-time";
        time.textContent = e.time;

        li.appendChild(label);
        li.appendChild(time);
        rail.appendChild(li);
      }

      var current = rail.querySelector(".is-current");
      if (current && current.scrollIntoView) {
        current.scrollIntoView({ block: "nearest" });
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
    // Enter/Space on a focused history step.
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

    // Leave native undo alone while the user is typing.
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
