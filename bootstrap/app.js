// Placeholder substitution and copy buttons. Without JavaScript the page still
// shows every command with its placeholders (MACHINE, GENERATION, ...).
//
// Each field fills the [data-slot] spans of the same key and is kept in the URL
// query (?m=, ?g=, ?c=, ?k=), so a prefilled link can be opened on the device.
// Values are inserted as text and restricted to characters that cannot break
// out of a shell word (no spaces, quotes, $, ;, & or |).
(() => {
  const FIELDS = {
    m: {
      placeholder: "MACHINE",
      clean: (v) => v.toLowerCase().replace(/[^a-z0-9-]/g, "").slice(0, 40),
      valid: (v) => /^[a-z0-9-]+$/.test(v),
    },
    g: {
      placeholder: "GENERATION",
      clean: (v) => v.replace(/[^A-Za-z0-9/._+=?-]/g, "").slice(0, 200),
      valid: (v) => /^\/nix\/store\/[0-9a-z]{32}-[A-Za-z0-9+._?=-]+$/.test(v),
    },
    c: {
      placeholder: "CACHE_URL",
      clean: (v) => v.replace(/[^A-Za-z0-9:/._-]/g, "").slice(0, 200),
      valid: (v) => /^https?:\/\/[A-Za-z0-9.-]+(:[0-9]+)?(\/[A-Za-z0-9._-]+)*\/?$/.test(v),
    },
    k: {
      placeholder: "CACHE_KEY",
      clean: (v) => v.replace(/[^A-Za-z0-9._+/=:-]/g, "").slice(0, 120),
      valid: (v) => /^[A-Za-z0-9._-]+:[A-Za-z0-9+/]+=*$/.test(v),
    },
  };

  const inputs = document.querySelectorAll("input[data-field]");
  const params = new URLSearchParams(window.location.search);

  function render(key, value) {
    document.querySelectorAll(`[data-slot="${key}"]`).forEach((slot) => {
      slot.textContent = value || FIELDS[key].placeholder;
    });
  }

  function remember(key, value) {
    const url = new URL(window.location.href);
    if (value) url.searchParams.set(key, value); else url.searchParams.delete(key);
    window.history.replaceState(null, "", url);
  }

  inputs.forEach((input) => {
    const key = input.dataset.field;
    const field = FIELDS[key];
    if (!field) return;
    const update = (value, store) => {
      const cleaned = field.clean(value);
      if (input.value !== cleaned) input.value = cleaned;
      input.setAttribute("aria-invalid", cleaned && !field.valid(cleaned) ? "true" : "false");
      render(key, cleaned);
      if (store) remember(key, cleaned);
    };
    input.addEventListener("input", () => update(input.value, true));
    update(params.get(key) || "", false);
  });

  document.querySelectorAll("form.fields").forEach((form) => {
    form.hidden = false;
    form.addEventListener("submit", (event) => event.preventDefault());
  });

  async function copy(text) {
    if (navigator.clipboard && window.isSecureContext) {
      await navigator.clipboard.writeText(text);
      return;
    }
    const area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.appendChild(area);
    area.select();
    const ok = document.execCommand("copy");
    area.remove();
    if (!ok) throw new Error("copy failed");
  }

  document.querySelectorAll("pre").forEach((pre) => {
    const code = pre.querySelector("code");
    const button = document.createElement("button");
    button.type = "button";
    button.className = "copy";
    button.textContent = "Copy";
    button.setAttribute("aria-label", "Copy command");
    button.addEventListener("click", async () => {
      try {
        await copy(code.textContent);
        button.textContent = "Copied";
        button.classList.add("done");
      } catch {
        button.textContent = "Select it";
      }
      window.setTimeout(() => {
        button.textContent = "Copy";
        button.classList.remove("done");
      }, 1600);
    });
    pre.classList.add("has-copy");
    pre.appendChild(button);
  });
})();
