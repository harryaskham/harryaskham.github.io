// Machine-name substitution and copy buttons. Without JavaScript the page still
// shows every command, with MACHINE as the placeholder.
(() => {
  const PLACEHOLDER = "MACHINE";
  const form = document.getElementById("machine-form");
  const input = document.getElementById("machine");
  const slots = document.querySelectorAll("pre .m, .hint .m");
  const clean = (value) => value.toLowerCase().replace(/[^a-z0-9-]/g, "").slice(0, 40);

  function apply(value) {
    const name = clean(value);
    slots.forEach((slot) => { slot.textContent = name || PLACEHOLDER; });
    const url = new URL(window.location.href);
    if (name) url.searchParams.set("m", name); else url.searchParams.delete("m");
    window.history.replaceState(null, "", url);
    return name;
  }

  form.hidden = false;
  form.addEventListener("submit", (event) => event.preventDefault());
  input.addEventListener("input", () => {
    const name = clean(input.value);
    if (input.value !== name) input.value = name;
    apply(name);
  });
  input.value = apply(new URLSearchParams(window.location.search).get("m") || "");

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
