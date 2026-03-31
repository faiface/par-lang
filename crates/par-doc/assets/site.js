(() => {
  const root = document.documentElement;
  const storageKey = "par-doc-theme";
  const mediaQuery = window.matchMedia("(prefers-color-scheme: dark)");

  const effectiveTheme = () => {
    if (root.dataset.theme === "dark") {
      return "dark";
    }
    if (root.dataset.theme === "light") {
      return "light";
    }
    return mediaQuery.matches ? "dark" : "light";
  };

  const updateThemeButton = () => {
    const button = document.querySelector("[data-theme-toggle]");
    if (!button) {
      return;
    }
    const theme = effectiveTheme();
    const nextTheme = theme === "dark" ? "light" : "dark";
    const icon = button.querySelector(".theme-toggle-icon");
    if (icon) {
      icon.textContent = "\u2600";
    }
    button.title = `Switch to ${nextTheme} theme`;
    button.setAttribute("aria-label", `Switch to ${nextTheme} theme`);
    button.setAttribute("aria-pressed", theme === "dark" ? "true" : "false");
  };

  const savedTheme = localStorage.getItem(storageKey);
  if (savedTheme === "light" || savedTheme === "dark") {
    root.dataset.theme = savedTheme;
  }
  updateThemeButton();

  document.querySelector("[data-theme-toggle]")?.addEventListener("click", () => {
    const nextTheme = effectiveTheme() === "dark" ? "light" : "dark";
    root.dataset.theme = nextTheme;
    localStorage.setItem(storageKey, nextTheme);
    updateThemeButton();
  });

  mediaQuery.addEventListener("change", () => {
    if (!localStorage.getItem(storageKey)) {
      updateThemeButton();
    }
  });

  document.querySelectorAll("[data-expand-all]").forEach((button) => {
    button.addEventListener("click", () => {
      const target = button.getAttribute("data-expand-target");
      const mode = button.getAttribute("data-expand-all");
      const scope = target ? document.querySelector(`[data-expand-scope="${target}"]`) : null;
      if (!scope) {
        return;
      }
      scope.querySelectorAll("details").forEach((details) => {
        details.open = mode === "open";
      });
    });
  });
})();
