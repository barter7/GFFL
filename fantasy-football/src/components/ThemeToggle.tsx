"use client";

// Theme override. The page defaults to the OS preference via
// prefers-color-scheme; this stamps data-theme on <html>, which the
// stylesheet gives higher precedence in BOTH directions so a reader
// on a dark OS can still force light.
//
// Deliberately not persisted to localStorage: reading it during a
// static export's hydration flashes the wrong theme, and the OS
// default is right for almost everyone almost always.

import { useState } from "react";

export default function ThemeToggle() {
  const [theme, setTheme] = useState<"light" | "dark" | null>(null);

  const flip = () => {
    const root = document.documentElement;
    const current =
      root.dataset.theme ??
      (window.matchMedia("(prefers-color-scheme: dark)").matches
        ? "dark"
        : "light");
    const next = current === "dark" ? "light" : "dark";
    root.dataset.theme = next;
    setTheme(next);
  };

  return (
    <button
      type="button"
      onClick={flip}
      aria-label={`Switch to ${theme === "dark" ? "light" : "dark"} theme`}
      title="Toggle light / dark"
      style={{ marginLeft: "auto", flex: "none" }}
    >
      ◐
    </button>
  );
}
