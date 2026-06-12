"use client";

import { ReactNode, useEffect } from "react";
import { createPortal } from "react-dom";

interface Props {
  open: boolean;
  onClose: () => void;
  title?: ReactNode;
  children: ReactNode;
  /** Visual theme: light card or dark (for the dark-themed pages). */
  dark?: boolean;
  maxWidth?: number;
}

/** Tap-friendly modal popup (replaces hover-only `title` tooltips). */
export default function Modal({ open, onClose, title, children, dark = false, maxWidth = 480 }: Props) {
  useEffect(() => {
    if (!open) return;
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    document.addEventListener("keydown", onKey);
    const prev = document.body.style.overflow;
    document.body.style.overflow = "hidden";
    return () => {
      document.removeEventListener("keydown", onKey);
      document.body.style.overflow = prev;
    };
  }, [open, onClose]);

  if (!open || typeof document === "undefined") return null;

  // Portal to <body>: ancestor transforms/z-index (e.g. trophy shelves,
  // banners) would otherwise trap the fixed overlay beneath page content.
  return createPortal(
    <div
      onClick={onClose}
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.72)",
        zIndex: 1100,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
    >
      <div
        onClick={(e) => e.stopPropagation()}
        style={{
          background: dark ? "#15151f" : "#fff",
          color: dark ? "#e8e8e8" : "#212529",
          borderRadius: 12,
          border: dark ? "1px solid #3a3a4e" : "none",
          boxShadow: "0 12px 48px rgba(0,0,0,0.5)",
          maxWidth,
          width: "100%",
          maxHeight: "85vh",
          overflowY: "auto",
          position: "relative",
        }}
      >
        <button
          onClick={onClose}
          aria-label="Close"
          style={{
            position: "absolute",
            top: 8,
            right: 12,
            background: "none",
            border: "none",
            fontSize: 26,
            lineHeight: 1,
            color: dark ? "#999" : "#666",
            cursor: "pointer",
            padding: 4,
          }}
        >
          ×
        </button>
        {title != null && (
          <div
            style={{
              padding: "16px 44px 12px 20px",
              borderBottom: dark ? "1px solid #2a2a3a" : "1px solid #e9ecef",
              fontWeight: 700,
              fontSize: 18,
            }}
          >
            {title}
          </div>
        )}
        <div style={{ padding: 20 }}>{children}</div>
      </div>
    </div>,
    document.body
  );
}
