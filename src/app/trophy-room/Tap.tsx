"use client";

// Tap-to-reveal wrapper: replaces hover-only `title` tooltips with a
// touch-friendly Modal. The server component computes plain serializable
// `info` and passes the (server-rendered) trophy markup as children.
// `display: contents` keeps the child's flex/grid layout untouched while
// still capturing taps; `cursor: pointer` is inherited by the children.

import { ReactNode, useState } from "react";
import Modal from "@/components/Modal";

export interface TapLine {
  label: string;
  value: string;
}

export interface TapInfo {
  /** Modal heading, e.g. "2018 League Champion" */
  title: string;
  /** Small line under the heading, e.g. "Connor — Championship Ring" */
  subtitle?: string;
  lines: TapLine[];
  /** Italic footnote, e.g. what the trophy represents */
  note?: string;
}

export default function Tap({
  info,
  dark = false,
  children,
}: {
  info: TapInfo;
  dark?: boolean;
  children: ReactNode;
}) {
  const [open, setOpen] = useState(false);
  return (
    <>
      <span
        role="button"
        tabIndex={0}
        aria-label={info.title}
        onClick={() => setOpen(true)}
        onKeyDown={(e) => {
          if (e.key === "Enter" || e.key === " ") {
            e.preventDefault();
            setOpen(true);
          }
        }}
        style={{ display: "contents", cursor: "pointer" }}
      >
        {children}
      </span>
      <Modal open={open} onClose={() => setOpen(false)} title={info.title} dark={dark} maxWidth={400}>
        {info.subtitle != null && (
          <div style={{ fontWeight: 600, marginBottom: 10, fontSize: 14, opacity: 0.85 }}>
            {info.subtitle}
          </div>
        )}
        {info.lines.length > 0 && (
          <table style={{ width: "100%", borderCollapse: "collapse" }}>
            <tbody>
              {info.lines.map((l, i) => (
                <tr
                  key={i}
                  style={{
                    borderBottom:
                      i < info.lines.length - 1
                        ? dark
                          ? "1px solid #2a2a3a"
                          : "1px solid #f0f0f0"
                        : "none",
                  }}
                >
                  <td
                    style={{
                      padding: "6px 8px 6px 0",
                      opacity: 0.65,
                      fontSize: 13,
                      whiteSpace: "nowrap",
                      verticalAlign: "top",
                    }}
                  >
                    {l.label}
                  </td>
                  <td style={{ padding: "6px 0", fontWeight: 600, fontSize: 13, textAlign: "right" }}>
                    {l.value}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
        {info.note != null && (
          <div style={{ marginTop: 12, fontSize: 12, fontStyle: "italic", opacity: 0.6 }}>
            {info.note}
          </div>
        )}
      </Modal>
    </>
  );
}
