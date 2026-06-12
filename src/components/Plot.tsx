"use client";

import { useEffect, useRef } from "react";

interface Props {
  data: Record<string, unknown>[];
  layout?: Record<string, unknown>;
  style?: React.CSSProperties;
}

/** Thin Plotly.js wrapper (replacement for plotlyOutput/renderPlotly). */
export default function Plot({ data, layout = {}, style }: Props) {
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    let cancelled = false;
    const el = ref.current;
    if (!el) return;
    import("plotly.js-cartesian-dist-min").then((mod) => {
      const Plotly = (mod as { default?: unknown }).default ?? mod;
      if (cancelled || !el) return;
      (Plotly as {
        react: (
          el: HTMLElement,
          data: unknown,
          layout: unknown,
          config: unknown
        ) => void;
      }).react(
        el,
        data,
        { margin: { t: 30, r: 20, b: 50, l: 60 }, ...layout },
        { displayModeBar: false, responsive: true }
      );
    });
    return () => {
      cancelled = true;
    };
  }, [data, layout]);

  return <div ref={ref} style={{ width: "100%", height: 400, ...style }} />;
}
