"use client";

// Port of output$draft_r1_plot / output$draft_pos_plot (app.R ~2254-2299):
// stacked percentage bar chart of draft picks by owner and position.

import { useMemo } from "react";
import Plot from "@/components/Plot";
import { DraftRow } from "@/lib/data";

// ggplot2 default discrete (hue) palettes, as rendered by ggplotly
const GGPLOT_HUE: Record<number, string[]> = {
  1: ["#F8766D"],
  2: ["#F8766D", "#00BFC4"],
  3: ["#F8766D", "#00BA38", "#619CFF"],
  4: ["#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"],
  5: ["#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"],
  6: ["#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"],
};

// Order positions with clear priority
const POS_ORDER = ["QB", "RB", "WR", "TE", "K", "DST"];

interface Props {
  rows: DraftRow[];
  yTitle: string;
}

export default function PosBreakdownPlot({ rows, yTitle }: Props) {
  const { traces } = useMemo(() => {
    // count(owner, pos) then pct of each owner's picks
    const counts = new Map<string, Map<string, number>>();
    const totals = new Map<string, number>();
    for (const r of rows) {
      const pos = r.pos ?? "NA";
      const byPos = counts.get(r.owner) ?? new Map<string, number>();
      byPos.set(pos, (byPos.get(pos) ?? 0) + 1);
      counts.set(r.owner, byPos);
      totals.set(r.owner, (totals.get(r.owner) ?? 0) + 1);
    }

    // Order owners alphabetically
    const owners = [...counts.keys()].sort((a, b) => a.localeCompare(b));

    const present = new Set<string>();
    for (const byPos of counts.values()) {
      for (const p of byPos.keys()) present.add(p);
    }
    const levels = POS_ORDER.filter((p) => present.has(p));
    // values outside the factor levels become NA (grey50) in ggplot
    const hasNA = [...present].some((p) => !POS_ORDER.includes(p));
    const palette = GGPLOT_HUE[levels.length] ?? GGPLOT_HUE[6];

    const allLevels = hasNA ? [...levels, "NA"] : levels;
    const traces = allLevels.map((pos, i) => {
      const x: string[] = [];
      const y: number[] = [];
      const text: string[] = [];
      for (const o of owners) {
        const byPos = counts.get(o)!;
        const n =
          pos === "NA"
            ? [...byPos.entries()]
                .filter(([p]) => !POS_ORDER.includes(p))
                .reduce((s, [, v]) => s + v, 0)
            : byPos.get(pos) ?? 0;
        if (n === 0) continue;
        const pct = Math.round((n / totals.get(o)!) * 1000) / 10;
        x.push(o);
        y.push(pct);
        text.push(`${o}<br>${pos}: ${pct}% (${n} picks)`);
      }
      return {
        type: "bar",
        name: pos,
        x,
        y,
        text,
        textposition: "none",
        hoverinfo: "text",
        marker: { color: pos === "NA" ? "#7F7F7F" : palette[i] },
      } as Record<string, unknown>;
    });

    return { traces };
  }, [rows]);

  return (
    <Plot
      data={traces}
      layout={{
        barmode: "stack",
        xaxis: { tickangle: -45 },
        yaxis: { title: { text: yTitle }, ticksuffix: "%" },
        legend: { title: { text: "Position" } },
      }}
      style={{ height: 400 }}
    />
  );
}
