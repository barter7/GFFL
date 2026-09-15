// Minimal markdown renderer for the recap subset: ## headings,
// paragraphs, *italic* / **bold** inline, --- rules, and > quotes.
// Recaps are authored in-repo, so this deliberately supports only
// what the column actually uses — no HTML passthrough.

import type { ReactNode } from "react";

function inline(text: string, keyBase: string): ReactNode[] {
  const out: ReactNode[] = [];
  const parts = text.split(/(\*\*[^*]+\*\*|\*[^*]+\*)/g);
  parts.forEach((part, i) => {
    if (part.startsWith("**") && part.endsWith("**")) {
      out.push(<strong key={`${keyBase}-${i}`}>{part.slice(2, -2)}</strong>);
    } else if (part.startsWith("*") && part.endsWith("*") && part.length > 2) {
      out.push(<em key={`${keyBase}-${i}`}>{part.slice(1, -1)}</em>);
    } else if (part) {
      out.push(part);
    }
  });
  return out;
}

export default function RecapBody({ body }: { body: string }) {
  const blocks = body.split(/\n{2,}/);
  return (
    <div className="gffl-recap">
      {blocks.map((block, i) => {
        const b = block.trim();
        if (!b) return null;
        if (b === "---") return <hr key={i} />;
        if (b.startsWith("## ")) {
          return (
            <h3 key={i} className="h5 fw-bold mt-4 mb-2">
              {inline(b.slice(3), `h${i}`)}
            </h3>
          );
        }
        if (b.startsWith("# ")) {
          return (
            <h2 key={i} className="h4 fw-bold">
              {inline(b.slice(2), `h${i}`)}
            </h2>
          );
        }
        if (b.startsWith("> ")) {
          return (
            <blockquote key={i} className="blockquote fst-italic ps-3 border-start border-3">
              {inline(b.replace(/^> /gm, ""), `q${i}`)}
            </blockquote>
          );
        }
        const lines = b.split("\n");
        return (
          <p key={i}>
            {lines.map((line, j) => (
              <span key={j}>
                {j > 0 && <br />}
                {inline(line, `p${i}-${j}`)}
              </span>
            ))}
          </p>
        );
      })}
    </div>
  );
}
