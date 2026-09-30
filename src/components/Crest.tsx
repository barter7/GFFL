// The league crest: a navy shield with a gold double border, a football
// over the GFFL monogram, and EST. 2016. `full` adds the monogram and the
// year (home page); the plain version is the small shield for the nav bar,
// where the monogram would be too small to read.
import { useId } from "react";

const SHIELD = "M100 6 L190 28 V108 C190 168 152 206 100 234 C48 206 10 168 10 108 V28 Z";
const INNER = "M100 17 L180 36.5 V108 C180 162 146 196 100 222 C54 196 20 162 20 108 V36.5 Z";

export default function Crest({ size = 32, full = false }: { size?: number; full?: boolean }) {
  const id = useId().replace(/:/g, "");
  const gold = `gold${id}`, navy = `navy${id}`, sheen = `sheen${id}`;
  return (
    <svg
      width={(size * 200) / 240}
      height={size}
      viewBox="0 0 200 240"
      role="img"
      aria-label="GFFL crest"
      style={{ display: "block", flex: "0 0 auto" }}
    >
      <defs>
        <linearGradient id={gold} x1="0" y1="0" x2="0" y2="1">
          <stop offset="0" stopColor="#fbeaa0" />
          <stop offset="0.45" stopColor="#e2b85a" />
          <stop offset="0.75" stopColor="#b8862f" />
          <stop offset="1" stopColor="#8b6914" />
        </linearGradient>
        <linearGradient id={navy} x1="0" y1="0" x2="0" y2="1">
          <stop offset="0" stopColor="#123d73" />
          <stop offset="1" stopColor="#03152e" />
        </linearGradient>
        <radialGradient id={sheen} cx="0.5" cy="0.18" r="0.7">
          <stop offset="0" stopColor="#ffffff" stopOpacity="0.16" />
          <stop offset="1" stopColor="#ffffff" stopOpacity="0" />
        </radialGradient>
      </defs>
      <path d={SHIELD} fill={`url(#${navy})`} stroke={`url(#${gold})`} strokeWidth="9" strokeLinejoin="round" />
      <path d={SHIELD} fill={`url(#${sheen})`} />
      <path d={INNER} fill="none" stroke={`url(#${gold})`} strokeWidth="2" opacity="0.75" />

      {/* football */}
      <g transform={full ? "translate(100 64)" : "translate(100 112) scale(1.9)"}>
        <ellipse rx="30" ry="17" fill={`url(#${gold})`} />
        <path d="M-30 0 Q-24 -6 -17 -9 M30 0 Q24 -6 17 -9 M-30 0 Q-24 6 -17 9 M30 0 Q24 6 17 9"
          stroke="#03152e" strokeWidth="2.2" fill="none" opacity="0.55" />
        <line x1="-11" y1="0" x2="11" y2="0" stroke="#03152e" strokeWidth="2.4" />
        {[-7, -2.3, 2.3, 7].map((x) => (
          <line key={x} x1={x} y1="-4.2" x2={x} y2="4.2" stroke="#03152e" strokeWidth="2.2" />
        ))}
      </g>

      {full && (
        <>
          <text x="100" y="140" textAnchor="middle" fill={`url(#${gold})`}
            style={{ fontFamily: "Cinzel, Georgia, serif", fontWeight: 900, fontSize: 50, letterSpacing: 2 }}>
            GFFL
          </text>
          <line x1="52" y1="156" x2="148" y2="156" stroke={`url(#${gold})`} strokeWidth="1.5" />
          <text x="100" y="177" textAnchor="middle" fill="#e2b85a"
            style={{ fontFamily: "Cinzel, Georgia, serif", fontWeight: 700, fontSize: 12, letterSpacing: 3 }}>
            EST 2016
          </text>
          {[-1, 1].map((s) => (
            <path key={s} transform={`translate(${100 + s * 47} 173)`}
              d="M0 -5 L1.5 -1.5 L5 0 L1.5 1.5 L0 5 L-1.5 1.5 L-5 0 L-1.5 -1.5 Z" fill="#e2b85a" />
          ))}
        </>
      )}
    </svg>
  );
}
