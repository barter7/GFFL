"use client";

// ESPN headshot <img> with the original onerror behavior from app.R:
// onerror = "this.style.display='none'"
export default function Headshot({
  src,
  style,
}: {
  src: string;
  style?: React.CSSProperties;
}) {
  return (
    // eslint-disable-next-line @next/next/no-img-element
    <img
      src={src}
      alt=""
      loading="lazy"
      style={style}
      onError={(e) => {
        e.currentTarget.style.display = "none";
      }}
    />
  );
}
