import type { ReactNode } from "react";

/** Title block for the data pages: section kicker, Cinzel title, one-line sub. */
export default function PageHeader({
  kicker,
  title,
  children,
  extra,
}: {
  kicker: string;
  title: ReactNode;
  children?: ReactNode;
  extra?: ReactNode;
}) {
  return (
    <header className="gffl-page-head d-flex flex-wrap align-items-end justify-content-between gap-2">
      <div>
        <div className="gffl-kicker">{kicker}</div>
        <h1 className="gffl-page-title">{title}</h1>
        {children && <p className="gffl-page-sub">{children}</p>}
      </div>
      {extra}
    </header>
  );
}
