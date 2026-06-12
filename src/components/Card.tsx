import { ReactNode } from "react";

interface Props {
  header?: ReactNode;
  headerExtra?: ReactNode;
  children: ReactNode;
  className?: string;
}

export default function Card({ header, headerExtra, children, className = "" }: Props) {
  return (
    <div className={`card ${className}`}>
      {header != null && (
        <div className="card-header d-flex justify-content-between align-items-center flex-wrap gap-2">
          <div>{header}</div>
          {headerExtra != null && <div>{headerExtra}</div>}
        </div>
      )}
      <div className="card-body">{children}</div>
    </div>
  );
}
