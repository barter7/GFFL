"use client";

import { ReactNode, useMemo, useState } from "react";

export interface Column<T> {
  key: keyof T & string;
  label: string;
  numeric?: boolean;
  render?: (row: T) => ReactNode;
}

interface Props<T> {
  columns: Column<T>[];
  rows: T[];
  pageSize?: number;
  searchable?: boolean;
  initialSort?: { key: string; dir: "asc" | "desc" };
  className?: string;
}

/** Sortable, searchable, paginated table — replacement for DT::datatable. */
export default function DataTable<T extends object>({
  columns,
  rows,
  pageSize = 25,
  searchable = true,
  initialSort,
  className = "",
}: Props<T>) {
  const [sort, setSort] = useState(initialSort ?? null);
  const [search, setSearch] = useState("");
  const [page, setPage] = useState(0);

  const filtered = useMemo(() => {
    if (!search.trim()) return rows;
    const q = search.toLowerCase();
    return rows.filter((r) =>
      columns.some((c) => String(r[c.key] ?? "").toLowerCase().includes(q))
    );
  }, [rows, search, columns]);

  const sorted = useMemo(() => {
    if (!sort) return filtered;
    const col = columns.find((c) => c.key === sort.key);
    const dir = sort.dir === "asc" ? 1 : -1;
    return [...filtered].sort((a, b) => {
      const av = a[sort.key as keyof T];
      const bv = b[sort.key as keyof T];
      if (av == null) return 1;
      if (bv == null) return -1;
      if (col?.numeric) return (Number(av) - Number(bv)) * dir;
      return String(av).localeCompare(String(bv)) * dir;
    });
  }, [filtered, sort, columns]);

  const pages = Math.max(1, Math.ceil(sorted.length / pageSize));
  const current = Math.min(page, pages - 1);
  const visible = sorted.slice(current * pageSize, (current + 1) * pageSize);

  const toggleSort = (key: string) => {
    setPage(0);
    setSort((prev) =>
      prev?.key === key
        ? { key, dir: prev.dir === "asc" ? "desc" : "asc" }
        : { key, dir: "asc" }
    );
  };

  return (
    <div>
      {searchable && rows.length > pageSize && (
        <div className="d-flex justify-content-end mb-2">
          <input
            type="search"
            className="form-control form-control-sm"
            style={{ maxWidth: 220 }}
            placeholder="Search…"
            value={search}
            onChange={(e) => {
              setSearch(e.target.value);
              setPage(0);
            }}
          />
        </div>
      )}
      <div className="table-responsive">
        <table className={`table table-striped table-hover gffl-table ${className}`}>
          <thead>
            <tr>
              {columns.map((c) => (
                <th
                  key={c.key}
                  className={c.numeric ? "text-end" : ""}
                  onClick={() => toggleSort(c.key)}
                >
                  {c.label}
                  {sort?.key === c.key && (
                    <span className="sort-arrow">{sort.dir === "asc" ? "▲" : "▼"}</span>
                  )}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {visible.map((row, i) => (
              <tr key={i}>
                {columns.map((c) => (
                  <td key={c.key} className={c.numeric ? "text-end" : ""}>
                    {c.render ? c.render(row) : String(row[c.key] ?? "")}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {pages > 1 && (
        <div className="d-flex justify-content-between align-items-center">
          <small className="text-muted">
            {sorted.length} rows — page {current + 1} of {pages}
          </small>
          <div className="btn-group btn-group-sm">
            <button
              className="btn btn-outline-secondary"
              disabled={current === 0}
              onClick={() => setPage(current - 1)}
            >
              ‹ Prev
            </button>
            <button
              className="btn btn-outline-secondary"
              disabled={current >= pages - 1}
              onClick={() => setPage(current + 1)}
            >
              Next ›
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
