// Verbatim old-record-book CSS from app.R output$records_book (tags$style, lines 4586-4700).
// Shared by the Records and Player Records pages (in Shiny both tabs lived on one
// page, so this style block applied to both).
export const RECORD_BOOK_CSS = `
        .record-book {
          background: #f4e8d0;
          background-image:
            radial-gradient(ellipse at top, rgba(139,105,20,0.12), transparent 60%),
            radial-gradient(ellipse at bottom, rgba(139,105,20,0.15), transparent 60%);
          padding: 40px 30px;
          border: 8px double #5c3a10;
          box-shadow: inset 0 0 80px rgba(92,58,16,0.2), 0 10px 40px rgba(0,0,0,0.3);
          border-radius: 4px;
          position: relative;
          max-width: 900px;
          margin: 0 auto;
        }
        .record-book::before, .record-book::after {
          content: '';
          position: absolute;
          top: 20px;
          bottom: 20px;
          width: 1px;
          background: rgba(92,58,16,0.3);
        }
        .record-book::before { left: 12px; }
        .record-book::after { right: 12px; }
        .record-book h1 {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 42px;
          text-align: center;
          color: #3d2a10;
          text-shadow: 1px 1px 0 rgba(255,255,255,0.4), 2px 2px 4px rgba(0,0,0,0.2);
          letter-spacing: 4px;
          margin: 0 0 10px;
          font-weight: normal;
        }
        .record-book .subtitle {
          font-family: 'Cormorant Garamond', Georgia, serif;
          font-style: italic;
          text-align: center;
          color: #5c3a10;
          font-size: 18px;
          margin-bottom: 20px;
          letter-spacing: 2px;
        }
        .record-book .fleuron {
          text-align: center;
          color: #8b6914;
          font-size: 24px;
          margin: 10px 0 20px;
          letter-spacing: 20px;
        }
        .record-table {
          width: 100%;
          border-collapse: collapse;
          font-family: 'Cormorant Garamond', Georgia, serif;
          color: #2a1a08;
          margin: 0 auto;
        }
        .record-table th {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 16px;
          color: #3d2a10;
          text-transform: uppercase;
          letter-spacing: 2px;
          text-align: left;
          padding: 8px 12px;
          border-bottom: 2px solid #5c3a10;
        }
        .record-table td {
          padding: 10px 12px;
          border-bottom: 1px dashed rgba(92,58,16,0.25);
          vertical-align: middle;
        }
        .record-table tr:nth-child(even) td {
          background: rgba(139,105,20,0.05);
        }
        .cell-record {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 16px;
          color: #3d2a10;
          font-weight: bold;
          text-transform: uppercase;
          letter-spacing: 1px;
          width: 30%;
        }
        .cell-owner {
          font-size: 20px;
          font-weight: bold;
          color: #3d2a10;
          width: 20%;
          font-family: 'IM Fell English', Georgia, serif;
        }
        .cell-metric {
          font-size: 18px;
          font-weight: 600;
          color: #3d2a10;
          width: 25%;
        }
        .cell-season {
          font-size: 16px;
          color: #8b6914;
          width: 25%;
        }
        @media (max-width:768px) {
          .record-book { padding: 20px 10px; }
          .record-book h1 { font-size: 28px; letter-spacing: 2px; }
          .record-book .subtitle { font-size: 14px; }
          .record-table th { font-size: 12px; padding: 6px 4px; }
          .record-table td { padding: 8px 4px; }
          .cell-record { font-size: 13px; }
          .cell-owner { font-size: 14px; }
          .cell-metric { font-size: 14px; }
          .cell-season { font-size: 12px; }
        }
        @media (max-width:576px) {
          .record-book {
            padding: 14px 6px;
            border-width: 6px;
            box-shadow: inset 0 0 40px rgba(92,58,16,0.2), 0 6px 24px rgba(0,0,0,0.25);
          }
          .record-book::before { left: 5px; top: 10px; bottom: 10px; }
          .record-book::after { right: 5px; top: 10px; bottom: 10px; }
          .record-book h1 { font-size: 22px; letter-spacing: 1px; margin-bottom: 6px; }
          .record-book .subtitle { font-size: 12px; letter-spacing: 1px; margin-bottom: 10px; }
          .record-book .fleuron { font-size: 16px; letter-spacing: 10px; margin: 6px 0 10px; }
          .record-table th { padding: 5px 3px; letter-spacing: 1px; }
          .record-table td { padding: 6px 3px; }
          .cell-record { font-size: 12px; letter-spacing: 0; }
          .cell-owner { font-size: 13px; }
          .cell-metric { font-size: 13px; }
          .cell-season { font-size: 12px; }
        }
`;

/** Fleuron glyph used between record-book sections (HTML &#10086;). */
export const FLEURON = "❦";
export const FLEURON_TRIPLE = "❦ ❦ ❦";
