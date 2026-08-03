import type { Metadata } from "next";
import "./globals.css";
import ThemeToggle from "@/components/ThemeToggle";

export const metadata: Metadata = {
  title: {
    default: "GFFL Fantasy Football Model",
    template: "%s | GFFL Fantasy Model",
  },
  description:
    "2026 NFL depth charts, offseason churn, fantasy-stock signals and a ranked draft board — built from nflverse play-by-play.",
};

export const viewport = {
  themeColor: [
    { media: "(prefers-color-scheme: light)", color: "#f4f2ec" },
    { media: "(prefers-color-scheme: dark)", color: "#14161a" },
  ],
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <body>
        <header className="masthead">
          <div className="mastinner">
            <div>
              <h1>
                GFFL <span className="accent">Fantasy</span>
              </h1>
              <div className="sub">
                2026 depth charts, offseason churn and a ranked draft board,
                computed from nflverse play-by-play.
              </div>
            </div>
            <ThemeToggle />
          </div>
        </header>
        <div className="wrap">{children}</div>
      </body>
    </html>
  );
}
