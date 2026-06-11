import type { Metadata } from "next";
import "bootstrap/dist/css/bootstrap.min.css";
import "./globals.css";
import Nav from "@/components/Nav";

export const metadata: Metadata = {
  title: "GFFL Archives",
  description: "Groupies Fantasy Football League — historical archives, est. 2016",
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <head>
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link
          rel="stylesheet"
          href="https://fonts.googleapis.com/css2?family=Cinzel:wght@700;900&family=Bebas+Neue&family=IM+Fell+English:ital@0;1&family=Cormorant+Garamond:ital,wght@0,400;0,600;1,400&display=swap"
        />
      </head>
      <body>
        <Nav />
        <main className="container-fluid py-3 px-3 px-md-4">{children}</main>
      </body>
    </html>
  );
}
