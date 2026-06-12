"use client";

// Client wrapper for the framed award photo: tapping it opens the photo
// full-size in the shared Modal (QOL for phone viewing). The framed-photo
// markup/styles are unchanged from the original server-rendered version.

import { useState } from "react";
import Modal from "@/components/Modal";

export default function CommishPhoto({
  photoSrc,
  frameSrc,
  name,
  year,
}: {
  photoSrc: string;
  frameSrc: string;
  name: string;
  year: number;
}) {
  const [open, setOpen] = useState(false);

  return (
    <>
      <div
        role="button"
        tabIndex={0}
        aria-label={`View ${name}'s ${year} photo full size`}
        onClick={() => setOpen(true)}
        onKeyDown={(e) => {
          if (e.key === "Enter" || e.key === " ") {
            e.preventDefault();
            setOpen(true);
          }
        }}
        style={{
          position: "relative",
          width: 120,
          height: 140,
          margin: "0 auto",
          cursor: "zoom-in",
        }}
      >
        <div
          style={{
            position: "absolute",
            top: "10%",
            left: "10%",
            width: "80%",
            height: "80%",
            overflow: "hidden",
          }}
        >
          {/* eslint-disable-next-line @next/next/no-img-element */}
          <img
            src={photoSrc}
            alt={name}
            loading="lazy"
            style={{
              width: "100%",
              height: "100%",
              objectFit: "cover",
              objectPosition: "top",
            }}
          />
        </div>
        {/* eslint-disable-next-line @next/next/no-img-element */}
        <img
          src={frameSrc}
          alt=""
          loading="lazy"
          style={{
            position: "absolute",
            top: 0,
            left: 0,
            width: "100%",
            height: "100%",
            pointerEvents: "none",
          }}
        />
      </div>

      <Modal
        open={open}
        onClose={() => setOpen(false)}
        title={`${name} — ${year}`}
        dark
        maxWidth={560}
      >
        {/* eslint-disable-next-line @next/next/no-img-element */}
        <img
          src={photoSrc}
          alt={name}
          style={{ width: "100%", borderRadius: 8, display: "block" }}
        />
      </Modal>
    </>
  );
}
