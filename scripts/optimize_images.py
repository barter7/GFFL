#!/usr/bin/env python3
"""Copy images from www/ into public/ as downsized WebP.

Originals in www/ are untouched. Filenames keep their stem but get a .webp
extension (the site's photoUrl() helper handles the mapping). Run after
adding new photos:
    pip install pillow
    python3 scripts/optimize_images.py
"""

import os

from PIL import Image

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MAX_DIM = 1400
QUALITY = 82

Image.MAX_IMAGE_PIXELS = None

# www/ originals that are NOT referenced anywhere in src/ (verified against
# photoUrl()/findPhoto() call sites, including fallback chains). Kept in www/
# for posterity but skipped here so their .webp never regenerates in public/.
SKIP_STEMS = {
    "photos": {
        "centurion_trophy",
        "lombardi",
    },
}


def process_dir(name: str) -> None:
    src_dir = os.path.join(ROOT, "www", name)
    dst_dir = os.path.join(ROOT, "public", name)
    skip = SKIP_STEMS.get(name, set())
    os.makedirs(dst_dir, exist_ok=True)
    # Clear stale outputs so renames in www/ don't leave orphans
    for f in os.listdir(dst_dir):
        os.remove(os.path.join(dst_dir, f))
    total = 0
    for fname in sorted(os.listdir(src_dir)):
        src = os.path.join(src_dir, fname)
        stem, ext = os.path.splitext(fname)
        if not os.path.isfile(src) or ext.lower() not in (".png", ".jpg", ".jpeg"):
            continue
        if stem in skip:
            continue
        dst = os.path.join(dst_dir, stem + ".webp")
        try:
            img = Image.open(src)
            img.load()
            if max(img.size) > MAX_DIM:
                img.thumbnail((MAX_DIM, MAX_DIM), Image.LANCZOS)
            if img.mode == "P":
                img = img.convert("RGBA")
            img.save(dst, "WEBP", quality=QUALITY, method=6)
            total += os.path.getsize(dst)
        except Exception as e:
            print(f"  SKIP {fname}: {e}")
    print(f"public/{name}: {total / 1024 / 1024:.1f} MB")


if __name__ == "__main__":
    process_dir("photos")
    process_dir("recaps")
