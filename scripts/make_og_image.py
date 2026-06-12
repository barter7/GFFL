#!/usr/bin/env python3
"""Generate the social share preview image (Open Graph / Twitter card).

1200x630, matching the site's home page: dark arena background, decorative
gold rules, and "GFFL" in huge metallic trophy lettering (Cinzel Decorative).

Fonts (OFL) are fetched from the google/fonts repo:
  curl -sLo /tmp/CinzelDecorative-Black.ttf \
    https://raw.githubusercontent.com/google/fonts/main/ofl/cinzeldecorative/CinzelDecorative-Black.ttf
  curl -sLo /tmp/Cinzel.ttf \
    "https://raw.githubusercontent.com/google/fonts/main/ofl/cinzel/Cinzel%5Bwght%5D.ttf"

Run: python3 scripts/make_og_image.py
"""

import os

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
W, H = 1200, 630

# --- Background: radial navy-to-black like the home page ---
yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
cx, cy = W / 2, H / 2
d = np.sqrt(((xx - cx) / (W * 0.62)) ** 2 + ((yy - cy) / (H * 0.62)) ** 2)
d = np.clip(d, 0, 1)
# center #1a1a2e -> edge #000008
r = (0x1A * (1 - d)).astype(np.uint8)
g = (0x1A * (1 - d)).astype(np.uint8)
b = (0x2E * (1 - d) + 0x08 * d).astype(np.uint8)
img = Image.fromarray(np.dstack([r, g, b]), "RGB").convert("RGBA")

# Subtle gold glow top and bottom
glow = Image.new("RGBA", (W, H), (0, 0, 0, 0))
gd = ImageDraw.Draw(glow)
gd.ellipse([W * 0.15, -H * 0.35, W * 0.85, H * 0.18], fill=(212, 168, 75, 26))
gd.ellipse([W * 0.15, H * 0.82, W * 0.85, H * 1.35], fill=(212, 168, 75, 18))
glow = glow.filter(ImageFilter.GaussianBlur(60))
img = Image.alpha_composite(img, glow)


def gold_rule(draw_img: Image.Image, y: int, width: int) -> None:
    """Horizontal gold gradient rule, bright in the middle."""
    rule = Image.new("RGBA", (width, 6), (0, 0, 0, 0))
    for x in range(width):
        t = x / width
        edge = min(t, 1 - t) * 2  # 0 at edges, 1 in middle
        alpha = int(255 * min(1, edge * 1.6))
        c = (
            int(0xC9 + (0xF0 - 0xC9) * edge),
            int(0xA8 + (0xD6 - 0xA8) * edge),
            int(0x4C + (0x75 - 0x4C) * edge),
            alpha,
        )
        for yy2 in range(6):
            rule.putpixel((x, yy2), c)
    draw_img.alpha_composite(rule, ((W - width) // 2, y))


def metallic_text(text: str, font: ImageFont.FreeTypeFont) -> Image.Image:
    """Render text with a vertical gold-metal gradient, bevel, and glow."""
    # Measure
    tmp = ImageDraw.Draw(Image.new("L", (1, 1)))
    box = tmp.textbbox((0, 0), text, font=font)
    tw, th = box[2] - box[0], box[3] - box[1]
    pad = 80
    cw, ch = tw + pad * 2, th + pad * 2

    mask = Image.new("L", (cw, ch), 0)
    ImageDraw.Draw(mask).text((pad - box[0], pad - box[1]), text, font=font, fill=255)

    # Vertical gradient: pale gold -> rich gold -> deep bronze
    stops = [(0.0, (252, 238, 160)), (0.42, (240, 214, 117)), (0.72, (200, 152, 60)), (1.0, (122, 88, 24))]
    grad = np.zeros((ch, cw, 3), dtype=np.uint8)
    ys = np.linspace(0, 1, ch)
    for i, t in enumerate(ys):
        for (t0, c0), (t1, c1) in zip(stops, stops[1:]):
            if t0 <= t <= t1:
                f = (t - t0) / (t1 - t0) if t1 > t0 else 0
                grad[i, :] = [int(a + (b2 - a) * f) for a, b2 in zip(c0, c1)]
                break
    grad_img = Image.fromarray(grad, "RGB").convert("RGBA")

    out = Image.new("RGBA", (cw, ch), (0, 0, 0, 0))

    # Outer glow
    glow_m = mask.filter(ImageFilter.GaussianBlur(18))
    out.paste(Image.new("RGBA", (cw, ch), (212, 168, 75, 110)), (0, 0), glow_m)

    # Drop shadow
    sh = mask.filter(ImageFilter.GaussianBlur(6))
    shadow_layer = Image.new("RGBA", (cw, ch), (0, 0, 0, 230))
    tmp2 = Image.new("RGBA", (cw, ch), (0, 0, 0, 0))
    tmp2.paste(shadow_layer, (0, 10), sh)
    out = Image.alpha_composite(out, tmp2)

    # Bevel: dark edge slightly offset down, bright edge offset up
    dark_edge = Image.new("RGBA", (cw, ch), (60, 38, 8, 255))
    t3 = Image.new("RGBA", (cw, ch), (0, 0, 0, 0))
    t3.paste(dark_edge, (0, 4), mask)
    out = Image.alpha_composite(out, t3)
    light_edge = Image.new("RGBA", (cw, ch), (255, 246, 200, 255))
    t4 = Image.new("RGBA", (cw, ch), (0, 0, 0, 0))
    t4.paste(light_edge, (0, -3), mask)
    out = Image.alpha_composite(out, t4)

    # Gradient face
    t5 = Image.new("RGBA", (cw, ch), (0, 0, 0, 0))
    t5.paste(grad_img, (0, 0), mask)
    out = Image.alpha_composite(out, t5)

    return out


title_font = ImageFont.truetype("/tmp/CinzelDecorative-Black.ttf", 330)
sub_font = ImageFont.truetype("/tmp/Cinzel.ttf", 44)

title = metallic_text("GFFL", title_font)
img.alpha_composite(title, ((W - title.width) // 2, (H - title.height) // 2 - 28))

gold_rule(img, 92, 760)
gold_rule(img, H - 122, 760)

# Subtitle
sub = "GROUPIES  FANTASY  FOOTBALL  ·  EST. 2016"
tmpd = ImageDraw.Draw(img)
sb = tmpd.textbbox((0, 0), sub, font=sub_font)
tmpd.text(((W - (sb[2] - sb[0])) / 2, H - 100), sub, font=sub_font, fill=(196, 158, 76, 255))

out_path = os.path.join(ROOT, "src", "app", "opengraph-image.png")
img.convert("RGB").save(out_path, "PNG", optimize=True)
print(out_path, os.path.getsize(out_path) // 1024, "KB")

tw_path = os.path.join(ROOT, "src", "app", "twitter-image.png")
img.convert("RGB").save(tw_path, "PNG", optimize=True)
print(tw_path, os.path.getsize(tw_path) // 1024, "KB")
