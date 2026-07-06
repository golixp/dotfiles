#!/usr/bin/env python3
"""Claude Code status line.

Reads the session JSON on stdin and prints a single, color-graded line:

    <project> · <branch> · <model> · ctx <bar> <pct> · 5h <pct> · 7d <pct>

All data comes from stdin (see `code.claude.com/docs/en/statusline`); the git
branch is read locally. No network calls, no external dependencies.
"""

import json
import os
import subprocess
import sys
import time

# ── 24-bit palette (dark theme, Claude coral accent) ──────────────────────────
CORAL = (215, 119, 87)     # Claude brand accent
TEXT = (223, 223, 223)     # bright foreground
DIM = (110, 110, 110)      # separators, labels, empty bar
VIOLET = (183, 154, 247)   # git branch
GREEN = (152, 195, 121)
YELLOW = (229, 192, 123)
ORANGE = (224, 140, 58)
RED = (224, 108, 117)

RESET = "\033[0m"

# reasoning-effort tiers → an ascending weak→strong color ramp. This is NOT
# grade()'s warning ramp: high effort is desirable, not dangerous, so it gets
# its own scale (dim → green → coral → orange → red). Unknown or future tiers
# fall back to the model's coral in main().
EFFORT_COLORS = {
    "low": DIM,
    "medium": GREEN,
    "high": CORAL,
    "xhigh": ORANGE,
    "max": RED,
}


def fg(rgb, s, bold=False):
    r, g, b = rgb
    b1 = "\033[1m" if bold else ""
    return f"{b1}\033[38;2;{r};{g};{b}m{s}{RESET}"


def grade(pct):
    """Map a 0-100 usage percentage to a traffic-light color."""
    if pct is None:
        return DIM
    if pct < 60:
        return GREEN
    if pct < 80:
        return YELLOW
    if pct < 90:
        return ORANGE
    return RED


def read_stdin():
    try:
        return json.load(sys.stdin)
    except Exception:
        return {}


def git_branch(cwd):
    try:
        out = subprocess.run(
            ["git", "-C", cwd, "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, timeout=0.5,
        )
        if out.returncode == 0:
            b = out.stdout.strip()
            return b if b and b != "HEAD" else None
    except Exception:
        pass
    return None


def pct_of(node):
    """Extract a numeric used_percentage from a rate_limits window, or None."""
    if not isinstance(node, dict):
        return None
    v = node.get("used_percentage")
    return float(v) if isinstance(v, (int, float)) else None


def fmt_reset(resets_at, now):
    """Compact countdown until a Unix-epoch reset time, e.g. '2h13m', '3d4h'."""
    if not isinstance(resets_at, (int, float)):
        return None
    secs = max(0, int(resets_at - now))
    d, rem = divmod(secs, 86400)
    h, rem = divmod(rem, 3600)
    m = rem // 60
    if d:
        return f"{d}d{h}h"
    if h:
        return f"{h}h{m}m"
    return f"{m}m"


def bar(pct, width=8):
    p = 0.0 if pct is None else max(0.0, min(100.0, pct))
    filled = round(p / 100 * width)
    color = grade(p)
    return fg(color, "█" * filled) + fg(DIM, "░" * (width - filled))


def main():
    data = read_stdin()

    # icons (Nerd Font v3) + Claude sparkle. Keep a space after every PUA
    # icon: Ghostty clamps Nerd Font glyphs to one cell, but other terminals
    # may draw them wider, and the space absorbs any overflow.
    I_DIR = ""    # nf-fa-folder
    I_GIT = ""    # powerline branch
    I_MODEL = "✳"  # Claude sparkle (plain unicode, buffered by a space)
    I_5H = ""     # nf-fa-clock_o
    I_7D = ""     # nf-fa-calendar
    I_RESET = ""  # nf-fa-refresh; U+27F3 ⟳ falls back to a
                    # non-mono font and overflows its cell in Ghostty
    SEP = fg(DIM, "  ·  ")

    segs = []

    # project
    ws = data.get("workspace") or {}
    proj_dir = ws.get("project_dir") or ws.get("current_dir") or data.get("cwd") or os.getcwd()
    project = os.path.basename(proj_dir.rstrip("/")) or proj_dir
    segs.append(fg(CORAL, I_DIR) + " " + fg(TEXT, project, bold=True))

    # git branch
    cwd = ws.get("current_dir") or data.get("cwd") or os.getcwd()
    branch = git_branch(cwd)
    if branch:
        if len(branch) > 24:
            branch = branch[:23] + "…"
        segs.append(fg(VIOLET, I_GIT) + " " + fg(VIOLET, branch))

    # model, with the reasoning-effort tier tucked right after it. effort.level
    # is one of low/medium/high/xhigh/max (see EFFORT_COLORS); anything else
    # falls back to the model's coral so a future tier still shows, just uncolored.
    model = (data.get("model") or {}).get("display_name") or "Claude"
    model_seg = fg(CORAL, I_MODEL + " " + model)
    effort = (data.get("effort") or {}).get("level")
    if isinstance(effort, str) and effort:
        model_seg += " " + fg(EFFORT_COLORS.get(effort, CORAL), effort, bold=True)
    segs.append(model_seg)

    # context window
    cw = data.get("context_window") or {}
    ctx = cw.get("used_percentage")
    ctx = float(ctx) if isinstance(ctx, (int, float)) else None
    if ctx is None:
        segs.append(fg(DIM, "ctx ") + bar(None) + fg(DIM, " –"))
    else:
        segs.append(fg(DIM, "ctx ") + bar(ctx) + " " + fg(grade(ctx), f"{ctx:.0f}%"))

    # rate limits: 5-hour and 7-day, each with a reset countdown
    now = time.time()
    rl = data.get("rate_limits") or {}
    for icon, label, key in ((I_5H, "5h", "five_hour"), (I_7D, "7d", "seven_day")):
        node = rl.get(key) or {}
        p = pct_of(node)
        if p is None:
            segs.append(fg(DIM, f"{icon} {label} –"))
        else:
            seg = fg(DIM, f"{icon} {label} ") + fg(grade(p), f"{p:.0f}%")
            reset = fmt_reset(node.get("resets_at"), now)
            if reset:
                seg += fg(DIM, f" {I_RESET} {reset}")
            segs.append(seg)

    sys.stdout.write(SEP.join(segs))


if __name__ == "__main__":
    try:
        main()
    except Exception:
        # Never leave the status line blank: fall back to the bare directory
        # name (stdin is already consumed, so os.getcwd is the best signal).
        sys.stdout.write(fg(DIM, os.path.basename(os.getcwd()) or "~"))
