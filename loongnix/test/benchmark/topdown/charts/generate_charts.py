#!/usr/bin/env python3
"""Deterministic SVG chart generator for the topdown experiment artifacts.

The generator reads only the checked-in parsed CSV. Missing metrics remain visibly
unavailable; no synthetic measurements or default values are inserted. PNG export
is optional and is reported in ``png-unavailable.txt`` when no renderer is installed.
"""
from __future__ import annotations

import argparse
import csv
import html
import importlib.util
import math
from pathlib import Path
from typing import Iterable

# Okabe-Ito-inspired, fixed order, with redundant labels/shapes in every chart.
PALETTE = {
    "compressed": "#0072B2",
    "uncompressed": "#D55E00",
    "green": "#009E73",
    "purple": "#CC79A7",
    "orange": "#E69F00",
    "ink": "#17202A",
    "muted": "#52606D",
    "grid": "#D9E2EC",
    "surface": "#FFFFFF",
    "unavailable": "#6B7280",
}

WIDTH, HEIGHT = 960, 600
MARGIN = (86, 56, 54, 76)  # left, top, right, bottom


def esc(value: object) -> str:
    return html.escape(str(value), quote=True)


def text(x: float, y: float, value: object, size: int = 14, *, fill: str = PALETTE["ink"],
         anchor: str = "start", weight: str = "400", family: str = "Arial, sans-serif") -> str:
    return (f'<text x="{x:.2f}" y="{y:.2f}" font-family="{family}" font-size="{size}px" '
            f'font-weight="{weight}" fill="{fill}" text-anchor="{anchor}">{esc(value)}</text>')


def svg_start(title: str, subtitle: str) -> list[str]:
    return [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" height="{HEIGHT}" '
        f'viewBox="0 0 {WIDTH} {HEIGHT}" role="img" aria-labelledby="title desc">',
        f'<title id="title">{esc(title)}</title>',
        f'<desc id="desc">{esc(subtitle)}</desc>',
        '<rect width="100%" height="100%" fill="#FFFFFF"/>',
        text(28, 34, title, 22, weight="700"),
        text(28, 52, subtitle, 12, fill=PALETTE["muted"]),
    ]


def svg_end(parts: list[str]) -> str:
    parts.append('</svg>')
    return '\n'.join(parts) + '\n'


def axes(parts: list[str], x_label: str, y_label: str, y_max: float, y_ticks: Iterable[float]) -> tuple[float, float, float, float]:
    left, top, right, bottom = MARGIN
    x0, y0, x1, y1 = left, top + 18, WIDTH - right, HEIGHT - bottom
    parts.append(f'<line x1="{x0}" y1="{y1}" x2="{x1}" y2="{y1}" stroke="{PALETTE["ink"]}"/>')
    parts.append(f'<line x1="{x0}" y1="{y0}" x2="{x0}" y2="{y1}" stroke="{PALETTE["ink"]}"/>')
    for tick in y_ticks:
        yy = y1 - (tick / y_max * (y1 - y0)) if y_max else y1
        parts.append(f'<line x1="{x0}" y1="{yy:.2f}" x2="{x1}" y2="{yy:.2f}" stroke="{PALETTE["grid"]}"/>')
        parts.append(text(x0 - 10, yy + 5, f"{tick:g}", 11, fill=PALETTE["muted"], anchor="end"))
    parts.append(text((x0 + x1) / 2, HEIGHT - 16, x_label, 13, anchor="middle"))
    parts.append(f'<text x="18" y="{(y0+y1)/2:.2f}" transform="rotate(-90 18 {(y0+y1)/2:.2f})" '
                 f'font-family="Arial, sans-serif" font-size="13px" fill="{PALETTE["ink"]}" text-anchor="middle">{esc(y_label)}</text>')
    return x0, y0, x1, y1


def load_rows(csv_path: Path) -> tuple[list[dict[str, str]], list[str]]:
    if not csv_path.exists():
        return [], [f"CSV unavailable: {csv_path}"]
    with csv_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if not reader.fieldnames:
            return [], ["CSV unavailable: missing header"]
        rows = [row for row in reader if any((v or "").strip() for v in row.values())]
    return rows, []


def numeric(row: dict[str, str], key: str) -> float | None:
    try:
        value = (row.get(key) or "").strip()
        return float(value) if value else None
    except ValueError:
        return None


def chart_workers(rows: list[dict[str, str]]) -> str:
    title = "Observed elapsed time by worker count"
    subtitle = "Host software measurements from parsed/host-experiments.csv; lower is better."
    p = svg_start(title, subtitle)
    valid = [r for r in rows if numeric(r, "workers") is not None and numeric(r, "elapsed_sec") is not None]
    configs = [("compressed", PALETTE["compressed"], "circle"), ("uncompressed", PALETTE["uncompressed"], "square")]
    ymax = max([numeric(r, "elapsed_sec") or 0 for r in valid] + [1]) * 1.12
    x0, y0, x1, y1 = axes(p, "Workers", "Elapsed time (s)", ymax, [0, ymax / 2, ymax])
    workers = sorted({numeric(r, "workers") for r in valid})
    xpos = {w: x0 + ((w - min(workers)) / (max(workers) - min(workers)) * (x1-x0) if len(workers)>1 else (x1-x0)/2) for w in workers}
    for config, color, shape in configs:
        points = sorted([(numeric(r, "workers"), numeric(r, "elapsed_sec")) for r in valid if r.get("compression") == config], key=lambda q: q[0])
        if points:
            path = " ".join(("M" if i == 0 else "L") + f" {xpos[w]:.2f} {y1 - v/ymax*(y1-y0):.2f}" for i, (w, v) in enumerate(points))
            p.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="2.5"/>')
            for w, v in points:
                xx, yy = xpos[w], y1 - v/ymax*(y1-y0)
                if shape == "circle": p.append(f'<circle cx="{xx:.2f}" cy="{yy:.2f}" r="6" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
                else: p.append(f'<rect x="{xx-6:.2f}" y="{yy-6:.2f}" width="12" height="12" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
        else:
            p.append(text(x1 - 10, y0 + 18 + 22*configs.index((config,color,shape)), f"{config}: unavailable", 12, fill=PALETTE["unavailable"], anchor="end"))
    for w in workers:
        p.append(text(xpos[w], y1 + 22, f"{int(w)}", 11, fill=PALETTE["muted"], anchor="middle"))
    p.append(f'<circle cx="{x1-180}" cy="{y0+12}" r="6" fill="{PALETTE["compressed"]}"/>')
    p.append(text(x1-166, y0+17, "compressed", 12))
    p.append(f'<rect x="{x1-180}" y="{y0+29}" width="12" height="12" fill="{PALETTE["uncompressed"]}"/>')
    p.append(text(x1-166, y0+40, "uncompressed", 12))
    if not valid:
        p.append(text((x0+x1)/2, (y0+y1)/2, "Unavailable: no numeric elapsed_sec rows", 16, fill=PALETTE["unavailable"], anchor="middle", weight="700"))
    return svg_end(p)


def chart_metric(rows: list[dict[str, str]], metric: str, label: str, filename_title: str) -> str:
    p = svg_start(filename_title, f"Parsed host measurements; {label.lower()}. Missing values are not imputed.")
    valid = [r for r in rows if numeric(r, "workers") is not None and numeric(r, metric) is not None]
    ymax = max([numeric(r, metric) or 0 for r in valid] + [1]) * 1.15
    x0, y0, x1, y1 = axes(p, "Workers", label, ymax, [0, ymax / 2, ymax])
    configs = [("compressed", PALETTE["compressed"], "circle"), ("uncompressed", PALETTE["uncompressed"], "square")]
    workers = sorted({numeric(r, "workers") for r in valid})
    xpos = {w: x0 + ((w-min(workers))/(max(workers)-min(workers))*(x1-x0) if len(workers)>1 else (x1-x0)/2) for w in workers}
    for config, color, shape in configs:
        points = sorted([(numeric(r,"workers"), numeric(r,metric)) for r in valid if r.get("compression") == config], key=lambda z:z[0])
        for w, v in points:
            xx, yy = xpos[w], y1-v/ymax*(y1-y0)
            if shape == "circle": p.append(f'<circle cx="{xx:.2f}" cy="{yy:.2f}" r="6" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
            else: p.append(f'<rect x="{xx-6:.2f}" y="{yy-6:.2f}" width="12" height="12" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
        if not points: p.append(text(x1-10, y0+20+20*configs.index((config,color,shape)), f"{config}: unavailable", 12, fill=PALETTE["unavailable"], anchor="end"))
    for w in workers: p.append(text(xpos[w], y1+22, int(w), 11, fill=PALETTE["muted"], anchor="middle"))
    p.append(text(x1-10, y0+17, "● compressed   ■ uncompressed", 12, anchor="end"))
    if not valid: p.append(text((x0+x1)/2, (y0+y1)/2, f"Unavailable: no numeric {metric} rows", 16, fill=PALETTE["unavailable"], anchor="middle", weight="700"))
    return svg_end(p)


def sensitivity(rows: list[dict[str, str]]) -> str:
    p = svg_start("Sensitivity: elapsed-time change", "Speedup is computed within each compression mode relative to its observed one-worker row.")
    valid = [r for r in rows if numeric(r,"workers") is not None and numeric(r,"elapsed_sec") is not None]
    grouped: dict[str, list[dict[str,str]]] = {}
    for r in valid: grouped.setdefault(r.get("compression", "unknown"), []).append(r)
    points: list[tuple[str,float,float]] = []
    for config, group in grouped.items():
        base = next((numeric(r,"elapsed_sec") for r in group if numeric(r,"workers") == 1), None)
        if base and base > 0:
            for r in group:
                v = numeric(r,"elapsed_sec")
                if v is not None: points.append((config, numeric(r,"workers") or 0, base/v))
    ymax = max([v for _,_,v in points] + [1]) * 1.15
    x0,y0,x1,y1 = axes(p,"Workers","Speedup vs observed w1 (×)",ymax,[0,ymax/2,ymax])
    workers=sorted({w for _,w,_ in points}); xpos={w:x0+((w-min(workers))/(max(workers)-min(workers))*(x1-x0) if len(workers)>1 else (x1-x0)/2) for w in workers}
    for config,color,shape in [("compressed",PALETTE["compressed"],"circle"),("uncompressed",PALETTE["uncompressed"],"square")]:
        ps=sorted([(w,v) for c,w,v in points if c==config])
        if ps:
            path=" ".join(("M" if i==0 else "L")+f" {xpos[w]:.2f} {y1-v/ymax*(y1-y0):.2f}" for i,(w,v) in enumerate(ps)); p.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="2.5"/>')
            for w,v in ps:
                xx,yy=xpos[w],y1-v/ymax*(y1-y0)
                mark=f'<circle cx="{xx:.2f}" cy="{yy:.2f}" r="6"' if shape=="circle" else f'<rect x="{xx-6:.2f}" y="{yy-6:.2f}" width="12" height="12"'
                p.append(mark+f' fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
        else: p.append(text(x1-10,y0+18, f"{config}: unavailable (no w1 baseline)",12,fill=PALETTE["unavailable"],anchor="end"))
    for w in workers:p.append(text(xpos[w],y1+22,int(w),11,fill=PALETTE["muted"],anchor="middle"))
    p.append(text(x1-10,y0+17,"● compressed   ■ uncompressed",12,anchor="end"))
    if not points:p.append(text((x0+x1)/2,(y0+y1)/2,"Unavailable: insufficient paired measurements",16,fill=PALETTE["unavailable"],anchor="middle",weight="700"))
    return svg_end(p)


def amdahl(rows: list[dict[str, str]]) -> str:
    p = svg_start("Amdahl view: observed speedup", "Only observed speedups are shown; serial fraction/model fit is unavailable without a validated model input.")
    valid = [r for r in rows if numeric(r,"workers") is not None and numeric(r,"elapsed_sec") is not None]
    grouped: dict[str,list[dict[str,str]]] = {}
    for r in valid: grouped.setdefault(r.get("compression","unknown"),[]).append(r)
    points=[]
    for config, group in grouped.items():
        base=next((numeric(r,"elapsed_sec") for r in group if numeric(r,"workers")==1),None)
        if base and base>0:
            points += [(config,numeric(r,"workers") or 0,base/(numeric(r,"elapsed_sec") or base)) for r in group]
    ymax=max([v for _,_,v in points]+[1])*1.2;x0,y0,x1,y1=axes(p,"Workers","Observed speedup (×)",ymax,[0,ymax/2,ymax])
    ws=sorted({w for _,w,_ in points}); xpos={w:x0+((w-min(ws))/(max(ws)-min(ws))*(x1-x0) if len(ws)>1 else (x1-x0)/2) for w in ws}
    for config,color,shape in [("compressed",PALETTE["compressed"],"circle"),("uncompressed",PALETTE["uncompressed"],"square")]:
        ps=sorted([(w,v) for c,w,v in points if c==config])
        if ps:
            path=" ".join(("M" if i==0 else "L")+f" {xpos[w]:.2f} {y1-v/ymax*(y1-y0):.2f}" for i,(w,v) in enumerate(ps)); p.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="2.5"/>')
            for w,v in ps:
                xx,yy=xpos[w],y1-v/ymax*(y1-y0)
                if shape=="circle":p.append(f'<circle cx="{xx:.2f}" cy="{yy:.2f}" r="6" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
                else:p.append(f'<rect x="{xx-6:.2f}" y="{yy-6:.2f}" width="12" height="12" fill="{color}" stroke="#FFFFFF" stroke-width="2"/>')
    p.append(text(x1-10,y0+17,"● compressed   ■ uncompressed",12,anchor="end"))
    p.append(text(x0+8,y0+28,"Model fit / serial fraction: unavailable",13,fill=PALETTE["unavailable"],weight="700"))
    if not points:p.append(text((x0+x1)/2,(y0+y1)/2,"Unavailable: no paired speedup observations",16,fill=PALETTE["unavailable"],anchor="middle",weight="700"))
    return svg_end(p)


def dataflow() -> str:
    p=svg_start("HWGC dataflow", "Conceptual flow diagram; this is an architecture aid, not a measured timing chart.")
    boxes=[("Mutator / roots",100,180,180,62,PALETTE["compressed"]),("Work queue",390,180,180,62,PALETTE["green"]),("Trace + copy",680,180,180,62,PALETTE["orange"]),("Survivor space",390,360,180,62,PALETTE["purple"])]
    p.append('<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto"><path d="M0,0 L8,4 L0,8 z" fill="#52606D"/></marker></defs>')
    for label,x,y,w,h,color in boxes:
        p.append(f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="8" fill="#FFFFFF" stroke="{color}" stroke-width="3"/>');p.append(text(x+w/2,y+37,label,15,anchor="middle",weight="700"))
    for x1,y1,x2,y2,lab in [(280,211,390,211,"enqueue"),(570,211,680,211,"dequeue"),(480,242,480,360,"copy result"),(680,242,570,391,"forward"),(390,391,280,242,"ref update")]:
        p.append(f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="#52606D" stroke-width="2" marker-end="url(#arrow)"/>');p.append(text((x1+x2)/2,(y1+y2)/2-7,lab,11,fill=PALETTE["muted"],anchor="middle"))
    p.append(text(480,500,"Measured queue depth / bandwidth: unavailable in parsed CSV",14,fill=PALETTE["unavailable"],anchor="middle",weight="700"))
    return svg_end(p)


def write_png_notice(out: Path, reason: str) -> None:
    (out / "png-unavailable.txt").write_text("PNG outputs were not generated. " + reason + "\n", encoding="utf-8")


def main() -> int:
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input",type=Path,default=Path(__file__).resolve().parents[1]/"parsed"/"host-experiments.csv")
    parser.add_argument("--output",type=Path,default=Path(__file__).resolve().parent)
    args=parser.parse_args(); args.output.mkdir(parents=True,exist_ok=True)
    rows, notices=load_rows(args.input)
    charts={"runtime-by-workers.svg":chart_workers(rows),"evacuation-ms.svg":chart_metric(rows,"evac_ms","Evacuation time (ms)","Observed evacuation time"),"sensitivity.svg":sensitivity(rows),"amdahl.svg":amdahl(rows),"dataflow.svg":dataflow()}
    for name,content in charts.items():(args.output/name).write_text(content,encoding="utf-8")
    # Keep raster generation optional and deterministic. Do not emit a fake/blank PNG.
    if importlib.util.find_spec("cairosvg") is None:
        write_png_notice(args.output, "install cairosvg to render the SVGs to PNG; SVG files remain the canonical outputs.")
    else:
        import cairosvg
        for name in charts:
            cairosvg.svg2png(bytestring=charts[name].encode("utf-8"), write_to=str(args.output/name.replace(".svg",".png")), output_width=WIDTH, output_height=HEIGHT)
    if notices:(args.output/"input-unavailable.txt").write_text("\n".join(notices)+"\n",encoding="utf-8")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
