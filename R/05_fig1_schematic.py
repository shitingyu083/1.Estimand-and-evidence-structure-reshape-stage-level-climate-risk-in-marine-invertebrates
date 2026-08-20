from pathlib import Path
import sys

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch, Rectangle
from matplotlib.ticker import PercentFormatter


ROOT = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parents[1]
OUT = ROOT / "figures" / "main" / "Fig1_classification_and_evidence_update"

stage_counts = pd.read_csv(ROOT / "data" / "source_data" / "Fig1a_stage_driver_counts.csv")
mode_counts = pd.read_csv(ROOT / "data" / "source_data" / "Fig1c_mode_counts.csv")
balance = pd.read_csv(ROOT / "data" / "processed" / "ER_evidence_balance_by_stage.csv")

mpl.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Arial", "DejaVu Sans"],
    "font.size": 7.3,
    "axes.titlesize": 8.3,
    "axes.labelsize": 7.4,
    "xtick.labelsize": 6.7,
    "ytick.labelsize": 6.7,
    "legend.fontsize": 6.4,
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "svg.fonttype": "none",
})

stage_levels = ["EMBRYO", "LARVA", "SETTLEMENT/METAMORPHOSIS", "JUVENILE/ADULT"]
stage_labels = {
    "EMBRYO": "Embryo",
    "LARVA": "Larva",
    "SETTLEMENT/METAMORPHOSIS": "Settlement /\nmetamorphosis",
    "JUVENILE/ADULT": "Juvenile /\nadult",
}
stressor_levels = ["Acidification", "Warming", "Warming + acidification"]
stressor_pal = {
    "Acidification": "#0072B2",
    "Warming": "#D55E00",
    "Warming + acidification": "#009E73",
}
stage_pal = {
    "EMBRYO": "#E69F00",
    "LARVA": "#56B4E9",
    "SETTLEMENT/METAMORPHOSIS": "#009E73",
    "JUVENILE/ADULT": "#CC79A7",
}
mode_pal = {
    "With metamorphic stage": "#0072B2",
    "Without metamorphic stage": "#D55E00",
    "Mixed/unknown": "#999999",
}
direction_pal = {"Direction-interpretable": "#0072B2", "Context-dependent": "#E69F00"}
grey, grey_dark = "#6b7280", "#374151"

fig = plt.figure(figsize=(190 / 25.4, 132 / 25.4), constrained_layout=False)
gs = fig.add_gridspec(
    2, 3,
    height_ratios=[0.72, 1.28],
    width_ratios=[1.30, 0.88, 1.18],
    hspace=0.30,
    wspace=0.40,
)
ax_a = fig.add_subplot(gs[0, :])
ax_b = fig.add_subplot(gs[1, 0])
ax_c = fig.add_subplot(gs[1, 1])
ax_d = fig.add_subplot(gs[1, 2])

ax_a.set_xlim(0, 10)
ax_a.set_ylim(0, 3)
ax_a.axis("off")
ax_a.text(-0.015, 1.03, "a", transform=ax_a.transAxes, fontsize=10, fontweight="bold", va="bottom")
ax_a.text(0.0, 2.92, "Observed stage is not developmental mode", fontsize=9.1, fontweight="bold", ha="left", va="top")
ax_a.text(0.0, 2.47, "Measured endpoint stage", fontsize=7.8, fontweight="bold", color=grey_dark, ha="left")

xs = [1.05, 3.05, 5.15, 7.42]
box_w = [1.22, 1.10, 1.75, 1.45]
for i, st in enumerate(stage_levels):
    x = xs[i]
    w = box_w[i]
    ax_a.add_patch(FancyBboxPatch(
        (x - w / 2, 1.65), w, 0.50,
        boxstyle="round,pad=0.03,rounding_size=0.08",
        linewidth=0,
        facecolor=stage_pal[st],
        alpha=0.95,
    ))
    ax_a.text(x, 1.90, stage_labels[st], color="white", ha="center", va="center",
              fontsize=7.1, fontweight="bold", linespacing=0.84)
    if i < len(stage_levels) - 1:
        ax_a.add_patch(FancyArrowPatch(
            (x + w / 2 + 0.08, 1.90),
            (xs[i + 1] - box_w[i + 1] / 2 - 0.08, 1.90),
            arrowstyle="-|>",
            mutation_scale=9,
            linewidth=0.9,
            color=grey,
        ))

ax_a.text(0.0, 0.98, "Species developmental mode", fontsize=7.8, fontweight="bold", color=grey_dark, ha="left")
for label, x, color in [
    ("With\nmetamorphic\nstage", 1.45, mode_pal["With metamorphic stage"]),
    ("Without\nmetamorphic\nstage", 3.90, mode_pal["Without metamorphic stage"]),
    ("Mixed /\nunknown", 6.12, mode_pal["Mixed/unknown"]),
]:
    ax_a.add_patch(FancyBboxPatch(
        (x - 0.66, 0.31), 1.32, 0.60,
        boxstyle="round,pad=0.04,rounding_size=0.07",
        linewidth=0.8,
        edgecolor=color,
        facecolor="white",
    ))
    ax_a.text(x, 0.61, label, color=grey_dark, ha="center", va="center", fontsize=6.8, linespacing=0.82)

ax_a.add_patch(FancyArrowPatch((7.42, 1.60), (7.42, 0.95), arrowstyle="-|>", mutation_scale=8, linewidth=0.75, color=grey_dark))
ax_a.text(9.78, 0.68, "Example: an adult endpoint is coded as juvenile/adult,\neven for species with pelagic larvae",
          ha="right", va="center", fontsize=6.75, color=grey_dark, linespacing=0.86)
ax_a.add_patch(Rectangle((0.0, 0.12), 10.0, 0.01, color="#d1d5db", lw=0))

ax_b.text(-0.15, 1.05, "b", transform=ax_b.transAxes, fontsize=10, fontweight="bold")
pivot = stage_counts.pivot(index="ObservedStage", columns="StressorLabel", values="EffectSizeCount").reindex(stage_levels)[stressor_levels].fillna(0)
x = np.arange(len(stage_levels))
bottom = np.zeros(len(stage_levels))
for lab in stressor_levels:
    vals = pivot[lab].to_numpy()
    ax_b.bar(x, vals, bottom=bottom, color=stressor_pal[lab], width=0.72, edgecolor="white", linewidth=0.25, label=lab)
    bottom += vals
ax_b.set_title("Stage coverage by driver", loc="left", fontweight="bold")
ax_b.set_ylabel("Effect sizes")
ax_b.set_xticks(x)
ax_b.set_xticklabels([stage_labels[s].replace("\n", " ") for s in stage_levels], rotation=24, ha="right")
ax_b.legend(frameon=False, ncol=1, loc="upper left", bbox_to_anchor=(-0.02, 1.00), handlelength=0.9, handletextpad=0.35)
ax_b.spines[["top", "right"]].set_visible(False)
ax_b.grid(axis="y", color="#e5e7eb", linewidth=0.45)
ax_b.set_axisbelow(True)

ax_c.text(-0.18, 1.05, "c", transform=ax_c.transAxes, fontsize=10, fontweight="bold")
mode_order = ["With metamorphic stage", "Without metamorphic stage", "Mixed/unknown"]
mx = np.arange(len(mode_order))
mv = [float(mode_counts.loc[mode_counts["DevelopmentalModeGroup"].eq(m), "EffectSizeCount"].iloc[0]) for m in mode_order]
ax_c.bar(mx, mv, color=[mode_pal[m] for m in mode_order], width=0.66, edgecolor="white", linewidth=0.25)
for xi, v in zip(mx, mv):
    ax_c.text(xi, v + max(mv) * 0.025, f"{int(v):,}", ha="center", va="bottom", fontsize=6.8)
ax_c.set_title("Developmental-mode boundary", loc="left", fontweight="bold")
ax_c.set_ylabel("Effect sizes")
ax_c.set_xticks(mx)
ax_c.set_xticklabels(["With\nmetamorphic\nstage", "Without\nmetamorphic\nstage", "Mixed /\nunknown"], linespacing=0.82)
ax_c.set_ylim(0, max(mv) * 1.16)
ax_c.spines[["top", "right"]].set_visible(False)
ax_c.grid(axis="y", color="#e5e7eb", linewidth=0.45)
ax_c.set_axisbelow(True)

ax_d.text(-0.15, 1.05, "d", transform=ax_d.transAxes, fontsize=10, fontweight="bold")
bal = balance.set_index("ObservedStage").reindex(stage_levels)
interp = bal["AlignedEffectSizes"] / bal["EffectSizeCount"]
ctx = bal["ContextDependentEffectSizes"] / bal["EffectSizeCount"]
xd = np.arange(len(stage_levels))
ax_d.bar(xd, interp, color=direction_pal["Direction-interpretable"], width=0.72, edgecolor="white", linewidth=0.25)
ax_d.bar(xd, ctx, bottom=interp, color=direction_pal["Context-dependent"], width=0.72, edgecolor="white", linewidth=0.25)
for xi, base, v in zip(xd, interp, ctx):
    ax_d.text(xi, base + v / 2, f"{v * 100:.1f}%", ha="center", va="center",
              fontsize=6.4, color="white" if v > 0.20 else "#7c4a00", fontweight="bold")
ax_d.set_title("Endpoint-direction composition", loc="left", fontweight="bold")
ax_d.set_ylabel("Share of effect sizes")
ax_d.set_ylim(0, 1.25)
ax_d.set_yticks(np.linspace(0, 1, 6))
ax_d.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
ax_d.set_xticks(xd)
ax_d.set_xticklabels([stage_labels[s].replace("\n", " ") for s in stage_levels], rotation=24, ha="right")
ax_d.legend(["Direction-interpretable", "Context-dependent"], frameon=False, loc="upper left",
            bbox_to_anchor=(-0.02, 1.00), ncol=1, handlelength=0.9, handletextpad=0.35)
ax_d.spines[["top", "right"]].set_visible(False)
ax_d.grid(axis="y", color="#e5e7eb", linewidth=0.45)
ax_d.set_axisbelow(True)

fig.subplots_adjust(left=0.072, right=0.985, top=0.955, bottom=0.14)
for suffix in ["pdf", "svg", "png", "tiff"]:
    fig.savefig(OUT.with_suffix("." + suffix), dpi=600 if suffix in {"png", "tiff"} else None, facecolor="white")
plt.close(fig)
print(OUT)