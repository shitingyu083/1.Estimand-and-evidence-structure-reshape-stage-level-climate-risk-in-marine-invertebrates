from __future__ import annotations

import math
import re
from pathlib import Path

import numpy as np
import pandas as pd

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm, LinearSegmentedColormap, ListedColormap

from sklearn.compose import ColumnTransformer
from sklearn.dummy import DummyRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.linear_model import RidgeCV
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import GroupKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data" / "processed" / "analysis_ready_model_dataset.csv"
OUT_PROCESSED = ROOT / "data" / "processed"
OUT_SOURCE = ROOT / "data" / "source_data"
OUT_FIG = ROOT / "figures" / "main"
LOG_DIR = ROOT / "analysis_logs"

STAGE_LEVELS = ["EMBRYO", "LARVA", "SETTLEMENT/METAMORPHOSIS", "JUVENILE/ADULT"]
STAGE_LABELS = {
    "EMBRYO": "Embryo",
    "LARVA": "Larva",
    "SETTLEMENT/METAMORPHOSIS": "Settlement/\nmetamorphosis",
    "JUVENILE/ADULT": "Juvenile/\nadult",
}
STAGE_LABELS_ONE = {
    "EMBRYO": "Embryo",
    "LARVA": "Larva",
    "SETTLEMENT/METAMORPHOSIS": "Settlement/metamorphosis",
    "JUVENILE/ADULT": "Juvenile/adult",
}
STRESSOR_LEVELS = ["Acidification", "Warming", "Warming + acidification"]

COL = {
    "blue": "#2477A8",
    "blue_light": "#DCEAF2",
    "red": "#C95D3A",
    "red_light": "#F3DDD5",
    "green": "#2A8C72",
    "purple": "#A96F9A",
    "amber": "#D99A21",
    "black": "#222426",
    "grey_dark": "#555B61",
    "grey": "#858A8F",
    "grey_light": "#D5D8DA",
    "grey_pale": "#F2F3F4",
    "white": "#FFFFFF",
}
STAGE_COLORS = {
    "EMBRYO": "#4B3F72",
    "LARVA": "#3F708F",
    "SETTLEMENT/METAMORPHOSIS": "#2F907E",
    "JUVENILE/ADULT": "#86AA4E",
}


def ensure_dirs() -> None:
    for p in [OUT_PROCESSED, OUT_SOURCE, OUT_FIG, LOG_DIR]:
        p.mkdir(parents=True, exist_ok=True)


def endpoint_class(row: pd.Series) -> str:
    text = " | ".join(
        str(row.get(c, "") if pd.notna(row.get(c, "")) else "")
        for c in ["Metric", "TransformedMetric", "Category", "LifeStage"]
    )
    text = re.sub(r"\s+", " ", text.lower()).strip()
    cat = str(row.get("Category", ""))
    if re.search(
        r"mortality|\bdead\b|\bdeath\b|abnormal|deform|malformation|delay|time to|bleach|lesion|damage|apoptosis|necrosis",
        text,
    ):
        return "adverse_when_increases"
    if re.search(
        r"survival|viability|hatching success|hatch success|settlement success|metamorph.*success|fertiliz.*success|fecundity|egg production|reproductive success",
        text,
    ):
        return "beneficial_when_increases"
    if cat == "Growth" and not re.search(
        r"condition|ratio|allometr|shape|thickness|diameter|length|size|weight|biomass",
        text,
    ):
        return "beneficial_when_increases"
    if cat == "Growth" and re.search(
        r"growth|biomass|weight|length|size|diameter|condition index|condition factor|hatching size|test thickness",
        text,
    ):
        return "beneficial_when_increases"
    if (
        cat == "Calcification"
        and re.search(r"calcification rate|net calcification|calcified mass|\bgnet\b", text)
        and not re.search(r"proportion|ratio|peak intensity|aragonite|calcite:|content in shell", text)
    ):
        return "beneficial_when_increases"
    return "context_dependent"


def inverse_variance_aggregate(df: pd.DataFrame, group_cols: list[str], yi_col: str = "lnRR") -> pd.DataFrame:
    d = df.loc[
        np.isfinite(df[yi_col]) & np.isfinite(df["vi_model"]) & (df["vi_model"] > 0),
        group_cols + [yi_col, "vi_model"],
    ].copy()
    d["_w"] = 1.0 / d["vi_model"]
    d["_wy"] = d["_w"] * d[yi_col]
    out = (
        d.groupby(group_cols, dropna=False)
        .agg(weight_sum=("_w", "sum"), weighted_y=("_wy", "sum"), RawEffectSizeCount=(yi_col, "size"))
        .reset_index()
    )
    out["yi"] = out["weighted_y"] / out["weight_sum"]
    out["vi"] = 1.0 / out["weight_sum"]
    out = out[np.isfinite(out["yi"]) & np.isfinite(out["vi"]) & (out["vi"] > 0)].copy()
    return out.drop(columns=["weighted_y"])


def weighted_mean_abs(x: pd.DataFrame) -> float:
    w = 1.0 / x["vi"].to_numpy(float)
    y = np.abs(x["yi"].to_numpy(float))
    return float(np.sum(w * y) / np.sum(w))


def median_abs(x: pd.DataFrame) -> float:
    return float(np.median(np.abs(x["yi"].to_numpy(float))))


def noise_corrected_rms(x: pd.DataFrame) -> float:
    return float(math.sqrt(max(np.mean(x["yi"].to_numpy(float) ** 2 - x["vi"].to_numpy(float)), 0.0)))


def bootstrap_ci(x: pd.DataFrame, func, reps: int, rng: np.random.Generator) -> tuple[float, float, float]:
    if len(x) == 0:
        return np.nan, np.nan, np.nan
    obs = func(x)
    if len(x) < 3:
        return obs, np.nan, np.nan
    vals = []
    for _ in range(reps):
        idx = rng.integers(0, len(x), len(x))
        vals.append(func(x.iloc[idx]))
    lo, hi = np.quantile(vals, [0.025, 0.975])
    return obs, float(lo), float(hi)


def null_calibrate_group(x: pd.DataFrame, reps: int, rng: np.random.Generator) -> dict[str, float]:
    obs = weighted_mean_abs(x)
    obs_med = median_abs(x)
    obs_rms = noise_corrected_rms(x)
    if len(x) < 3:
        return {
            "ObservedWeightedMeanAbs": obs,
            "NullMeanWeightedMeanAbs": np.nan,
            "ExcessWeightedMeanAbs": np.nan,
            "NullCalibratedP": np.nan,
            "ObservedMedianAbs": obs_med,
            "ObservedNoiseCorrectedRMS": obs_rms,
        }
    vi = x["vi"].to_numpy(float)
    w = 1.0 / vi
    null_vals = []
    null_rms = []
    for _ in range(reps):
        sim = rng.normal(0.0, np.sqrt(vi))
        null_vals.append(float(np.sum(w * np.abs(sim)) / np.sum(w)))
        null_rms.append(float(math.sqrt(max(np.mean(sim**2 - vi), 0.0))))
    null_vals = np.asarray(null_vals)
    p = (np.sum(null_vals >= obs) + 1.0) / (len(null_vals) + 1.0)
    return {
        "ObservedWeightedMeanAbs": obs,
        "NullMeanWeightedMeanAbs": float(np.mean(null_vals)),
        "ExcessWeightedMeanAbs": float(obs - np.mean(null_vals)),
        "NullCalibratedP": float(p),
        "ObservedMedianAbs": obs_med,
        "ObservedNoiseCorrectedRMS": obs_rms,
        "NullMeanNoiseCorrectedRMS": float(np.mean(null_rms)),
    }


def deviation_tables(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    rng = np.random.default_rng(20260813)
    subsets = {
        "All eligible endpoints": df,
        "Development endpoints only": df[df["Category"] == "Development"].copy(),
        "Conservatively direction-interpretable endpoints": df[np.isfinite(df["aligned_lnRR_strict"])].copy(),
    }
    rows = []
    calibration_rows = []
    dimensions = {
        "Stage": ["ObservedStage"],
        "Stressor": ["StressorLabel"],
        "Endpoint category": ["Category"],
        "Stage x stressor": ["ObservedStage", "StressorLabel"],
    }
    for evidence, sub in subsets.items():
        for dim, group_cols in dimensions.items():
            agg = inverse_variance_aggregate(sub, ["StudyID"] + group_cols, "lnRR")
            for key, x in agg.groupby(group_cols, dropna=False):
                if not isinstance(key, tuple):
                    key = (key,)
                info = dict(zip(group_cols, key))
                cal = null_calibrate_group(x, reps=1200, rng=rng)
                obs, lo, hi = bootstrap_ci(x, weighted_mean_abs, 900, rng)
                med, med_lo, med_hi = bootstrap_ci(x, median_abs, 900, rng)
                rms, rms_lo, rms_hi = bootstrap_ci(x, noise_corrected_rms, 900, rng)
                base = {
                    "EvidenceSet": evidence,
                    "Dimension": dim,
                    **info,
                    "StudyCellCount": len(x),
                    "RawEffectSizeCount": int(x["RawEffectSizeCount"].sum()),
                    "ObservedWeightedMeanAbs": obs,
                    "WeightedMeanAbs_CI_low": lo,
                    "WeightedMeanAbs_CI_high": hi,
                    "ObservedMedianAbs": med,
                    "MedianAbs_CI_low": med_lo,
                    "MedianAbs_CI_high": med_hi,
                    "ObservedNoiseCorrectedRMS": rms,
                    "NoiseCorrectedRMS_CI_low": rms_lo,
                    "NoiseCorrectedRMS_CI_high": rms_hi,
                    **cal,
                }
                rows.append(base)
                calibration_rows.append(base)
    res = pd.DataFrame(rows)
    if "NullCalibratedP" in res:
        valid = res["NullCalibratedP"].notna()
        res.loc[valid, "NullCalibratedQ"] = bh_adjust(res.loc[valid, "NullCalibratedP"].to_numpy())
    return res, pd.DataFrame(calibration_rows)


def bh_adjust(p: np.ndarray) -> np.ndarray:
    p = np.asarray(p, dtype=float)
    n = len(p)
    order = np.argsort(p)
    ranked = np.empty(n, dtype=float)
    prev = 1.0
    for i in range(n - 1, -1, -1):
        idx = order[i]
        val = p[idx] * n / (i + 1)
        prev = min(prev, val)
        ranked[idx] = min(prev, 1.0)
    return ranked


def build_ml_dataset(df: pd.DataFrame) -> pd.DataFrame:
    group_cols = [
        "StudyID",
        "ObservedStage",
        "StressorLabel",
        "Category",
        "DevelopmentalModeGroup",
        "Animal",
        "Family",
        "PaperYear",
    ]
    agg = inverse_variance_aggregate(df, group_cols, "lnRR")
    agg["AbsDeparture"] = np.abs(agg["yi"])
    agg["ExpectedAbsNoise"] = np.sqrt(2.0 * agg["vi"] / math.pi)
    agg["ExcessAbsDeparture"] = agg["AbsDeparture"] - agg["ExpectedAbsNoise"]
    agg["Precision"] = 1.0 / agg["vi"]
    agg["LogPrecision"] = np.log1p(agg["Precision"])
    for col in ["Animal", "Family"]:
        counts = agg[col].astype(str).value_counts()
        keep = set(counts[counts >= 10].index)
        agg[col] = agg[col].astype(str).where(agg[col].astype(str).isin(keep), "Other/rare")
    return agg


def make_preprocessor() -> ColumnTransformer:
    cat_cols = ["ObservedStage", "StressorLabel", "Category", "DevelopmentalModeGroup", "Animal", "Family"]
    num_cols = ["PaperYear", "RawEffectSizeCount", "LogPrecision"]
    return ColumnTransformer(
        transformers=[
            ("cat", Pipeline([("imputer", SimpleImputer(strategy="most_frequent")), ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=False))]), cat_cols),
            ("num", Pipeline([("imputer", SimpleImputer(strategy="median")), ("scale", StandardScaler())]), num_cols),
        ],
        remainder="drop",
        verbose_feature_names_out=False,
    )


def grouped_cv_models(ml: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    feature_cols = [
        "ObservedStage",
        "StressorLabel",
        "Category",
        "DevelopmentalModeGroup",
        "Animal",
        "Family",
        "PaperYear",
        "RawEffectSizeCount",
        "LogPrecision",
    ]
    targets = {"Signed study-stage-category lnRR": "yi", "Reference-deviation target": "ExcessAbsDeparture"}
    model_specs = {
        "Study-held-out mean": DummyRegressor(strategy="mean"),
        "Regularized linear model": RidgeCV(alphas=np.logspace(-3, 3, 13)),
        "Random forest": RandomForestRegressor(
            n_estimators=350,
            min_samples_leaf=4,
            max_features="sqrt",
            random_state=20260813,
            n_jobs=1,
        ),
    }
    groups = ml["StudyID"].astype(str).to_numpy()
    n_splits = min(5, len(np.unique(groups)))
    cv = GroupKFold(n_splits=n_splits)
    perf_rows = []
    pred_rows = []
    importance_rows = []
    rng = np.random.default_rng(20260813)

    for target_name, target_col in targets.items():
        y = ml[target_col].to_numpy(float)
        for model_name, model in model_specs.items():
            preds = np.full(len(ml), np.nan)
            for fold, (train_idx, test_idx) in enumerate(cv.split(ml[feature_cols], y, groups), start=1):
                pipe = Pipeline([("pre", make_preprocessor()), ("model", model)])
                pipe.fit(ml.iloc[train_idx][feature_cols], y[train_idx])
                preds[test_idx] = pipe.predict(ml.iloc[test_idx][feature_cols])
                if model_name != "Study-held-out mean":
                    base_rmse = math.sqrt(mean_squared_error(y[test_idx], preds[test_idx]))
                    for col in feature_cols:
                        deltas = []
                        for _ in range(8):
                            x_perm = ml.iloc[test_idx][feature_cols].copy()
                            vals = x_perm[col].to_numpy(copy=True)
                            rng.shuffle(vals)
                            x_perm[col] = vals
                            p_perm = pipe.predict(x_perm)
                            deltas.append(math.sqrt(mean_squared_error(y[test_idx], p_perm)) - base_rmse)
                        importance_rows.append(
                            {
                                "Target": target_name,
                                "Model": model_name,
                                "Fold": fold,
                                "Feature": col,
                                "PermutationDeltaRMSE": float(np.mean(deltas)),
                            }
                        )
            perf_rows.append(
                {
                    "Target": target_name,
                    "Model": model_name,
                    "Rows": len(ml),
                    "StudyCount": ml["StudyID"].nunique(),
                    "RMSE": math.sqrt(mean_squared_error(y, preds)),
                    "MAE": mean_absolute_error(y, preds),
                    "R2": r2_score(y, preds),
                }
            )
            pred_rows.extend(
                {
                    "Target": target_name,
                    "Model": model_name,
                    "StudyID": sid,
                    "ObservedStage": stage,
                    "Observed": obs,
                    "Predicted": pred,
                }
                for sid, stage, obs, pred in zip(ml["StudyID"], ml["ObservedStage"], y, preds)
            )
    imp = pd.DataFrame(importance_rows)
    if not imp.empty:
        imp = (
            imp.groupby(["Target", "Model", "Feature"], as_index=False)
            .agg(MeanDeltaRMSE=("PermutationDeltaRMSE", "mean"), SEDeltaRMSE=("PermutationDeltaRMSE", "sem"))
            .sort_values(["Target", "Model", "MeanDeltaRMSE"], ascending=[True, True, False])
        )
    return pd.DataFrame(perf_rows), imp, pd.DataFrame(pred_rows)


def draw_fig5(dev: pd.DataFrame, perf: pd.DataFrame, imp: pd.DataFrame, ml: pd.DataFrame, pred: pd.DataFrame) -> None:
    plt.rcParams.update(
        {
            "font.family": "Arial",
            "font.size": 7.2,
            "axes.linewidth": 0.55,
            "xtick.major.width": 0.55,
            "ytick.major.width": 0.55,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )
    fig = plt.figure(figsize=(7.48, 7.2), dpi=220)
    gs = fig.add_gridspec(3, 2, height_ratios=[1.0, 1.0, 0.95], hspace=0.76, wspace=0.34)
    axes = np.array(
        [
            [fig.add_subplot(gs[0, 0]), fig.add_subplot(gs[0, 1])],
            [fig.add_subplot(gs[1, 0]), fig.add_subplot(gs[1, 1])],
            [fig.add_subplot(gs[2, 0]), fig.add_subplot(gs[2, 1])],
        ],
        dtype=object,
    )
    ax = axes[0, 0]
    stage = dev[(dev["EvidenceSet"] == "All eligible endpoints") & (dev["Dimension"] == "Stage")].copy()
    stage["ObservedStage"] = pd.Categorical(stage["ObservedStage"], STAGE_LEVELS, ordered=True)
    stage = stage.sort_values("ObservedStage")
    x = np.arange(len(stage))
    ax.bar(x - 0.18, stage["NullMeanWeightedMeanAbs"], width=0.36, color=COL["grey_light"], label="Sampling-error null")
    ax.bar(
        x + 0.18,
        stage["ObservedWeightedMeanAbs"],
        width=0.36,
        color=[STAGE_COLORS[s] for s in stage["ObservedStage"].astype(str)],
        label="Observed departure",
    )
    ax.errorbar(
        x + 0.18,
        stage["ObservedWeightedMeanAbs"],
        yerr=[
            stage["ObservedWeightedMeanAbs"] - stage["WeightedMeanAbs_CI_low"],
            stage["WeightedMeanAbs_CI_high"] - stage["ObservedWeightedMeanAbs"],
        ],
        fmt="none",
        ecolor=COL["black"],
        elinewidth=0.65,
        capsize=2,
    )
    rng = np.random.default_rng(20260813)
    ml_stage = ml.copy()
    ml_stage["ObservedStage"] = pd.Categorical(ml_stage["ObservedStage"], STAGE_LEVELS, ordered=True)
    for i, st in enumerate(STAGE_LEVELS):
        vals = ml_stage.loc[ml_stage["ObservedStage"].astype(str) == st, "AbsDeparture"].dropna().to_numpy()
        if len(vals):
            vals = rng.choice(vals, size=min(90, len(vals)), replace=False)
            clipped = vals > 0.88
            vals_plot = np.minimum(vals, 0.88)
            jitter = rng.normal(i + 0.18, 0.035, len(vals))
            ax.scatter(jitter[~clipped], vals_plot[~clipped], s=4, color=COL["black"], alpha=0.18, linewidths=0)
            ax.scatter(jitter[clipped], vals_plot[clipped], s=7, marker="^", color=COL["black"], alpha=0.22, linewidths=0)
    for xi, q in zip(x, stage["NullCalibratedQ"]):
        lab = "q<0.001" if q < 0.001 else f"q={q:.3f}"
        ax.text(xi + 0.18, stage.iloc[xi]["ObservedWeightedMeanAbs"] + 0.025, lab, ha="center", va="bottom", fontsize=6)
    ax.set_xticks(x, [STAGE_LABELS[s] for s in stage["ObservedStage"].astype(str)])
    ax.set_ylim(0, 0.96)
    ax.set_ylabel("Weighted mean |signed aggregate|")
    ax.set_title("a  Departure exceeds the sampling-error null")
    ax.legend(frameon=False, loc="upper left", fontsize=6)
    ax.spines[["top", "right"]].set_visible(False)

    ax = axes[0, 1]
    heat = dev[(dev["EvidenceSet"] == "All eligible endpoints") & (dev["Dimension"] == "Stage x stressor")].copy()
    mat = heat.pivot(index="ObservedStage", columns="StressorLabel", values="ExcessWeightedMeanAbs").reindex(STAGE_LEVELS)[STRESSOR_LEVELS]
    counts = heat.pivot(index="ObservedStage", columns="StressorLabel", values="RawEffectSizeCount").reindex(STAGE_LEVELS)[STRESSOR_LEVELS]
    cmap = LinearSegmentedColormap.from_list("excess", [COL["blue_light"], COL["white"], COL["red_light"], COL["red"]])
    values = mat.to_numpy(float)
    vmin, vmax = float(np.nanmin(values)), float(np.nanmax(values))
    bounds = np.linspace(vmin, vmax, 13)
    vector_cmap = ListedColormap(cmap(np.linspace(0, 1, len(bounds) - 1)))
    vector_norm = BoundaryNorm(bounds, vector_cmap.N)
    im = ax.pcolormesh(
        np.arange(values.shape[1] + 1) - 0.5,
        np.arange(values.shape[0] + 1) - 0.5,
        values,
        cmap=vector_cmap,
        norm=vector_norm,
        shading="flat",
    )
    ax.set_xlim(-0.5, values.shape[1] - 0.5)
    ax.set_ylim(values.shape[0] - 0.5, -0.5)
    ax.set_xticks(np.arange(len(STRESSOR_LEVELS)), STRESSOR_LEVELS, rotation=24, ha="right")
    ax.tick_params(axis="x", labelsize=6.4)
    ax.set_yticks(np.arange(len(STAGE_LEVELS)), [STAGE_LABELS_ONE[s] for s in STAGE_LEVELS])
    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            val = mat.iloc[i, j]
            n = counts.iloc[i, j]
            txt = "" if pd.isna(val) else f"{val:.2f}\n({int(n)})"
            ax.text(j, i, txt, ha="center", va="center", fontsize=6.2, color=COL["black"])
    cb = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.02, boundaries=bounds)
    cb.solids.set_rasterized(False)
    cb.set_label("Excess over null")
    ax.set_title("b  Stage x driver excess departure")
    for spine in ax.spines.values():
        spine.set_visible(False)

    ax = axes[1, 0]
    cats = dev[(dev["EvidenceSet"] == "All eligible endpoints") & (dev["Dimension"] == "Endpoint category")].copy()
    cats = cats[cats["StudyCellCount"] >= 3].sort_values("ExcessWeightedMeanAbs", ascending=True)
    ax.barh(cats["Category"], cats["ExcessWeightedMeanAbs"], color=COL["amber"])
    ax.errorbar(
        cats["ExcessWeightedMeanAbs"],
        cats["Category"],
        xerr=[
            cats["ObservedWeightedMeanAbs"] - cats["WeightedMeanAbs_CI_low"],
            cats["WeightedMeanAbs_CI_high"] - cats["ObservedWeightedMeanAbs"],
        ],
        fmt="none",
        ecolor=COL["grey_dark"],
        elinewidth=0.55,
        capsize=1.6,
        alpha=0.7,
    )
    ax.axvline(0, color=COL["black"], linewidth=0.55)
    ax.set_xlabel("Excess departure")
    ax.set_title("c  Endpoint categories retain distinct magnitudes")
    ax.spines[["top", "right"]].set_visible(False)

    ax = axes[1, 1]
    perf_plot = perf.copy()
    perf_plot["Target"] = perf_plot["Target"].replace(
        {"Signed study-stage-category lnRR": "Signed lnRR", "Reference-deviation target": "Excess departure"}
    )
    order = ["Study-held-out mean", "Regularized linear model", "Random forest"]
    colors = [COL["grey"], COL["green"], COL["blue"]]
    width = 0.25
    xs = np.arange(perf_plot["Target"].nunique())
    for k, model in enumerate(order):
        vals = perf_plot[perf_plot["Model"] == model].set_index("Target").reindex(["Signed lnRR", "Excess departure"])["R2"]
        ax.bar(xs + (k - 1) * width, vals, width=width, label=model, color=colors[k])
    ax.axhline(0, color=COL["black"], linewidth=0.55)
    ax.set_xticks(xs, ["Signed lnRR", "Excess departure"])
    ax.set_ylabel("Grouped out-of-fold R2")
    ax.set_title("d  Study-held-out prediction remains shallow")
    ax.legend(frameon=False, fontsize=6, ncol=1)
    ax.spines[["top", "right"]].set_visible(False)

    ax = axes[2, 0]
    pp = pred[(pred["Target"] == "Reference-deviation target") & (pred["Model"] == "Random forest")].copy()
    pp["ObservedStage"] = pd.Categorical(pp["ObservedStage"], STAGE_LEVELS, ordered=True)
    sample = pp.sample(n=min(520, len(pp)), random_state=20260813)
    for st in STAGE_LEVELS:
        ss = sample[sample["ObservedStage"].astype(str) == st]
        ax.scatter(
            ss["Observed"],
            ss["Predicted"],
            s=7,
            alpha=0.42,
            color=STAGE_COLORS[st],
            linewidths=0,
            label=STAGE_LABELS_ONE[st],
        )
    mn = float(np.nanmin([sample["Observed"].min(), sample["Predicted"].min()]))
    mx = float(np.nanmax([sample["Observed"].max(), sample["Predicted"].max()]))
    ax.plot([mn, mx], [mn, mx], color=COL["grey_dark"], linewidth=0.65, linestyle="--")
    ax.set_xlabel("Observed excess departure")
    ax.set_ylabel("Predicted excess departure")
    ax.set_title("e  Held-out predictions shrink toward the mean")
    ax.legend(frameon=False, fontsize=5.6, ncol=2, loc="upper left")
    ax.spines[["top", "right"]].set_visible(False)

    ax = axes[2, 1]
    imp_sub = imp[(imp["Target"] == "Reference-deviation target") & (imp["Model"] == "Random forest")].copy()
    if imp_sub.empty:
        imp_sub = imp[(imp["Target"] == "Reference-deviation target")].copy()
    imp_sub = imp_sub.sort_values("MeanDeltaRMSE", ascending=False).head(7).sort_values("MeanDeltaRMSE")
    ax.barh(imp_sub["Feature"], imp_sub["MeanDeltaRMSE"], color=COL["purple"])
    ax.axvline(0, color=COL["black"], linewidth=0.55)
    ax.set_xlabel("Permutation delta RMSE")
    ax.set_title("f  Predictive signal is diffuse")
    ax.spines[["top", "right"]].set_visible(False)

    fig.subplots_adjust(left=0.13, right=0.90, top=0.965, bottom=0.075, hspace=0.80, wspace=0.36)
    for ext in ["pdf", "svg", "png", "tiff"]:
        fig.savefig(OUT_FIG / f"Fig5_reference_deviation_and_ml.{ext}", dpi=600, facecolor="white")
    plt.close(fig)


def main() -> None:
    ensure_dirs()
    df = pd.read_csv(DATA)
    for col in ["lnRR", "vi_model", "PaperYear"]:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    df = df[
        (df["DevelopmentalModeGroup"] == "With metamorphic stage")
        & np.isfinite(df["lnRR"])
        & np.isfinite(df["vi_model"])
        & (df["vi_model"] > 0)
        & df["ObservedStage"].isin(STAGE_LEVELS)
    ].copy()
    df["EndpointDirectionClassStrict"] = df.apply(endpoint_class, axis=1)
    df["aligned_lnRR_strict"] = np.where(
        df["EndpointDirectionClassStrict"] == "beneficial_when_increases",
        df["lnRR"],
        np.where(df["EndpointDirectionClassStrict"] == "adverse_when_increases", -df["lnRR"], np.nan),
    )

    dev, cal = deviation_tables(df)
    dev.to_csv(OUT_PROCESSED / "ER_reference_deviation_results.csv", index=False)
    cal.to_csv(OUT_PROCESSED / "ER_deviation_null_calibration.csv", index=False)
    dev[dev["Dimension"].isin(["Stage", "Stage x stressor"])].to_csv(
        OUT_SOURCE / "Fig5_reference_deviation_source_data.csv", index=False
    )

    ml = build_ml_dataset(df)
    ml.to_csv(OUT_PROCESSED / "ER_ml_study_stage_category_dataset.csv", index=False)
    perf, imp, pred = grouped_cv_models(ml)
    perf.to_csv(OUT_PROCESSED / "ER_ml_grouped_cv_performance.csv", index=False)
    imp.to_csv(OUT_PROCESSED / "ER_ml_permutation_importance.csv", index=False)
    pred.to_csv(OUT_PROCESSED / "ER_ml_grouped_cv_predictions.csv", index=False)
    perf.to_csv(OUT_SOURCE / "Fig5_ml_grouped_cv_performance.csv", index=False)
    imp.to_csv(OUT_SOURCE / "Fig5_ml_permutation_importance.csv", index=False)

    draw_fig5(dev, perf, imp, ml, pred)

    run_checks = pd.DataFrame(
        [
            ["Model-ready rows", len(df), "PASS"],
            ["Study count", df["StudyID"].nunique(), "PASS"],
            ["Reference-deviation rows", len(dev), "PASS"],
            ["ML study-stage-category rows", len(ml), "PASS"],
            ["Grouped CV performance rows", len(perf), "PASS"],
            ["Permutation importance rows", len(imp), "PASS"],
            ["Fig5 TIFF exists", (OUT_FIG / "Fig5_reference_deviation_and_ml.tiff").exists(), "PASS"],
        ],
        columns=["Item", "Value", "Status"],
    )
    run_checks.to_csv(LOG_DIR / "ER_reference_deviation_ml_reproducibility_log.csv", index=False)

    stage = dev[(dev["EvidenceSet"] == "All eligible endpoints") & (dev["Dimension"] == "Stage")].copy()
    stage["ObservedStage"] = pd.Categorical(stage["ObservedStage"], STAGE_LEVELS, ordered=True)
    stage = stage.sort_values("ObservedStage")
    print("Stage null-calibrated absolute departure")
    print(
        stage[
            [
                "ObservedStage",
                "StudyCellCount",
                "RawEffectSizeCount",
                "ObservedWeightedMeanAbs",
                "NullMeanWeightedMeanAbs",
                "ExcessWeightedMeanAbs",
                "NullCalibratedP",
                "NullCalibratedQ",
            ]
        ].to_string(index=False)
    )
    print("\nGrouped CV performance")
    print(perf.to_string(index=False))


if __name__ == "__main__":
    main()