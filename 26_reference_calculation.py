"""Recompute reference-departure summaries from the deduplicated primary subset.
Retains the archived calibration and bootstrap definitions; no prediction models.
"""
import math
from pathlib import Path
import numpy as np
import pandas as pd
ROOT = Path(__file__).resolve().parents[1]
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
    yi = x["yi"].to_numpy(float)
    w = 1.0 / vi
    fixed_mean = float(np.sum(w * yi) / np.sum(w))
    q_stat = float(np.sum(w * (yi - fixed_mean) ** 2))
    c_stat = float(np.sum(w) - np.sum(w**2) / np.sum(w))
    tau2_dl = max((q_stat - (len(x) - 1)) / c_stat, 0.0) if c_stat > 0 else 0.0
    null_vals = []
    heterogeneity_null_vals = []
    null_rms = []
    for _ in range(reps):
        sim = rng.normal(0.0, np.sqrt(vi))
        null_vals.append(float(np.sum(w * np.abs(sim)) / np.sum(w)))
        sim_heterogeneity = rng.normal(0.0, np.sqrt(vi + tau2_dl))
        heterogeneity_null_vals.append(float(np.sum(w * np.abs(sim_heterogeneity)) / np.sum(w)))
        null_rms.append(float(math.sqrt(max(np.mean(sim**2 - vi), 0.0))))
    null_vals = np.asarray(null_vals)
    heterogeneity_null_vals = np.asarray(heterogeneity_null_vals)
    p = (np.sum(null_vals >= obs) + 1.0) / (len(null_vals) + 1.0)
    p_heterogeneity = (np.sum(heterogeneity_null_vals >= obs) + 1.0) / (len(heterogeneity_null_vals) + 1.0)
    return {
        "ObservedWeightedMeanAbs": obs,
        "NullMeanWeightedMeanAbs": float(np.mean(null_vals)),
        "ExcessWeightedMeanAbs": float(obs - np.mean(null_vals)),
        "NullCalibratedP": float(p),
        "Tau2_DL": tau2_dl,
        "HeterogeneityNullMeanWeightedMeanAbs": float(np.mean(heterogeneity_null_vals)),
        "HeterogeneityAdjustedExcess": float(obs - np.mean(heterogeneity_null_vals)),
        "HeterogeneityCalibratedP": float(p_heterogeneity),
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
    if "HeterogeneityCalibratedP" in res:
        valid = res["HeterogeneityCalibratedP"].notna()
        res.loc[valid, "HeterogeneityCalibratedQ"] = bh_adjust(res.loc[valid, "HeterogeneityCalibratedP"].to_numpy())
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



if __name__ == "__main__":
    d = pd.read_csv(ROOT / "data/processed/analysis_ready_model_dataset_deduplicated.csv")
    coding = pd.read_csv(ROOT / "data/processed/ER_endpoint_direction_row_coding.csv")
    assert len(d) == len(coding)
    assert np.array_equal(d.StudyID, coding.StudyID)
    assert np.allclose(d.lnRR, coding.lnRR)
    d["aligned_lnRR_strict"] = coding.aligned_lnRR_strict
    d = d[d.DevelopmentalModeGroup.eq("With metamorphic stage")].copy()
    result, calibration = deviation_tables(d)
    result.to_csv(ROOT / "data/processed/ER_reference_deviation_results.csv", index=False)
    calibration.to_csv(ROOT / "data/source_data/Review_reference_calibration_recomputed.csv", index=False)
    print("Recomputed reference-deviation groups:", len(result), flush=True)

