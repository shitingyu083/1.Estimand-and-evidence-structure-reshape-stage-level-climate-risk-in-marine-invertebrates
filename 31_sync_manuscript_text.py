"""Build manuscript text and a traceable numerical ledger from canonical CSV results."""
from pathlib import Path
import pandas as pd
import numpy as np
ROOT=Path(__file__).resolve().parents[1]
def read(name): return pd.read_csv(ROOT/name)
d=read("data/processed/analysis_ready_model_dataset_deduplicated.csv")
primary=d[d.DevelopmentalModeGroup.eq("With metamorphic stage")]
stages=["EMBRYO","LARVA","SETTLEMENT/METAMORPHOSIS","JUVENILE/ADULT"]
labels=dict(zip(stages,["embryo","larval","settlement/metamorphosis","juvenile/adult"]))
ledger=[]
def record(file,df):
    for i,r in df.iterrows():
        for field,value in r.items(): ledger.append(dict(SourceFile=file,SourceRow=int(i)+2,Field=field,Value=value))
    return df
def src(file): return record(file,read(file))
rve=src("data/processed/ER_RVE_stage_estimates.csv")
omni=src("data/processed/omnibus_stage_tests.csv")
paired=src("data/source_data/Fig4b_estimand_target_differences.csv")
paired=paired[paired.P_value.notna()].copy()
sub=src("data/source_data/Fig3_subgroup_estimand_paired_differences.csv")
est=src("data/source_data/Fig2_subgroup_three_estimands.csv")
matched=src("data/source_data/Fig4a_common_subset_estimand_driver_estimates.csv")
mod=src("data/source_data/Fig3e_moderator_omnibus_tests.csv")
direct=src("data/source_data/Review_direct_outcome_RVE.csv")
cal=src("data/processed/ER_reference_deviation_results.csv")
signed=src("data/source_data/Review_signed_REML_distribution_sensitivity.csv")
trunc=src("data/source_data/Review_RMS_truncation_audit.csv")
counts=src("data/source_data/Review_analysis_population_counts.csv")
pc=src("data/source_data/Review_primary_stage_counts.csv")
bal=src("data/processed/ER_evidence_balance_by_stage.csv")
dev=src("data/processed/ER_development_only_sensitivity.csv")
loo=src("data/source_data/SupplementaryTableS7_leave_one_study_out_influence.csv")
eg=src("data/processed/egger_small_study_checks.csv")
f=lambda x:f"{float(x):.3f}"
def q(x): return "< 0.001" if float(x)<.001 else "= "+f(x)
def result(r): return f"{f(r.Estimate)} (95% CI {f(r.CI_low)} to {f(r.CI_high)}; k = {int(r.EffectSizeCount)}, m = {int(r.StudyCount)}; q {q(r.FDR_q_value)})"
emb=rve[rve.Rho.eq(.8)&rve.Estimand.str.startswith("Raw")&rve.ObservedStage.eq("EMBRYO")].iloc[0]
aligned=rve[rve.Rho.eq(.8)&~rve.Estimand.str.startswith("Raw")]
sig=paired[paired.FDR_q_value.lt(.05)]
fold=sig[sig.TargetDifference.str.startswith("Absolute")]
ns=len(sig); nf=len(fold)
def modtext(m,s):
    r=mod[mod.Moderator.eq(m)&mod.StressorLabel.eq(s)].iloc[0]
    return f"F({int(r.df_num)},{int(r.df_denom)}) = {r.QM:.2f}, q {q(r.FDR_q_value)}"
body=(ROOT/"R/manuscript_revision_template.txt").read_text(encoding="utf-8")
lines=body.splitlines()
def replace(prefix,new):
    ids=[i for i,l in enumerate(lines) if l.startswith(prefix)]
    if len(ids)!=1: raise ValueError((prefix,ids))
    lines[ids[0]]=new
replace("Life-stage comparisons can inform",f"Life-stage comparisons under ocean warming and acidification depend on what experiments measure. We reanalysed a curated database of {len(d):,} log response ratios from {d.StudyID.nunique()} StudyID clusters after removing 32 cross-layer duplicate records. Directional responses varied among taxonomic classes and endpoint pathways. In the primary metamorphic-taxa subset ({len(primary):,} effects; {primary.StudyID.nunique()} clusters), embryos had the only FDR-supported negative stage mean in the primary robust variance model (lnRR = {f(emb.Estimate)}, 95% confidence interval {f(emb.CI_low)} to {f(emb.CI_high)}; q {q(emb.FDR_q_value)}). Endpoint-aligned stage means and corrected stage contrasts were unsupported. On matched stage-by-driver evidence, {ns} of {len(paired)} numerical contrasts between response definitions survived false-discovery-rate correction; {nf} involved absolute transformation. These contrasts identify differences between target quantities, rather than differences in biological harm. Settlement/metamorphosis evidence was insufficient for performance inference: 10 studies contributed 41 effects, but only one supplied five direction-interpretable effects. Absolute departures were sensitive to between-study heterogeneity. Endpoint coding lacked independent review, and sparse exposure metadata limited dose and duration inference. The synthesis identifies how endpoint definitions and evidence structure shape experimental stage comparisons. Recruitment assessment requires experiments that link settlement and subsequent survival under comparable exposures.")
replace("We conducted a secondary", "We conducted a secondary quantitative analysis of a curated effect-size database, supplemented by a targeted search in 2026 for studies published through 2025. The archived input is dated 26 June 2026, with an update trail through 28 July 2026. Before duplicate resolution it contained 2,238 original-layer effects, 75 earlier-expansion effects and 99 current-update effects. The original search and full exclusion history could not be reconstructed. We therefore describe the study as a curated evidence reanalysis rather than a new systematic review (Page et al., 2021).")
replace("The data hierarchy is", "The analysis unit is an endpoint-specific treatment-control log response ratio; StudyID groups linked source records for dependence control. We identified duplicates using normalized StudyID, species, observed stage, driver, metric, transformed metric, endpoint category, lnRR and variance. Thirty-two exact cross-layer pairs involved StudyIDs 3, 10, 13 and 193. We retained the Original curated evidence record and removed its Earlier validated expansion counterpart. Records with different effect values were retained for source review, not collapsed. The audit records candidate groups, retained and removed row identifiers, update layers and decision reasons. RowKey uniqueness alone was not used as evidence of independence. The 162 REF values map to 162 StudyIDs; experiment and shared-sample identities remain incomplete (Supplementary Table S10).")
replace("Full-database descriptions use",f"Full-database descriptions use {len(d):,} effects, {d.StudyID.nunique()} StudyIDs and {d.SpeciesID.nunique()} species or higher-taxon labels. Primary stage and reference-deviation analyses use taxa coded as having a metamorphic stage: {len(primary):,} effects from {primary.StudyID.nunique()} StudyIDs. The full and primary datasets contain 187 and 174 StudyID-by-stage cells, respectively; the primary subset contains 442 StudyID-by-driver cells. We use k for effects, m for StudyID clusters and s for analysis-specific cells. Source-backed metadata recovery supplied temperature contrasts for 34 effects, pH contrasts for 36 effects and durations for 26 effects. These sparse records were not used for dose or duration meta-regression. Shared-control covariance was not imputed; recovered values and their source locations are supplied in Supplementary Table S3.")
replace("The dictionary retains",f"The dictionary retains {int(counts.iloc[2].k):,} effects from {int(counts.iloc[2].m)} StudyIDs in the full database and {int(counts.iloc[3].k):,} effects from {int(counts.iloc[3].m)} StudyIDs in the primary subset. Settlement/metamorphosis retains five effects from one StudyID. Independent duplicate coding was not performed, so no inter-rater agreement or validated alignment is claimed. Supplementary Table S4b provides the metric-level rules, biological rationale, coding confidence and exclusions. Strict, broad and explicit-outcome restrictions evaluate sensitivity to these rules.")
# Remove bibliometric numbers from ecological motivation.
for i,l in enumerate(lines):
    if l.startswith("Recent syntheses"):
        lines[i]=l.split(" An OpenAlex context search")[0]
# Rebuild Results from computed values, not replacements of rounded stale statistics.
res=["3. Results","3.1 Evidence structure and coverage"]
res.append("The full database contains "+", ".join(f"{int(bal.set_index('ObservedStage').loc[s,'EffectSizeCount']):,} {labels[s]}" for s in stages)+" effects (Fig. 1). Primary metamorphic-taxa counts are "+", ".join(str(int(pc.set_index('ObservedStage').loc[s,'k'])) for s in stages)+". Of 149 primary StudyIDs, 127 measured one stage, 19 measured two and three measured three. Stage comparisons therefore mostly combine different experiments.")
res.append("All 41 settlement/metamorphosis effects belong to Development and represent 11 species or higher-taxon labels. Five effects from one StudyID met the direction dictionary. Greater juvenile/adult coverage measures evidence availability, not biological importance. The main topology summarizes 138 OTT tips; complete italicized species labels are supplied in Supplementary Fig. S12.")
res+=["3.2 Ecological heterogeneity in the full database"]
res.append("All-endpoint directional moderator tests describe heterogeneity across the available evidence (Supplementary Fig. S9). Matched-target subgroup estimates instead hold eligible rows constant within each subgroup when comparing definitions (Fig. 2). Both analyses in this section use the full database.")
res.append("Class heterogeneity was supported under acidification ("+modtext("Taxonomic class","Acidification")+") and warming ("+modtext("Taxonomic class","Warming")+"), but not combined exposure ("+modtext("Taxonomic class","Warming + acidification")+"). Phylum tests did not survive correction (minimum q = "+f(mod[mod.Moderator.eq("Phylum")].FDR_q_value.min())+").")
res.append("Endpoint heterogeneity was supported under acidification ("+modtext("Endpoint pathway","Acidification")+"), warming ("+modtext("Endpoint pathway","Warming")+") and combined exposure ("+modtext("Endpoint pathway","Warming + acidification")+"). Driver-specific stage heterogeneity was supported under warming ("+modtext("Life-history stage","Warming")+") and combined exposure ("+modtext("Life-history stage","Warming + acidification")+"), but not acidification ("+modtext("Life-history stage","Acidification")+"). These tests differ in population and dependence model from the primary stage tests below.")
res.append("Benthic-mobile and benthic-sessile classes were the eligible adult guild strata. No guild test survived correction (acidification q "+q(mod[mod.Moderator.eq("Ecological guild")&mod.StressorLabel.eq("Acidification")].iloc[0].FDR_q_value)+"; warming and combined exposure q "+q(mod[mod.Moderator.eq("Ecological guild")&mod.StressorLabel.eq("Warming")].iloc[0].FDR_q_value)+"). Mixed-uncertain classes were excluded, and nektonic-swimming classes had fewer than three StudyIDs per driver.")
res+=["3.2.1 Matched response definitions within subgroups",f"Across five moderators, {int(est.Estimate.notna().sum())} of {len(est)} subgroup-by-driver-by-target combinations met the three-cell threshold. Figure 2 displays classes, endpoint categories, adult guilds and stages; phylum estimates remain in source data. The paired subgroup analysis contained {len(sub)} contrasts, of which {int(sub.FDR_q_value.lt(.05).sum())} survived its separate BH correction (Fig. 4). The matched analysis covers dictionary-eligible endpoints and does not estimate every physiological pathway."]
res+=["3.3 Directional and endpoint-aligned responses in the primary subset",f"The primary RVE embryo estimate was {result(emb)} at rho = 0.8. Other directional stage intervals crossed zero. Full coefficient, standard-error, degrees-of-freedom and interval changes across assumed rho values are supplied in Supplementary Table S13; Figure 5a displays unscaled coefficient differences."]
for _,r in omni.iterrows():
    target="Directional" if str(r.Estimand).startswith("Raw") else "Endpoint-aligned"
    res.append(f"The {target.lower()} omnibus test did not detect stage differences (CR2 HTZ F({int(r['df'])},{r.df_denom:.2f}) = {r.Statistic:.3f}, P {q(r.P_value)}). Pairwise tests used contrast-specific Satterthwaite degrees of freedom and separate BH families (Fig. 5c).")
res.append("Endpoint-aligned estimates were "+ "; ".join(f"{labels[r.ObservedStage]} {result(r)}" for _,r in aligned.iterrows() if np.isfinite(r.Estimate))+". Settlement/metamorphosis was not estimable. Alignment therefore weakened the performance interpretation of the negative embryo directional mean.")
res+=["3.4 Matched evidence and numerical target differences"]
range_parts=[]
for target,x in matched.groupby("PanelLabel"):
    x=x[x.Estimate.notna()]
    range_parts.append(f"{target.lower()} {f(x.Estimate.min())} to {f(x.Estimate.max())}")
res.append("Across nine estimable primary stage-by-driver combinations, matched estimates spanned "+ "; ".join(range_parts)+". All targets used identical eligible cells within each combination (Fig. 3a). Settlement/metamorphosis supplied one eligible cell per driver.")
res.append(f"{ns} of {len(paired)} paired numerical contrasts survived BH correction (Fig. 3b,c): "+", ".join(f"{int(x.FDR_q_value.lt(.05).sum())} {name.lower()}" for name,x in paired.groupby("TargetDifference"))+f". Of the supported contrasts, {nf} involved absolute folding.")
for _,r in sig[~sig.TargetDifference.str.startswith("Absolute")].iterrows():
    res.append(f"The supported aligned-minus-directional contrast was for {labels[r.ObservedStage]} endpoints under {r.StressorLabel.lower().replace('warming + acidification','combined exposure')} (difference {f(r.Estimate)}, 95% bootstrap CI {f(r.CI_low)} to {f(r.CI_high)}; s = {int(r.StudyCellCount)}; q {q(r.FDR_q_value)}). Supplementary Table S18 reports the paired differences; folded absolute contrasts are descriptive.")
res.append("Magnitude orderings changed with endpoint restriction and transformation order (Supplementary Fig. S10). Taking absolute values before aggregation prevents cancellation of opposite endpoint responses; folding the signed aggregate permits that cancellation. These summaries answer different numerical questions.")
res+=["3.5 Sensitivity analyses"]
de=dev[dev.ObservedStage.eq("EMBRYO")].iloc[0]
le=loo[loo.ObservedStage.eq("EMBRYO")&loo.Estimand.str.startswith("Directional")].iloc[0]
res.append(f"Development-only embryo estimates remained negative ({f(de.Estimate)}, 95% CI {f(de.CI_low)} to {f(de.CI_high)}; k = {int(de.EffectSizeCount)}, m = {int(de.StudyCount)}; Fig. 5b). Leave-one-StudyID-out embryo REML means ranged from {f(le.MinLeaveOneOut)} to {f(le.MaxLeaveOneOut)}. Alternative dependence-cluster results are reported on their common family-complete subset in Supplementary Table S7.")
res.append("Explicit-outcome alignment yielded "+ "; ".join(f"{labels[r.ObservedStage]} {f(r['b.r'])} (95% CI {f(r['CI.L'])} to {f(r['CI.U'])}; k = {int(r.k)}, m = {int(r.m)}; q {q(r.FDR_q_value)})" for _,r in direct.iterrows())+". This restriction tests coding sensitivity; it is not independent human review.")
res.append("Signed StudyID-level Egger diagnostics and precision plots are provided in Supplementary Figs. S2 and S4. Small-study asymmetry can arise from endpoint composition, heterogeneity or selection; the diagnostic does not distinguish these mechanisms.")
res+=["3.6 Reference departure and signed-distribution sensitivity"]
ca=cal[cal.EvidenceSet.eq("All eligible endpoints")&cal.Dimension.eq("Stage")].set_index("ObservedStage")
res.append("Weighted absolute departures by stage were "+", ".join(f"{f(ca.loc[s,'ObservedWeightedMeanAbs'])} ({labels[s]})" for s in stages)+". Heterogeneity-adjusted indices were "+", ".join(f(ca.loc[s,"HeterogeneityAdjustedExcess"]) for s in stages)+" in the same order (Fig. 6; Supplementary Table S5). These indices compare numerical departure with a fitted zero-centred benchmark.")
se=signed[signed.EvidenceSet.eq("All eligible endpoints")&signed.Rho.eq(0)&signed.ObservedStage.eq("EMBRYO")].iloc[0]
ss=signed[signed.EvidenceSet.eq("All eligible endpoints")&signed.Rho.eq(0)&signed.ObservedStage.eq("SETTLEMENT/METAMORPHOSIS")].iloc[0]
res.append(f"The signed REML embryo estimate was {f(se.Estimate)} (95% Hartung-Knapp CI {f(se.CI_low)} to {f(se.CI_high)}; q {q(se.FDR_q_value)}). For settlement/metamorphosis, the model-implied latent absolute mean was {f(ss.ImpliedLatentAbsoluteMean)}, compared with {f(ss.CenteredHeterogeneityAbsoluteMean)} under its centred heterogeneity distribution. Most implied magnitude therefore reflected dispersion rather than a common mean shift (Supplementary Fig. S11a).")
res.append(f"Of {len(trunc)} eligible RMS specifications, {int(trunc.PointEstimateTruncated.sum())} point estimates were truncated to zero. Bootstrap truncation nevertheless reached {100*trunc.BootstrapTruncationFraction.max():.1f}%, exposing boundary sensitivity not visible from the point estimates alone (Supplementary Fig. S11b; Supplementary Table S12).")
start=lines.index("3. Results"); end=lines.index("4. Discussion")
lines[start:end]=res
body="\n".join(lines)
body=body.replace("evidence composition","evidence structure").replace("clearest downward directional shift","only FDR-supported negative directional stage mean")
body=body.replace("Twelve of the 13 supported primary contrasts",f"{nf} of the {ns} supported primary contrasts")
body=body.replace("we favour specifying biologically justified endpoint directions before synthesis. That preference depends on endpoint-specific validation; it is not evidence that the present dictionary measures harm more accurately.","endpoint-aligned summaries are preferable when the direction rule is independently justified for the measured outcome. The present dictionary has not undergone that validation.")
body=body.replace("A research priority is to measure","Future experiments should measure").replace("This is a proposed design improvement, not a monitoring benefit validated by the present data.","")
body=body.replace("The practical contribution is an endpoint-specific interpretation framework. ","")
body=body.replace("The current exposure fields are too incomplete","The recovered exposure fields are too sparse")
body=body.replace("Its 216 estimable contrasts",f"Its {len(sub)} estimable contrasts")
body=body.replace("Calibrated descriptive indices","Calibrated indices").replace("calibrated descriptive index","calibrated index")
# Explicit legend updates include all actual panels.
ls=body.splitlines()
for i,l in enumerate(ls):
    if l.startswith("Figure 1."):
        ls[i]=l.replace("species names use italic Arial.","the main tree omits crowded tip labels. Supplementary Fig. S12 supplies all species names in italic Arial. The topology is for taxonomy visualization only; no phylogenetic covariance model was fitted.")
    if l.startswith("Figure 2."):
        ls[i]=l.replace("Matched response definitions within ecological subgroups.","Full-database matched response definitions within ecological subgroups.")
    if l.startswith("Figure 3."):
        ls[i]=f"Figure 3. Direct numerical comparison of response definitions in the primary metamorphic-taxa subset. (a) Separate target rows show REML means, 95% confidence intervals and thin grey prediction intervals from matched stage-by-driver cells. (b) Equal-cell-weight paired differences with 95% percentile intervals from 9,999 paired bootstrap resamples. Stars indicate BH q < 0.05 (*), 0.01 (**) and 0.001 (***) across {len(paired)} estimable contrasts; {ns} survive correction and {nf} involve absolute folding. Settlement/metamorphosis has only one eligible cell per driver and is not estimated. (c) The same paired tests shown on a -log10(q) scale. The dashed line is -log10(0.05) = 1.301; filled points denote q < 0.05, open points denote non-significant tests, and ns denotes q >= 0.05. Horizontal guide lines in panel c are not confidence intervals. Panels b and c use the same 27-test family, not independent evidence. Source data provide every effect count, cell count, interval, P value and q value."
    if l.startswith("Figure 4."):
        ls[i]=f"Figure 4. Full-database paired target differences within ecological subgroups. Panels show (a) class, (b) endpoint category, (c) adult guild and (d) life stage. Driver colours and symbols match Figure 2. Points show equal-cell-weight mean differences; bars are 95% paired bootstrap intervals. Columns compare aligned minus directional, absolute minus directional, and absolute minus aligned. Stars denote BH q < 0.05 (*), 0.01 (**) and 0.001 (***) within one family of {len(sub)} estimable subgroup-by-driver contrasts; {int(sub.FDR_q_value.lt(.05).sum())} survive correction. Phylum contrasts are retained in source data. Figure 3 uses a different population and a separate 27-test family. A significant numerical target difference is not a test of biological harm."
    if l.startswith("Figure 5."): ls[i]=l.replace("multiplied by one million","shown in the original lnRR units with scientific notation").replace("the figure is descriptive, whereas","whereas")
    if l.startswith("Figure 6."): ls[i]=l.replace("(b-d) Sampling-error-offset","(b) Diverging colours are centred at zero; grey NE cells have fewer than three cells and are not assigned a numerical colour. Labels show k effects and s cells, including sparse settlement strata. (b-d) Sampling-error-offset")
body="\n".join(ls)+"\n"
body=body.replace("cells; whereas", "cells, whereas")
body=body.replace("Unknown shared-control covariance and exposure differences persist after clustering and aggregation,", "Unknown shared-control covariance and exposure differences persist after clustering and aggregation. For StudyID 244, public source data identify tank and genet structure that the archived nubbin-level extraction does not resolve; its 24 effects require experiment-level re-extraction. These limitations remain")
body=body.replace("12 of the 13 supported primary contrasts", "Twelve of the 13 supported primary contrasts")
body=body.replace("the main tree omits crowded tip labels. Supplementary Fig. S12 supplies all species names in italic Arial.","All 138 matched tip names appear in italic Arial across three consecutive vertical ranges of the same topology, read left to right from upper to lower tips. These are display segments, not three independent trees. Dotted terminal guides connect tips to names. Supplementary Fig. S12 supplies the uninterrupted tree.")
body=body.replace("(c) The same paired tests shown on a -log10(q) scale. The dashed line is -log10(0.05) = 1.301; filled points denote q < 0.05, open points denote non-significant tests, and ns denotes q >= 0.05. Horizontal guide lines in panel c are not confidence intervals.","(c) A direct significance matrix of the same 27 paired tests. Each cell reports the mean numerical difference (Δ), BH-adjusted q value and significance symbol; teal cells indicate q < 0.05 and pale cells indicate non-significant comparisons (ns). Columns identify the subtracted targets and report the number of supported comparisons. The matrix contains 1/9 aligned-minus-directional, 7/9 absolute-minus-directional and 5/9 absolute-minus-aligned significant contrasts.")
body=body.replace("Figure 3c locates every q value relative to the BH significance threshold", "Supplementary Table S18 reports paired differences and adjusted q values where inferential tests are defined")
body=body.replace("Diverging colours are centred at zero", "A blue–white–red scale spans −1 to +1 and is centred at zero; positive indices are red and negative indices blue")
body=body.replace("(b-d) Sampling-error-offset indices", "Panel c uses teal points and intervals for endpoint summaries. (b-d) Sampling-error-offset indices")
(ROOT/"manuscript_revision_body.txt").write_text(body,encoding="utf-8")
pd.DataFrame(ledger).to_csv(ROOT/"data/source_data/Review_manuscript_facing_results.csv",index=False)
print("Manuscript body generated; numerical ledger rows:",len(ledger))

