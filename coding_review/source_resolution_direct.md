# 直接终点标记与专家别名的源码核查

核查日期：2026-09-14。性质：源码、归档数据及已发表评分表的技术审查；不是新增真人独立编码或人工签署裁定。没有改写模型输入、编码或分析结果。

## 关键结论

36 个差异条目中，22 项涉及 DirectOutcome。现有 DirectOutcome 不是“所有生态学直接结果”的完整分类，而是 R/24_review_revision_analysis.R 第 51–53 行定义的特定敏感性子集：在原始与转换后名称的拼接字符串中匹配 survival、viability、mortality、dead、death、hatching success、hatch success、settlement success、metamorph.*success、fertiliz.*success、fecundity、egg production、reproductive success，并同时要求 aligned 有限。

因此两角色按更宽生物学语义给出的 True，不能直接据此认定当前 False 是程序错误。反之，当前 True 也不能证明原始指标到转换后指标的映射具有实验依据。建议将这个字段说明为“预定义狭义直接结果敏感性集资格”，另设生物学终点语义字段；若扩充词典，必须作为另一个有版本号的敏感性分析重跑，不能悄悄改词典后宣称原结果不变。

## 逐项按现有政策的结果

下表建议只针对现有算法是否正确落实；原始测量到代表指标的科学有效性单独列出。若源映射裁定改变 aligned 资格，必须重新计算 DirectOutcome。

|条目|效应数|原始指标 → 代表指标|现有政策应得值|原因及科学语义|
|---|---:|---|---|---|
|END-072|74|Abnormalities → Normal|False|Normal 不在狭义正则中；正常发育可有直接意义，不能据此改写既定子集。|
|END-073|5|Arm assymetry → Normal|False|正则未命中；连续不对称量与正常率是否等价须核实。|
|END-074|12|Arm asymmetry → Normal|False|同上；原始库 StudyID 37、63 有映射记录，记录存在不等于转换公式有效。|
|END-075|5|Asymmetry → Normal|False|同上。|
|END-082|7|Malformation → Normal|False|正则未命中；如原始为异常比例可有补事件关系，须据原始单位确定。|
|END-084|12|Metamorphosis → Metamorphosis|False|既无 success 字样，也无 eligible aligned；专家表 metamorphosis 为 ambiguous。|
|END-086|45|Normal → Normal|False|正常发育不在狭义正则中。|
|END-087|8|Normal Blastulae → Normal|False|同上。|
|END-088|9|Normal development → Normal|False|同上。|
|END-089|7|Normal gastrulae → Normal|False|同上。|
|END-090|5|Normal larvae → Normal|False|同上。|
|END-091|18|Settlement → Settlement|False|正则未命中且专家评分 ambiguous。|
|END-092|5|Settlement success → Settlement|False|原始名称虽命中，aligned 非有限；“生物学上直接”不等于本子集有可解释方向。|
|END-098|12|abnormalities → Normal|False|同 END-072。|
|END-105|1|normal embryonic development → Normal embryos|False|现有正则未命中；43 的 exact-match 白名单也没有 Normal embryos。语义别名缺口应单列，不是擅自扩充已发表评分。|
|END-106|6|settlement → Settlement|False|同 END-091。|
|END-231|81|Fertilization → Fertilization|False|fertiliz.*success 要求 success；已有 positive 专家评分只决定 alignment，不自动决定 DirectOutcome。|
|END-233|19|Fertilized eggs → Fertilization|False|同上。|
|END-236|7|ovigerous female → ovigerous female|False|正则未收录；published positive 不能扩张为所有直接繁殖结果。|
|END-255|10|Density → Survival|True，条件性|Survival 命中且现行 aligned 有限；密度与存活率不天然等同，需原文或明确转换依据。|
|END-256|44|Hatching → developmental success|False|既无 hatching success，又无 developmental success 正则分支；44 个效应涉及 StudyID 9、49、58。|
|END-258|8|Health score → Survival|True，条件性|Survival 命中且现行 aligned 有限；评分不天然等同存活，StudyID 46 原始工作簿有该映射但需原始测量说明。|

## 专家评分和别名的精确依据

R/43_endpoint_source_rules.py 使用 strip/lower/合并空白后的**精确名称匹配**，没有完整语义别名系统。先判断已发表评分，随后才使用显式成功指标白名单。白名单的 normal、developmental success 与专家评分来源不同，应分别标明。

`data/raw/boundary_sources/published_five_expert_scores.csv`（行号包含表头）：

- 第 77 行 fertilization：五位均 1，positive；第 88 行 hatching success：五位均 1，positive；第 140 行 ovigerous female：五位均 1，positive。
- 第 87 行 hardness：五位均 1，positive；第 127 行 microhardness：仅一位 1，其余空缺，ambiguous。不能把原始 Microhardness 名称对应的 ambiguous 说成五位一致；当前 positive 来源是代表名称 Hardness。
- 第 53 行 density：非一致评分，ambiguous；第 120 行 mean normal：仅一位 1，ambiguous；第 125 行 metamorphosis：仅一位 1，ambiguous；第 168 行 settlement：三位 1、两位空缺，ambiguous。
- END-105 的 Normal embryos 没有 exact-match 评分和白名单项。因此其 context_dependent 是源码规则的结果。扩大白名单应标为本次规则修订，有来源与重跑结果。

Alter et al. (2024), doi:10.1038/s41467-024-47064-3，归档全文 `data/raw/boundary_sources/alter_paper_text.txt` 的 Methods 说明：

> “we only assigned a positive or negative direction to a metric if all five co-authors unanimously agreed on that direction.”

原文另说明把“mortality rate and survival rate, morphological normality (%) and abnormality (%)”统一为正向测量。这个依据支持比例补事件的统一，不足以单独证明 density → survival、health score → survival、continuous asymmetry → normal proportion 或 skeletal microdensity → crushing force 的量纲转换。

模型来源记录可追溯到 original_invertebrates.xlsx 的 StudyID 46 / SourceRow 603、605、606、608、609、611、612、613（Health score）；StudyID 23 的 Density 主要为 SourceRow 300–306、316；StudyID 60 的 Microhardness 为 SourceRow 883、889、891–893、895。存在归档来源只是证明继承，并不替代源研究校准证据。

## 可复现流程发现的独立软件问题

R/61_human_endorsed_coding_review.py 并非幂等：

1. README 更新依赖 `t.index('## Independent review and remaining evidence limits')`，第一次改为 `## Human-endorsed rule audit and unresolved decisions` 后再次运行必然找不到旧标题，抛 ValueError。当前 README 第 35 行已经是新标题，故现在重跑会触发。
2. 脚本每次把 `HumanFinalDirection`、`HumanFinalExcludedFromAligned`、`HumanFinalDirectOutcome` 清空，又重建空白 Human_adjudication_required_36_entries.csv；可能覆盖后来补入的真实裁定。
3. 脚本固定写入 unresolved_audit_entries=36、issues_reported=True；即使下游完成源证据判定，重跑仍会回退状态。
4. 将一次性正文迁移和记录生成耦合在一起，前半段文件可能已更新而 README 处失败，造成部分写入。建议拆成可重复生成器、保留式登记和一次性迁移，并在全部输入验证后再输出。

这些是实际可复现/静态可确定的问题；本核查只报告，没有修改文件或覆盖人工信息。
