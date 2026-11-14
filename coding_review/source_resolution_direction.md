# 20 个方向规则差异条目的来源核查

本表为计算辅助的来源核查与修订建议，不是人工独立编码、人工最终裁定或原论文逐篇复核。未修改分析数据。行号包括文件表头；A 指 `data/raw/boundary_sources/Alter2024_DataInverts.csv`；E 指同目录 `published_five_expert_scores.csv`。

政策基准：`R/43_endpoint_source_rules.py:13` 先取 TransformedMetric，`17–23` 按发表的全体专家共识或明确的成功/存活指标赋方向。此代码缺少组合别名解析，也未证明原指标到替代标签的测量等价性。标签更名本身不构成有效转换。建议保持原始方向数据不变，修正有依据的方向编码；无充分依据的代理指标不得因重命名而获得专家共识。

|条目|原指标|来源依据|建议及结论|
|---|---|---|---|
|END-040|肌肉撕裂所需力|A:1923，REF204，单位 N，原库标 positive；E 无此指标共识|现有 context_dependent 可保留。不能仅凭类似 crushing force 推定五专家共识；原库 positive 可作为来源信息，非独立共识。|
|END-043|微硬度|A:948，REF60，VHM，原始/转换后225.44/225.44484；E:127 microhardness ambiguous；E:87 hardness positive|确有证据来源归属错误：不能用 hardness 的全体共识覆盖已单列的 microhardness 模糊分类。建议按原指标 context_dependent。|
|END-045|骨骼微密度|A:418，REF27，g cm³，2.439020→2.396340；E:48 为 crushing force positive|确有语义映射错误。密度不是压碎力，未见标定/计算力公式；现有 consensus 归属不成立，建议 context_dependent，保留原方向效应。|
|END-047|全部足丝的拉断力|A:1315，REF96，N；E:79 force of byssal threads 五人均1|现有排除由精确字符串漏匹配造成，建议加入有记录的指标别名，beneficial_when_increases。不要改为直接存活终点。|
|END-073|腕不对称（拼写 assymetry）|A:1484，REF111，mm；Note 明确 inversed ratio because negative metric；ei=-0.581，而ln(0.0165785/0.009277)为正|原库实际反转比值，不是正常率互补。可保留已定向效果的正向性能解释，但不得说指标已转换为正常胚胎比例或原始 ln(T/C)。须报告为已反向编码的不对称性；其专家共识依据不足，严格共识政策下排除。|
|END-074|腕不对称|A:537，REF37，µm，Note inversed ratio because negative metric；ei=-0.557|同上。原库可证实反向处理，不能证实“正常率”；须修正文中转换类型，不得再反转一次。|
|END-075|不对称性|A:676，REF48，µm，Note inversed ratio because negative metric；ei=-0.493|同上。可证实方向反转，不能将其称为正常发育比例。严格共识政策下仍需原指标方向依据。|
|END-099|钙化幼体|A:1141，REF76，%，改名 Normal 但59.7保持59.7；E:31 calcified larvae ambiguous（1与0）|确有专家分类被替代标签绕过问题。钙化幼体比例不等于所有正常发育，建议 context_dependent；若原文另有明确发育成功定义，须另附依据。|
|END-105|正常胚胎发育|processed/analysis_ready_model_dataset_deduplicated.csv:2381；data/source_data/exposure_source_cache/PANGAEA_964219.tsv:29、35 明确 Normal; Optic microscope|确有漏匹配：Normal embryos 未列入成功终点白名单。建议 beneficial_when_increases，并可为直接发育结局；有正常胚胎百分比原始来源支持。|
|END-108|腕长度|A:541，REF37，µm，均值138.67保持138.67，更名 growth rate；E:85 是速率共识|来源只证明长度，不证明速率；不能继承速率共识。建议 context_dependent，除非取得相同起始长度/时间及增长定义的原文证据。|
|END-113|体况指数|A:1920，REF204；E:43 原文组合词条 condition factor, fulton index, condition index，五人均1|确有组合别名解析漏项，应为 beneficial_when_increases。|
|END-116|卵体积|A:2005，REF240，mm3，0.012857881保持0.012858；REF220亦mm3|不是增长速率，较大体积与性能关系可能含发育/水分因素。现有增长速率共识归属不成立，建议 context_dependent。|
|END-126|幼体大小→增长速率|A:2075，REF232，实际单位 %/d；注意A:217 REF10 是另一条 Larval size→Length、单位mm2，不是该字典条目|此条具时间归一单位，支持 represented growth rate，可保留 beneficial_when_increases。不能把其他REF的静态大小误用于裁定此条。|
|END-136|组织生物量|A:1661，REF190，mg/cm2，7.655170保持7.655170|不是速率；静态生物量不可直接继承增长速率共识。建议 context_dependent，等待原文性能解释或预定义独立生物量规则。|
|END-137|组织生产|A:1078，REF70，mg*g AFDW-1*d-1，确为速率；E:198 tissue production ambiguous|测量为质量生产速率，可说明与生长有关，但原指标在专家表单独列 ambiguous。严格原指标共识政策下应 context_dependent；若采用上位 growth rate 规则需明确这是新规则并作敏感性比较。|
|END-150|体积变化|A:2041，REF218，记录单位mg/cm3，没有时间分母；均值0.572保持0.572|名称与单位也不相符，证据不足，不能当作已验证的增长速率。建议 context_dependent，需原文图/表及单位校正。|
|END-152|体况因子|A:428，REF29，%；E:43 组合词条五人均1|确有组合别名漏项，应为 beneficial_when_increases。|
|END-155|grow efficiency→growth rate|A:1367 REF100 mm day-1；A:1381 REF101 % d-1；E:84 growth efficiency ambiguous，E:85 growth rate positive|源记录的实际单位均为生长速率而非投入产出效率，支持保留 represented growth rate 正向。必须记录单位依据，不能只是别名替换。|
|END-255|幼体密度|A:350，REF23，larvae/ml，原均值与转换值相同；E:53 density ambiguous|没有初始密度/培养体积/计数时间等证明为存活比例；建议 context_dependent。密度可由多种过程决定，不能将 Survival 标签视为已验证转换。|
|END-258|健康评分|A:654，REF46，% of before，90.107527保持90.107527；E:89 Health score ambiguous|健康评分不是存活率，未见原文评分量表及生死阈值。建议 context_dependent，保留来源原指标名称，不得声称直接存活结局。|

## 对主分析的重要影响

1. END-113/152 和 END-105 是清楚可修复的字符串/白名单漏项。END-047 为有单位支持的命名差别。
2. END-045/116/108/136/150/255/258 的替代标签不能支撑所声称的物理量转换。END-043/099/137 还存在原指标已被专家表列为 ambiguous、却因新标签获得 positive 的问题。
3. END-073/074/075 原库明确反转了比值。当前论文若统一把 directional 定义为原指标 ln(T/C)，或把所有 Normal 解释为100减异常比例，均需修正。此问题超出新增人工复核文案，关乎估计目标定义。
4. 以上是数据及政策一致性核查建议，不能生成或代替两位真人的最终裁定记录。任何规则变化后需重算 aligned 结果、配对 bootstrap、BH-FDR、主图及正文数字；旧显著性结果不能预先沿用。

