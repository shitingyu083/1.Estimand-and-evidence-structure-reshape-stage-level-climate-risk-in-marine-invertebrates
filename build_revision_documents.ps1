param([string]$ArchivePath=$PSScriptRoot, [string]$PackagePath=(Join-Path $PSScriptRoot 'documents_rebuilt'))
$ErrorActionPreference='Stop'
Add-Type -AssemblyName System.IO.Compression.FileSystem
Add-Type -AssemblyName System.Drawing
$pkg=$PackagePath
$arc=$ArchivePath
$summary=Get-Content -LiteralPath (Join-Path $arc 'data/source_data/Boundary_manuscript_summary.json') -Raw | ConvertFrom-Json
New-Item -ItemType Directory -Path $pkg -Force | Out-Null
$work=Join-Path (Join-Path $arc 'analysis_logs') ('document_build_'+(Get-Date -Format yyyyMMdd_HHmmss))
New-Item -ItemType Directory -Path $work | Out-Null
$wn='http://schemas.openxmlformats.org/wordprocessingml/2006/main'
$oldTitle='Interpreting stage-level climate responses in marine invertebrate syntheses: the roles of effect definitions and evidence composition'
$newTitle='Interpreting life-stage responses to ocean warming and acidification through direction, performance and magnitude'
function OpenDoc($name){
  $dir=Join-Path $work ([IO.Path]::GetFileNameWithoutExtension($name))
  $original=Join-Path (Join-Path $arc 'data/raw/document_templates') $name
  [IO.Compression.ZipFile]::ExtractToDirectory($original,$dir)
  $stylesPath=Join-Path $dir 'word/styles.xml'
  $styles=[xml][IO.File]::ReadAllText($stylesPath)
  $sns=[Xml.XmlNamespaceManager]::new($styles.NameTable);$sns.AddNamespace('w',$wn)
  $base=$styles.SelectSingleNode('//w:style[@w:type="paragraph" and @w:default="1"]',$sns).GetAttribute('styleId',$wn)
  $font=if($name -eq 'ER_manuscript.docx'){'Times New Roman'}else{'Arial'}
  $headingSize=if($name -eq 'ER_manuscript.docx'){'24'}else{'20'}
  foreach($level in 1..3){
    $st=$styles.CreateElement('w','style',$wn);[void]$st.SetAttribute('type',$wn,'paragraph');[void]$st.SetAttribute('styleId',$wn,"ERHeading$level")
    $st.InnerXml="<w:name xmlns:w='$wn' w:val='ER Heading $level'/><w:basedOn xmlns:w='$wn' w:val='$base'/><w:next xmlns:w='$wn' w:val='$base'/><w:qFormat xmlns:w='$wn'/><w:pPr xmlns:w='$wn'><w:keepNext/><w:keepLines/><w:spacing w:before='120' w:after='60'/><w:outlineLvl w:val='$($level-1)'/></w:pPr><w:rPr xmlns:w='$wn'><w:rFonts w:ascii='$font' w:hAnsi='$font'/><w:b/><w:color w:val='000000'/><w:sz w:val='$headingSize'/></w:rPr>"
    [void]$styles.DocumentElement.AppendChild($st)
  }
  $styles.Save($stylesPath)
  $xml=[xml][IO.File]::ReadAllText((Join-Path $dir 'word/document.xml'))
  $ns=[Xml.XmlNamespaceManager]::new($xml.NameTable);$ns.AddNamespace('w',$wn)
  return @{Dir=$dir;Xml=$xml;Ns=$ns;Name=$name;Body=$xml.SelectSingleNode('//w:body',$ns)}
}
function TextOf($node,$h){ return (@($node.SelectNodes('.//w:t',$h.Ns)) | ForEach-Object InnerText) -join '' }
function Para($h,[string]$text,[bool]$heading=$false,[bool]$page=$false){
  $p=$h.Xml.CreateElement('w','p',$wn)
  $pr=$h.Xml.CreateElement('w','pPr',$wn)
  if($heading){$level=if($text -match '^[1-5]\.\d+\.\d+ '){3}elseif($text -match '^[1-5]\.\d+ '){2}else{1};$s=$h.Xml.CreateElement('w','pStyle',$wn);[void]$s.SetAttribute('val',$wn,"ERHeading$level");[void]$pr.AppendChild($s)}
  if($page){[void]$pr.AppendChild($h.Xml.CreateElement('w','pageBreakBefore',$wn))}
  [void]$p.AppendChild($pr)
  $r=$h.Xml.CreateElement('w','r',$wn);$t=$h.Xml.CreateElement('w','t',$wn);$t.InnerText=$text
  $sa=$h.Xml.CreateAttribute('xml','space','http://www.w3.org/XML/1998/namespace');$sa.Value='preserve';[void]$t.Attributes.Append($sa)
  [void]$r.AppendChild($t);[void]$p.AppendChild($r);return ,$p
}
function SetPara($node,$h,[string]$text){
  foreach($c in @($node.ChildNodes)){if($c.LocalName -ne 'pPr'){[void]$node.RemoveChild($c)}}
  $p=Para $h $text;foreach($c in @($p.ChildNodes)){if($c.LocalName -ne 'pPr'){[void]$node.AppendChild($c)}}
}
function AddP($h,[string]$text,[bool]$heading=$false,[bool]$page=$false){
  $p=Para $h $text $heading $page;$sect=$h.Body.SelectSingleNode('w:sectPr',$h.Ns)
  if($sect){[void]$h.Body.InsertBefore($p,$sect)}else{[void]$h.Body.AppendChild($p)}
}
function AddTable($h,$rows,$tableWidth=9200){
  $tbl=$h.Xml.CreateElement('w','tbl',$wn);$cols=$rows[0].Count;$cw=[int]($tableWidth/$cols)
  $pr=$h.Xml.CreateElement('w','tblPr',$wn)
  $pr.InnerXml='<w:tblW xmlns:w="'+$wn+'" w:w="9200" w:type="dxa"/><w:tblBorders xmlns:w="'+$wn+'"><w:top w:val="single" w:sz="4" w:color="888888"/><w:bottom w:val="single" w:sz="4" w:color="888888"/><w:insideH w:val="single" w:sz="2" w:color="DDDDDD"/></w:tblBorders><w:tblLayout xmlns:w="'+$wn+'" w:type="fixed"/>'
  $pr.SelectSingleNode('w:tblW',$h.Ns).SetAttribute('w',$wn,"$tableWidth") | Out-Null
  [void]$tbl.AppendChild($pr);$grid=$h.Xml.CreateElement('w','tblGrid',$wn)
  for($i=0;$i -lt $cols;$i++){$gc=$h.Xml.CreateElement('w','gridCol',$wn);[void]$gc.SetAttribute('w',$wn,"$cw");[void]$grid.AppendChild($gc)}
  [void]$tbl.AppendChild($grid);$ri=0
  foreach($row in $rows){
    $tr=$h.Xml.CreateElement('w','tr',$wn)
    if($ri -eq 0){$trp=$h.Xml.CreateElement('w','trPr',$wn);[void]$trp.AppendChild($h.Xml.CreateElement('w','tblHeader',$wn));[void]$tr.AppendChild($trp)}
    foreach($cell in $row){$tc=$h.Xml.CreateElement('w','tc',$wn);$tcp=$h.Xml.CreateElement('w','tcPr',$wn);$ww=$h.Xml.CreateElement('w','tcW',$wn);[void]$ww.SetAttribute('w',$wn,"$cw");[void]$ww.SetAttribute('type',$wn,'dxa');[void]$tcp.AppendChild($ww);[void]$tc.AppendChild($tcp)
      $p=Para $h ([string]$cell);$r=$p.SelectSingleNode('w:r',$h.Ns);$rp=$h.Xml.CreateElement('w','rPr',$wn);$sz=$h.Xml.CreateElement('w','sz',$wn);[void]$sz.SetAttribute('val',$wn,'16');[void]$rp.AppendChild($sz)
      if($ri -eq 0){[void]$rp.AppendChild($h.Xml.CreateElement('w','b',$wn))};[void]$r.PrependChild($rp);[void]$tc.AppendChild($p);[void]$tr.AppendChild($tc)}
    [void]$tbl.AppendChild($tr);$ri++
  }
  $sect=$h.Body.SelectSingleNode('w:sectPr',$h.Ns);[void]$h.Body.InsertBefore($tbl,$sect)
}
function AddCsv($h,$path,$columns){
  if($path -match 'TableS1[89]_'){
    $labels=@{ObservedStage='Life stage';StressorLabel='Exposure';TargetDifference='Paired contrast';StudyCellCount='Study cells (s)';Estimate='Mean difference';CI_low='95% CI lower';CI_high='95% CI upper';Statistic='Test statistic';P_value='Bootstrap P';FDR_q_value='BH q';Significance='Significance'}
    $rows=[Collections.Generic.List[object]]::new();$rows.Add(@($columns | ForEach-Object {$labels[$_]}))
    foreach($row in (Import-Csv -LiteralPath (Join-Path $arc $path))){
      $v=@();foreach($col in $columns){
        $value=[string]$row.$col
        if($col -eq 'ObservedStage'){$value=$value.Replace('JUVENILE/ADULT','Juvenile/adult').Replace('EMBRYO','Embryo').Replace('LARVA','Larva')}
        if($col -eq 'StressorLabel'){$value=$value.Replace('Warming + acidification','Combined')}
        if($col -in @('Estimate','CI_low','CI_high','Statistic','P_value','FDR_q_value')){if($value){$value=([double]$value).ToString('0.0000',[Globalization.CultureInfo]::InvariantCulture)}else{$value=''}}
        if($col -eq 'Significance' -and !$value){$value='ns'}
        $v+=$value
      };$rows.Add($v)
    };AddTable $h $rows 15200;return
  }
  $rows=[Collections.Generic.List[object]]::new();$rows.Add([string[]]$columns)
  foreach($row in (Import-Csv -LiteralPath (Join-Path $arc $path))){$v=@();foreach($col in $columns){$value=[string]$row.$col; $number=0.0;if($value.Length -gt 9 -and [double]::TryParse($value,[ref]$number)){$value=$number.ToString('0.####')};$v+=$value};$rows.Add($v)}
  AddTable $h $rows
}
function SaveDoc($h){
  $h.Xml.Save((Join-Path $h.Dir 'word/document.xml'))
  $new=Join-Path $work $h.Name
  [IO.Compression.ZipFile]::CreateFromDirectory($h.Dir,$new)
  Copy-Item -LiteralPath $new -Destination (Join-Path $pkg $h.Name) -Force
  $text=(@($h.Xml.SelectNodes('//w:p',$h.Ns)) | ForEach-Object {TextOf $_ $h}) -join "`n"
  [IO.File]::WriteAllText((Join-Path $pkg ($h.Name+'.txt')),$text,[Text.UTF8Encoding]::new($false))
}
$main=OpenDoc 'ER_manuscript.docx'
$pars=@($main.Body.SelectNodes('w:p',$main.Ns));$start=$null;$end=$null
foreach($p in $pars){$t=TextOf $p $main;if($t -eq 'Abstract'){$start=$p};if($t -eq 'Data availability'){$end=$p;break}}
if(!$start -or !$end){throw 'Main document replacement boundaries missing'}
$node=$start
while($node -ne $end){$next=$node.NextSibling;[void]$main.Body.RemoveChild($node);$node=$next}
$newLines=Get-Content -LiteralPath (Join-Path $arc 'manuscript_revision_body.txt') -Encoding utf8
foreach($line in $newLines){if(!$line.Trim()){continue};$heading=$line -match '^([1-5](\.\d+)*\.? |Abstract$|Keywords$|Figure legends$)';[void]$main.Body.InsertBefore((Para $main $line $heading),$end)}
foreach($p in @($main.Body.SelectNodes('w:p',$main.Ns))){
  $t=TextOf $p $main
  if($t.Contains($oldTitle)){SetPara $p $main ($t.Replace($oldTitle,$newTitle))}
  if($t.StartsWith('The analysis-ready effect-size data')){SetPara $p $main 'The analysis-ready data, coding rules, source tables and analysis scripts for this revision are supplied in the accompanying ER_data_and_code_archive_taxonomy_meta_final_review_20260918.zip. The existing repository is https://github.com/shitingyu083/1.Estimand-and-evidence-structure-reshape-stage-level-climate-risk-in-marine-invertebrates; the submission archive defines this revision and may be newer than the repository.'}
}
foreach($p in @($main.Body.SelectNodes('w:p',$main.Ns))){
  $t=TextOf $p $main
  if($t -match '^(Bridge, R\.,|Khalil, M\.,|Bacus, S\.C\.,|Borburema, C\.,|Buchheister, A\.,|Poertner, H\.-O\., Farrell,|Somero, G\.N\.,|Wang, J\.,|Widdicombe, S\.,)'){[void]$main.Body.RemoveChild($p)}
}
SaveDoc $main

# Rebuild supplementary body and images from canonical files, excluding legacy ML.
$supp=OpenDoc 'ER_supplementary_material.docx'
$oldTables=@($supp.Body.SelectNodes('w:tbl',$supp.Ns) | ForEach-Object {$_.CloneNode($true)})
$sect=$supp.Body.SelectSingleNode('w:sectPr',$supp.Ns).CloneNode($true)
$supp.Body.RemoveAll();[void]$supp.Body.AppendChild($sect)
$relsPath=Join-Path $supp.Dir 'word/_rels/document.xml.rels';$rels=[xml][IO.File]::ReadAllText($relsPath)
foreach($r in @($rels.DocumentElement.ChildNodes)){if($r.Type -like '*/image'){[void]$rels.DocumentElement.RemoveChild($r)}}
$media=Join-Path $supp.Dir 'word/media'
if(Test-Path -LiteralPath $media){Get-ChildItem -LiteralPath $media -File | Remove-Item}
New-Item -ItemType Directory -Path $media -Force | Out-Null
AddP $supp ('Supplementary material: '+$newTitle) $true
AddP $supp 'Figures S1-S13; Tables S1-S19. Complete metric-level decision tables and all statistical outputs are supplied as machine-readable CSV files in the accompanying source archive. Figures S12 and S13 are also supplied as standalone PDFs. Table S17 records endpoint-coding verification.'
$bases=@('SupplementaryFigS1_evidence_update_flow','SupplementaryFigS11_directional_funnel_plots','SupplementaryFigS13_absolute_funnel_plots','SupplementaryFigS12_small_study_and_heterogeneity','SupplementaryFigS4_reference_deviation_calibration','SupplementaryFigS10_sensitivity_and_additivity','SupplementaryFigS8_species_topology_class_responses','SupplementaryFigS9_ecological_guild_stressor_meta','SupplementaryFigS7_ecological_moderator_meta','SupplementaryFigS3_magnitude_definition_sensitivity','SupplementaryFigS5_signed_model_and_truncation')
$captions=@(
'Targeted update flow. The retained search trail is distinguished from the inherited curated dataset; historical search completeness is unknown.',
'Directional precision plots by stage. Points represent StudyID-by-stage aggregates. Egger-type results are small-study diagnostics and can reflect heterogeneity or endpoint composition.',
'Exploratory absolute-response precision plots. Folded responses are displayed descriptively and are not tested against zero.',
'Small-study and heterogeneity diagnostics. Panels show Egger-type statistics, heterogeneity summaries, pooled means and evidence support. Forest labels give the estimate, studies (s) and effects (k); intervals are 95% intervals. Folded-target diagnostics are descriptive; signed diagnostics require at least 10 StudyIDs.',
'Three response questions and reference-deviation calibration, reproduced from main Figure 6. Panels a-d show absolute-response histograms, magnitude-performance concordance by quartile, endpoint calibration bars and directional-sign proportions. Panel e summarizes directional RVE, aligned RVE and absolute aggregate results on their respective evidence sets, without implying a matched comparison. Full interpretation and encodings follow the main Figure 6 legend; numerical results are in the accompanying Excel file and Tables S5, S11 and S13.',
'Balanced and coding sensitivities. Panels a and b show equal-cell-weight stage summaries and matched signed combined-minus-single-stressor sums; panel c compares direction dictionaries. Small dots in a are observed study-stage aggregates. Black labels give estimates and studies (s), balanced sample sizes or matched-cell counts. The balanced interval is a subsampling interval, not a conventional model confidence interval; when all available settlement cells are retained it degenerates to a point. Panel c reports unadjusted P values without BH stars. Missing covariance prevents a formal interaction test.',
'Species topology and class-level directional responses. (a) Compact OpenTree topology of the 138 species, coloured by phylum with species counts; unit branches show topology only, and species names are given in Supplementary Figure S12. (b) Class-by-driver directional lnRR at within-cell correlation rho = 0.8; * BH q < 0.05; NE, fewer than three independent studies. (c) Corrected inference status of the same class-by-driver estimates at rho = 0, 0.5 and 0.8. Estimates, intervals and q values are reported with source data.',
'Adult-guild directional responses and corrected inference status. (a) Guild-by-driver directional lnRR at rho = 0.8; * BH q < 0.05; NE, fewer than three independent studies. (b) Corrected inference status at rho = 0, 0.5 and 0.8. These operational class-based strata have incomplete trait validation. Mixed-uncertain groups are shown descriptively but excluded from formal guild comparisons.',
'Full-database all-endpoint ecological moderator analysis. Panels show phylum, endpoint and stage means and 15 driver-by-moderator omnibus tests. Tile colours give directional lnRR (a-c) or -log10 BH q (d); stars in d use BH correction across the 15-test family (ns, not significant; NE, not estimable). Figure 2 instead uses matched dictionary-eligible rows.',
'Magnitude-definition sensitivity. Four summaries have different mathematical targets: mean endpoint-level absolute response, absolute signed aggregate, median absolute aggregate and noise-adjusted RMS. Black labels give estimates, contributing study cells (s) and effects (k). Intervals describe each target, not a common harm null; no stars or artificial observation clouds are added.',
'Signed-distribution and truncation sensitivity. (a) Signed REML stage means with 95% Hartung-Knapp intervals at within-cell correlations 0, 0.5 and 0.8. Small dots repeat the same observed study-stage cells across correlation settings and are not additional evidence. Labels give estimates, studies (s) and effects (k); stars use archived BH q values. (b) Fractions of 4,000 bootstrap resamples whose noise-adjusted squared moment was truncated to zero. All panels use metamorphic taxa; source tables provide other subsets.'
)
for($i=0;$i -lt $bases.Count;$i++){
  $n=$i+1;AddP $supp "Supplementary Figure S$n" $true $true
  $folder='extended';$src=Join-Path $arc ("figures/$folder/"+$bases[$i]+'.png')
  if(!(Test-Path -LiteralPath $src)){throw "Missing figure $src"}
  $name="review_s$n.png";Copy-Item -LiteralPath $src -Destination (Join-Path $media $name)
  $im=[Drawing.Image]::FromFile($src);$scale=[Math]::Min(5600000/$im.Width,6600000/$im.Height);$cx=[long]($im.Width*$scale);$cy=[long]($im.Height*$scale);$im.Dispose()
  $rid="rIdReview$n";$rel=$rels.CreateElement('Relationship',$rels.DocumentElement.NamespaceURI);[void]$rel.SetAttribute('Id',$rid);[void]$rel.SetAttribute('Type','http://schemas.openxmlformats.org/officeDocument/2006/relationships/image');[void]$rel.SetAttribute('Target',"media/$name");[void]$rels.DocumentElement.AppendChild($rel)
  $p=$supp.Xml.CreateElement('w','p',$wn)
  $p.InnerXml="<w:r xmlns:w='$wn'><w:drawing><wp:inline xmlns:wp='http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing'><wp:extent cx='$cx' cy='$cy'/><wp:docPr id='$n' name='Supplementary Figure S$n' descr='Supplementary Figure S$n. See the accompanying panel caption for estimands and uncertainty definitions.'/><a:graphic xmlns:a='http://schemas.openxmlformats.org/drawingml/2006/main'><a:graphicData uri='http://schemas.openxmlformats.org/drawingml/2006/picture'><pic:pic xmlns:pic='http://schemas.openxmlformats.org/drawingml/2006/picture'><pic:nvPicPr><pic:cNvPr id='$n' name='$name'/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships' r:embed='$rid'/><a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x='0' y='0'/><a:ext cx='$cx' cy='$cy'/></a:xfrm><a:prstGeom prst='rect'><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>"
  [void]$supp.Body.InsertBefore($p,$sect);AddP $supp ("Supplementary Figure S$n. "+$captions[$i])
}
AddP $supp 'Supplementary Figure S12. Complete species topology' $true $true
AddP $supp 'The complete 138-tip tree is supplied as SupplementaryFigS2_complete_species_topology.pdf at its full readable size. All species labels are in italic Arial. Unit branches encode topology, not divergence time; the tree is not a fitted covariance model. Figure 1 presents this topology alongside the bibliometric and evidence panels.'
$rels.Save($relsPath)
$ctPath=Join-Path $supp.Dir '[Content_Types].xml';$ct=[xml][IO.File]::ReadAllText($ctPath)
if(!(@($ct.DocumentElement.ChildNodes) | Where-Object Extension -eq 'png')){$el=$ct.CreateElement('Default',$ct.DocumentElement.NamespaceURI);[void]$el.SetAttribute('Extension','png');[void]$el.SetAttribute('ContentType','image/png');[void]$ct.DocumentElement.AppendChild($el);$ct.Save($ctPath)}

AddP $supp 'Supplementary Table S1. Targeted update and duplicate resolution' $true $true
AddP $supp 'The inherited database contained 2,313 effects; the targeted update added 99 effects from four StudyIDs after screening 21 candidates. Cross-layer review removed 32 exact duplicate effects. The original input is preserved and is not the downstream analysis dataset.'
AddCsv $supp 'data/processed/duplicate_resolution_summary.csv' @('Scope','InputRows','RetainedRows','DeletedRows','StudyIDs')
AddP $supp 'Targeted-update candidate decisions are retained in data/processed/screened_candidate_records.csv; they are not the complete inherited search history. The subsequent boundary audit restored 11 dose-distinct records, giving 2,391 effects and 2,214 primary effects. Boundary_source_pointer_repair.csv records source corrections and dose restoration; the duplicate-resolution table above describes the preceding cross-layer step only.'
AddP $supp 'Supplementary Table S2. Endpoint coding rules' $true $true
AddTable $supp @(@('Rule','Sign','Interpretation'),@('Represented endpoint: positive consensus or explicit success','Retain sign','Use TransformedMetric; already converted survival and normality are not flipped again'),@('Represented endpoint: negative consensus','Reverse sign','Published five-expert negative direction, including shell damage'),@('Ambiguous or unsupported mapping','Exclude from alignment','Retain in all-endpoint directional analysis; do not assign a fitness interpretation'))
AddP $supp 'Supplementary Table S3. Recovered exposure information' $true $true
AddCsv $supp 'data/source_data/Review_exposure_recovery_summary.csv' @('Field','RecoveredEffects','StudyIDs','MissingEffects')
AddP $supp 'Recovery uses the original workbook, Alter et al. (2024) data, the Williams public chemistry table, PANGAEA and source full texts. Acclimation days are separate from exposure duration. StudyID 244 reports approximately eight months of treatment exposure, nearly one year of study and a final approximately nine-week assay; its Dryad description contains an eight-week wording. StudyID 243 reports a 92-day experiment; StudyID 246 reports 48 hours. Driver-specific dose models are reported in Table S15; durations were too sparse for meta-regression.'
AddP $supp 'Supplementary Table S4. Response definitions and coding review' $true $true
AddTable $supp @(@('Definition','Question','Reference'),@('Directional','Does the measured trait increase or decrease?','ln(treatment/control)'),@('Endpoint-aligned','Does the change follow a provisionally beneficial or adverse direction?','Signed endpoint dictionary'),@('Absolute aggregated','How large is the signed study-cell departure?','Absolute value after signed aggregation'))
AddP $supp 'Table S4b is supplied in Review_endpoint_dictionary.csv and Review_endpoint_direction_decisions.csv. Coding confidence describes a rule, not inter-rater reliability. Review provenance and source-verification decisions are documented in Table S17.'
AddP $supp 'Supplementary Table S6. Point-estimate orderings and transformation order' $true $true
AddP $supp 'All effect estimates, intervals and stage orderings are regenerated in Fig2c_magnitude_definition_sensitivity.csv. Orderings are not tests of separated ranks.'
AddCsv $supp 'data/source_data/Fig2c_magnitude_definition_sensitivity.csv' @('EvidenceSet','Definition','ObservedStage','Estimate','StageRank')
AddP $supp 'Supplementary Table S8. Primary metamorphic-taxa evidence counts' $true $true
AddCsv $supp 'data/source_data/Review_primary_stage_counts.csv' @('ObservedStage','k','m','Aligned_k','Aligned_m')
AddP $supp 'Supplementary Table S5. Reference-deviation calibration' $true $true
AddP $supp 'Complete values and bootstrap intervals are in data/processed/ER_reference_deviation_results.csv. Sampling-error and heterogeneity-inclusive probabilities are model-check diagnostics, not tests of biological harm.'
$cal=Import-Csv -LiteralPath (Join-Path $arc 'data/processed/ER_reference_deviation_results.csv') | Where-Object {$_.EvidenceSet -eq 'All eligible endpoints' -and $_.Dimension -eq 'Stage'}
$rows=[Collections.Generic.List[object]]::new();$rows.Add(@('Stage','k / s','Weighted absolute','95% CI','Heterogeneity index','Adjusted q'))
foreach($r in $cal){$rows.Add(@($r.ObservedStage,($r.RawEffectSizeCount+' / '+$r.StudyCellCount),('{0:F3}' -f [double]$r.ObservedWeightedMeanAbs),('{0:F3} to {1:F3}' -f [double]$r.WeightedMeanAbs_CI_low,[double]$r.WeightedMeanAbs_CI_high),('{0:F3}' -f [double]$r.HeterogeneityAdjustedExcess),$r.HeterogeneityCalibratedQ))};AddTable $supp $rows
AddP $supp 'Supplementary Table S7. Dependence and influence sensitivities' $true $true
AddP $supp 'StudyID, species and family clustering use the family-complete subset. Primary stage models retain 149 StudyIDs; this comparison retains 146. Complete files: Supplementary_RVE_cluster_sensitivity.csv, Supplementary_taxa_subset_stage_sensitivity.csv and SupplementaryTableS7_leave_one_study_out_influence.csv in data/source_data. Updated contrast-specific tests are in Table S13. '
AddCsv $supp 'data/source_data/Supplementary_RVE_cluster_sensitivity.csv' @('ClusterDefinition','Stage','Estimate','CI_low','CI_high','ClusterCount')
AddP $supp 'Supplementary Table S9. Paired-target and moderator test families' $true $true
AddP $supp ("The primary signed paired family contains $($summary.primary_signed_tests) aligned-minus-directional tests, with $($summary.primary_signed_significant) significant after correction. Folded absolute primary contrasts are retained as descriptive paired differences with no zero-null P or q values. The subgroup signed paired family contains $($summary.subgroup_signed_tests) aligned-minus-directional tests, with $($summary.subgroup_signed_significant) significant after correction. Folded subgroup contrasts are likewise descriptive. Complete machine-readable estimates, intervals, P values and q values are in Fig3b_estimand_target_differences.csv and Fig4_subgroup_estimand_paired_differences.csv. The 15 all-endpoint moderator tests remain a separate family.")
AddP $supp 'Supplementary Table S10. Provenance and analytical populations' $true $true
AddCsv $supp 'data/source_data/Review_analysis_population_counts.csv' @('Scope','k','m','StudyStageCells','StudyDriverCells','StudyStageDriverCells')
AddP $supp 'The canonical input removes 32 exact cross-layer duplicates from 32 pairs. Review_duplicate_resolution_audit.csv preserves retained and removed identifiers and decision rules. Source locators may refer to extraction datasets rather than original pages. Experiment identities and complete legacy screening history remain incomplete. Sire de Vilar datasets share StudyID 246.'
AddP $supp 'Supplementary Table S11. Magnitude terminology and mathematical targets' $true $true
AddTable $supp @(@('Term','Definition','Use'),@('Absolute aggregated response','Absolute value of inverse-variance signed cell mean','Figures 2-4'),@('Endpoint-level absolute response','Aggregate absolute endpoint lnRR before pooling','Figure S10 sensitivity'),@('Calibrated descriptive index','Absolute cell response minus expected absolute noise','Figure 6'),@('Noise-adjusted RMS','sqrt(max(mean(a_i squared - v_i),0))','Figure S10 and Table S12'),@('Model-implied latent absolute mean','E|theta| under signed REML normal distribution','Revision sensitivity, Table S12'))
AddP $supp 'Supplementary Table S12. Direct-outcome and signed-distribution sensitivities' $true $true
AddCsv $supp 'data/source_data/Review_direct_outcome_RVE.csv' @('ObservedStage','b.r','CI.L','CI.U','k','m','FDR_q_value')
AddP $supp 'Direct outcomes are an exploratory restriction, not a second human coding exercise. All eligible signed REML specifications and RMS truncation audits are supplied in Review_signed_REML_distribution_sensitivity.csv and Review_RMS_truncation_audit.csv. Figure S11 visualizes the primary all-endpoint subset. Bootstrap resamples: 4,000; within-cell correlations: 0, 0.5, 0.8.'
AddP $supp 'Supplementary Table S13. Updated CR2 stage inference' $true $true
AddCsv $supp 'data/processed/omnibus_stage_tests.csv' @('Estimand','Statistic','df','df_denom','P_value')
AddP $supp 'Omnibus tests use CR2 covariance and HTZ small-sample F statistics. Pairwise tests use contrast-specific Satterthwaite degrees of freedom. ER_RVE_stage_estimates.csv and ER_RVE_stage_contrasts.csv in data/processed report all means, counts and tests across rho = 0, 0.5 and 0.8. Figure 5c and the Excel attachment report the rho = 0.8 pairwise stage contrasts; none survives its BH correction.'
# Normalize inherited table wording without altering recorded numbers.
AddP $supp 'Supplementary Figure S13. Exposure-adjusted meta-regression' $true $true
AddP $supp 'SupplementaryFigS6_exposure_adjusted_meta_regression.pdf shows driver-specific dose and control-temperature slopes for directional and source-aligned responses. Points and intervals show RVE coefficients and 95% small-sample intervals; colours identify drivers. Black labels give coefficients, studies (s), effects (k), BH q and small-sample degrees of freedom. Raw scatter is not overlaid on adjusted slopes. Temperature increments use degrees Celsius and CO2 increments use log2 treatment/control ratios. Forest axes in Figures S4, S6, S10, S11 and S13 use asinh(x/0.15) positions with original-unit ticks and labels; this display transformation does not alter inference. Table S15 supplies all tests, degrees of freedom and BH adjustments.'
AddP $supp 'Supplementary Table S14. Source-based alignment and dependence sensitivities' $true $true
AddCsv $supp 'data/source_data/Boundary_stage_sensitivities.csv' @('Specification','Stage','Estimate','CI_low','CI_high','FDR_q_value')
AddP $supp 'The published-consensus-only restriction retains significant embryo, juvenile/adult means after correction. Study 244 exclusion and omission of restored dose rows are sensitivity specifications, not independent evidence.'
AddP $supp 'Supplementary Table S15. Exposure-adjusted stage tests' $true $true
AddCsv $supp 'data/source_data/Boundary_dose_adjusted_stage_tests.csv' @('Target','Driver','F_stat','df_num','df_denom','P_value','FDR_q_value')
AddP $supp 'Boundary_dose_meta_regression.csv contains every adjusted coefficient and confidence interval; Boundary_dose_model_diagnostics.csv reports counts, predictor ranges, rank and condition diagnostics. Six stage-omnibus tests form one BH family; coefficient tests form a separate family within each response definition. Acclimation was not treated as exposure duration.'
AddP $supp 'Supplementary Table S16. Experimental-unit repair and covariance' $true $true
AddCsv $supp 'data/source_data/Boundary_Study244_hierarchical_effects.csv' @('SpeciesID','StressorLabel','Estimate','Variance','CI_low','CI_high')
AddP $supp 'Study 244 uses equal-header means and 9,999 joint draws; only two headers per treatment limit inference. The complete covariance and its stage sensitivity are archived in Boundary_Study244_sampling_covariance.csv and Boundary_explicit_covariance_stage_REML.csv. Study 243 aquarium-level corrections are in Boundary_Williams_morphology_repair.csv. Methods section 2.7 describes both re-extractions.'
AddP $supp 'Supplementary Table S17. Endpoint-coding verification and provenance' $true $true
$review=Get-Content -LiteralPath (Join-Path $arc 'data/source_data/Review_independent_coding_summary.json') -Raw -Encoding utf8 | ConvertFrom-Json
$reviewer=if($review.reviewer){$review.reviewer}else{'Not supplied'}
$reviewDate=if($review.review_date){$review.review_date}else{'Not supplied'}
$reviewScope=if($review.scope){$review.scope}else{'Endpoint coding; item coverage not specified'}
AddTable $supp @(@('Review field','Record'),@('Process','Two non-blinded rule-audit outputs; human endorsement reported by author'),@('Human endorsement','Two researchers endorsed the audit; their individual decision records were not supplied'),@('Historical reviewer record','Li Zhuoqing; 1 August 2026; not the date of this audit endorsement'),@('Current endorsement identities and actual date','Not supplied; author statement recorded 14 September 2026'),@('Coverage','266 entries linked to 2391 effects'),@('Direction agreement between audit outputs','258/266; 96.99%; kappa = 0.9080'),@('Exclusion agreement between audit outputs','258/266; 96.99%; kappa = 0.9071'),@('Direct-outcome agreement between audit outputs','259/266; 97.37%; kappa = 0.8536'),@('Human inter-rater agreement','Not estimable: original independent human codes were not supplied'),@('Source verification','36 flagged entries technically resolved: 14 direction changes and 22 retained decisions; see source-resolution table'))
AddP $supp 'The two audits used shared sources and could see existing reference codes; their agreement is descriptive and does not establish inter-rater reliability or correctness. Review_independent_coding_record.csv records author-reported endorsement, not independent human codes. The archived audit files retain both audit outputs and their comparisons. Source_verification_resolutions_36_entries.csv records subsequent evidence-based technical decisions and 14 changes to direction rules. Analyses and figures were rerun after these changes. The earlier human endorsement applies to the original audit; no independent human itemwise codes or approval of this later source revision are imputed.'
AddP $supp 'Supplementary Table S18. Pairwise differences among response definitions' $true $true
AddP $supp 'Equal-weight study-cell differences; 95% percentile paired-bootstrap intervals; 9999 draws. Only aligned-minus-directional contrasts receive centered-null bootstrap P values and BH correction. Folded absolute contrasts are descriptive because absolute-minus-signed differences are structurally non-negative. Combined denotes warming plus acidification. Values are rounded to four decimals; exact values are in source data. These are numerical target comparisons, not tests of biological harm.'
AddCsv $supp 'data/source_data/TableS18_pairwise_target_differences.csv' @('ObservedStage','StressorLabel','TargetDifference','Estimate','CI_low','CI_high','P_value','FDR_q_value','Significance')
AddP $supp 'Supplementary Table S19. Joint equality tests of three response definitions' $true $true
AddP $supp 'Exploratory centered-bootstrap maximum standardized paired-contrast test, 9999 joint draws. Nine stage-by-exposure tests form a separate BH family. Combined denotes warming plus acidification. Stars and rounding follow Table S18. Rejection indicates at least one unequal mean; it does not indicate three significant pairwise differences.'
  AddCsv $supp 'data/source_data/TableS19_joint_target_tests.csv' @('ObservedStage','StressorLabel','StudyCellCount','Statistic','P_value','FDR_q_value','Significance')
  AddP $supp 'Accompanying Excel attachment: All_meta_estimates_and_tests.xlsx contains all forest-plot estimates, confidence intervals, study/effect counts, P values, BH q values, joint tests and the zero/missing-evidence audit in separate worksheets. The workbook is supplied at the submission-package root and in the data archive.'
foreach($p in @($supp.Xml.SelectNodes('//w:p',$supp.Ns))){$t=TextOf $p $supp;$new=$t.Replace('Conservatively aligned performance','Endpoint-aligned response').Replace('aligned-performance','endpoint-aligned').Replace('biological performance decline after unambiguous endpoint directions are aligned','the dictionary-aligned response decline').Replace('top magnitude rank','largest point estimate').Replace('Beneficial when increasing when the metric directly represents performance','Provisionally aligned positively; fitness interpretation is unvalidated').Replace('Make adverse direction coherent','Apply the recorded direction rule').Replace('Make beneficial direction coherent','Apply the recorded direction rule').Replace('Retain direct performance evidence','Assess proxy interpretation in sensitivity analysis');if($new -ne $t){SetPara $p $supp $new}}
$tableGroups=[Collections.Generic.List[object]]::new();$currentGroup=$null
foreach($node in @($supp.Body.ChildNodes)){
  if($node.LocalName -eq 'sectPr'){continue}
  $t=if($node.LocalName -eq 'p'){TextOf $node $supp}else{''}
  if($t -match '^Supplementary Table S(\d+)\.'){
    $currentGroup=@{Number=[int]$Matches[1];Nodes=[Collections.Generic.List[object]]::new()}
    $tableGroups.Add($currentGroup)
  }
  if($null -ne $currentGroup){$currentGroup.Nodes.Add($node)}
}
foreach($group in $tableGroups){foreach($node in $group.Nodes){[void]$supp.Body.RemoveChild($node)}}
$sect=$supp.Body.SelectSingleNode('w:sectPr',$supp.Ns)
foreach($group in ($tableGroups | Sort-Object {$_.Number})){foreach($node in $group.Nodes){[void]$supp.Body.InsertBefore($node,$sect)}}
SaveDoc $supp

foreach($name in @('ER_title_page.docx','ER_cover_letter.docx','Highlights.docx','Submission_Declarations.docx')){
  $h=OpenDoc $name
  foreach($p in @($h.Xml.SelectNodes('//w:p',$h.Ns))){$t=TextOf $p $h
    $new=$t.Replace($oldTitle,$newTitle)
    if($name -eq 'ER_cover_letter.docx'){
      if($t -match '^\d+ September 2026$'){$new='19 September 2026'}
      if($t.StartsWith('The manuscript examines')){$new='The manuscript asks what measured direction, biological performance and response magnitude each reveal about marine-invertebrate sensitivity to ocean warming and acidification. It organizes ecological subgroup and stage analyses around the question answered by each response definition.'}
      if($t.StartsWith('The analysis includes')){$new='The database contains 2,391 effects from 162 studies; primary analyses use 2,214 effects from 149 studies in metamorphic taxa. Directional analysis identifies a negative embryo signal, endpoint alignment identifies negative embryo, larval and juvenile/adult means, and all-endpoint absolute summaries identify the largest departure at settlement/metamorphosis without establishing harm. These broader analyses differ in endpoint eligibility and weighting. The matched signed comparisons do not establish corrected differences between definitions, and no corrected primary pairwise stage contrast supports a universal hierarchy. The six main figures follow evidence coverage, ecological subgroups, matched definitions, robustness and the three question-specific conclusions.'}
      if($t.StartsWith('These results are relevant')){$new='The environmental relevance is the interpretation of experimental endpoints under marine climate exposures. The current main and supplementary figure set, endpoint-coding audit and source-verification records accompany the revised manuscript. The Zenodo concept DOI identifies an earlier public deposit; the revised manuscript and accompanying archive are supplied with this submission, and equivalence with the earlier public version is not asserted. Source transformations must be respected before assigning biological direction, and absolute departure must be distinguished from harm. Remaining limits concern historical screening records and incomplete experimental metadata.'}
    }
    if($name -eq 'ER_title_page.docx'){
      if($t.StartsWith('Abstract word count')){$new='Abstract word count: '+(($newLines[1] -split '\s+').Count)}
      if($t.StartsWith('Main text word count')){$bodyText=($newLines[5..($newLines.IndexOf('Figure legends')-1)] -join ' ');$new='Main text word count (Introduction through Conclusions): '+(($bodyText -split '\s+').Count)}
      if($t.StartsWith('Main figures:')){$new='Main figures: 6; Supplementary figures: 13; Supplementary tables: 19 (Tables S4, S17-S19 include machine-readable records).'}
    }
    if($name -eq 'Highlights.docx'){
      if($t.StartsWith('1.')){$new='1. Directional analysis identifies a negative embryo response.'}
      if($t.StartsWith('2.')){$new='2. Alignment identifies negative embryo, larval and juvenile/adult performance.'}
      if($t.StartsWith('3.')){$new='3. Settlement shows large departure but lacks aligned performance evidence.'}
      if($t.StartsWith('4.')){$new='4. No matched signed contrast passes BH correction.'}
      if($t.StartsWith('5.')){$new='5. Direction, performance and magnitude answer different ecological questions.'}
    }
    if($t.StartsWith('Data availability:')){$new='Data availability: The Zenodo concept DOI identifies an earlier public deposit. The revised manuscript and accompanying archive are supplied with this submission; equivalence with the earlier public version is not asserted. The revision dataset, coding rules, provenance audit, source tables and code are supplied in ER_data_and_code_archive_taxonomy_meta_final_review_20260918.zip.'}
    if($new -ne $t){SetPara $p $h $new}
  };SaveDoc $h
}
foreach($name in @('ER_manuscript.docx.txt','ER_supplementary_material.docx.txt')){Copy-Item -LiteralPath (Join-Path $pkg $name) -Destination (Join-Path $arc $name) -Force}
Copy-Item -LiteralPath (Join-Path $pkg 'ER_manuscript.docx.txt') -Destination (Join-Path $arc 'ER_manuscript_taxonomy_synced.txt') -Force
Copy-Item -LiteralPath (Join-Path $pkg 'ER_supplementary_material.docx.txt') -Destination (Join-Path $arc 'ER_supplementary_material_taxonomy_synced.txt') -Force
Write-Output $work
