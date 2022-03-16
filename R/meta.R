#' Metadata data frame containing information about the data mapping used to configure safetyGraphics charts for the dm domain. One record per unique data mapping
#'
#' @format A data frame with X rows and 10 columns
#' \describe{
#'    \item{domain}{Data domain}
#'    \item{text_key}{Text key indicating the setting name. \code{'--'} delimiter indicates a field level data mapping}
#'    \item{col_key}{Key for the column mapping}
#'    \item{field_key}{Key for the field mapping (if any)}
#'    \item{type}{type of mapping - "field" or "column"}
#'    \item{label}{Label}
#'    \item{description}{Description}
#'    \item{multiple}{Mapping supports multiple columns/fields }
#'    \item{standard_adam}{Default values for the ADaM data standard}
#'    \item{standard_sdtm}{Default values for the SDTM data standard}
#' }    
#' 
#' @source Created for this package
"meta_dm"

#' ECG Meta Data
#' 
#' Metadata data frame containing information about the data mapping used to configure 
#' safetyGraphics charts for the ecg domain. 
#' One record per unique data mapping
#'
#' @format A data frame with 22 rows and 10 columns
#' \describe{
#'    \item{domain}{Data domain}
#'    \item{text_key}{Text key indicating the setting name. \code{'--'} delimiter indicates a field level data mapping}
#'    \item{col_key}{Key for the column mapping}
#'    \item{field_key}{Key for the field mapping (if any)}
#'    \item{type}{type of mapping - "field" or "column"}
#'    \item{label}{Label}
#'    \item{description}{Description}
#'    \item{multiple}{Mapping supports multiple columns/fields }
#'    \item{standard_adam}{Default values for the ADaM data standard}
#'    \item{standard_sdtm}{Default values for the SDTM data standard}
#' }
#'
#' @source Created for this package
"meta_ecg"

#' Phase I SAD-MAD Study ECG data.
#'
#' ECG data from Phase I SAD/MAD studies 
#' 
#' @format
#' \describe{
#'   \item{PART}{SAD or MAD.}
#'   \item{ID}{Subject identifier.}
#'   \item{DAY}{Day number as in the source data. See DAY2.}
#'   \item{DAY2}{For the SAD part, DAY2 = 1 for the day of and the two days following the dose administration, whereas DAY = 1, 2, 3.}
#'   \item{VISIT}{Visit number as in the source data. See VISIT2.}
#'   \item{VISIT2}{For the SAD part, VISIT2 = 1 for the day of and the two days following the dose administration, whereas VISIT = 1, 2, 3.}
#'   \item{TIME}{Time post dose, in hours, reported in the source data. See TIME2.}
#'   \item{TIME2}{For the SAD part, TIME is missing for the screening and pre-treatment visits; TIME2 = 0 then. For the SAD part, TIME is missing for the observation on DAY 3; TIME2 = 48 hours then.}
#'   \item{PARAM}{HR for heart rate, or QTCF for QTcF.}
#'   \item{VALIE}{Observed value of HR, in beats per minute, or QTcF, in milliseconds.}
#'   \item{BASE}{Baseline VALUE.}
#'   \item{CHANGE}{Change from baseline for observations after the baseline; missing otherwise.}
#' }
#' @source source data 
"eg_ph1"


#' Phase I SAD-MAD Study demographic data.
#'
#' Phase 1 study with a single-ascending-dose (SAD) part and a multiple-ascending-dose (MAD) part.
#'   SAD design{
#'   •, Five parallel treatment arms, with treatments labeled A, B, C, D, E.
#'   •, Three screening and pre-treatment visits, numbered -3, -2, -1.
#'   •, Single dose on visit 1 (day 1) at TIME 0.
#'   •, ECGs 0, 3, 6, 8, 12, 20, 24, and 48 hours post dose. 
#'   •, The ECG at 0 hours post dose on visit (and day) 1 is considered the baseline.
#'   MAD design{
#'   •, Four parallel treatment arms, with treatments labeled A, B, C, D, which are the same as A, B, C, D in the SAD.
#'   •, Three screening and pre-treatment visits, numbered -3, -2, -1.
#'   •, Daily doses on days 1 - 14 at time = 0 on each day.
#'   •, ECGs at 0 and 6 hours post dose on DAYs 1, 2, 3, 8, 12, 14, and 24 and 168 hours after the 14th dose.
#'   •, The ECG at 0 hours post dose on VISIT (and DAY) 1 is considered baseline.
#'
#' @format
#' \describe{
#'    \item{PART}{SAD or MAD.}
#'    \item{ID}{Subject identifier.}
#'    \item{TREAT}{A, B, C, D, or E.}
#'    \item{AGE}{Subjects age in years.}
#'    \item{SEX}{Subjects sex, M=Male, F=Female.}
#'    \item{RACE}{Subjects race.}
#' }
#' @source source data 
"dm_ph1"

#' Phase 2b Study ECG data.
#' 
#' Design{
#'   Four parallel treatment arms, with treatments labeled A, B, C, D
#'   One screening visit, visit 1.
#'   Daily doses on days 1 - 56 at time = 0 on each day.
#'   ECGs at 0 hours post dose on visits 2 - 8, which are supposed to be on days 1, 15, 29, 43, 57, and 70, respectively, but may deviate somewhat. 
#'   ECG at 2 hours post dose on visit 4, which should be on day 29 but may deviate.
#'   The ECG at 0 hours post dose on visit 2, day 1, is considered baseline.
#'
#' @format
#' \describe{
#'   \item{ID}{Subject identifier.}
#'   \item{VISIT}{Visit number.}
#'   \item{DAY}{Day number.}
#'   \item{TIME}{Time relative to dose, or "EARLY WITHDRAWAL", or missing, as reported in the source data. See TIME2.}
#'   \item{TIME2}{2 if TIME == "2 HOURS POST DOSE"; 0 otherwise. }
#'   \item{PARAM}{HR for heart rate, or QTCF for QTcF.}
#'   \item{VALIE}{Observed value of HR, in beats per minute, or QTcF, in milliseconds.}
#'   \item{BASE}{Baseline VALUE.}
#'   \item{CHANGE}{Change from baseline for observations after the baseline; missing otherwise.}
#'   \item{TREAT}{A, B, C, or D.}
#' }
#' @source source data 
"eg_ph2"


#' Phase 2b Study demographic data.
#'
#' Demogrpahic data
#' 
#' @format
#' \describe{
#'   \item{ID}{Subject identifier.}
#'   \item{TREAT}{A, B, C, or D.}
#'   \item{AGE}{Subjects age in years.}
#'   \item{SEX}{Subjects sex, M=Male, F=Female.}
#'   \item{RACE}{Subjects race.}
#' }
#' @source source data 
"dm_ph2"


#' TQT Study ADEG data.
#'
#' CiPA ECG Validation Study 
#' See https://physionet.org/content/ecgcipa/1.0.0/
#'
#' 
#' @format
#' \describe{
#'   \item{STUDYID}{Study Identifier, Char}
#'   \item{USUBJID}{Unique Subject Identifier, Char}
#'   \item{TRTSEQP}{Planned Sequence of Treatments, Char}
#'   \item{TRTP}{Planned Treatment, Char}
#'   \item{TRTSEQA}{Actual Sequence of Treatments, Char}
#'   \item{TRTA}{Actual Treatment, Char}
#'   \item{PARAM}{Parameter, Char}
#'   \item{PARAMCD}{Parameter Code, Char}
#'   \item{APERIOD}{Period , Num}
#'   \item{APERIODC}{Period (C), Char}
#'   \item{AVISIT}{Analysis Visit, Char}
#'   \item{AVISITN}{Analysis Visit (N), Num}
#'   \item{ATPT}{Analysis Time Point, Char}
#'   \item{ATPTN}{Analysis Time Point (N), Num}
#'   \item{ATPTREF}{Analysis Reference Time Point, Char}
#'   \item{ARDTM}{Date/Time of Reference, Num}
#'   \item{AARDTM}{Actual Date/Time of Reference, Num}
#'   \item{ARRLT}{Actual Relative Time from Reference Time TimePoint, Num}
#'   \item{NRRLT}{Nominal Relative Time from Reference Time Point, Num}
#'   \item{RRLTU}{Relative Time from Reference Time Point Units, Char}
#'   \item{ADTM}{Analysis Date and Time, Num}
#'   \item{ADT}{Analysis Date, Num}
#'   \item{ATM}{Analysis Time, Num}
#'   \item{ADY}{Analysis Relative Day, Num}
#'   \item{AVISDY}{Analysis Visit Day, Num}
#'   \item{APERDAY}{Analysis Nominal Period Day, Num}
#'   \item{EGREPNUM}{ECG Replicate Number, Num}
#'   \item{AVAL}{Analysis Value, Num}
#'   \item{AVALC}{Analysis Value, Char}
#'   \item{EGSTRESU}{Units of Analysis Value, Char}
#'   \item{DTYPE}{Derivation Type, Char}
#'   \item{ABLFL}{Baseline Record Flag, Char}
#'   \item{AEGBLFL}{Baseline Flag for Records used to Derive the Baseline, Char}
#'   \item{BASE}{Baseline Value, Num}
#'   \item{CHG}{Change from Baseline, Num}
#'   \item{BASETYPE}{Baseline Type, Char}
#'   \item{ACOMPFL}{Comparator Record Flag, Char}
#'   \item{COMP}{Comparator Value, Num}
#'   \item{COMPBASE}{Baseline Value of the Comparator, Num}
#'   \item{COMPCHG}{Change from Baseline in the Comparator, Num}
#'   \item{CCOMPCHG}{Change from Baseline Corrected for the Comparator, Num}
#'   \item{COMPTYPE}{Comparator Type, Char}
#'   \item{ECGPCFL}{ECG Matching PK Flag, Char}
#'   \item{EGSEQ}{Sequence Number, Num}
#'   \item{EGLEAD}{Lead Location Used for Measurement, Char}
#'   \item{EGREFID}{ECG Reference ID, Char}
#'   \item{CRITy}{Analysis Criterion y, Char}
#'   \item{CRITyFL}{Criterion y Evaluation Result Flag, Char}
#'   \item{IUTANLFL}{Intersection Union Test Analysis Flag, Char}
#'   \item{CATANLFL}{Categorical Analysis Flag, Char}
#'   \item{ASEANLFL}{Assay Sensitivity Analysis Flag, Char}
#'   \item{CQTANLFL}{Concentration QT Analysis Flag, Char}
#' }
#' @source https://physionet.org/content/ecgcipa/1.0.0/
"adeg"

#' TQT Study subject level ADSL data.
#'
#' CiPA ECG Validation Study 
#' See https://physionet.org/content/ecgcipa/1.0.0/
#'
#' 
#' @format
#' \describe{
#'   \item{STUDYID}{Study Identifier, Char}
#'   \item{USUBJID}{Unique Subject Identifier, Char}
#'   \item{SUBJID}{Subject Identifier for the Study, Char}
#'   \item{SITEID}{Site Identifier, Char}
#'   \item{AGE}{Age, Num}
#'   \item{AGEU}{Age Units, Char}
#'   \item{SEX}{Sex, Char}
#'   \item{RACE}{Race, Char}
#'   \item{ETHNIC}{Ethnic, Char}
#'   \item{ARM}{Planned Arm, Char}
#'   \item{ACTARM}{Actual Arm, Char}
#'   \item{TRT01P}{Planned Treatment for Period 1, Char}
#'   \item{TRT02P}{Planned Treatment for Period 2, Char}
#'   \item{TRT01PN}{Planned Treatment for Period 1 (N), Num}
#'   \item{TRT02PN}{Planned Treatment for Period 2 (N), Num}
#'   \item{TRT01A}{Actual Treatment for Period 1, Char}
#'   \item{TRT02A}{Actual Treatment for Period 2, Char}
#'   \item{TRT01AN}{Actual Treatment for Period 1 (N), Num}
#'   \item{TRT02AN}{Actual Treatment for Period 2 (N), Num}
#'   \item{TRTSEQP}{Planned Sequence of Treatments, Char}
#'   \item{TRTSEQPN}{Planned Sequence of Treatments (N), Num}
#'   \item{TRTSEQA}{Actual Sequence of Treatments, Char}
#'   \item{TRTSEQAN}{Actual Sequence of Treatments (N), Num}
#' }
#' @source https://physionet.org/content/ecgcipa/1.0.0/
"adsl"