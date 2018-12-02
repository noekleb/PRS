&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR wLinje        AS CHAR NO-UNDO.
DEF VAR wLoop         AS INT  NO-UNDO.
DEF VAR wTotAnt       AS INT  NO-UNDO.

/* Variabler for håndtering av importerte data */
DEF VAR wKUNDKOD      AS CHAR              no-undo.
DEF VAR wBUNT         AS INT               no-undo.
DEF VAR wDATUM        AS DATE              NO-UNDO.
DEF VAR wTid          AS char              NO-UNDO.
DEF VAR wMerknad      AS CHAR              NO-UNDO.
DEF VAR wStopp        AS LOG INITIAL FALSE NO-UNDO.
DEF VAR rBatchRowId   AS ROWID             NO-UNDO.
DEF VAR wBAtchNr      AS INT               NO-UNDO.               
DEF VAR rRecid        AS RECID             NO-UNDO.
DEF VAR cFolder       AS CHAR              NO-UNDO.
DEF VAR lCanceled     AS LOGICAL           NO-UNDO.
DEF VAR lSvar         AS LOG               NO-UNDO.
DEF VAR lBekreft      AS LOG               NO-UNDO.
DEF VAR cErrLst       AS CHAR              NO-UNDO.
DEF VAR hProcHandle   AS HANDLE EXTENT 13  NO-UNDO.
DEF VAR iSeqNr        AS INT               NO-UNDO.
DEF VAR lLapTop       as log               no-undo.
DEF VAR cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cTabell       LIKE ImpKonv.Tabell     NO-UNDO.
DEF VAR lSvar2        AS LOG               NO-UNDO.
DEF VAR cImpSesong    AS CHAR              NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

define temp-table tmpChild
  field wChild as handle.

{runlib.i}

DEF TEMP-TABLE tVariant
  FIELD LinjeNr  AS INT
  FIELD Tabell   AS CHAR
  FIELD MTRL     AS CHAR
  FIELD MTRLTEXT AS CHAR 
  FIELD FARG     AS CHAR
  FIELD FARGTEXT AS CHAR 
  FIELD LEVFARG  AS CHAR
  FIELD KOLLKOD  AS CHAR
  FIELD ARTKOD   AS CHAR
  FIELD LEVMTRL  AS CHAR
  .
DEF TEMP-TABLE tPris 
  FIELD LinjeNr  AS INT 
  FIELD Tabell   AS CHAR
  FIELD MTRL     AS CHAR
  FIELD FARG     AS CHAR
  FIELD TYP      AS CHAR     
  FIELD SORTTILL AS CHAR
  FIELD BRUTTO   AS CHAR  
  FIELD VALUTA   AS CHAR  
  FIELD NETTO    AS CHAR    
  FIELD UTPRIS   AS CHAR
  .
DEF TEMP-TABLE tSort
  FIELD LinjeNr AS INT
  FIELD Tabell   AS CHAR
  FIELD MTRL     AS CHAR
  FIELD FARG     AS CHAR
  FIELD SORT    AS CHAR 
  FIELD ARTGRP  AS CHAR
  FIELD VM      AS CHAR 
  FIELD VMTEXT  AS CHAR 
  .

DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cSkoTex          AS CHAR  NO-UNDO.

DEF STREAM sExportFile.
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Slett B-AvbrytImport B-SjekkFil ~
B-ArtBasExcel B-SLev-12 B-Artikkel B-ArtikkelOppdat T-ArtikkelOppdat ~
T-ArtikkelNye B-IBest B-BestOppdat B-BestExcel B-SLev-11 T-BestOppdat ~
T-BestNye B-HgInn B-HgExcel B-SLev-10 B-IHg T-HgOppdat T-HgNye ~
T-HgOverskriv B-VgInn T-VgOppdat T-VgNye T-VgOverskriv B-VgExcel B-SLev-2 ~
B-IHg-2 B-KategoriInn B-KategoriExcel B-SLev-3 B-IKategori T-KategoriOppdat ~
T-KategoriNye T-KategoriOverskriv B-SesongInn B-SesongExcel B-SLev-4 ~
B-ISasong T-SesongOppdat T-SesongNye T-SesongOverskriv B-FargeInn ~
B-FargExcel B-SLev-5 B-IFarge T-FargOppdat T-FargNye T-FargOverskriv ~
B-MaterialInn B-MaterialExcel B-SLev-6 B-IMaterial T-MaterialOppdat ~
T-MaterialNye T-MaterialOverskriv B-VmOppdat B-VmExcel B-SLev-7 B-IVm ~
T-VmOppdat T-VmNye T-VmOverskriv B-ValutaOppdat B-ValutaExcel B-SLev-8 ~
B-IValuta T-ValutaOppdat T-ValutaNye T-ValutaOverskriv B-IStrType ~
B-StrTypeOppdat B-StrTypeExcel B-SLev-9 T-StrTypeOppdat T-StrTypeNye ~
T-StrTypeOverskriv B-ILev B-LevOppdat B-LevExcel B-SLev T-LevOppdat ~
T-LevNye T-LevOverskriv B-ILevInd B-LevIndOppdat B-Lagre B-LevIndExcel ~
BUTTON-SokImportHode B-SLev-13 T-LevIndOppdat T-LevIndNye B-Ny B-Exit ~
T-LevIndOverskriv Btn_Help RECT-18 RECT-19 RECT-20 RECT-21 RECT-22 RECT-23 ~
RECT-24 RECT-25 RECT-26 
&Scoped-Define DISPLAYED-OBJECTS FI-BatchNr FI-Batch FI-Katalog ~
FI-BildeKatalog FI-Artikkel T-Artikkel T-ArtikkelOppdat T-ArtikkelNye ~
T-ArtikkelOverskriv FI-Best T-Best T-BestOppdat T-BestNye T-BestOverskriv ~
FI-Hg T-HgOppdat T-HgNye T-HgOverskriv T-Hg T-VgOppdat T-VgNye ~
T-VgOverskriv FI-Vg T-Vg FI-Kategori T-KategoriOppdat T-KategoriNye ~
T-KategoriOverskriv T-Kategori FI-Sesong T-SesongOppdat T-SesongNye ~
T-SesongOverskriv T-Sesong FI-Farge T-FargOppdat T-FargNye T-FargOverskriv ~
T-Farge FI-Material T-MaterialOppdat T-MaterialNye T-MaterialOverskriv ~
T-Material FI-Vm T-VmOppdat T-VmNye T-VmOverskriv T-Vm FI-Valuta ~
T-ValutaOppdat T-ValutaNye T-ValutaOverskriv T-Valuta FI-StrType ~
T-StrTypeOppdat T-StrTypeNye T-StrTypeOverskriv T-StrType FI-Lev T-Lev ~
T-LevOppdat T-LevNye T-LevOverskriv FI-LevInd T-LevIndOppdat T-LevIndNye ~
T-LevIndOverskriv T-LevInd FI-Behandler 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Angre 
     IMAGE-UP FILE "icon/e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre".

DEFINE BUTTON B-ArtBasExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-Artikkel 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-ArtikkelOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Artikkler 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "Button 4" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-AvbrytImport 
     LABEL "Avbryt import" 
     SIZE 19 BY 1.1.

DEFINE BUTTON B-Best 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 2" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-BestExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-BestOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-BildeKatalog 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "ImportBatch" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON B-Farge 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 7" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-FargeInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-FargExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-Hg 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 3" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-HgExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-HgInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-IBest 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IFarge 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IHg 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IHg-2 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IKategori 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-ILev 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-ILevInd 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IMaterial 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-ISasong 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IStrType 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IValuta 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-IVm 
     LABEL "Les inn.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-Katalog 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "ImportBatch" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-Kategori 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 5" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-KategoriExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-KategoriInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Lagre 
     IMAGE-UP FILE "icon/e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre - Ved ny oppprettes ny liste, ved endre, oppdateres valgt liste.".

DEFINE BUTTON B-Lev 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 12" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-LevExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-LevInd 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 13" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-LevIndExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-LevIndOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-LevOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Material 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 8" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-MaterialExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-MaterialInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny liste".

DEFINE BUTTON B-Sesong 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 6" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-SesongExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SesongInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-SjekkFil 
     LABEL "Sjekk filnavn" 
     SIZE 17 BY 1.1.

DEFINE BUTTON B-Slett 
     IMAGE-UP FILE "icon/e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Sl&ett kolleksjonsliste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sletter valgt liste.".

DEFINE BUTTON B-SLev 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-10 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-11 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-12 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-13 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-2 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-3 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-4 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-5 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-6 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-7 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-8 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-SLev-9 
     LABEL "Ta bort" 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-Start 
     LABEL "Start import" 
     SIZE 18.6 BY 1.1.

DEFINE BUTTON B-StartOppdat 
     LABEL "Start oppdatering" 
     SIZE 18.6 BY 1.1.

DEFINE BUTTON B-StrType 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 11" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-StrTypeExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-StrTypeOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Val 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 10" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-ValutaExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-ValutaOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Vg 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 4" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-VgExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-VgInn 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON B-Vm 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "B best 9" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-VmExcel 
     LABEL "Eksport.." 
     SIZE 11.8 BY 1.

DEFINE BUTTON B-VmOppdat 
     LABEL "Oppdater..." 
     SIZE 13 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokImportHode 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE FI-Artikkel AS CHARACTER FORMAT "X(256)":U INITIAL "artikel.exp" 
     LABEL "Artikkler" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Batch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BatchNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Import batch" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Behandler AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 155 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Best AS CHARACTER FORMAT "X(256)":U INITIAL "best.exp" 
     LABEL "Bestillinger" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BildeKatalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bildekatalog" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Farge AS CHARACTER FORMAT "X(256)":U INITIAL "färg.exp" 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Hg AS CHARACTER FORMAT "X(256)":U INITIAL "huvudgrupp.exp" 
     LABEL "Hovedgrupper" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Katalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Importkatalog" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kategori AS CHARACTER FORMAT "X(256)":U INITIAL "kategori.exp" 
     LABEL "Kategori" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lev AS CHARACTER FORMAT "X(256)":U INITIAL "lev.exp" 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevInd AS CHARACTER FORMAT "X(256)":U INITIAL "levsort.exp" 
     LABEL "Leverandørsinndelinger" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Material AS CHARACTER FORMAT "X(256)":U INITIAL "material.exp" 
     LABEL "Material" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(256)":U INITIAL "säsong.exp" 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrType AS CHARACTER FORMAT "X(256)":U INITIAL "storleksint.exp" 
     LABEL "Størrelsestyper" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Valuta AS CHARACTER FORMAT "X(256)":U INITIAL "valuta.exp" 
     LABEL "Valuta" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS CHARACTER FORMAT "X(256)":U INITIAL "artgrp.exp" 
     LABEL "Varegrupper" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vm AS CHARACTER FORMAT "X(256)":U INITIAL "varumärke.exp" 
     LABEL "Varemerker" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY 20.71.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY 2.38.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 158.6 BY .1.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 158.6 BY .1.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .2 BY 16.48.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .2 BY 16.48.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .2 BY 16.48.

DEFINE VARIABLE T-Artikkel AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-ArtikkelNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-ArtikkelOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-ArtikkelOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Best AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-BestNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-BestOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-BestOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Farge AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-FargNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-FargOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-FargOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Hg AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-HgNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-HgOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-HgOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Kategori AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-KategoriNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-KategoriOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-KategoriOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Lev AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-LevInd AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-LevIndNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-LevIndOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-LevIndOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-LevNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-LevOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-LevOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Material AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-MaterialNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-MaterialOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-MaterialOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Sesong AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-SesongNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-SesongOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-SesongOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-StrType AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-StrTypeNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-StrTypeOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-StrTypeOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Valuta AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-ValutaNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-ValutaOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-ValutaOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Vg AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-VgNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-VgOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-VgOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.

DEFINE VARIABLE T-Vm AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-VmNye AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Nye poster opprettes" NO-UNDO.

DEFINE VARIABLE T-VmOppdat AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Oppdater v/automatisk oppdatering" NO-UNDO.

DEFINE VARIABLE T-VmOverskriv AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Eksisterende poster overskrives" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Slett AT ROW 1.33 COL 12
     B-Start AT ROW 1.29 COL 24
     B-StartOppdat AT ROW 1.29 COL 43.2
     B-AvbrytImport AT ROW 1.29 COL 85.2
     B-SjekkFil AT ROW 1.29 COL 105.2
     FI-BatchNr AT ROW 3.14 COL 19 COLON-ALIGNED
     FI-Batch AT ROW 3.14 COL 33 COLON-ALIGNED NO-LABEL
     FI-Katalog AT ROW 4.33 COL 19 COLON-ALIGNED
     B-Katalog AT ROW 4.33 COL 85
     FI-BildeKatalog AT ROW 5.52 COL 19 COLON-ALIGNED
     B-BildeKatalog AT ROW 5.52 COL 85
     B-Artikkler AT ROW 7.19 COL 84.8
     B-ArtBasExcel AT ROW 7.19 COL 133
     B-SLev-12 AT ROW 7.19 COL 146
     FI-Artikkel AT ROW 7.24 COL 29.8 COLON-ALIGNED
     B-Artikkel AT ROW 7.24 COL 94.2
     B-ArtikkelOppdat AT ROW 7.24 COL 107.6
     T-Artikkel AT ROW 7.33 COL 90.6
     T-ArtikkelOppdat AT ROW 7.33 COL 121.6
     T-ArtikkelNye AT ROW 7.33 COL 125
     T-ArtikkelOverskriv AT ROW 7.33 COL 128.4
     FI-Best AT ROW 8.38 COL 29.8 COLON-ALIGNED
     B-IBest AT ROW 8.38 COL 94
     B-BestOppdat AT ROW 8.38 COL 107.6
     B-BestExcel AT ROW 8.38 COL 133
     B-SLev-11 AT ROW 8.38 COL 146
     B-Best AT ROW 8.43 COL 84.8
     T-Best AT ROW 8.48 COL 90.6
     T-BestOppdat AT ROW 8.48 COL 121.6
     T-BestNye AT ROW 8.48 COL 125
     T-BestOverskriv AT ROW 8.48 COL 128.4
     B-Hg AT ROW 10.05 COL 84.8
     B-HgInn AT ROW 10.05 COL 107.6
     B-HgExcel AT ROW 10.05 COL 133
     B-SLev-10 AT ROW 10.05 COL 146
     FI-Hg AT ROW 10.1 COL 29.8 COLON-ALIGNED
     B-IHg AT ROW 10.1 COL 94
     T-HgOppdat AT ROW 10.1 COL 121.6
     T-HgNye AT ROW 10.1 COL 125
     T-HgOverskriv AT ROW 10.1 COL 128.4
     T-Hg AT ROW 10.19 COL 90.6
     B-Vg AT ROW 11.24 COL 84.8
     B-VgInn AT ROW 11.24 COL 107.6
     T-VgOppdat AT ROW 11.24 COL 121.6
     T-VgNye AT ROW 11.24 COL 125
     T-VgOverskriv AT ROW 11.24 COL 128.4
     B-VgExcel AT ROW 11.24 COL 133
     B-SLev-2 AT ROW 11.24 COL 146
     FI-Vg AT ROW 11.29 COL 29.8 COLON-ALIGNED
     B-IHg-2 AT ROW 11.29 COL 94
     T-Vg AT ROW 11.38 COL 90.6
     B-Kategori AT ROW 12.43 COL 84.8
     B-KategoriInn AT ROW 12.43 COL 107.6
     B-KategoriExcel AT ROW 12.43 COL 133
     B-SLev-3 AT ROW 12.43 COL 146
     FI-Kategori AT ROW 12.48 COL 29.8 COLON-ALIGNED
     B-IKategori AT ROW 12.48 COL 94
     T-KategoriOppdat AT ROW 12.52 COL 121.6
     T-KategoriNye AT ROW 12.52 COL 125
     T-KategoriOverskriv AT ROW 12.52 COL 128.4
     T-Kategori AT ROW 12.57 COL 90.6
     B-Sesong AT ROW 13.62 COL 84.8
     B-SesongInn AT ROW 13.62 COL 107.6
     B-SesongExcel AT ROW 13.62 COL 133
     B-SLev-4 AT ROW 13.62 COL 146
     FI-Sesong AT ROW 13.67 COL 29.8 COLON-ALIGNED
     B-ISasong AT ROW 13.67 COL 94
     T-SesongOppdat AT ROW 13.67 COL 121.6
     T-SesongNye AT ROW 13.67 COL 125
     T-SesongOverskriv AT ROW 13.67 COL 128.4
     T-Sesong AT ROW 13.76 COL 90.6
     B-Farge AT ROW 14.81 COL 84.8
     B-FargeInn AT ROW 14.81 COL 107.6
     B-FargExcel AT ROW 14.81 COL 133
     B-SLev-5 AT ROW 14.81 COL 146
     FI-Farge AT ROW 14.86 COL 29.8 COLON-ALIGNED
     B-IFarge AT ROW 14.86 COL 94
     T-FargOppdat AT ROW 14.91 COL 121.4
     T-FargNye AT ROW 14.91 COL 124.8
     T-FargOverskriv AT ROW 14.91 COL 128.2
     T-Farge AT ROW 14.95 COL 90.6
     B-Material AT ROW 16 COL 84.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.6 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     B-MaterialInn AT ROW 16 COL 107.6
     B-MaterialExcel AT ROW 16 COL 133
     B-SLev-6 AT ROW 16 COL 146
     FI-Material AT ROW 16.05 COL 29.8 COLON-ALIGNED
     B-IMaterial AT ROW 16.05 COL 94
     T-MaterialOppdat AT ROW 16.1 COL 121.4
     T-MaterialNye AT ROW 16.1 COL 124.8
     T-MaterialOverskriv AT ROW 16.1 COL 128.2
     T-Material AT ROW 16.14 COL 90.6
     B-Vm AT ROW 17.19 COL 84.8
     B-VmOppdat AT ROW 17.19 COL 107.6
     B-VmExcel AT ROW 17.19 COL 133
     B-SLev-7 AT ROW 17.19 COL 146
     FI-Vm AT ROW 17.24 COL 29.8 COLON-ALIGNED
     B-IVm AT ROW 17.24 COL 94
     T-VmOppdat AT ROW 17.29 COL 121.4
     T-VmNye AT ROW 17.29 COL 124.8
     T-VmOverskriv AT ROW 17.29 COL 128.2
     T-Vm AT ROW 17.33 COL 90.6
     B-Val AT ROW 18.38 COL 84.8
     B-ValutaOppdat AT ROW 18.38 COL 107.6
     B-ValutaExcel AT ROW 18.38 COL 133
     B-SLev-8 AT ROW 18.38 COL 146
     FI-Valuta AT ROW 18.43 COL 29.8 COLON-ALIGNED
     B-IValuta AT ROW 18.43 COL 94
     T-ValutaOppdat AT ROW 18.43 COL 121.4
     T-ValutaNye AT ROW 18.43 COL 124.8
     T-ValutaOverskriv AT ROW 18.43 COL 128.2
     T-Valuta AT ROW 18.52 COL 90.6
     B-StrType AT ROW 19.57 COL 84.8
     B-IStrType AT ROW 19.57 COL 94
     B-StrTypeOppdat AT ROW 19.57 COL 107.6
     B-StrTypeExcel AT ROW 19.57 COL 133
     B-SLev-9 AT ROW 19.57 COL 146
     FI-StrType AT ROW 19.62 COL 29.8 COLON-ALIGNED
     T-StrTypeOppdat AT ROW 19.62 COL 121.4
     T-StrTypeNye AT ROW 19.62 COL 124.8
     T-StrTypeOverskriv AT ROW 19.62 COL 128.2
     T-StrType AT ROW 19.71 COL 90.6
     FI-Lev AT ROW 20.76 COL 29.8 COLON-ALIGNED
     B-Lev AT ROW 20.76 COL 84.8
     B-ILev AT ROW 20.76 COL 94
     B-LevOppdat AT ROW 20.76 COL 107.6
     B-LevExcel AT ROW 20.76 COL 133
     B-SLev AT ROW 20.76 COL 146.2
     T-Lev AT ROW 20.91 COL 90.6
     T-LevOppdat AT ROW 20.91 COL 121.6
     T-LevNye AT ROW 20.91 COL 125
     T-LevOverskriv AT ROW 20.91 COL 128.4
     FI-LevInd AT ROW 21.95 COL 29.8 COLON-ALIGNED
     B-LevInd AT ROW 21.95 COL 84.8
     B-ILevInd AT ROW 21.95 COL 94
     B-LevIndOppdat AT ROW 21.95 COL 107.6
     B-Lagre AT ROW 1.33 COL 7
     B-LevIndExcel AT ROW 21.95 COL 133
     BUTTON-SokImportHode AT ROW 3.14 COL 85
     B-SLev-13 AT ROW 21.95 COL 146
     B-Angre AT ROW 1.33 COL 17
     T-LevIndOppdat AT ROW 22 COL 121.6
     T-LevIndNye AT ROW 22 COL 125
     B-Ny AT ROW 1.33 COL 2
     B-Exit AT ROW 1.29 COL 154.6
     T-LevIndOverskriv AT ROW 22 COL 128.4
     T-LevInd AT ROW 22.1 COL 90.6
     Btn_Help AT ROW 1.29 COL 149.6
     FI-Behandler AT ROW 23.62 COL 3 NO-LABEL
     RECT-18 AT ROW 2.67 COL 2
     RECT-19 AT ROW 23.38 COL 2
     RECT-20 AT ROW 1.1 COL 1
     RECT-21 AT ROW 2.48 COL 1
     RECT-22 AT ROW 9.67 COL 2
     RECT-23 AT ROW 6.91 COL 2
     RECT-24 AT ROW 6.91 COL 106.6
     RECT-25 AT ROW 6.91 COL 132
     RECT-26 AT ROW 6.91 COL 145.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.6 BY 25.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import av FlexiCON bestillingsfil"
         HEIGHT             = 25.24
         WIDTH              = 158.6
         MAX-HEIGHT         = 34.62
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.62
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON B-Angre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Artikkler IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-AvbrytImport:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Best IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-BildeKatalog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Farge IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Katalog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Kategori IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Lev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-LevInd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Material IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Sesong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Start IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-StartOppdat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-StrType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Val IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Vm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Artikkel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Batch IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BatchNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Behandler IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Best IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BildeKatalog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Farge IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Katalog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kategori IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Lev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevInd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Material IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Valuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Vm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Artikkel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-ArtikkelOverskriv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Best IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-BestOverskriv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Farge IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Hg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Kategori IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Lev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-LevInd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Material IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Sesong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-StrType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Valuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Vm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 24.81
       COLUMN          = 3
       HEIGHT          = .71
       WIDTH           = 155
       HIDDEN          = no
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */
      CtrlFrame:MOVE-AFTER(FI-Behandler:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import av FlexiCON bestillingsfil */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av FlexiCON bestillingsfil */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Angre C-Win
ON CHOOSE OF B-Angre IN FRAME DEFAULT-FRAME
DO:
    /*
  if wModus = "NY" then
    do:
      find last Lister no-lock where
        Lister.ListeType = wListeType and
        Lister.Eier      = userid("dictdb") and
        Lister.ListeNr   > 0 no-error.

      assign
        wListerRecid = if available Lister 
                         then recid(Lister)
                         else ?
        FI-ListeNr:sensitive = false.
        
      if available Lister then
        do:
          run VisListe.
          RUN DefaultKnapper.
        end.
      else do:
        run DefaultVerdier.
        RUN RensSkjerm.
        apply "choose":U to B-Ny in frame DEFAULT-FRAME.
      end.
    end.
    
  else do:
    run VisListe.
    RUN DefaultKnapper.
  end.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ArtBasExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ArtBasExcel C-Win
ON CHOOSE OF B-ArtBasExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Artikkel",
                "#SÄSONG,#LEVKOD,#LEVART,#KAT,#STATGRP,#VM,#VMTEXT,#STLKINT,#BESK,#ÖVRIGT," +     
                "#ANNONS,#FRIA,#BILD,#LEVTID1,#LEVTID2,#KOLLKOD,#GRUPPART,#MINSTA,#NETTO,#RABATT," +    
                "#BILDNAMN,#ARTKOD,#ARTGRP,#TEMPGRP,#GRUPPARTTYP,#LÄST,#KLACK,#INNERSULA," +
                "#YTTERSULA,#FODER",
                "finnartikkel.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkel C-Win
ON CHOOSE OF B-Artikkel IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportArtikkler.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ArtikkelOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ArtikkelOppdat C-Win
ON CHOOSE OF B-ArtikkelOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
  IF lBekreft THEN
  DO:
      ASSIGN
          lSvar2 = FALSE
          .
      MESSAGE "Skal oppdatering starte?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
      IF lSvar2 = FALSE then
          RETURN.
  END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Artikkel") THEN
  DO:
    RUN import-flexi-artikkler.w PERSISTENT SET hProcHandle[1] 
        (INPUT INPUT FI-BatchNr, "Artikkel").
    RUN SettPara IN hProcHandle[1] (INPUT INPUT T-ArtikkelNye, INPUT INPUT T-ArtikkelOverskriv).
    RUN StartImport IN hProcHandle[1].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkler C-Win
ON CHOOSE OF B-Artikkler IN FRAME DEFAULT-FRAME /* Button 4 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Artikkel    
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Artikkel
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvbrytImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvbrytImport C-Win
ON CHOOSE OF B-AvbrytImport IN FRAME DEFAULT-FRAME /* Avbryt import */
DO:
  ASSIGN
    wStopp = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Best
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Best C-Win
ON CHOOSE OF B-Best IN FRAME DEFAULT-FRAME /* B best 2 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Best
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Best 
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestExcel C-Win
ON CHOOSE OF B-BestExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("BEST",
                "#SÄSONG,#LEVKOD,#LEVART,#LEVTID,#MTRL,#FÄRG,#SORT,#SST,#ANTAL,#PAR," + 
                "#APRIS,#BESTNR,#TYP,#BUNT,#INPRIS,#KURS,#RABATT,#VALUTA,#ARTGRP," +
                "#ORDERNR,#EXPLOK,BESTDATO,BESTTID",
                "finnbest.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BestOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestOppdat C-Win
ON CHOOSE OF B-BestOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "BEST") THEN
  DO:
    RUN import-flexi-best.w PERSISTENT SET hProcHandle[2] 
        (INPUT INPUT FI-BatchNr, "BEST").
    RUN SettPara IN hProcHandle[2] (INPUT INPUT T-BestNye, INPUT INPUT T-BestOverskriv).
    RUN StartImport IN hProcHandle[2].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BildeKatalog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BildeKatalog C-Win
ON CHOOSE OF B-BildeKatalog IN FRAME DEFAULT-FRAME /* ImportBatch */
DO:
/* a test/demo program */
 
RUN BrowseForFolder.p ("Velg katalog hvor bildene ligger",
                       OUTPUT cFolder, 
                       OUTPUT lCanceled).
IF lCanceled = TRUE THEN
    RETURN NO-APPLY.
ASSIGN
    FI-BildeKatalog              = cFolder
    FI-BildeKatalog:SCREEN-VALUE = FI-BildeKatalog
    B-Start:SENSITIVE            = TRUE
    B-StartOppdat:SENSITIVE      = TRUE
    .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Farge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Farge C-Win
ON CHOOSE OF B-Farge IN FRAME DEFAULT-FRAME /* B best 7 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Farge
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Farge
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FargeInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FargeInn C-Win
ON CHOOSE OF B-FargeInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Farge") THEN
  DO:
    RUN import-flexi-farge.w PERSISTENT SET hProcHandle[7] 
        (INPUT INPUT FI-BatchNr, "Farge").
    RUN SettPara IN hProcHandle[7] (INPUT INPUT T-FargNye, INPUT INPUT T-FargOverskriv).
    RUN StartImport IN hProcHandle[7].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FargExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FargExcel C-Win
ON CHOOSE OF B-FargExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Farge",
                "#KOD,#GRFÄRG,#BESK,#EGENFÄRG",
                "finnfarge.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Hg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Hg C-Win
ON CHOOSE OF B-Hg IN FRAME DEFAULT-FRAME /* B best 3 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Hg
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Hg
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HgExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgExcel C-Win
ON CHOOSE OF B-HgExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("HuvGr",
                "#KOD,#BESK",
                "finnhg.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HgInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgInn C-Win
ON CHOOSE OF B-HgInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "HuvGr") THEN
  DO:
    RUN import-flexi-hg.w PERSISTENT SET hProcHandle[3] 
        (INPUT INPUT FI-BatchNr, "HuvGr").
    RUN SettPara IN hProcHandle[3] (INPUT INPUT T-HgNye, INPUT INPUT T-HgOverskriv).
    RUN StartImport IN hProcHandle[3].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IBest C-Win
ON CHOOSE OF B-IBest IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportBest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IFarge C-Win
ON CHOOSE OF B-IFarge IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportFarge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IHg C-Win
ON CHOOSE OF B-IHg IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportHg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IHg-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IHg-2 C-Win
ON CHOOSE OF B-IHg-2 IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportVg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IKategori C-Win
ON CHOOSE OF B-IKategori IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportKategori.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ILev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ILev C-Win
ON CHOOSE OF B-ILev IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportLev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ILevInd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ILevInd C-Win
ON CHOOSE OF B-ILevInd IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportLevInd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IMaterial C-Win
ON CHOOSE OF B-IMaterial IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportMaterial.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ISasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ISasong C-Win
ON CHOOSE OF B-ISasong IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportSesong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IStrType C-Win
ON CHOOSE OF B-IStrType IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportStrType.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IValuta C-Win
ON CHOOSE OF B-IValuta IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  IF lBekreft = TRUE THEN
    RUN settsesong.p (INPUT-OUTPUT cImpSesong).
  RUN ImportValuta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-IVm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-IVm C-Win
ON CHOOSE OF B-IVm IN FRAME DEFAULT-FRAME /* Les inn.. */
DO:
  RUN ImportVm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Katalog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Katalog C-Win
ON CHOOSE OF B-Katalog IN FRAME DEFAULT-FRAME /* ImportBatch */
DO:
/* a test/demo program */
 
RUN BrowseForFolder.p ("Velg katalog hvor importfilene ligger",
                       OUTPUT cFolder, 
                       OUTPUT lCanceled).
IF lCanceled = TRUE THEN
    RETURN NO-APPLY.
ASSIGN
    FI-Katalog              = cFolder
    FI-Katalog:SCREEN-VALUE = FI-Katalog
    B-Start:SENSITIVE       = TRUE
    .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kategori C-Win
ON CHOOSE OF B-Kategori IN FRAME DEFAULT-FRAME /* B best 5 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Kategori 
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Kategori 
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KategoriExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KategoriExcel C-Win
ON CHOOSE OF B-KategoriExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Kategori",
                "#KOD,#BESK",
                "finnkategori.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KategoriInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KategoriInn C-Win
ON CHOOSE OF B-KategoriInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Kategori") THEN
  DO:
    RUN import-flexi-kategori.w PERSISTENT SET hProcHandle[5] 
        (INPUT INPUT FI-BatchNr, "Kategori").
    RUN SettPara IN hProcHandle[5] (INPUT INPUT T-KategoriNye, INPUT INPUT T-KategoriOverskriv).
    RUN StartImport IN hProcHandle[5].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagre C-Win
ON CHOOSE OF B-Lagre IN FRAME DEFAULT-FRAME /* Lagre */
DO:

  RUN LagrePost.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lev C-Win
ON CHOOSE OF B-Lev IN FRAME DEFAULT-FRAME /* B best 12 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Lev
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Lev
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevExcel C-Win
ON CHOOSE OF B-LevExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Lev",
                 "#KOD,#NAMN,#FÖRETAG,#REFERENS,#ADRESS1,#ADRESS2,#POSTNR,#POSTORT,#LAND" +     
                 ",#TELEFON,#TELEFAX,#EMAIL,#WWW,#VALUTA,#LEVVILLKOR,#KUNDKOD,#BETVILLKOR,#RABPROC" +
                 ",#MARG,#KOSTPROC,#KOSTKR,#TULLPROC,#FRAKT1,#FRAKT2,#GROSS,#TYP",
                "finnlevbas.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevInd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevInd C-Win
ON CHOOSE OF B-LevInd IN FRAME DEFAULT-FRAME /* B best 13 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-LevInd
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-LevInd
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevIndExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevIndExcel C-Win
ON CHOOSE OF B-LevIndExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("LevInd",
                "#KOD,#SORTKOD,#STLKINT,#BESK,#STLK1,#STLK2,#STLK3,#STLK4,#STLK5,#STLK6,#STLK7," +  
                "#STLK8,#STLK9,#STLK10,#STLK11,#STLK12,#STLK13,#STLK14,#STLK15,#STLK16,#STLK17," + 
                "#STLK18,#STLK19,#STLK20,#STLK21,#STLK22,#STLK23,#STLK24,#STLK25,#STLK26,#STLK27," + 
                "#STLK28,#STLK29,#STLK30,#STLK31,#STLK32,#STLK33,#STLK34,#STLK35,#STLK36,#STLK37," + 
                "#STLK38,#STLK39,#STLK40,#ERSATT,#SORTBESK",
                "finnlevinn.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevIndOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevIndOppdat C-Win
ON CHOOSE OF B-LevIndOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "LevInd") THEN
  DO:
    RUN import-flexi-levind.w PERSISTENT SET hProcHandle[13] 
        (INPUT INPUT FI-BatchNr, "LevInd").
    RUN SettPara IN hProcHandle[13] (INPUT INPUT T-LevIndNye, INPUT INPUT T-LevIndOverskriv).
    RUN StartImport IN hProcHandle[13].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevOppdat C-Win
ON CHOOSE OF B-LevOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Lev") THEN
  DO:
    RUN import-flexi-lev.w PERSISTENT SET hProcHandle[12] 
        (INPUT INPUT FI-BatchNr, "Lev").
    RUN SettPara IN hProcHandle[12] (INPUT INPUT T-LevNye, INPUT INPUT T-LevOverskriv).
    RUN StartImport IN hProcHandle[12].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Material
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Material C-Win
ON CHOOSE OF B-Material IN FRAME DEFAULT-FRAME /* B best 8 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Material
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Material
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MaterialExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MaterialExcel C-Win
ON CHOOSE OF B-MaterialExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Material",
                "#KOD,#BESK",
                "finnmaterial.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MaterialInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MaterialInn C-Win
ON CHOOSE OF B-MaterialInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Material") THEN
  DO:
    RUN import-flexi-material.w PERSISTENT SET hProcHandle[8] 
        (INPUT INPUT FI-BatchNr, "Material").
    RUN SettPara IN hProcHandle[8] (INPUT INPUT T-MaterialNye, INPUT INPUT T-MaterialOverskriv).
    RUN StartImport IN hProcHandle[8].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
    assign 
      rRecid = ?.
    run d-vimporthode.w (input-output rRecid,"Ny").
    if return-value = "AVBRYT" then
    DO:
      /*
      ASSIGN
        FI-Katalog:SENSITIVE = FALSE
        B-Katalog:SENSITIVE  = FALSE
        .
      */
      return no-apply.

    END.
    find ImportHode no-lock where
      recid(ImportHode) = rRecid no-error.
    DISPLAY
      ImportHode.BatchNr       @ FI-BatchNr
      ImportHode.Beskrivelse   @ FI-Batch
      ImportHode.ImportKatalog @ FI-Katalog
      ImportHode.BildeKatalog  @ FI-BildeKatalog
    WITH FRAME Default-Frame.

    ASSIGN
        FI-Katalog:SENSITIVE      = TRUE
        B-Katalog:SENSITIVE       = TRUE
        FI-BildeKatalog:SENSITIVE = TRUE
        B-BildeKatalog:SENSITIVE  = TRUE
        .

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong C-Win
ON CHOOSE OF B-Sesong IN FRAME DEFAULT-FRAME /* B best 6 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Sesong
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Sesong   
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongExcel C-Win
ON CHOOSE OF B-SesongExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Sesong",
                "#KOD,#BESK,#FRÅNDAT,#TILLDAT",
                "finnSesong.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongInn C-Win
ON CHOOSE OF B-SesongInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Sesong") THEN
  DO:
    RUN import-flexi-sesong.w PERSISTENT SET hProcHandle[6] 
        (INPUT INPUT FI-BatchNr, "Sesong").
    RUN SettPara IN hProcHandle[6] (INPUT INPUT T-SesongNye, INPUT INPUT T-SesongOverskriv).
    RUN StartImport IN hProcHandle[6].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SjekkFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SjekkFil C-Win
ON CHOOSE OF B-SjekkFil IN FRAME DEFAULT-FRAME /* Sjekk filnavn */
DO:
  RUN SjekkFil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Win
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Slett kolleksjonsliste */
DO:
  lSvar = FALSE.
  MESSAGE "Skal importbuffer nullstilles?"
      VIEW-AS ALERT-BOX question BUTTONS YES-NO TITLE "Bekreft"
      UPDATE lSvar.
  IF lSvar THEN
  DO:
      FOR EACH importLinje WHERE
          ImportLinje.BatchNr = INPUT FI-BatchNr:

          FOR EACH ImportDetalj OF ImportLinje:
              DELETE ImportDetalj.
          END.
          DELETE ImportLinje.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev C-Win
ON CHOOSE OF B-SLev IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Lev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-10 C-Win
ON CHOOSE OF B-SLev-10 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "HuvGr").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-11 C-Win
ON CHOOSE OF B-SLev-11 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "BEST").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-12 C-Win
ON CHOOSE OF B-SLev-12 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Artikkel").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-13 C-Win
ON CHOOSE OF B-SLev-13 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "LevInd").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-2 C-Win
ON CHOOSE OF B-SLev-2 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Vg").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-3 C-Win
ON CHOOSE OF B-SLev-3 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Kategori").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-4 C-Win
ON CHOOSE OF B-SLev-4 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Sesong").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-5 C-Win
ON CHOOSE OF B-SLev-5 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Farge").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-6 C-Win
ON CHOOSE OF B-SLev-6 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Material").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-7 C-Win
ON CHOOSE OF B-SLev-7 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Varemerke").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-8 C-Win
ON CHOOSE OF B-SLev-8 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "Valuta").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SLev-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SLev-9 C-Win
ON CHOOSE OF B-SLev-9 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  RUN SlettBuffer (INPUT "StrType").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start import */
DO:
  ASSIGN  
    wStopp                = FALSE
    B-AvbrytImport:HIDDEN = FALSE
    lBekreft              = FALSE
    cErrLst               = ""
    .

  RUN settsesong.p (INPUT-OUTPUT cImpSesong).
  IF RETURN-VALUE = "AVBRYT" 
      THEN RETURN.
  RUN StartImport.

  IF cErrLst <> "" THEN
      MESSAGE "Ukjente felt:" SKIP
              cErrLst
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  ASSIGN  
    wStopp                = FALSE
    B-AvbrytImport:HIDDEN = TRUE
    lBekreft              = TRUE
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StartOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartOppdat C-Win
ON CHOOSE OF B-StartOppdat IN FRAME DEFAULT-FRAME /* Start oppdatering */
DO:
  ASSIGN  
    wStopp                = FALSE
    B-AvbrytImport:HIDDEN = FALSE
    lBekreft              = FALSE
    cErrLst               = ""
    .

  RUN StartOppdat.

  IF cErrLst <> "" THEN
      MESSAGE "Oppdateringsfeil:" SKIP
              cErrLst
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  ASSIGN  
    wStopp                = FALSE
    B-AvbrytImport:HIDDEN = TRUE
    lBekreft              = TRUE
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StrType C-Win
ON CHOOSE OF B-StrType IN FRAME DEFAULT-FRAME /* B best 11 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-StrType
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-StrType
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StrTypeExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StrTypeExcel C-Win
ON CHOOSE OF B-StrTypeExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("StrType",
                "#KOD,#BESK,#STLK1,#STLK2,#STLK3,#STLK4,#STLK5,#STLK6,#STLK7,#STLK8,#STLK9," +
                "#STLK10,#STLK11,#STLK12,#STLK13,#STLK14,#STLK15,#STLK16,#STLK17,#STLK18," + 
                "#STLK19,#STLK20,#STLK21,#STLK22,#STLK23,#STLK24,#STLK25,#STLK26,#STLK27," +
                "#STLK28,#STLK29,#STLK30,#STLK31,#STLK32,#STLK33,#STLK34,#STLK35,#STLK36," +
                "#STLK37,#STLK38,#STLK39,#STLK40",
                "finnstrtype.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StrTypeOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StrTypeOppdat C-Win
ON CHOOSE OF B-StrTypeOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "StrType") THEN
  DO:
    RUN import-flexi-strtype.w PERSISTENT SET hProcHandle[11] 
        (INPUT INPUT FI-BatchNr, "StrType").
    RUN SettPara IN hProcHandle[11] (INPUT INPUT T-StrTypeNye, INPUT INPUT T-StrTypeOverskriv).
    RUN StartImport IN hProcHandle[11].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Val
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Val C-Win
ON CHOOSE OF B-Val IN FRAME DEFAULT-FRAME /* B best 10 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Valuta
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Valuta
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ValutaExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ValutaExcel C-Win
ON CHOOSE OF B-ValutaExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Valuta",
                "#KOD,#SÄSONG,#BESK,#AKTKURS,#KURSISEK,#ÄNDRING",
                "finnvaluta.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ValutaOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ValutaOppdat C-Win
ON CHOOSE OF B-ValutaOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Valuta") THEN
  DO:
    RUN import-flexi-valuta.w PERSISTENT SET hProcHandle[10] 
        (INPUT INPUT FI-BatchNr, "Valuta").
    RUN SettPara IN hProcHandle[10] (INPUT INPUT T-ValutaNye, INPUT INPUT T-ValutaOverskriv).
    RUN StartImport IN hProcHandle[10].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Vg C-Win
ON CHOOSE OF B-Vg IN FRAME DEFAULT-FRAME /* B best 4 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Vg
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Vg
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgExcel C-Win
ON CHOOSE OF B-VgExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Vg",
                "#KOD,#HGRUPP,#BESK,#MOMS,#MARGINAL,#STARTSÄSONG,#SLUTSÄSONG,#GRPARTGRP",
                "finnvargr.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgInn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgInn C-Win
ON CHOOSE OF B-VgInn IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Vg") THEN
  DO:
    RUN import-flexi-vg.w PERSISTENT SET hProcHandle[4] 
        (INPUT INPUT FI-BatchNr, "Vg").
    RUN SettPara IN hProcHandle[4] (INPUT INPUT T-VgNye, INPUT INPUT T-VgOverskriv).
    RUN StartImport IN hProcHandle[4].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Vm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Vm C-Win
ON CHOOSE OF B-Vm IN FRAME DEFAULT-FRAME /* B best 9 */
DO:
  SYSTEM-DIALOG GET-FILE
    FI-Vm
    INITIAL-DIR "."
    RETURN-TO-START-DIR
    MUST-EXIST
    TITLE "Angi eller merk fil som skal importeres."
    .
  DISPLAY
    FI-Vm
  WITH FRAME Default-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VmExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VmExcel C-Win
ON CHOOSE OF B-VmExcel IN FRAME DEFAULT-FRAME /* Eksport.. */
DO:

  RUN Utskrift ("Varemerke",
                "#KOD,#LEVKOD,#BESK,#KOSTNAD",
                "finnvaremerke.r"
                ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VmOppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VmOppdat C-Win
ON CHOOSE OF B-VmOppdat IN FRAME DEFAULT-FRAME /* Oppdater... */
DO:
    IF lBekreft THEN
    DO:
        ASSIGN
            lSvar2 = FALSE
            .
        MESSAGE "Skal oppdatering starte?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar2.
        IF lSvar2 = FALSE then
            RETURN.
    END.
  IF CAN-FIND(FIRST ImportLinje WHERE
                    ImportLinje.BatchNr = INPUT FI-BatchNr AND
                    ImportLinje.Tabell  = "Varemerke") THEN
  DO:
    RUN import-flexi-vm.w PERSISTENT SET hProcHandle[9] 
        (INPUT INPUT FI-BatchNr, "Varemerke").
    RUN SettPara IN hProcHandle[9] (INPUT INPUT T-VmNye, INPUT INPUT T-VmOverskriv).
    RUN StartImport IN hProcHandle[9].
  END.
  ELSE
      MESSAGE "Importbuffer er tomt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokImportHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokImportHode C-Win
ON CHOOSE OF BUTTON-SokImportHode IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-BatchNr
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-BatchNr
    &Program     = d-bimporthode.w
    &ParamType   = "Input"
    &ExtraParam  = "'S'"
    &Frame       = Default-Frame
    &PostRun     = "find ImportHode no-lock where
                    recid(ImportHode) = int(return-value) no-error."
    &OptDisp     = "ImportHode.Beskrivelse   @ FI-Batch 
                    ImportHode.ImportKatalog @ FI-Katalog
                    ImportHode.BildeKatalog  @ FI-BildeKatalog
                    "
  }   
  ASSIGN
      FI-Katalog:SENSITIVE      = TRUE
      B-Katalog:SENSITIVE       = TRUE
      FI-BildeKatalog:SENSITIVE = TRUE
      B-BildeKatalog:SENSITIVE  = TRUE
      B-Start:SENSITIVE    = IF ImportHode.ImportKatalog <> ""
                               THEN TRUE
                               ELSE FALSE
      B-StartOppdat:SENSITIVE    = IF ImportHode.ImportKatalog <> ""
                               THEN TRUE
                               ELSE FALSE
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BildeKatalog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BildeKatalog C-Win
ON ANY-PRINTABLE OF FI-BildeKatalog IN FRAME DEFAULT-FRAME /* Bildekatalog */
DO:
  IF SELF:SCREEN-VALUE = "" THEN
      ASSIGN
          B-Start:SENSITIVE = FALSE
          B-StartOppdat:SENSITIVE = FALSE 
          .
  ELSE
      assign
          B-Start:SENSITIVE = TRUE
          B-StartOppdat:SENSITIVE = TRUE
          .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Katalog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Katalog C-Win
ON ANY-PRINTABLE OF FI-Katalog IN FRAME DEFAULT-FRAME /* Importkatalog */
DO:
  IF SELF:SCREEN-VALUE = "" THEN
      ASSIGN
          B-Start:SENSITIVE = FALSE
          B-StartOppdat:SENSITIVE = FALSE
          .
  ELSE
      assign
          B-Start:SENSITIVE = TRUE
          B-StartOppdat:SENSITIVE = TRUE
          .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Importrutine FleciCON"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end.
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Sjekker om det kjøres på en LapTop */
if valid-handle(wLibHAndle) then
  run SjekkLapTop in wLibHandle (output lLapTop).

ASSIGN
  lBekreft    = TRUE
  .

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  FIND LAST ImportHode NO-LOCK NO-ERROR.
  IF AVAILABLE ImportHode then
      rBatchRowId = rowid(ImportHode).
  ELSE
      rBatchRowId = ?.

  RUN enable_UI.
  {lng.i}

  ASSIGN  
    B-AvbrytImport:HIDDEN = TRUE
    chCtrlFrame:Visible   = false.

  RUN VisPost.

  APPLY "ENTRY":U TO FI-Artikkel IN FRAME default-frame.
  STATUS default "Angi filnavn (full sti m/ekstent) på importfil.".
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-imp-flexicon.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-imp-flexicon.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY FI-BatchNr FI-Batch FI-Katalog FI-BildeKatalog FI-Artikkel T-Artikkel 
          T-ArtikkelOppdat T-ArtikkelNye T-ArtikkelOverskriv FI-Best T-Best 
          T-BestOppdat T-BestNye T-BestOverskriv FI-Hg T-HgOppdat T-HgNye 
          T-HgOverskriv T-Hg T-VgOppdat T-VgNye T-VgOverskriv FI-Vg T-Vg 
          FI-Kategori T-KategoriOppdat T-KategoriNye T-KategoriOverskriv 
          T-Kategori FI-Sesong T-SesongOppdat T-SesongNye T-SesongOverskriv 
          T-Sesong FI-Farge T-FargOppdat T-FargNye T-FargOverskriv T-Farge 
          FI-Material T-MaterialOppdat T-MaterialNye T-MaterialOverskriv 
          T-Material FI-Vm T-VmOppdat T-VmNye T-VmOverskriv T-Vm FI-Valuta 
          T-ValutaOppdat T-ValutaNye T-ValutaOverskriv T-Valuta FI-StrType 
          T-StrTypeOppdat T-StrTypeNye T-StrTypeOverskriv T-StrType FI-Lev T-Lev 
          T-LevOppdat T-LevNye T-LevOverskriv FI-LevInd T-LevIndOppdat 
          T-LevIndNye T-LevIndOverskriv T-LevInd FI-Behandler 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Slett B-AvbrytImport B-SjekkFil B-ArtBasExcel B-SLev-12 B-Artikkel 
         B-ArtikkelOppdat T-ArtikkelOppdat T-ArtikkelNye B-IBest B-BestOppdat 
         B-BestExcel B-SLev-11 T-BestOppdat T-BestNye B-HgInn B-HgExcel 
         B-SLev-10 B-IHg T-HgOppdat T-HgNye T-HgOverskriv B-VgInn T-VgOppdat 
         T-VgNye T-VgOverskriv B-VgExcel B-SLev-2 B-IHg-2 B-KategoriInn 
         B-KategoriExcel B-SLev-3 B-IKategori T-KategoriOppdat T-KategoriNye 
         T-KategoriOverskriv B-SesongInn B-SesongExcel B-SLev-4 B-ISasong 
         T-SesongOppdat T-SesongNye T-SesongOverskriv B-FargeInn B-FargExcel 
         B-SLev-5 B-IFarge T-FargOppdat T-FargNye T-FargOverskriv B-MaterialInn 
         B-MaterialExcel B-SLev-6 B-IMaterial T-MaterialOppdat T-MaterialNye 
         T-MaterialOverskriv B-VmOppdat B-VmExcel B-SLev-7 B-IVm T-VmOppdat 
         T-VmNye T-VmOverskriv B-ValutaOppdat B-ValutaExcel B-SLev-8 B-IValuta 
         T-ValutaOppdat T-ValutaNye T-ValutaOverskriv B-IStrType 
         B-StrTypeOppdat B-StrTypeExcel B-SLev-9 T-StrTypeOppdat T-StrTypeNye 
         T-StrTypeOverskriv B-ILev B-LevOppdat B-LevExcel B-SLev T-LevOppdat 
         T-LevNye T-LevOverskriv B-ILevInd B-LevIndOppdat B-Lagre B-LevIndExcel 
         BUTTON-SokImportHode B-SLev-13 T-LevIndOppdat T-LevIndNye B-Ny B-Exit 
         T-LevIndOverskriv Btn_Help RECT-18 RECT-19 RECT-20 RECT-21 RECT-22 
         RECT-23 RECT-24 RECT-25 RECT-26 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportArtikkler C-Win 
PROCEDURE ImportArtikkler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL artikel
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #SÄSONG 002
  #LEVKOD 228
  #LEVART 43811
  #KAT 1
  #STATGRP 
  #VM 
  #VMTEXT 
  #STLKINT 01
  #BESK 
  #ÖVRIGT 
  #ANNONS 0
  #FRIA 0
  #BILD 
  #LEVTID1 081
  #LEVTID2 
  #KOLLKOD 0
  #GRUPPART 1
  #MINSTA 27
  #NETTO 0
  #RABATT 0
  #BILDNAMN 
  #ARTKOD 
  #ARTGRP 34
  #TEMPGRP 
  #GRUPPARTTYP E
  #LÄST 
  #KLACK 
  #INNERSULA 
  #YTTERSULA 
  #FODER 
  #ARTVAR_MTRL 10
  #ARTVAR_MTRLTEXT Vesuvio
  #ARTVAR_FÄRG 10
  #ARTVAR_FÄRGTEXT 
  #ARTVAR_LEVFÄRG Nero993
  #ARTVAR_KOLLKOD 0
  #ARTVAR_ARTKOD 
  #ARTVAR_LEVMTRL 
  #ARTVAR_PRIS_TYP A
  #ARTVAR_PRIS_SORTTILL 3154
  #ARTVAR_PRIS_BRUTTO 25933,3333
  #ARTVAR_PRIS_VALUTA ITL
  #ARTVAR_PRIS_NETTO 132
  #ARTVAR_PRIS_UTPRIS 0
  #ARTSORT_SORT U187
  #ARTSORT_ARTGRP 
  #ARTSORT_VM 
  #ARTSORT_VMTEXT 
    
------------------------------------------------------------------------------*/
  /* Import av Hovedgrupper */
  {imp-flexicon1.i
      &Subjekt = "Artikkler"
      &Tabell  = "Artikkel"
      &Sjekk   = "T-Artikkel"
      &Kode    = "#SÄSONG"
      &FilNavn = "FI-Artikkel"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' ')).
                 "
      &Case2   = "WHEN '#SÄSONG'               THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LEVKOD'               THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LEVART'               THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#KAT'                  THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#STATGRP'              THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#VM'                   THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#VMTEXT'               THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#STLKINT'              THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'                 THEN ASSIGN ImportLinje.Felt[ 9] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#ÖVRIGT'               THEN ASSIGN ImportLinje.Felt[10] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#ANNONS'               THEN ASSIGN ImportLinje.Felt[11] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#FRIA'                 THEN ASSIGN ImportLinje.Felt[12] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BILD'                 THEN ASSIGN ImportLinje.Felt[13] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LEVTID1'              THEN ASSIGN ImportLinje.Felt[14] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LEVTID2'              THEN ASSIGN ImportLinje.Felt[15] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#KOLLKOD'              THEN ASSIGN ImportLinje.Felt[16] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#GRUPPART'             THEN ASSIGN ImportLinje.Felt[17] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#MINSTA'               THEN ASSIGN ImportLinje.Felt[18] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#NETTO'                THEN ASSIGN ImportLinje.Felt[19] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#RABATT'               THEN ASSIGN ImportLinje.Felt[20] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BILDNAMN'             THEN ASSIGN ImportLinje.Felt[21] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#ARTKOD'               THEN ASSIGN ImportLinje.Felt[22] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#ARTGRP'               THEN ASSIGN ImportLinje.Felt[23] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#TEMPGRP'              THEN ASSIGN ImportLinje.Felt[24] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#GRUPPARTTYP'          THEN ASSIGN ImportLinje.Felt[25] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LÄST'                 THEN ASSIGN ImportLinje.Felt[26] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#KLACK'                THEN ASSIGN ImportLinje.Felt[27] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#INNERSULA'            THEN ASSIGN ImportLinje.Felt[28] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#YTTERSULA'            THEN ASSIGN ImportLinje.Felt[29] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#FODER'                THEN ASSIGN ImportLinje.Felt[30] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  "
      &Nullstill = "For each tVariant: Delete tVariant. End.
                    FOR EACH tPris:    DELETE tPris.    END.
                    FOR EACH tSort:    DELETE tSort.    END.
                   "                  
      &Variant = "
    IF entry(1,wLinje,' ') BEGINS '#ARTVAR' THEN
    VARIANT:
    DO:
      IF entry(1,wLinje,' ') = '#ARTVAR_MTRL' THEN
      DO:
        CREATE tVariant.
        ASSIGN
            tVariant.LinjeNr = ImportLinje.LinjeNr
            tVariant.Tabell  = ImportLinje.Tabell
            .
      END.
      CASE entry(1,wLinje,' '):
        WHEN '#ARTVAR_MTRL'     THEN ASSIGN tVariant.MTRL     = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_MTRLTEXT' then ASSIGN tVariant.MTRLTEXT = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_FÄRG'     then ASSIGN tVariant.FARG     = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_FÄRGTEXT' THEN ASSIGN tVariant.FARGTEXT = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)). 
        WHEN '#ARTVAR_LEVFÄRG'  THEN ASSIGN tVariant.LEVFARG  = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_KOLLKOD'  then ASSIGN tVariant.KOLLKOD  = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_ARTKOD'   THEN ASSIGN tVariant.ARTKOD   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_LEVMTRL'  THEN ASSIGN tVariant.LEVMTRL  = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      END CASE.
    END. 
    "
      &Pris = "
    IF entry(1,wLinje,' ') BEGINS '#ARTVAR_PRIS' THEN
    PRIS:
    DO:
      IF entry(1,wLinje,' ') = '#ARTVAR_PRIS_TYP' THEN
      DO:
        CREATE tPris.
        ASSIGN
            tPris.LinjeNr = ImportLinje.LinjeNr
            tPris.Tabell  = ImportLinje.Tabell
            tPris.MTRL    = tVariant.MTRL
            tPris.FARG    = tVariant.FARG
            .
      END.
      CASE entry(1,wLinje,' '):
        WHEN '#ARTVAR_PRIS_TYP'      THEN ASSIGN tPris.TYP      = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_PRIS_SORTTILL' THEN ASSIGN tPris.SORTTILL = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_PRIS_BRUTTO'   then ASSIGN tPris.BRUTTO   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_PRIS_VALUTA'   then ASSIGN tPris.VALUTA   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTVAR_PRIS_NETTO'    THEN ASSIGN tPris.NETTO    = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)). 
        WHEN '#ARTVAR_PRIS_UTPRIS'   THEN ASSIGN tPris.UTPRIS   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
     END CASE.
    END. 
    "
      &Sortiment = "
    IF entry(1,wLinje,' ') BEGINS '#ARTSORT' THEN
    PRIS:
    DO:
      IF entry(1,wLinje,' ') = '#ARTSORT_SORT' THEN
      DO:
        CREATE tSort.
        ASSIGN
            tSort.LinjeNr = ImportLinje.LinjeNr
            tSort.Tabell  = ImportLinje.Tabell
            tSort.MTRL    = tVariant.MTRL
            tSort.FARG    = tVariant.FARG
            .
      END.
      CASE entry(1,wLinje,' '):
        WHEN '#ARTSORT_SORT'   THEN ASSIGN tSort.SORT     = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTSORT_ARTGRP' THEN ASSIGN tSort.ARTGRP   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTSORT_VM'     then ASSIGN tSort.VM       = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
        WHEN '#ARTSORT_VMTEXT' then ASSIGN tsort.VMTEXT   = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
     END CASE.
    END. 
    "
   }

STATUS INPUT "Legger opp detaljer. Vent litt.".
/* Legger opp variantdetaljer. */
ASSIGN
  iSeqNr = 0
  .
FOR EACH tVariant NO-LOCK 
    BREAK BY tVariant.LinjeNr
          BY tVariant.MTRL
          BY tVariant.FARG:
    assign
      iSeqNr = iSeqNr + 1
      .
    FIND ImportDetalj EXCLUSIVE-LOCK WHERE
      ImportDetalj.BatchNr = ImportHode.BatchNr AND 
      ImportDetalj.Tabell  = tVariant.Tabell AND
      ImportDetalj.TYPE    = "VARIANT" AND
      ImportDetalj.LinjeNr = tVariant.LinjeNr AND
      ImportDetalj.SeqNr   = iSeqNr NO-ERROR.
    IF NOT AVAILABLE ImportDetalj THEN
    DO:
      CREATE ImportDetalj.
      ASSIGN
        ImportDetalj.BatchNr = ImportHode.BatchNr
        ImportDetalj.Tabell  = tVariant.Tabell
        ImportDetalj.TYPE    = "VARIANT"
        ImportDetalj.LinjeNr = tVariant.LinjeNr
        ImportDetalj.SeqNr   = iSeqNr
        .
    END.
    ASSIGN
      ImportDetalj.Felt[1] = tVariant.MTRL    
      ImportDetalj.Felt[2] = tVariant.MTRLTEXT
      ImportDetalj.Felt[3] = tVariant.FARG    
      ImportDetalj.Felt[4] = tVariant.FARGTEXT
      ImportDetalj.Felt[5] = tVariant.LEVFARG 
      ImportDetalj.Felt[6] = tVariant.KOLLKOD 
      ImportDetalj.Felt[7] = tVariant.ARTKOD  
      ImportDetalj.Felt[8] = tVariant.LEVMTRL 
      /* Felt[9] oppdateres i oppdateringsrutinen. */
      /* Flagger om bestilling finnes.             */
      .
END.

/* Legger opp prisDetaljer. */
ASSIGN
  iSeqNr = 0
  .
FOR EACH tPris NO-LOCK 
    BREAK BY tPris.LinjeNr
          BY tPris.Typ
          BY tPris.SortTil:
    assign
      iSeqNr = iSeqNr + 1
      .
    FIND ImportDetalj EXCLUSIVE-LOCK WHERE
      ImportDetalj.BatchNr = ImportHode.BatchNr AND 
      ImportDetalj.Tabell  = tPris.Tabell AND
      ImportDetalj.TYPE    = "PRIS" AND
      ImportDetalj.LinjeNr = tPris.LinjeNr AND
      ImportDetalj.SeqNr   = iSeqNr NO-ERROR.
    IF NOT AVAILABLE ImportDetalj THEN
    DO:
      CREATE ImportDetalj.
      ASSIGN
        ImportDetalj.BatchNr = ImportHode.BatchNr
        ImportDetalj.Tabell  = tPris.Tabell
        ImportDetalj.TYPE    = "PRIS"
        ImportDetalj.LinjeNr = tPris.LinjeNr
        ImportDetalj.SeqNr   = iSeqNr
        .
    END.
    ASSIGN
      ImportDetalj.Felt[1] = tPris.MTRL    
      ImportDetalj.Felt[2] = tPris.FARG    
      ImportDetalj.Felt[3] = tPris.TYP
      ImportDetalj.Felt[4] = tPris.SORTTILL
      ImportDetalj.Felt[5] = tPris.BRUTTO  
      ImportDetalj.Felt[6] = tPris.VALUTA  
      ImportDetalj.Felt[7] = tPris.NETTO   
      ImportDetalj.Felt[8] = tPris.UTPRIS
      .
END.

/* Legger opp sortiment. */
ASSIGN
  iSeqNr = 0
  .
FOR EACH tSort NO-LOCK 
    BREAK BY tSort.LinjeNr
          BY tSort.Sort:
    assign
      iSeqNr = iSeqNr + 1
      .
    FIND ImportDetalj EXCLUSIVE-LOCK WHERE
      ImportDetalj.BatchNr = ImportHode.BatchNr AND 
      ImportDetalj.Tabell  = tSort.Tabell AND
      ImportDetalj.TYPE    = "SORT" AND
      ImportDetalj.LinjeNr = tSort.LinjeNr AND
      ImportDetalj.SeqNr   = iSeqNr NO-ERROR.
    IF NOT AVAILABLE ImportDetalj THEN
    DO:
      CREATE ImportDetalj.
      ASSIGN
        ImportDetalj.BatchNr = ImportHode.BatchNr
        ImportDetalj.Tabell  = tSORT.Tabell
        ImportDetalj.TYPE    = "SORT"
        ImportDetalj.LinjeNr = tSort.LinjeNr
        ImportDetalj.SeqNr   = iSeqNr
        .
    END.
    ASSIGN
      ImportDetalj.Felt[1] = tSort.MTRL    
      ImportDetalj.Felt[2] = tSort.FARG
      ImportDetalj.Felt[3] = tSort.SORT  
      ImportDetalj.Felt[4] = tSort.ARTGRP
      ImportDetalj.Felt[5] = tSort.VM    
      ImportDetalj.Felt[6] = tsort.VMTEXT
      .
END.

STATUS INPUT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportBest C-Win 
PROCEDURE ImportBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL Bestilling
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #SÄSONG 002
  #LEVKOD 066
  #LEVART 48JEAN
  #LEVTID 082
  #MTRL 10
  #FÄRG 10
  #SORT B01
  #SST 100
  #ANTAL 2
  #PAR 15
  #APRIS 199
  #BESTNR 175
  #TYP A
  #BUNT 2
  #INPRIS 199
  #KURS 1
  #RABATT 0
  #VALUTA SEK
  #ARTGRP 73
  #ORDERNR 0
  #EXPLOK 1
      
------------------------------------------------------------------------------*/
  /* Import av Hovedgrupper */
  {imp-flexicon1.i
      &Subjekt = "Bestillinger"
      &Tabell  = "Best"
      &Sjekk   = "T-Best"
      &Kode    = "#SÄSONG"
      &FilNavn = "FI-Best"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(2,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "
      WHEN '#SÄSONG'              THEN ASSIGN 
                                        ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))
                                        ImportLinje.Felt[22] = pcDato
                                        ImportLinje.Felt[23] = pcTid
                                        .
      WHEN '#LEVKOD'              THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#LEVART'              THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#LEVTID'              THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#MTRL'                THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#FÄRG'                THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#SORT'                THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#SST'                 THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ANTAL'               THEN ASSIGN ImportLinje.Felt[ 9] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#PAR'                 THEN ASSIGN ImportLinje.Felt[10] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#APRIS'               THEN ASSIGN ImportLinje.Felt[11] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#BESTNR'              THEN ASSIGN ImportLinje.Felt[12] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#TYP'                 THEN ASSIGN ImportLinje.Felt[13] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#BUNT'                THEN ASSIGN ImportLinje.Felt[14] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#INPRIS'              THEN ASSIGN ImportLinje.Felt[15] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#KURS'                THEN ASSIGN ImportLinje.Felt[16] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#RABATT'              THEN ASSIGN ImportLinje.Felt[17] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#VALUTA'              THEN ASSIGN ImportLinje.Felt[18] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ARTGRP'              THEN ASSIGN ImportLinje.Felt[19] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ORDERNR'             THEN ASSIGN ImportLinje.Felt[20] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#EXPLOK'              THEN ASSIGN ImportLinje.Felt[21] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportFarge C-Win 
PROCEDURE ImportFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL huvudgrupp
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 36
  #GRFÄRG 3
  #BESK Bordeaux/Svart
  #EGENFÄRG 0
      
------------------------------------------------------------------------------*/
  /* Import av farger */
  {imp-flexicon1.i
      &Subjekt = "Farger"
      &Tabell  = "Farge"
      &Kode    = "#KOD"
      &FilNavn = "FI-Farge"
      &Sjekk   = "T-Farge"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#GRFÄRG'      THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#EGENFÄRG'    THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."
   }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportHg C-Win 
PROCEDURE ImportHg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL huvudgrupp
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 1
  #BESK Herrskor
      
------------------------------------------------------------------------------*/

  /* Import av Hovedgrupper */
  {imp-flexicon1.i
      &Subjekt = "Hovedgrupper"
      &Tabell  = "HuvGr"
      &Kode    = "#KOD"
      &Sjekk   = "T-Hg"
      &FilNavn = "FI-Hg"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'                  THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'                 THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."

   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportKategori C-Win 
PROCEDURE ImportKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL artikel
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 1
  #BESK ALLA
    
------------------------------------------------------------------------------*/
  /* Import av Kategori */
  {imp-flexicon1.i
      &Subjekt = "Kategori"
      &Tabell  = "Kategori"
      &Sjekk   = "T-Kategori"
      &Kode    = "#KOD"
      &FilNavn = "FI-Kategori"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'  THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK' THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportLev C-Win 
PROCEDURE ImportLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL leverandør
  #DATUM 2001-02-06 18:07:47
  
  /* Linjerecord for bunten. */
  #KOD *
  #NAMN FF
  #FÖRETAG Fem Fackmän AB
  #REFERENS 
  #ADRESS1 
  #ADRESS2 
  #POSTNR 
  #POSTORT 
  #LAND SVERIGE
  #TELEFON 
  #TELEFAX 
  #EMAIL 
  #WWW 
  #VALUTA SEK
  #LEVVILLKOR 
  #KUNDKOD 
  #BETVILLKOR 
  #RABPROC 0
  #MARG 0
  #KOSTPROC 0
  #KOSTKR 0
  #TULLPROC 0
  #FRAKT1 0
  #FRAKT2 0
  #GROSS 
  #TYP 
        
------------------------------------------------------------------------------*/
    /* Import av Størrelsestyper */
  {imp-flexicon1.i
      &Subjekt = "Leverandører"
      &Tabell  = "Lev"
      &Sjekk   = "T-Lev"
      &Kode    = "#KOD"
      &FilNavn = "FI-Lev"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "
      WHEN '#KOD'        THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#NAMN'       THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#FÖRETAG'    THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#REFERENS'   THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ADRESS1'    THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ADRESS2'    THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#POSTNR'     THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#POSTORT'    THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#LAND'       THEN ASSIGN ImportLinje.Felt[ 9] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#TELEFON'    THEN ASSIGN ImportLinje.Felt[10] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#TELEFAX'    THEN ASSIGN ImportLinje.Felt[11] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#EMAIL'      THEN ASSIGN ImportLinje.Felt[12] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#WWW'        THEN ASSIGN ImportLinje.Felt[13] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#VALUTA'     THEN ASSIGN ImportLinje.Felt[14] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#LEVVILLKOR' THEN ASSIGN ImportLinje.Felt[15] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#KUNDKOD'    THEN ASSIGN ImportLinje.Felt[16] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#BETVILLKOR' THEN ASSIGN ImportLinje.Felt[17] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#RABPROC'    THEN ASSIGN ImportLinje.Felt[18] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#MARG'       THEN ASSIGN ImportLinje.Felt[19] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#KOSTPROC'   THEN ASSIGN ImportLinje.Felt[20] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#KOSTKR'     THEN ASSIGN ImportLinje.Felt[21] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#TULLPROC'   THEN ASSIGN ImportLinje.Felt[22] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#FRAKT1'     THEN ASSIGN ImportLinje.Felt[23] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#FRAKT2'     THEN ASSIGN ImportLinje.Felt[24] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#GROSS'      THEN ASSIGN ImportLinje.Felt[25] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#TYP'        THEN ASSIGN ImportLinje.Felt[26] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      "                
   }                   
                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportLevInd C-Win 
PROCEDURE ImportLevInd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL leverandørsinndelinger
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 045
  #SORTKOD P9
  #STLKINT 01
  #BESK Automatiskt inläst
  #STLK1 0
  #STLK2 0
  #STLK3 0
  #STLK4 0
  #STLK5 0
  #STLK6 0
  #STLK7 0
  #STLK8 0
  #STLK9 0
  #STLK10 0
  #STLK11 0
  #STLK12 0
  #STLK13 0
  #STLK14 0
  #STLK15 0
  #STLK16 0
  #STLK17 0
  #STLK18 0
  #STLK19 0
  #STLK20 0
  #STLK21 2
  #STLK22 3
  #STLK23 4
  #STLK24 3
  #STLK25 2
  #STLK26 1
  #STLK27 0
  #STLK28 0
  #STLK29 0
  #STLK30 0
  #STLK31 0
  #STLK32 0
  #STLK33 0
  #STLK34 0
  #STLK35 0
  #STLK36 0
  #STLK37 0
  #STLK38 0
  #STLK39 0
  #STLK40 0
  #ERSATT 
  #SORTBESK 
    
------------------------------------------------------------------------------*/
  /* Import av Leverandørsinndelinger */
  {imp-flexicon1.i
      &Subjekt = "Leverandørsinndelinger"
      &Tabell  = "LevInd"
      &Sjekk   = "T-LevInd"
      &Kode    = "#KOD"
      &FilNavn = "FI-LevInd"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "
      WHEN '#KOD'      THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#SORTKOD'  THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLKINT'  THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#BESK'     THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK1'    THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK2'    THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK3'    THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK4'    THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK5'    THEN ASSIGN ImportLinje.Felt[ 9] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK6'    THEN ASSIGN ImportLinje.Felt[10] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK7'    THEN ASSIGN ImportLinje.Felt[11] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK8'    THEN ASSIGN ImportLinje.Felt[12] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK9'    THEN ASSIGN ImportLinje.Felt[13] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK10'   THEN ASSIGN ImportLinje.Felt[14] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK11'   THEN ASSIGN ImportLinje.Felt[15] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK12'   THEN ASSIGN ImportLinje.Felt[16] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK13'   THEN ASSIGN ImportLinje.Felt[17] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK14'   THEN ASSIGN ImportLinje.Felt[18] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK15'   THEN ASSIGN ImportLinje.Felt[19] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK16'   THEN ASSIGN ImportLinje.Felt[20] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK17'   THEN ASSIGN ImportLinje.Felt[21] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK18'   THEN ASSIGN ImportLinje.Felt[22] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK19'   THEN ASSIGN ImportLinje.Felt[23] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK20'   THEN ASSIGN ImportLinje.Felt[24] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK21'   THEN ASSIGN ImportLinje.Felt[25] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK22'   THEN ASSIGN ImportLinje.Felt[26] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK23'   THEN ASSIGN ImportLinje.Felt[27] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK24'   THEN ASSIGN ImportLinje.Felt[28] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK25'   THEN ASSIGN ImportLinje.Felt[29] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK26'   THEN ASSIGN ImportLinje.Felt[30] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK27'   THEN ASSIGN ImportLinje.Felt[31] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK28'   THEN ASSIGN ImportLinje.Felt[32] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK29'   THEN ASSIGN ImportLinje.Felt[33] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK30'   THEN ASSIGN ImportLinje.Felt[34] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK31'   THEN ASSIGN ImportLinje.Felt[35] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK32'   THEN ASSIGN ImportLinje.Felt[36] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK33'   THEN ASSIGN ImportLinje.Felt[37] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK34'   THEN ASSIGN ImportLinje.Felt[38] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK35'   THEN ASSIGN ImportLinje.Felt[39] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK36'   THEN ASSIGN ImportLinje.Felt[40] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK37'   THEN ASSIGN ImportLinje.Felt[41] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK38'   THEN ASSIGN ImportLinje.Felt[42] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK39'   THEN ASSIGN ImportLinje.Felt[43] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK40'   THEN ASSIGN ImportLinje.Felt[44] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#ERSATT'   THEN ASSIGN ImportLinje.Felt[45] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#SORTBESK' THEN ASSIGN ImportLinje.Felt[46] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportMaterial C-Win 
PROCEDURE ImportMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL material
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 10
  #BESK Skinn
      
------------------------------------------------------------------------------*/
  /* Import av Material */
  {imp-flexicon1.i
      &Subjekt = "Material"
      &Tabell  = "Material"
      &Kode    = "#KOD"
      &Sjekk   = "T-Material"
      &FilNavn = "FI-Material"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."
   }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportSesong C-Win 
PROCEDURE ImportSesong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL säsong
  #DATUM 2001-02-06 18:07:47
  
  /* Linjerecord for bunten. */
  #KOD 991
  #BESK Våren 1999
  #FRÅNDAT 
  #TILLDAT       
------------------------------------------------------------------------------*/
  /* Import av Sesonger */
  {imp-flexicon1.i
      &Subjekt = "Sesong"
      &Tabell  = "Sesong"
      &Kode    = "#KOD"
      &Sjekk   = "T-Sesong"
      &FilNavn = "FI-Sesong"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#FRÅNDAT'     THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#TILLDAT'     THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."
  }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportStrType C-Win 
PROCEDURE ImportStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL storleksint
  #DATUM 2001-02-06 18:07:47
  
  /* Linjerecord for bunten. */
  #KOD 01
  #BESK Franska helnummer
  #STLK1 16
  #STLK2 17
  #STLK3 18
  #STLK4 19
  #STLK5 20
  #STLK6 21
  #STLK7 22
  #STLK8 23
  #STLK9 24
  #STLK10 25
  #STLK11 26
  #STLK12 27
  #STLK13 28
  #STLK14 29
  #STLK15 30
  #STLK16 31
  #STLK17 32
  #STLK18 33
  #STLK19 34
  #STLK20 35
  #STLK21 36
  #STLK22 37
  #STLK23 38
  #STLK24 39
  #STLK25 40
  #STLK26 41
  #STLK27 42
  #STLK28 43
  #STLK29 44
  #STLK30 45
  #STLK31 46
  #STLK32 47
  #STLK33 48
  #STLK34 49
  #STLK35 50
  #STLK36 51
  #STLK37 52
  #STLK38 53
  #STLK39 54
  #STLK40 55
      
------------------------------------------------------------------------------*/
    /* Import av Størrelsestyper */
  {imp-flexicon1.i
      &Subjekt = "Størrelsestyper"
      &Tabell  = "StrType"
      &Sjekk   = "T-StrType"
      &Kode    = "#KOD"
      &FilNavn = "FI-StrType"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "
      WHEN '#KOD'    THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#BESK'   THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK1'  THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK2'  THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK3'  THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK4'  THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK5'  THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK6'  THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK7'  THEN ASSIGN ImportLinje.Felt[ 9] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK8'  THEN ASSIGN ImportLinje.Felt[10] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK9'  THEN ASSIGN ImportLinje.Felt[11] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK10' THEN ASSIGN ImportLinje.Felt[12] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK11' THEN ASSIGN ImportLinje.Felt[13] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK12' THEN ASSIGN ImportLinje.Felt[14] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK13' THEN ASSIGN ImportLinje.Felt[15] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK14' THEN ASSIGN ImportLinje.Felt[16] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK15' THEN ASSIGN ImportLinje.Felt[17] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK16' THEN ASSIGN ImportLinje.Felt[18] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK17' THEN ASSIGN ImportLinje.Felt[19] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK18' THEN ASSIGN ImportLinje.Felt[20] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK19' THEN ASSIGN ImportLinje.Felt[21] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK20' THEN ASSIGN ImportLinje.Felt[22] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK21' THEN ASSIGN ImportLinje.Felt[23] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK22' THEN ASSIGN ImportLinje.Felt[24] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK23' THEN ASSIGN ImportLinje.Felt[25] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK24' THEN ASSIGN ImportLinje.Felt[26] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK25' THEN ASSIGN ImportLinje.Felt[27] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK26' THEN ASSIGN ImportLinje.Felt[28] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK27' THEN ASSIGN ImportLinje.Felt[29] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK28' THEN ASSIGN ImportLinje.Felt[30] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK29' THEN ASSIGN ImportLinje.Felt[31] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK30' THEN ASSIGN ImportLinje.Felt[32] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK31' THEN ASSIGN ImportLinje.Felt[33] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK32' THEN ASSIGN ImportLinje.Felt[34] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK33' THEN ASSIGN ImportLinje.Felt[35] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK34' THEN ASSIGN ImportLinje.Felt[36] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK35' THEN ASSIGN ImportLinje.Felt[37] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK36' THEN ASSIGN ImportLinje.Felt[38] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK37' THEN ASSIGN ImportLinje.Felt[39] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK38' THEN ASSIGN ImportLinje.Felt[40] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK39' THEN ASSIGN ImportLinje.Felt[41] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      WHEN '#STLK40' THEN ASSIGN ImportLinje.Felt[42] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
      "
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportValuta C-Win 
PROCEDURE ImportValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL valuta
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD SEK
  #SÄSONG 992
  #BESK Svenska kronor
  #AKTKURS 1
  #KURSISEK 1
  #ÄNDRING 1999-02-01
      
------------------------------------------------------------------------------*/
  /* Import av Valuta */
  {imp-flexicon1.i
      &Subjekt = "Valuta"
      &Tabell  = "Valuta"
      &Kode    = "#KOD"
      &Sjekk   = "T-Valuta"
      &FilNavn = "FI-Valuta"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#SÄSONG'      THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#AKTKURS'     THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#KURSISEK'    THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#ÄNDRING'     THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  "
      &PreSjekk = "if entry(1,wLinje,' ') = '#SÄSONG' then
                  DO:
                   if cImpSesong <> trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)) then
                      NEXT IMP-LOOP.
                  END."
   }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportVg C-Win 
PROCEDURE ImportVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL huvudgrupp
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 10
  #HGRUPP 1
  #BESK Herrboots tunna
  #MOMS 
  #MARGINAL 0
  #STARTSÄSONG 
  #SLUTSÄSONG 
  #GRPARTGRP 
      
------------------------------------------------------------------------------*/
  /* Import av Hovedgrupper */
  {imp-flexicon1.i
      &Subjekt = "Varegrupper"
      &Tabell  = "Vg"
      &Kode    = "#KOD"
      &Sjekk   = "T-Vg"
      &FilNavn = "FI-Vg"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#HGRUPP'      THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#MOMS'        THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#MARGINAL'    THEN ASSIGN ImportLinje.Felt[ 5] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#STARTSÄSONG' THEN ASSIGN ImportLinje.Felt[ 6] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#SLUTSÄSONG'  THEN ASSIGN ImportLinje.Felt[ 7] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#GRPARTGRP'   THEN ASSIGN ImportLinje.Felt[ 8] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))."
   }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportVm C-Win 
PROCEDURE ImportVm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  

  /* Header record for bunten */
  #TABELL varemerke
  #DATUM 2001-02-06 18:07:03
  
  /* Linjerecord for bunten. */
  #KOD 10
  #LEVKOD 
  #BESK Andiamo men
  #KOSTNAD 0
      
------------------------------------------------------------------------------*/
  /* Import av Material */
  {imp-flexicon1.i
      &Subjekt = "Varemerke"
      &Tabell  = "Varemerke"
      &Kode    = "#KOD"
      &Sjekk   = "T-Vm"
      &FilNavn = "FI-Vm"
      &Case1   = "WHEN '#DATUM'   THEN ASSIGN pcDato   = (ENTRY(3,wLinje,' '))
                                              pcTid    = (ENTRY(3,wLinje,' '))."
      &Case2   = "WHEN '#KOD'         THEN ASSIGN ImportLinje.Felt[ 1] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#LEVKOD'      THEN ASSIGN ImportLinje.Felt[ 2] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#BESK'        THEN ASSIGN ImportLinje.Felt[ 3] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  WHEN '#KOSTNAD'     THEN ASSIGN ImportLinje.Felt[ 4] = trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
                  "
   }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
      chCtrlFrame:Visible             = false
      chCtrlFrame:ProgressBar:Min     = 1
      chCtrlFrame:ProgressBar:Max     = 100
      chCtrlFrame:ProgressBar:Value   = 1.
    HIDE FRAME CtrlFrame NO-PAUSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-Win 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bImportHode FOR ImportHode.

  DO TRANSACTION WITH FRAME Default-Frame:
      FIND bImportHode EXCLUSIVE-LOCK WHERE
          ROWID(bImportHode) = ROWID(ImportHode).
      ASSIGN
          FI-Katalog
          FI-BildeKatalog
          ImportHode.ImportKatalog = FI-Katalog
          ImportHode.BildeKatalog  = FI-BildeKatalog
          .

      RELEASE bImportHode.
  END.
  FIND CURRENT ImportHode NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyBatch C-Win 
PROCEDURE NyBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN d-imp-flexicon-ny.w (INPUT-OUTPUT FI-Batch).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "AVBRYT".

  DO TRANSACTION WITH FRAME Default-FRAME:
    FIND LAST ImportHode NO-LOCK NO-ERROR.
    IF AVAILABLE ImportHode THEN
        wBatchNr = ImportHode.BatchNr + 1.
    ELSE
        wBatchNr = 1.

    CREATE ImportHode.
    ASSIGN
        ImportHode.BatchNr = wBatchNr
        ImportHode.Merknad  = FI-Batch
        rBatchRowId          = ROWID(ImportHode)
        .
    RELEASE ImportHode.
  END.

  RUN VisPost.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkFil C-Win 
PROCEDURE SjekkFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME Default-Frame:
  ASSIGN
      T-Hg = IF SEARCH(INPUT FI-Katalog + "/" + FI-Hg) = ?
               THEN FALSE
               ELSE TRUE
      T-Vg = IF SEARCH(INPUT FI-Katalog + "/" + FI-Vg) = ?
               THEN FALSE
               ELSE TRUE
      T-Kategori = IF SEARCH(INPUT FI-Katalog + "/" + FI-Kategori) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Sesong = IF SEARCH(INPUT FI-Katalog + "/" + FI-Sesong) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Farge = IF SEARCH(INPUT FI-Katalog + "/" + FI-Farge) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Material = IF SEARCH(INPUT FI-Katalog + "/" + FI-Material) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Vm = IF SEARCH(INPUT FI-Katalog + "/" + FI-Vm) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Valuta = IF SEARCH(INPUT FI-Katalog + "/" + FI-Valuta) = ?
                     THEN FALSE
                     ELSE TRUE
      T-StrType = IF SEARCH(INPUT FI-Katalog + "/" + FI-StrType) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Lev = IF SEARCH(INPUT FI-Katalog + "/" + FI-Lev) = ?
                     THEN FALSE
                     ELSE TRUE
      T-LevInd = IF SEARCH(INPUT FI-Katalog + "/" + FI-LevInd) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Artikkel = IF SEARCH(INPUT FI-Katalog + "/" + FI-Artikkel) = ?
                     THEN FALSE
                     ELSE TRUE
      T-Best = IF SEARCH(INPUT FI-Katalog + "/" + FI-Best) = ?
                     THEN FALSE
                     ELSE TRUE
      .
  DISPLAY 
      T-Hg
      T-Vg
      T-Kategori
      T-Sesong  
      T-Farge   
      T-Material
      T-Vm      
      T-Valuta  
      T-StrType 
      T-Lev     
      T-LevInd  
      T-Best
      T-Artikkel
  WITH FRAME Default-Frame.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBuffer C-Win 
PROCEDURE SlettBuffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER pcTabell  AS CHAR NO-UNDO.
  
  DEF VAR piAntPoster   AS INT  NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.
  DEF VAR piTotAnt      AS INT  NO-UNDO.

  FRAMESCOOP:
  DO WITH FRAME {&FRAME-NAME}:

  TELL:
  FOR EACH ImportLinje NO-LOCK WHERE
      ImportLinje.BatchNr = INPUT FI-BatchNr AND
      ImportLinje.Tabell  = pcTabell:

    ASSIGN
        piTotAnt = piTotAnt + 1
        .
  END. /* TELL */

  IF piTotAnt > 5 THEN
  DO:
    ASSIGN
      chCtrlFrame:Visible             = true
      chCtrlFrame:ProgressBar:Min     = 1
      chCtrlFrame:ProgressBar:Max     = piTotAnt
      chCtrlFrame:ProgressBar:Value   = 1.
  END.
  
  /* Eksporterer data */
  SLETT:
  FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
      ImportLinje.BatchNr = INPUT FI-BatchNr AND
      ImportLinje.Tabell  = pcTabell:

    ASSIGN
        piAntPoster   = piAntPoster + 1
        .
    
    
    DELETE ImportLinje.

    IF piAntPoster > 5 THEN
    DO:
      if piAntPoster MODULO 10 = 0 then
      DO:
        chCtrlFrame:ProgressBar:Value = IF piAntPoster > wTotAnt
                                          THEN piTotAnt
                                          ELSE piAntPoster.      
      END.
    END.

  END. /* SLETT */

  IF pcTabell = "Artikkel" THEN
  DO:
    FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
        ImportLinje.BatchNr = INPUT FI-BatchNr AND
        ImportLinje.Tabell  = "VARIANT":
      DELETE ImportLinje.
    END. 
    FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
        ImportLinje.BatchNr = INPUT FI-BatchNr AND
        ImportLinje.Tabell  = "PRIS":
      DELETE ImportLinje.
    END. 
    FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
        ImportLinje.BatchNr = INPUT FI-BatchNr AND
        ImportLinje.Tabell  = "SORT":
      DELETE ImportLinje.
    END. 
  END.

  STATUS DEFAULT " ".
  ASSIGN
    chCtrlFrame:Visible             = false
    chCtrlFrame:ProgressBar:Min     = 1
    chCtrlFrame:ProgressBar:Max     = 100
    chCtrlFrame:ProgressBar:Value   = 1.

  END. /* FRAMESCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartImport C-Win 
PROCEDURE StartImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bImportHode FOR ImportHode.

  FRAME-SCOOP:
  DO FOR bImportHode WITH FRAME Default-Frame:
    IF NOT available ImportHode THEN
    DO:
        MESSAGE "ImportHode er ikke opprettet"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
    /* Oppdaterer importkatalog. */
    RUN LagrePost.

    DISPLAY "Import av hovedgrupper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Hg THEN RUN ImportHg.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av varegrupper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Vg THEN RUN ImportVg.
    IF wStopp THEN RETURN "AVBRYT".
    
    DISPLAY "Import av farger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Farge THEN RUN ImportFarge.
    IF wStopp THEN RETURN "AVBRYT".
    
    DISPLAY "Import av kategori pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Kategori THEN RUN ImportKategori.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av materialkoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Material THEN RUN ImportMaterial.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av sesonkoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Sesong THEN RUN ImportSesong.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av valutakoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Valuta THEN RUN ImportValuta.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av varemerker pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Vm THEN RUN ImportVm.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av leverandører pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Lev THEN RUN ImportLev.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av størresestyper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-StrType THEN RUN ImportStrType.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av leverandørinndelinger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-LevInd THEN RUN ImportLevInd.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av artikkler pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Artikkel THEN RUN ImportArtikkler.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Import av bestillinger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-Best THEN RUN ImportBest.
    IF wStopp THEN RETURN "AVBRYT".

  END. /* FRAME-SCOOP */
  DISPLAY 
    " " @ FI-Behandler
  WITH FRAME Default-Frame.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartOppdat C-Win 
PROCEDURE StartOppdat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FRAME-SCOOP:
  DO WITH FRAME Default-Frame:

    DISPLAY "Oppdatering av hovedgrupper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-HgOppdat THEN APPLY "CHOOSE" TO B-HgInn.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av varegrupper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-VgOppdat THEN APPLY "CHOOSE" TO B-VgInn.
    IF wStopp THEN RETURN "AVBRYT".
    
    DISPLAY "Oppdatering av farger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-FargOppdat THEN APPLY "CHOOSE" TO B-FargeInn.
    IF wStopp THEN RETURN "AVBRYT".
    
    DISPLAY "Oppdatering av kategori pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-KategoriOppdat THEN APPLY "CHOOSE" TO B-KategoriInn.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av materialkoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-MaterialOppdat THEN APPLY "CHOOSE" TO B-MaterialInn.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av sesonkoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-SesongOppdat THEN APPLY "CHOOSE" TO B-SesongInn.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av valutakoder pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-ValutaOppdat THEN APPLY "CHOOSE" TO B-ValutaOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av varemerker pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-VmOppdat THEN APPLY "CHOOSE" TO B-VmOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av leverandører pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-LevOppdat THEN APPLY "CHOOSE" TO B-LevOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av størresestyper pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-StrTypeOppdat THEN APPLY "CHOOSE" TO B-StrTypeOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av leverandørinndelinger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-LevIndOppdat THEN APPLY "CHOOSE" TO B-LevIndOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av artikkler pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-ArtikkelOppdat THEN APPLY "CHOOSE" TO B-ArtikkelOppdat.
    IF wStopp THEN RETURN "AVBRYT".

    DISPLAY "Oppdatering av bestillinger pågår." @ FI-Behandler WITH FRAME Default-Frame.
    IF INPUT T-BestOppdat THEN APPLY "CHOOSE" TO B-BestOppdat.
    IF wStopp THEN RETURN "AVBRYT".
  END. /* FRAME-SCOOP */
  DISPLAY 
    " " @ FI-Behandler
  WITH FRAME Default-Frame.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Win 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER pcTabell  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcLabler  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcFinnPrg AS CHAR NO-UNDO.
              
  DEF VAR piAntPoster   AS INT  NO-UNDO.
  DEF VAR cIntervall    AS CHAR NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.
  DEF VAR pi2Loop       AS INT  NO-UNDO.
  DEF VAR pcKonvert     AS CHAR NO-UNDO.
  DEF VAR pcBeskrivelse AS CHAR NO-UNDO.
  DEF VAR piTabell      AS INT  NO-UNDO.
  DEF VAR pcKeyFelt     AS CHAR NO-UNDO.

  FRAMESCOOP:
  DO WITH FRAME {&FRAME-NAME}:

  {sww.i}
  
  /* Systemparameter linjenummer */
  CASE pcTabell:
      WHEN "Artikkel"  THEN piTabell = 6.
      WHEN "BEST"      THEN piTabell = 7.
      WHEN "Lev"       THEN piTabell = 1000.
      WHEN "Sesong"    THEN piTabell = 1001.
      WHEN "HuvGr"     THEN piTabell = 1002.
      WHEN "Vg"        THEN piTabell = 1003.
      WHEN "Farge"     THEN piTabell = 1004.
      WHEN "Material"  THEN piTabell = 1005.
      WHEN "Kategori"  THEN piTabell = 1006.
      WHEN "Varemerke" THEN piTabell = 1007.
      WHEN "StrType"   THEN piTabell = 1008.
      WHEN "LevInd"    THEN piTabell = 1009.
      WHEN "Valuta"    THEN piTabell = 1010.
  END CASE.

  /* Henter parametre for konvertering. */
  {syspara.i 1 2 piTabell cEDB-System}
  {syspar2.i 1 2 piTabell cTabell}

  /* TEST */
  /* EDB-system må velges av bruker */
  ASSIGN
      cEDB-System = ENTRY(1,cEDB-System).

  /* Bygger opp listen slik at det alltid er nok entries.*/
  pi2Loop = num-entries(pcLabler).
  DO piLoop = num-entries(pcLabler) TO 50:
    ASSIGN
        pcLabler = pcLabler + 
                   (IF pcLabler <> ""
                      THEN ","
                      ELSE "") 
        .
    IF NUM-ENTRIES(pcLabler) >= 50 THEN
        LEAVE.
  END.
  
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Import", cExcEkstent, output cFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "IMPORTBUFFER"
    ""
    ""
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "BatchNr"
    "Tabell"
    "LinjeNr"
    "Konvert"
    "Beskrivelse"
    ENTRY( 1,pcLabler)
    ENTRY( 2,pcLabler)
    ENTRY( 3,pcLabler)
    ENTRY( 4,pcLabler)
    ENTRY( 5,pcLabler)
    ENTRY( 6,pcLabler)
    ENTRY( 7,pcLabler)
    ENTRY( 8,pcLabler)
    ENTRY( 9,pcLabler)
    ENTRY(10,pcLabler)
    ENTRY(11,pcLabler)
    ENTRY(12,pcLabler)
    ENTRY(13,pcLabler)
    ENTRY(14,pcLabler)
    ENTRY(15,pcLabler)
    ENTRY(16,pcLabler)
    ENTRY(17,pcLabler)
    ENTRY(18,pcLabler)
    ENTRY(19,pcLabler)
    ENTRY(20,pcLabler)
    ENTRY(21,pcLabler)
    ENTRY(22,pcLabler)
    ENTRY(23,pcLabler)
    ENTRY(24,pcLabler)
    ENTRY(25,pcLabler)
    ENTRY(26,pcLabler)
    ENTRY(27,pcLabler)
    ENTRY(28,pcLabler)
    ENTRY(29,pcLabler)
    ENTRY(30,pcLabler)
    ENTRY(31,pcLabler)
    ENTRY(32,pcLabler)
    ENTRY(33,pcLabler)
    ENTRY(34,pcLabler)
    ENTRY(35,pcLabler)
    ENTRY(36,pcLabler)
    ENTRY(37,pcLabler)
    ENTRY(38,pcLabler)
    ENTRY(39,pcLabler)
    ENTRY(40,pcLabler)
    ENTRY(41,pcLabler)
    ENTRY(42,pcLabler)
    ENTRY(43,pcLabler)
    ENTRY(44,pcLabler)
    ENTRY(45,pcLabler)
    ENTRY(46,pcLabler)
    ENTRY(47,pcLabler)
    ENTRY(48,pcLabler)
    ENTRY(49,pcLabler)
    ENTRY(50,pcLabler)
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH ImportLinje NO-LOCK WHERE
      ImportLinje.BatchNr = INPUT FI-BatchNr AND
      ImportLinje.Tabell  = pcTabell:

    ASSIGN
        piAntPoster   = piAntPoster + 1
        pcBeskrivelse = ""
        .

    CASE pcTabell:
        WHEN "Artikkel"  THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "BEST"      THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Lev"       THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Sesong"    THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "HuvGr"     THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Vg"        THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Farge"     THEN pcKeyFelt = trim(ImportLinje.Felt[ 2]).
        WHEN "Material"  THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Kategori"  THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "Varemerke" THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "StrType"   THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
        WHEN "LevInd"    THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]) + ";" + 
                                          trim(ImportLinje.Felt[ 2]).
        WHEN "Valuta"    THEN pcKeyFelt = trim(ImportLinje.Felt[ 1]).
    END CASE.
    
    /* Sjekker om koden skal konverteres. */
    IF "{&ImpKonv}" = "" THEN
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = cTabell AND 
          ImpKonv.EksterntId = pcKeyFelt NO-ERROR.
      IF AVAILABLE ImpKonv THEN
        ASSIGN
          pcKonvert = ImpKonv.InterntId
          .
      ELSE
        assign
          pcKonvert = ""
          .
      ASSIGN
        pcBeskrivelse = (IF pcKonvert <> ""
                           THEN "** Ugyldig kode **"
                           ELSE "")
        .
      /* Legger på beskrivelse */
      IF search(pcFinnPrg) <> ? THEN
        RUN VALUE(pcFinnPrg) (INPUT pcKonvert, OUTPUT pcBeskrivelse).

      /* Så slipper vi posten så den ikke skaper problemer */
      RELEASE ImpKonv.
    END.
    
    /* BugHider */
    IF pcKonvert = "" THEN
    DO:
      ASSIGN
        pcBeskrivelse = ""
        .
      /* Sjekker om posten finnes i databasen */
      IF search(pcFinnPrg) <> ? THEN
        RUN VALUE(pcFinnPrg) (INPUT pcKeyFelt, OUTPUT pcBeskrivelse).
      IF pcBeskrivelse BEGINS "**" THEN
          pcBeskrivelse = "".
      ELSE
          pcBeskrivelse = "*".
    END.

    EXPORT STREAM sExportFile DELIMITER ";"
      ImportLinje.BatchNr
      ImportLinje.Tabell
      ImportLinje.LinjeNr
      pcKonvert
      pcBeskrivelse
      ImportLinje.Felt[ 1]
      ImportLinje.Felt[ 2]
      ImportLinje.Felt[ 3]
      ImportLinje.Felt[ 4]
      ImportLinje.Felt[ 5]
      ImportLinje.Felt[ 6]
      ImportLinje.Felt[ 7]
      ImportLinje.Felt[ 8]
      ImportLinje.Felt[ 9]
      ImportLinje.Felt[10]
      ImportLinje.Felt[11]
      ImportLinje.Felt[12]
      ImportLinje.Felt[13]
      ImportLinje.Felt[14]
      ImportLinje.Felt[15]
      ImportLinje.Felt[16]
      ImportLinje.Felt[17]
      ImportLinje.Felt[18]
      ImportLinje.Felt[19]
      ImportLinje.Felt[20]
      ImportLinje.Felt[21]
      ImportLinje.Felt[22]
      ImportLinje.Felt[23]
      ImportLinje.Felt[24]
      ImportLinje.Felt[25]
      ImportLinje.Felt[26]
      ImportLinje.Felt[27]
      ImportLinje.Felt[28]
      ImportLinje.Felt[29]
      ImportLinje.Felt[30]
      ImportLinje.Felt[31]
      ImportLinje.Felt[32]
      ImportLinje.Felt[33]
      ImportLinje.Felt[34]
      ImportLinje.Felt[35]
      ImportLinje.Felt[36]
      ImportLinje.Felt[37]
      ImportLinje.Felt[38]
      ImportLinje.Felt[39]
      ImportLinje.Felt[40]
      ImportLinje.Felt[41]
      ImportLinje.Felt[42]
      ImportLinje.Felt[43]
      ImportLinje.Felt[44]
      ImportLinje.Felt[45]
      ImportLinje.Felt[46]
      ImportLinje.Felt[47]
      ImportLinje.Felt[48]
      ImportLinje.Felt[49]
      ImportLinje.Felt[50]
      .
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     

  /* Leser inn filen. */
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:BA2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:BA2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("E:E"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("A:A"):NumberFormat = "# ##0".
  chWorkSheets:Range("F:G"):NumberFormat = "@".

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:BA1"):Merge().
  chWorkSheets:Range("A1:BA1"):HorizontalAlignment = 3.
  /*      
  chWorkSheets:Range("C2:C2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F2:F2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("H2:H2"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("J2:J2"):HorizontalAlignment = 4.   
  */

  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:BA"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:E".
  chWorkSheets:PageSetup:Orientation    = 1.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("D3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */

  STATUS DEFAULT "".

  {swn.i}
  
  END. /* FRAMESCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-Win 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND ImportHode NO-LOCK WHERE
      ROWID(ImportHode) = rBatchRowId NO-ERROR.
  IF NOT AVAILABLE ImportHode THEN
      RETURN "AVBRYT".

  DISPLAY
      ImportHode.BatchNr       @ FI-BatchNr
      ImportHode.Merknad       @ FI-Batch
      ImportHode.ImportKatalog @ FI-Katalog
      ImportHode.BildeKatalog  @ FI-BildeKatalog
  WITH FRAME Default-Frame.
  
  ASSIGN
      FI-Katalog:SENSITIVE      = TRUE
      B-Katalog:SENSITIVE       = TRUE
      FI-BildeKatalog:SENSITIVE = TRUE
      B-BildeKatalog:SENSITIVE  = TRUE
      B-Start:SENSITIVE    = IF ImportHode.ImportKatalog <> ""
                               THEN TRUE
                               ELSE FALSE
      B-StartOppdat:SENSITIVE    = IF ImportHode.ImportKatalog <> ""
                               THEN TRUE
                               ELSE FALSE
      .
  APPLY "CHOOSE":U TO B-Sjekkfil.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

