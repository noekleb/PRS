&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpLevBas NO-UNDO LIKE LevBas.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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

DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_fstperiode AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_dstlinje   AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterTxt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterNr AS CHARACTER  NO-UNDO.
                
/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi,HgBeskr,AvdelingNavn" */
/*        cLabels = "Leverandør,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr,HgBeskr,AvdelingNavn"                     */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2,,"                                                                                                                                                                                                                              */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,,". /* Fält som skall högerjust i XPrint */                                                                                                                                                                            */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
"AntSolgt,BruttoSolgt,VerdiSolgt,MvaVerdi,DbKr,AntRabatt,VerdiRabatt,VVarekost,KjopAnt,KjopVerdi,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi," +
"OvVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi".
ASSIGN cFieldDefs = 
        /*  1 */ "DataObjekt;Leverandør;;1," +
        /*  2 */ "Beskrivelse;Beskrivelse;;," +
        /*  3 */ "PerLinTxt;Periode;;," +
        /*  4 */ "AntSolgt;Solgt;3;1," +
        /* 4b */ "BruttoSolgt;Solgt brutto;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto;2;1," +
        /*  6 */ "Solgt%;Solgt%;2;1," +
        /*  7 */ "MvaVerdi;Mva verdi;2;1," +
        /*  8 */ "DbKr;DbKr;2;1," +
        /*  9 */ "Db%;Db%;2;1," +
        /* 10 */ "AntRabatt;Rabatter;;1," +
        /* 11 */ "VerdiRabatt;Rabatt kr;2;1," +
        /* 12 */ "Rab%;Rab%;2;1," +
        /* 13 */ "VVarekost;VVarekost;2;1," +
        /* 14 */ "KjopAnt;Kjøpt;;1," +
        /* 15 */ "KjopVerdi;Kjøpt kr;2;1," +
        /* 16 */ "ReklAnt;Kunderekl;3;1," +
        /* 17 */ "ReklVerdi;Kunderekl kr;2;1," +
        /* 18 */ "ReklLAnt;Levrekl;3;1," +
        /* 19 */ "ReklLVerdi;Levrekl kr;2;1," +
        /* 20 */ "OvVerdi;Overført kr;;1," +
        /* 21 */ "SvinnAnt;Svinn;;1," +
        /* 22 */ "SvinnVerdi;Svinn kr;;1," +
        /* 23 */ "GjenkjopAnt;Returer;3;1," +
        /* 24 */ "GjenkjopVerdi;Returer kr;2;1," +
        /* 25 */ "AntTilbSolgt;Tilbud;;1," +
        /* 26 */ "VerdiTilbSolgt;Tilbud kr;2;1," +
        /* 27 */ "BrekkAnt;Brekkasje;;1," +
        /* 28 */ "BrekkVerdi;Brekkasje kr;;1".

/* ASSIGN cFelter = "Aar,AntSolgt,VerdiSolgt,MvaVerdi,DbKr,Db%,AntRabatt,AntTilbSolgt,Beskrivelse,BrekkAnt,BrekkVerdi,BrukerID,Butik,DataObjekt,Diverse,DiverseAnt,Diverseverdi,EDato,ETid,GjenkjopAnt,GjenkjopVerdi,Hg,IntAnt,IntVerdi,JustAnt,JustVerdi,KjopAnt,KjopVerdi,LagerAnt,LagerVerdi,NedAnt,NedVerdi,OmlHast,OvAnt,OvVerdi,PerId,PerLinNr,PerLinTxt,PrimoAnt,Primoverdi,RegistrertAv,RegistrertDato,RegistrertTid,ReklAnt,ReklLAnt,ReklLVerdi,ReklVerdi,StTypeId,SvinnAnt,SvinnVerdi,TilbMvaVerdi,TilbVVarekost,TotalPost,Utsolgt%,VerdiRabatt,VerdiTilbSolgt,VisBut,VVarekost"                                                                                                                                                                  */
/*        cLabels = "År,Antall solgt,Verdi solgt,Mva verdi,DbKr,Db%,Antall rabatt,Antall solgt på tilbud,Beskrivelse,Brekkasje,Verdi av brekasje,Bruker,Butikknummer,Dataobjekt,Diverse,,,Endret,Endret tid,Gjenkjøp fra kunde,Verdi av gjenkjøpte varer,,Internt forbruk,Verdi av internt forbruk,Justert antall,Justert verdi,Innkjopt antall,Verdi kjøpt,,,Nedskrevet antall,Verdi nedskrevet,,Overført antall,Verdi av overførte varer,PeriodeId,PeriodeLinje,,,,Registrert av,Registrert dato,Registreringstidspunkt,Kundereklamasjoner,Rekl.lev.antall,Verdi av leveerandørreklamasjoner,Verdi kundereklamasjoner,Statistikktype,Antall svinn,Svinn verdi,Tilb Mva verdi,Varekost tilbudssalg,,,Verdi rabatt,Verdi solgt på tilbud,,Vektet varekost". */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StLinje

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH StLinje NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH StLinje NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain StLinje
&Scoped-define FIRST-TABLE-IN-QUERY-fMain StLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Tg-VisPeriode B-Avdeling B-LevNrBlank ~
RS-SumLevel B-Aktiver B-AvdelingBlank B-HgBlank B-VgBlank Tg-VisPerBut ~
B-SesongBlank B-Sesong B-LevNr B-HuvGr B-VarGr 
&Scoped-Define DISPLAYED-OBJECTS Tg-VisPeriode FI-LevNr RS-SumLevel ~
FI-Avdeling FI-HuvGr FI-VarGr Tg-VisPerBut FI-Sesong 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Sesong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VarGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-SumLevel AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Leverandør", "",
"Leverandør/Avd", "Avd",
"Leverandør/Hg", "Hg",
"Leverandør/Vg", "Vg",
"Leverandør/Sesong", "Sasong"
     SIZE 26 BY 5.29 NO-UNDO.

DEFINE VARIABLE Tg-VisPerBut AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Tg-VisPeriode AS LOGICAL INITIAL no 
     LABEL "Vis periodelinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      StLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Tg-VisPeriode AT ROW 1.19 COL 1
     B-Avdeling AT ROW 2.19 COL 51.8 NO-TAB-STOP 
     FI-LevNr AT ROW 1.19 COL 34.8 COLON-ALIGNED
     B-LevNrBlank AT ROW 1.19 COL 56.8
     RS-SumLevel AT ROW 1.19 COL 72.8 NO-LABEL
     B-Aktiver AT ROW 2.19 COL 1
     FI-Avdeling AT ROW 2.19 COL 34.8 COLON-ALIGNED
     B-AvdelingBlank AT ROW 2.19 COL 56.8
     FI-HuvGr AT ROW 3.19 COL 34.8 COLON-ALIGNED
     B-HgBlank AT ROW 3.19 COL 56.8
     FI-VarGr AT ROW 4.19 COL 34.8 COLON-ALIGNED
     B-VgBlank AT ROW 4.19 COL 56.8
     Tg-VisPerBut AT ROW 4.91 COL 1
     FI-Sesong AT ROW 5.29 COL 34.8 COLON-ALIGNED
     B-SesongBlank AT ROW 5.29 COL 56.8
     B-Sesong AT ROW 5.29 COL 51.8 NO-TAB-STOP 
     B-LevNr AT ROW 1.19 COL 51.8 NO-TAB-STOP 
     B-HuvGr AT ROW 3.19 COL 51.8 NO-TAB-STOP 
     B-VarGr AT ROW 4.19 COL 51.8 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.8 BY 5.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpLevBas T "NEW SHARED" NO-UNDO skotex LevBas
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 5.76
         WIDTH              = 106.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.StLinje"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cSumCols       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpFieldDefs   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEntry AS INTEGER INIT 2   NO-UNDO.
  DEFINE VARIABLE cExtraFelt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilleggsFelter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  IF NOT DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
     OUTPUT cKriterier /* CHARACTER */) THEN
      RETURN.
  
  IF FI-LevNr <> "*" THEN
    ASSIGN pcFeltListe = "LevNr"
           pcVerdier   = FI-LevNr.
  /* Om vi har avd,hg,vg filter och vi inte avgrænsat på Kasserer lægger vi in lev før enklare hantering i SDO */
  IF RS-SumLevel:SCREEN-VALUE = "Sasong" THEN
      ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN "," ELSE "LevNr,") + "Sasong"
             pcVerdier   = pcVerdier   + (IF pcVerdier <> "" THEN CHR(1) ELSE "*" + CHR(1)) + FI-Sesong.
  ELSE IF FI-Avdeling <> "*" OR FI-HuvGr <> "*" OR FI-VarGr <> "*" THEN
    ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN "," ELSE "LevNr,") + IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE IF FI-HuvGr <> "*" THEN "Hg" ELSE "Vg"
           pcVerdier   = pcVerdier   + (IF pcVerdier <> "" THEN CHR(1) ELSE "*" + CHR(1)) + IF FI-Avdeling <> "*" THEN FI-Avdeling ELSE IF FI-HuvGr <> "*" THEN FI-HuvGr ELSE FI-VarGr.
  cTilleggsFelter = "LevNr,AvdelingNr,Hg,Vg,Levnamn,AvdelingNavn,HgBeskr,VgBeskr,Butik,Butnamn,Sasong,SasBeskr".
  PUBLISH "VisTxtBox" ("Søker data......").
  RUN StartSok (ENTRY(2,cKriterier,CHR(1))).
  RUN StLinjeToTT IN h_dstlinje
    ( OUTPUT TTH,cStTypeId,ENTRY(1,cKriterier,CHR(1)),pcFeltListe + ";" + pcVerdier,
       (IF RS-SumLevel:SCREEN-VALUE = "" THEN "ENTRY1" ELSE RS-SumLevel:SCREEN-VALUE) + IF Tg-VisPerBut:CHECKED THEN CHR(1) + "J" ELSE "",Tg-VisPeriode:CHECKED).
  /* här måste vi manipulera med alla styrsträngar till query och grid */
  IF RS-SumLevel:SCREEN-VALUE <> "" THEN DO:
    ASSIGN iEntry          = 4
           cTmpFieldDefs   = cFieldDefs
           ENTRY(1,cFieldDefs) = "LevNr;Levnr;;1,levnamn;Leverandør;;".
    CASE RS-SumLevel:SCREEN-VALUE:
        WHEN "Avd" THEN
            ASSIGN ENTRY(3,cFieldDefs) = "AvdelingNr;Avd;;1,AvdelingNavn;Avdeling;;".
        WHEN "Hg" THEN
            ASSIGN ENTRY(3,cFieldDefs) = "Hg;Hg;;1,HgBeskr;Hovedgruppe;;".
        WHEN "Vg" THEN
            ASSIGN ENTRY(3,cFieldDefs) = "Vg;Vg;;1,VgBeskr;Varegruppe;;".
        WHEN "Sasong" THEN
            ASSIGN ENTRY(3,cFieldDefs) = "Sasong;Sesong;;1,SasBeskr;Sesongbeskr;;".
    END CASE.
    IF Tg-VisPerBut:CHECKED THEN
      ASSIGN ENTRY(iEntry,cFieldDefs) = ENTRY(iEntry,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
    RUN FixStrings.
    ASSIGN cFieldDefs = cTmpFieldDefs.
  END.
  ELSE DO:
      IF Tg-VisPerBut:CHECKED THEN DO:
        ASSIGN cTmpFieldDefs       = cFieldDefs
               ENTRY(iEntry,cFieldDefs) = ENTRY(iEntry,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
        RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
      END.
      ELSE
          RUN FixStrings.
  END.
  CREATE QUERY qh.
  qh:SET-BUFFERS(TTH).
  qh:QUERY-PREPARE("for each TT_StLinje").
  qh:QUERY-OPEN().
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("TT_StLinje","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
  PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
  PUBLISH "LoadGrid" (cFileName,INT(getSumFelter("PerLinTxt")) - IF Tg-VisPerBut:CHECKED THEN 1 ELSE 0).  /* antall frozen cols  */
  ASSIGN cSumCols = getSumFelter(cSummerFelter)
         /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
         cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                   + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt")
         /* Col för SummaRadTxt, SUM = txt  */
         cSumString = getSumFelter("PerLinTxt") + ",SUM" .
  IF cVisFelterTxt <> "" THEN DO:
      cExtrafelt = "".
      DO ii = 1 TO NUM-ENTRIES(cTilleggsFelter):
          IF CAN-DO(cFelter,ENTRY(ii,cTilleggsFelter)) THEN
              cExtraFelt = cExtraFelt + "," + ENTRY(ii,cTilleggsFelter).
      END.
      cVisFelterNr = getSumFelter(cVisFelterTxt + cExtraFelt).
  END.
  /* nästa rad måste stå före 'Summer' */
  PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).
  PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).
  IF cVisFelterNr <> "" THEN
      PUBLISH "VisKun" (cVisFelterNr,"SKJUL").

  qh:QUERY-CLOSE().
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  DELETE OBJECT TTH NO-ERROR.
  DELETE OBJECT qh NO-ERROR.
  ASSIGN TTH = ?
         qh  = ?.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avdeling fFrameWin
ON CHOOSE OF B-Avdeling IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Avdeling;AvdelingNr;AvdelingNavn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "AvdelingNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Avdeling:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Avdeling     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Avdeling:TOOLTIP = IF FI-Avdeling = "*" THEN "" ELSE FI-Avdeling.
        IF FI-Avdeling <> "*" THEN DO:
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            APPLY "CHOOSE" TO B-SesongBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Avdeling:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA   = ""
                   FI-Avdeling:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvdelingBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvdelingBlank fFrameWin
ON CHOOSE OF B-AvdelingBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Avdeling:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Avdeling:SCREEN-VALUE = cAlle
               FI-Avdeling              = "*"
               FI-Avdeling:TOOLTIP      = ""
               FI-Avdeling:BGCOLOR      = ?
               B-Avdeling:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgBlank fFrameWin
ON CHOOSE OF B-HgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-HuvGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-HuvGr:SCREEN-VALUE = cAlle
               FI-HuvGr              = "*"
               FI-HuvGr:TOOLTIP      = ""
               FI-HuvGr:BGCOLOR      = ?
               B-HuvGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HuvGr fFrameWin
ON CHOOSE OF B-HuvGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "HuvGr;Hg;HgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Hg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-HuvGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-HuvGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-HuvGr:TOOLTIP = IF FI-HuvGr = "*" THEN "" ELSE FI-HuvGr.
        IF FI-HuvGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            APPLY "CHOOSE" TO B-SesongBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-HuvGr:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-HuvGr:BGCOLOR  = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNr fFrameWin
ON CHOOSE OF B-LevNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "LevBas;LevNr;Levnamn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "LevNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Levnr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Levnr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Levnr:TOOLTIP = IF FI-Levnr = "*" THEN "" ELSE FI-Levnr.
        IF FI-Levnr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Levnr:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-Levnr:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-LevNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-LevNr:SCREEN-VALUE = cAlle
               FI-LevNr              = "*"
               FI-LevNr:TOOLTIP      = ""
               FI-Levnr:BGCOLOR      = ?
               B-LevNr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong fFrameWin
ON CHOOSE OF B-Sesong IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "SaSong;Sasong;SasBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Sasong",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        ASSIGN
          FI-Sesong:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Sesong     = IF cIdList = ""
                            THEN "*"
                            ELSE REPLACE(cIdList,"|",",")
          FI-Sesong:TOOLTIP = IF FI-Sesong = "*" THEN "" ELSE FI-Sesong.
        IF FI-Sesong <> "*" THEN DO:
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Sesong:BGCOLOR = 11.
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Sesong:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBlank fFrameWin
ON CHOOSE OF B-SesongBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Sesong:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Sesong:SCREEN-VALUE = cAlle
               FI-Sesong              = "*"
               FI-Sesong:TOOLTIP      = ""
               FI-Sesong:BGCOLOR      = ?
               B-Sesong:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr fFrameWin
ON CHOOSE OF B-VarGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "VarGr;Vg;VgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Vg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-VarGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-VarGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-VarGr:TOOLTIP = IF FI-VarGr = "*" THEN "" ELSE FI-VarGr.
        IF FI-VarGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-SesongBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-VarGr:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-VarGr:BGCOLOR = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank fFrameWin
ON CHOOSE OF B-VgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-VarGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-VarGr:SCREEN-VALUE = cAlle
               FI-VarGr              = "*"
               FI-VarGr:TOOLTIP      = ""
               FI-VarGr:BGCOLOR      = ?
               B-VarGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-SumLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-SumLevel fFrameWin
ON VALUE-CHANGED OF RS-SumLevel IN FRAME fMain
DO:
    IF VALID-HANDLE(h_Window) THEN
        RUN PrintKnappHidden IN h_Window (SELF:SCREEN-VALUE <> "").
    RUN EnaDis (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF
{lng.i &SDO = "SDO"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  DISPLAY Tg-VisPeriode FI-LevNr RS-SumLevel FI-Avdeling FI-HuvGr FI-VarGr 
          Tg-VisPerBut FI-Sesong 
      WITH FRAME fMain.
  ENABLE Tg-VisPeriode B-Avdeling B-LevNrBlank RS-SumLevel B-Aktiver 
         B-AvdelingBlank B-HgBlank B-VgBlank Tg-VisPerBut B-SesongBlank 
         B-Sesong B-LevNr B-HuvGr B-VarGr 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDis fFrameWin 
PROCEDURE EnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cWhat AS CHARACTER   NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            B-Avdeling     :SENSITIVE = cWhat <> "Sasong"
            B-AvdelingBlank:SENSITIVE = cWhat <> "Sasong"
            B-HgBlank      :SENSITIVE = cWhat <> "Sasong"
            B-HuvGr        :SENSITIVE = cWhat <> "Sasong"
        /*     B-LevNr        :SENSITIVE = cWhat <> "Sesong" */
        /*     B-LevNrBlank   :SENSITIVE = cWhat <> "Sesong" */
            B-Sesong       :SENSITIVE = cWhat = "Sasong"
            B-SesongBlank  :SENSITIVE = cWhat = "Sasong"
            B-VarGr        :SENSITIVE = cWhat <> "Sasong"
            B-VgBlank      :SENSITIVE = cWhat <> "Sasong".
        IF cWhat <> "Sasong" THEN
            APPLY "CHOOSE" TO B-SesongBlank.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStrings fFrameWin 
PROCEDURE FixStrings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cFelter = FILL(",",NUM-ENTRIES(cFieldDefs) - 1)
       cLabels = cFelter
       cDecimaler = cFelter
       cRightCols = cFelter.
DO iCount = 1 TO NUM-ENTRIES(cFieldDefs):
    ASSIGN ENTRY(iCount,cFelter) = ENTRY(1,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cLabels) = ENTRY(2,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefs),";")
           ENTRY(iCount,cRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefs),";").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN FixStrings.
  RUN InitLeverandor.  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN
      ASSIGN h_fstperiode = DYNAMIC-FUNCTION('geth_fstperiode':U IN h_Window)
             h_dstlinje   = DYNAMIC-FUNCTION('geth_dstlinje':U IN h_Window).
  {syspara.i 1 100 1 cAlle}
  {syspara.i 220 1 1 cVisFelterTxt}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cStTypeId = "LEVERAN"
             cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt"
             FI-LevNr  = "*"
             FI-LevNr:SCREEN-VALUE = cAlle
             FI-Avdeling  = "*"
             FI-Avdeling:SCREEN-VALUE = cAlle
             FI-HuvGr  = "*"
             FI-HuvGr:SCREEN-VALUE = cAlle
             FI-VarGr  = "*"
             FI-VarGr:SCREEN-VALUE = cAlle
             FI-Sesong = "*"
             FI-Sesong:SCREEN-VALUE = cAlle.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLeverandor fFrameWin 
PROCEDURE InitLeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wLeverandorer AS CHARACTER  NO-UNDO.
  wLeverandorer = "".

  /* Leverandører - Leses fra Bestillingene */
/*   for each tmpLevBas: delete tmpLevBas. end. */
  for each LevBas no-lock where LevBas.LevNr > 0:
    assign
      wLeverandorer = wLeverandorer + 
                      (if wLeverandorer = ""
                         then ""
                         else ",") + string(LevBas.LevNr).
    create tmpLevBas.
    assign
      tmpLevBas.LevNr   = LevBas.LevNr
      tmpLevBas.LevNamn = LevBas.LeVNamn.          
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFeltInfo fFrameWin 
PROCEDURE SendFeltInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cField#List AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cColAlign   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
  ASSIGN cField#List = getSumFelter(cFeltListe).
         cColAlign   = FILL(",",NUM-ENTRIES(cField#List) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cField#List):
      IF ENTRY(INT(ENTRY(iCount,cField#List)),cRightCols) = "1" THEN 
          ASSIGN ENTRY(iCount,cColAlign) = "1".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFilterValues fFrameWin 
PROCEDURE SendFilterValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFilterVerdier AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cColAlign      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFraAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFraPerLinNr    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilPerLinNr    AS CHARACTER  NO-UNDO.

  DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
       OUTPUT cFstPeriode /* CHARACTER */).
/*   ASSIGN cButikker   = "Butikker: " + ENTRY(1,cFstPeriode,CHR(1)) */
/*          cPeriodeTmp = ENTRY(2,cFstPeriode,CHR(1)).               */
  RELEASE Butiker.
  IF NUM-ENTRIES(ENTRY(1,cFstPeriode,CHR(1))) = 1 THEN
      FIND Butiker WHERE Butiker.Butik = INT(ENTRY(1,cFstPeriode,CHR(1))) NO-LOCK NO-ERROR.
  ASSIGN cButikker   = (IF AVAIL Butiker THEN "Butikk: " ELSE "Butikker: ") + IF AVAIL Butiker THEN Butiker.Butnamn ELSE
                                         ENTRY(1,cFstPeriode,CHR(1))
         cPeriodeTmp = ENTRY(2,cFstPeriode,CHR(1)).
  CASE ENTRY(1,cPeriodeTmp):
      WHEN "AAR" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                             ENTRY(2,cPeriodeTmp) + "-" +
                             ENTRY(3,cPeriodeTmp).
      END.
      OTHERWISE DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                               ENTRY(2,cPeriodeTmp) + "-" +
                               ENTRY(3,cPeriodeTmp).
      END.
  END CASE.
  ASSIGN cFilterVerdier = cButikker + CHR(10) + cFilterVerdier
         cColAlign = cRightCols.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipKriterier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  CASE ENTRY(1,ipKriterier):
      WHEN "AAR" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,ipKriterier)))).
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,ipKriterier)))).
                 cFraPerLinNr = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1)).
                 cTilPerLinNr = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1)).
      END.
      OTHERWISE DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,ipKriterier))))
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,ipKriterier))))
                 cFraPerLinNr = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1))
                 cTilPerLinNr = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1)).
      END.
  END CASE.
  ASSIGN cStTypeId = IF RS-SumLevel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "LEVERAN" ELSE IF RS-SumLevel:SCREEN-VALUE = "Sasong" THEN "LEVERAN-SA" ELSE "LEVERAN-VG"
         cStTypeId = IF cStTypeId = "LEVERAN" AND (FI-Avdeling <> "*" OR FI-HuvGr <> "*" OR FI-VarGr <> "*") THEN "LEVERAN-VG" ELSE cStTypeId
         cQryString = 
            "FOR EACH StLinje WHERE SUBSTBUTIK AND StTypeId = '&1' AND PerId = '&2' AND AarPerLinNr >= &3 AND AarPerLinNr <= &4 use-index AarPerLinNr no-lock"
            cFraAarPer = cFraAar + STRING(INT(cFraPerLinNr),"999")
            cTilAarPer = cTilAar + STRING(INT(cTilPerLinNr),"999")
            cQryString = SUBSTITUTE(cQryString,cStTypeId,ENTRY(1,ipKriterier),cFraAarPer,cTilAarPer).
  DYNAMIC-FUNCTION('setQueryString':U IN h_dstlinje,
     INPUT cQryString /* CHARACTER */).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject fFrameWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "VALUE-CHANGED" TO RS-SumLevel IN FRAME {&FRAME-NAME}.
  PUBLISH "ClearGrid" (cLabels).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltNumListe AS CHARACTER  NO-UNDO.
  ASSIGN cFeltNumListe = FILL(",",NUM-ENTRIES(cFeltnavnListe) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cFeltnavnListe):
      ASSIGN ENTRY(iCount,cFeltNumListe) = STRING(LOOKUP(ENTRY(iCount,cFeltnavnListe),cFelter)).
  END.
  RETURN cFeltNumListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

