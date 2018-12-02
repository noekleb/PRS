&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
DEFINE VARIABLE h_Window AS HANDLE     NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dTotSum AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qh      AS HANDLE     NO-UNDO.
DEFINE VARIABLE lButikkBruker  AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE TT_ArtBas NO-UNDO LIKE ArtBas.
DEFINE TEMP-TABLE TT_Kalkylekontroll NO-UNDO LIKE ArtBas
    FIELD PaaTilbud AS CHAR
    FIELD DBKr1 AS DECIMAL
    FIELD db%1 AS DECIMAL
    FIELD Pris1 AS DECIMAL
    FIELD Varekost1 AS DECIMAL
    FIELD DBKr2 AS DECIMAL
    FIELD db%2 AS DECIMAL
    FIELD Pris2 AS DECIMAL
    FIELD Varekost2 AS DECIMAL
    FIELD Levnamn AS CHAR
    FIELD VgBeskr AS CHAR
    FIELD HgBeskr AS CHAR
    FIELD AvdBeskr AS CHAR
    INDEX ArtikkelNr ArtikkelNr.

                
/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi,Vg,VgBeskr,LevNr,Levnamn,Sasong,SasBeskr,Farg,FarBeskr,MatKod,MatBeskr,VMId,VMBeskr" */
/*        cLabels = "ArtikkelNr,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr,Vg,VgBeskr,LevNr,Levnamn,Sesong,SasBeskr,Farve,FarBeskr,MatKod,MatBeskr,VareMerke,VMBeskr"               */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2,,,,,,,,,,,,"                                                                                                                                                                                                                                                                                   */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,,1,,1,,1,,1,,1,". /* Fält som skall högerjust i XPrint */                                                                                                                                                                                                                           */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cFieldDefs = "ArtikkelNr;ArtikkelNr;;1," +
                    "Beskr;Beskr;;," +
                    "LevKod;Levkod;;," +
                    "LevFargKod;Levfarge;;," +
                    "DB%1;DB%;2;1," +
                    "DBKr1;DBKr;2;1," +
                    "Pris1;Pris;2;1," +
                    "VareKost1;Varekost;2;1," +
                    "PaaTilbud;På tilbud;;1," +
                    "DB%2;DB% tilb;2;1," +
                    "DBKr2;DBKr tilb;2;1," +
                    "Pris2;Pris tilb;2;1," +
                    "VareKost2;Varekost tilb;2;1," +
                    "LevNr;Levnr;;1," +
                    "levnamn;Leverandør;;," +
                    "Vg;Vg;;1," +
                    "VgBeskr;Vgbeskr;;," +
                    "LopNr;Lopnr;;1," +
                    "HgBeskr;Hgbeskr;;," +
                    "AvdBeskr;Avdbeskr;;".

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
&Scoped-Define ENABLED-OBJECTS B-Aktiver CB-PP FI-DB% FI-Avvik RS-Type ~
RS-Avvik B-Artikkelkort FI-TypeTxt FI-Avvikstype 
&Scoped-Define DISPLAYED-OBJECTS FI-Utvalg CB-PP FI-DB% FI-Avvik RS-Type ~
RS-Avvik FI-TypeTxt FI-Avvikstype 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Artikkelutvalg" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Kalkylekontroll 
     LABEL "Ka&lkylekontroll" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-PP AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avvik AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Avvik %" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avvikstype AS CHARACTER FORMAT "X(256)":U INITIAL "Avvikstype:" 
      VIEW-AS TEXT 
     SIZE 13 BY .71 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Godkjent db%" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TypeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Kalkyletype:" 
      VIEW-AS TEXT 
     SIZE 14.2 BY .71 NO-UNDO.

DEFINE VARIABLE FI-Utvalg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Poster i utvalg" 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS-Avvik AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "- Avvik", 1,
"+ Avvik", 2,
"-/+ Avvik", 3,
"Godkjente", 4
     SIZE 60.4 BY .95 NO-UNDO.

DEFINE VARIABLE RS-Type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordinær", 1,
"Tilbud", 2,
"Aktiv", 3
     SIZE 37.2 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      StLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Utvalg AT ROW 1.24 COL 33.2 COLON-ALIGNED
     B-Aktiver AT ROW 1.29 COL 1
     CB-PP AT ROW 2.38 COL 33.2 COLON-ALIGNED
     FI-DB% AT ROW 2.38 COL 77.4 COLON-ALIGNED HELP
          "DB%"
     FI-Avvik AT ROW 2.38 COL 101 COLON-ALIGNED HELP
          "DB%"
     B-Kalkylekontroll AT ROW 2.95 COL 1
     RS-Type AT ROW 3.62 COL 35.2 NO-LABEL
     RS-Avvik AT ROW 4.52 COL 35.2 NO-LABEL
     B-Artikkelkort AT ROW 4.62 COL 1
     FI-TypeTxt AT ROW 3.67 COL 18.8 COLON-ALIGNED NO-LABEL
     FI-Avvikstype AT ROW 4.57 COL 18.8 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.4 BY 5.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 5.71
         WIDTH              = 130.4.
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

/* SETTINGS FOR BUTTON B-Kalkylekontroll IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Utvalg IN FRAME fMain
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
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Artikkelutvalg */
DO:
  PUBLISH "ClearGrid" (cLabels).
  RUN SetFIUtvalg IN THIS-PROCEDURE ("0").
  RUN Avancerat.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort fFrameWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
  RUN ArtikkelKort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kalkylekontroll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kalkylekontroll fFrameWin
ON CHOOSE OF B-Kalkylekontroll IN FRAME fMain /* Kalkylekontroll */
DO:
  RUN Analyse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Analyse fFrameWin 
PROCEDURE Analyse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dArtikkelNr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iDB%idx AS INTEGER    NO-UNDO.
            
  EMPTY TEMP-TABLE TT_KalkyleKontroll.
  PUBLISH "VisTxtBox" ("Søker data......").
  qh:QUERY-OPEN().
  DO WITH FRAME {&FRAME-NAME}:
    REPEAT:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN
            LEAVE.
        dArtikkelNr = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE().
        FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
        FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = INPUT CB-PP NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN DO:
            ASSIGN iDB%idx = IF RS-Type:SCREEN-VALUE <> "3" then
                      INT(RS-Type:SCREEN-VALUE) ELSE (IF NOT ArtPris.Tilbud THEN 1 ELSE 2).
            IF INPUT RS-Avvik = 1 THEN DO:
                IF NOT ArtPris.DB%[iDB%idx] < INPUT FI-DB% - INPUT FI-Avvik THEN
                    NEXT.
            END.
            ELSE IF INPUT RS-Avvik = 2 THEN DO:
                IF NOT ArtPris.DB%[iDB%idx] > INPUT FI-DB% + INPUT FI-Avvik THEN
                    NEXT.
            END.
            ELSE IF INPUT RS-Avvik = 3 THEN DO:
                IF NOT (ArtPris.DB%[iDB%idx] < INPUT FI-DB% - INPUT FI-Avvik OR 
                   ArtPris.DB%[iDB%idx] > INPUT FI-DB% + INPUT FI-Avvik) THEN
                    NEXT.
            END.
            ELSE IF INPUT RS-Avvik = 4 THEN DO:
                IF NOT ((ArtPris.DB%[iDB%idx] >= INPUT FI-DB% - INPUT FI-Avvik AND 
                   ArtPris.DB%[iDB%idx] <= INPUT FI-DB% + INPUT FI-Avvik)) THEN
                    NEXT.
            END.
            BUFFER-COPY ArtBas TO TT_Kalkylekontroll
                ASSIGN TT_Kalkylekontroll.DB%1      = ArtPris.DB%[1]
                       TT_Kalkylekontroll.DB%2      = ArtPris.DB%[2]
                       TT_Kalkylekontroll.PaaTilBud = STRING(ArtPris.TilBud,"J/")
                       TT_Kalkylekontroll.DBKr1     = ArtPris.DBKr[1] 
                       TT_Kalkylekontroll.DBKr2     = ArtPris.DBKr[2] 
                       TT_Kalkylekontroll.Pris1     = ArtPris.Pris[1]
                       TT_Kalkylekontroll.Pris2     = ArtPris.Pris[2]
                       TT_Kalkylekontroll.VareKost1 = ArtPris.VareKost[1]
                       TT_Kalkylekontroll.VareKost2 = ArtPris.VareKost[2]
                       .
            FIND LevBas OF TT_Kalkylekontroll NO-LOCK NO-ERROR.
            FIND VarGr OF TT_Kalkylekontroll NO-LOCK NO-ERROR.
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
            ASSIGN TT_Kalkylekontroll.Levnamn   = IF AVAIL LevBas THEN LevBas.levnamn ELSE ""
                   TT_Kalkylekontroll.VgBeskr   = IF AVAIL VarGr THEN VarGr.vgbeskr ELSE ""
                   TT_Kalkylekontroll.HgBeskr   = IF AVAIL HuvGr THEN HuvGr.hgbeskr ELSE ""
                   TT_Kalkylekontroll.AvdBeskr  = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "".
            RELEASE TT_Kalkylekontroll.
        END.
    END.
    IF NOT CAN-FIND(FIRST TT_Kalkylekontroll) THEN DO:
        MESSAGE "Ingen statistikk funnet"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        PUBLISH "VisTxtBox" ("").
        IF B-Kalkylekontroll:SENSITIVE = FALSE THEN
            APPLY "CLOSE" TO h_Window.
        RETURN.
    END.
    RUN EksportLesInn.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort fFrameWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("ArtikkelNr",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  DYNAMIC-FUNCTION('fLockvindu':U IN h_Window,
    INPUT TRUE /* LOGICAL */).
/*   fLockvindu(TRUE). */
  IF lButikkBruker = TRUE THEN
      RUN ArtBasVisTime.w (THIS-PROCEDURE,artbas.artikkelnr).
  ELSE
      run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE) + ",2").
  DYNAMIC-FUNCTION('fLockvindu':U IN h_Window,
    INPUT FALSE /* LOGICAL */).
/*   fLockvindu(FALSE). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avancerat fFrameWin 
PROCEDURE Avancerat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN tmpUtvalg IN h_Window (THIS-PROCEDURE).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportLesInn fFrameWin 
PROCEDURE EksportLesInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iTime AS INTEGER    NO-UNDO.
   PUBLISH "VisTxtBox" ("Leser ut data......").
   CREATE QUERY qh.
   qh:SET-BUFFERS(BUFFER TT_Kalkylekontroll:HANDLE).
   qh:QUERY-PREPARE("FOR EACH TT_Kalkylekontroll").
   qh:QUERY-OPEN().
   PUBLISH "VisTxtBox" ("Leser ut data......").
   RUN rappgenqry.p ("","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
   RUN LesInnIGrid.
   PUBLISH "VisTxtBox" ("").
   
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.

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
  DISPLAY FI-Utvalg CB-PP FI-DB% FI-Avvik RS-Type RS-Avvik FI-TypeTxt 
          FI-Avvikstype 
      WITH FRAME fMain.
  ENABLE B-Aktiver CB-PP FI-DB% FI-Avvik RS-Type RS-Avvik B-Artikkelkort 
         FI-TypeTxt FI-Avvikstype 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideObject fFrameWin 
PROCEDURE hideObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(qh) AND VALID-HANDLE(h_Window) THEN DO:
      RUN Nullstill IN h_Window NO-ERROR.
  END.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB fFrameWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  FOR EACH PrisProfil NO-LOCK:
      ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") + 
            Prisprofil.KortNavn + "," + STRING(Prisprofil.ProfilNr).
  END.
  ASSIGN CB-PP:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs
         CB-PP = 1.

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
  ASSIGN h_Window = SOURCE-PROCEDURE.
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND Bruker.BrukerType = 1 THEN
      lButikkBruker = FALSE.
  ELSE
      lButikkBruker = TRUE.
  RUN InitCB.
  RUN FixStrings.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridkalk.txt".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnIGrid fFrameWin 
PROCEDURE LesInnIGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      DO:
          PUBLISH "LoadGrid" (cFileName,3).  /* 3 = antall frozen cols  */
          PUBLISH "AlignCol" (3,2).  /* 3 = antall frozen cols  */
          /* getSumFelter ger colnr för resp fält */
/*           ASSIGN cSumCols = getSumFelter(cSummerFelter)                                                                                                              */
/*                  /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */                                   */
/*                  cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"                                        */
/*                            + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt") */
/*                  /* Col för SummaRadTxt, SUM = txt  */                                                                                                               */
/*                  cSumString = getSumFelter("PerLinTxt") + ",SUM" .                                                                                                   */
/*           /* nästa rad måste stå före 'Summer' */                                                                                                                    */
/*           PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).                                                                 */
/*           PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).                                                                                                  */
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext fFrameWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.

    ASSIGN cGetVerdier = STRING(LOOKUP("ArtikkelNr",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,cRettning).      
        IF cArtikkelNr = "" THEN
          RETURN.
        PUBLISH "ByttArtikkel" (DECI(cArtikkelNr)).
    END.
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
DEFINE        VARIABLE  cFraAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTTId          AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cFilterVerdier = FI-DB%:LABEL + " " + FI-DB%:SCREEN-VALUE + " " + FI-Avvik:LABEL + " " + FI-Avvik:SCREEN-VALUE + CHR(10) +
             FI-TypeTxt + ENTRY(LOOKUP(RS-Type:SCREEN-VALUE,RS-Type:RADIO-BUTTONS) - 1,RS-Type:RADIO-BUTTONS) + CHR(10) +
             FI-Avvikstype + ENTRY(LOOKUP(RS-Avvik:SCREEN-VALUE,RS-Avvik:RADIO-BUTTONS) - 1,RS-Avvik:RADIO-BUTTONS)
             cColAlign = cRightCols.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFIUtvalg fFrameWin 
PROCEDURE SetFIUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cAntall AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-Utvalg:SCREEN-VALUE = IF cAntall = "0" THEN "Ingen poster" ELSE cAntall
             FI-Utvalg:FGCOLOR      = IF cAntall = "0" THEN 12 ELSE ?.
             B-Kalkylekontroll:SENSITIVE = cAntall <> "0".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArtDyn fFrameWin 
PROCEDURE StartSokArtDyn :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipqh      AS HANDLE  NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal    AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cStatus AS CHARACTER  NO-UNDO.
  IF cButiker BEGINS "HENTINTERNT" THEN DO:
      ASSIGN cStatus =  ENTRY(1,ENTRY(2,cButiker,CHR(1)),"(")
             cButiker = TRIM(ENTRY(1,cButiker,CHR(1))).
  END.

  ASSIGN qh = ipqh.
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT cButiker = "HENTINTERNT" THEN
          APPLY "CHOOSE" TO B-Kalkylekontroll.
      ELSE DO:
          RUN SetFIUtvalg IN THIS-PROCEDURE (cStatus).
      END.
  END.

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
  PUBLISH "ClearGrid" (cLabels).
  RUN SetFIUtvalg IN THIS-PROCEDURE ("0").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

