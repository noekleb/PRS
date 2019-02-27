&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cFieldDefsBunt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefsRapp AS CHARACTER  NO-UNDO.


/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter = "Avd1,Ant1,Avd2,Ant2,Avd3,Ant3,Avd4,Ant4,Avd5,Ant5,Avd6,Ant6,Avd7,Ant7,Avd8,Ant8,Avd9,Ant9,Ukjent,AntU,hSum,hAntSum".
cFieldDefsRapp = 
/*                  "Tekst;Tekst;;," + */
                 "ButFra;Fra butikk;;," +
                 "ButFnavn;Navn;;," +
                 "ButTil;Til butikk;;," +
                 "ButTnavn;Navn;;," +
                 "Avd1;&1;2;1," +
                 "Ant1;Ant;;1," +
                 "Avd2;&2;2;1," +
                 "Ant2;Ant;;1," +
                 "Avd3;&3;2;1," +
                 "Ant3;Ant;;1," +
                 "Avd4;&4;2;1," +
                 "Ant4;Ant;;1," +
                 "Avd5;&5;2;1," +
                 "Ant5;Ant;;1," +
                 "Avd6;&6;2;1," +
                 "Ant6;Ant;;1," +
                 "Avd7;&7;2;1," +
                 "Ant7;Ant;;1," +
                 "Avd8;&8;2;1," +
                 "Ant8;Ant;;1," +
                 "Avd9;&9;2;1," +
                 "Ant9;Ant;;1," +
                 "Ukjent;Ukjent;2;1," +
                 "AntU;Ant;;1," +
                 "hSum;SUM;2;1," +
                 "hAntSum;AntSUM;;1".


cFieldDefsBunt = "BuntNr;BuntNr;;1,"           +
                 "Merknad;Merknad;;," +
                 "ButFra;Fra butikk(er);;1," +
                 "ButTil;Til butikk(er);;1," +
                 "RegistrertDato;RegistrertDato;;," +
                 "RegistrertTid;RegistrertTid;;," +
                 "BrukerID;BrukerID;;," +
                 "RegistrertAv;RegistrertAv;;," +
                 "opphav;Opphav;;1," +
                 "EDato;EDato;;," +
                 "ETid;ETid;;"
                 .
cFieldDefs = cFieldDefsRapp.
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Dato FI-DatoTil RS-Datotyp RS-Rapptyp ~
CB-Maned CB-Butik TG-TilBut B-Aktiver BUTTON-SokDatoTil BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil RS-Datotyp RS-Rapptyp ~
CB-Maned CB-Butik TG-TilBut 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getManedDato fFrameWin 
FUNCTION getManedDato RETURNS CHARACTER
  ( INPUT cAarManed AS CHAR )  FORWARD.

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
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Butik AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "","1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Maned AS CHARACTER FORMAT "X(256)":U 
     LABEL "Måned" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 12
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Datotyp AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Interval", 1,
"Måned", 2
     SIZE 13 BY 2.38 NO-UNDO.

DEFINE VARIABLE RS-Rapptyp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Oveføringer", 1,
"Ikke oppdaterte", 2
     SIZE 43 BY 1.1 NO-UNDO.

DEFINE VARIABLE TG-TilBut AS LOGICAL INITIAL no 
     LABEL "Til butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Dato AT ROW 1.19 COL 14 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.19 COL 32 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     RS-Datotyp AT ROW 1.24 COL 53 NO-LABEL
     RS-Rapptyp AT ROW 1.24 COL 70 NO-LABEL
     CB-Maned AT ROW 2.48 COL 14 COLON-ALIGNED
     CB-Butik AT ROW 3.62 COL 14 COLON-ALIGNED HELP
          "Butikknummer"
     TG-TilBut AT ROW 3.67 COL 47
     B-Aktiver AT ROW 4.86 COL 47
     BUTTON-SokDatoTil AT ROW 1.19 COL 47.6
     BUTTON-SokDato AT ROW 1.19 COL 29.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 6.29.


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
         HEIGHT             = 6.29
         WIDTH              = 155.
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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "NO-LOCK INDEXED-REPOSITION KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
    DEFINE VARIABLE TTH_Table AS HANDLE.
    DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
    DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cParaString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato AS CHARACTER  NO-UNDO.
/*     Dessa är flyttade till definitionsblocket för att kunna hanteras om vi har valt avancerat */
/*     eller kommer från artikkelutvalg */
/*     DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO. */
/*     DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    IF FI-Dato:SENSITIVE THEN DO:
        IF INPUT FI-Dato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Feil dato, fra dato > til dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-DatoTIl.
            RETURN NO-APPLY.
        END.
    END.
/*     RUN StartSok. */
/*     IF RS-RappTyp:SCREEN-VALUE = "1" AND (CB-ButikTil:SCREEN-VALUE = ? OR CB-ButikTil:SCREEN-VALUE = CB-ButikFra:SCREEN-VALUE) THEN DO: */
/*         MESSAGE "Velg gyldig butikk"                                                                                                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                          */
/*         RETURN NO-APPLY.                                                                                                                */
/*     END.                                                                                                                                */
    IF RS-Rapptyp:SCREEN-VALUE = "1" THEN DO:
        ocButiker = CB-Butik:SCREEN-VALUE.
        IF FI-Dato:SENSITIVE THEN
            cDato = STRING(INPUT FI-Dato) + "," + (IF INPUT FI-DatoTil = ? THEN STRING(INPUT FI-Dato) ELSE STRING(INPUT FI-DatoTil)).
        ELSE 
            cDato = getManedDato(CB-Maned:SCREEN-VALUE).
    END.

/*     iTime3 = ETIME.                           */
/*     MESSAGE "Polygon test, skal fjernes" SKIP */
/*     iTime1 SKIP iTime2 SKIP iTime3            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
    PUBLISH "VisTxtBox" ("Søker data......").

    /* 
     Prameter: Entry 1 ";" : butikfra,Butiktil
               Entry 2     : datofra,datotil
               Entry 3     : rapporttyp 1 = overføringer, 2 = ikke oppdaterte
     */

    cParaString = ocButiker + ";" + cDato + ";" + RS-Rapptyp:SCREEN-VALUE + ";" + STRING(INPUT TG-TilBut,"J/N")+ "|".

    TTH_Table = DYNAMIC-FUNCTION("getTempTable","get_overforrapp.p",cParaString,?).
    TTH = TTH_Table:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY qh.
    qh:SET-BUFFERS(TTH).
    qh:QUERY-PREPARE("for each " + TTH:NAME).
    qh:QUERY-OPEN().
/* MESSAGE TTH:NAME SKIP cFilename SKIP VALID-HANDLE(qh) */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("for each " + TTH:NAME,"",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

  DO:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,0).  /* 3 = antall frozen cols  */
      PUBLISH "AlignCol" (LOOKUP("Storl",cFelter),8). /* högerjustert */
      IF RS-Rapptyp:SCREEN-VALUE = "1" THEN DO:
          ASSIGN cSumCols   = getSumFelter(cSummerFelter)
                 cSumString = getSumFelter("ButTnavn") + ",SUM" .
          PUBLISH "Summer" (cSumCols,cSumString).
      END.

  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato fFrameWin
ON CHOOSE OF BUTTON-SokDato IN FRAME fMain /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Feil dato, > Til dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil fFrameWin
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME fMain /* ... */
or F10 of FI-DatoTil
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato < INPUT FI-Dato THEN DO:
            MESSAGE "Feil dato, < Fra dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-DatoTil:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Datotyp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Datotyp fFrameWin
ON VALUE-CHANGED OF RS-Datotyp IN FRAME fMain
DO:
      CB-Maned:SENSITIVE          = SELF:SCREEN-VALUE = "2".
      FI-Dato:SENSITIVE           = SELF:SCREEN-VALUE = "1".
      FI-DatoTil:SENSITIVE        = SELF:SCREEN-VALUE = "1".
      BUTTON-SokDato:SENSITIVE    = SELF:SCREEN-VALUE = "1".
      BUTTON-SokDatoTil:SENSITIVE = SELF:SCREEN-VALUE = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Rapptyp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Rapptyp fFrameWin
ON VALUE-CHANGED OF RS-Rapptyp IN FRAME fMain
DO:
  IF SELF:SCREEN-VALUE = "1" THEN DO:
      cFieldDefs = cFieldDefsRapp.
      cTidFelter = "".
  END.
  ELSE DO:
      cFieldDefs = cFieldDefsBunt.
      cTidFelter = "RegistrertTid,ETid".
  END.
  CB-Butik:SENSITIVE = SELF:SCREEN-VALUE = "1".
  TG-TilBut:SENSITIVE = SELF:SCREEN-VALUE = "1".
  RS-Datotyp:SENSITIVE  = SELF:SCREEN-VALUE = "1".
  IF SELF:SCREEN-VALUE = "1" THEN DO:
      APPLY "VALUE-CHANGED" TO RS-Datotyp.
  END.
  ELSE DO:
      FI-Dato:SENSITIVE = FALSE.
      FI-DatoTil:SENSITIVE = FALSE.
      CB-Maned:SENSITIVE = FALSE.
  END.
  RUN FixStrings.
  PUBLISH "ClearGrid" (cLabels).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort fFrameWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  fLockvindu(TRUE).
  run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

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
  DISPLAY FI-Dato FI-DatoTil RS-Datotyp RS-Rapptyp CB-Maned CB-Butik TG-TilBut 
      WITH FRAME fMain.
  ENABLE FI-Dato FI-DatoTil RS-Datotyp RS-Rapptyp CB-Maned CB-Butik TG-TilBut 
         B-Aktiver BUTTON-SokDatoTil BUTTON-SokDato 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikVis fFrameWin 
PROCEDURE FixButikVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
/*       IF cButikerIdList <> "" THEN                                                                                   */
/*           ASSIGN CB-Butik:LIST-ITEM-PAIRS = ",INGEN"                                                                 */
/*                  FI-Butikker = cButikerIdList                                                                        */
/*                  FI-Butikker:BGCOLOR = 15                                                                            */
/*                  FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"                      */
/*                  FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")                                               */
/*                  CB-Butik:SCREEN-VALUE = "INGEN"                                                                     */
/*                  CB-Butik:SENSITIVE    = FALSE.                                                                      */
/*       ELSE                                                                                                           */
/*           ASSIGN FI-Butikker:BGCOLOR = ?                                                                             */
/*                  FI-Butikker = ""                                                                                    */
/*                  FI-Butikker:SCREEN-VALUE = ""                                                                       */
/*                  FI-Butikker:TOOLTIP = ""                                                                            */
/*                  CB-Butik:LIST-ITEM-PAIRS = cListItemPairs                                                           */
/*                  CB-Butik:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN */
/*                                                     cUserDefaultBut ELSE ENTRY(2,cListItemPairs)                     */
/*                  CB-Butik:SENSITIVE    = TRUE.                                                                       */
/*                  .                                                                                                   */
/*      APPLY "VALUE-CHANGED" TO CB-Butik.                                                                              */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixLablar fFrameWin 
PROCEDURE FixLablar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c1 AS CHARACTER INIT "Ukjent" NO-UNDO.
DEFINE VARIABLE c2 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c3 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c4 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c5 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c6 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c7 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c8 AS CHARACTER INIT "Ukjent"  NO-UNDO.
DEFINE VARIABLE c9 AS CHARACTER INIT "Ukjent"  NO-UNDO.
    
    FOR EACH avdeling WHERE avdeling.avdelingnr < 10 NO-LOCK.
        CASE Avdeling.avdelingnr:
            WHEN 1 THEN c1 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c1.
            WHEN 2 THEN c2 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c2.
            WHEN 3 THEN c3 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c3.
            WHEN 4 THEN c4 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c4.
            WHEN 5 THEN c5 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c5.
            WHEN 6 THEN c6 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c6.
            WHEN 7 THEN c7 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c7.
            WHEN 8 THEN c8 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c8.
            WHEN 9 THEN c9 = IF TRIM(Avdeling.avdelingnavn) <> "" THEN REPLACE(avdeling.avdelingnavn,","," ") ELSE c9.
        END CASE.
    END.
    cFieldDefsRapp = SUBSTITUTE(cFieldDefsRapp,c1,c2,c3,c4,c5,c6,c7,c8,c9).

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
DO WITH FRAME {&FRAME-NAME}:
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
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos fFrameWin 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cAarMan AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cMax AS CHAR    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:

        /* Butiker start */
        FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
        ASSIGN cListItemPairs  = "".
        /* Leta upp unika förekomster av butiker genom brukers team */
        FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                  ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
            FOR EACH ButikkKobling OF ButikkTeam.
                FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
                IF NOT AVAIL TT_TillgButikker THEN DO:
                        CREATE TT_TillgButikker.
                        ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
                END.
            END.
        END.
        FOR EACH TT_TillgButikker:
            FIND Butiker OF TT_TillgButikker NO-LOCK NO-ERROR.
            IF NOT AVAIL Butiker THEN
                NEXT.
            ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(Butiker.Butik)
                   cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                                    Butiker.butnamn + "," + STRING(Butiker.Butik).
                   cUserDefaultBut = IF Butiker.Butik = Bruker.ButikkNr THEN STRING(Butiker.Butik) ELSE cUserDefaultBut.
        END.
        ASSIGN cUserDefaultBut = IF TRIM(cUserDefaultBut) = "" THEN TRIM(ENTRY(2,cListItemPairs)) ELSE cUserDefaultBut
               CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
               CB-Butik:SCREEN-VALUE = cUserDefaultBut.

        /* Combo Månad */
        DO  ii = 1 TO 12:
            cAarMan = cAarMan + (IF cAarMan <> "" THEN "," ELSE "") + 
                    STRING(YEAR(TODAY) - IF ii < MONTH(TODAY) THEN 0 ELSE 1) + STRING(ii,"99").
            cMax = IF ENTRY(ii,cAarman) > cMax THEN ENTRY(ii,cAarman) ELSE cMax.
        END.

        CB-Maned:LIST-ITEMS = cAarMan + "," + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99").
        CB-Maned:SCREEN-VALUE = cMax.
        /* Butiker end */

/* gammal butikshantering */
/*       FOR EACH Butiker /* WHERE CAN-FIND(FIRST Kasse WHERE Kasse.Butik = Butiker.Butik) */ NO-LOCK:                                           */
/*           ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") + Butiker.ButNamn + "," + STRING(Butiker.Butik). */
/*       END.                                                                                                                                    */
/*       IF NUM-ENTRIES(cListItemPairs) > 2 THEN                                                                                                 */
/*           ASSIGN cListItemPairs = cAlle + ",," + cListItemPairs.                                                                              */
/*       ASSIGN CB-Butik:LIST-ITEM-PAIRS = cListItemPairs                                                                                        */
/*              CB-Butik:SCREEN-VALUE = IF NUM-ENTRIES(cListItemPairs) > 2 THEN " " ELSE ENTRY(2,cListItemPairs).                                */
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
  ASSIGN h_Window = SOURCE-PROCEDURE.

  RUN FixLablar.
  RUN FixStrings.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  ASSIGN FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridoverfor.txt".
  APPLY "VALUE-CHANGED" TO CB-Butik.
  APPLY "VALUE-CHANGED" TO RS-Datotyp.
  APPLY "VALUE-CHANGED" TO RS-Rapptyp.
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
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR.
      ASSIGN cFilterVerdier = "Butikk: " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") + CHR(10) +
             "Dato: " + FI-Dato:SCREEN-VALUE
             cColAlign = cRightCols.
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
  APPLY "ENTRY" TO FI-Dato IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getManedDato fFrameWin 
FUNCTION getManedDato RETURNS CHARACTER
  ( INPUT cAarManed AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDato1 AS DATE       NO-UNDO.
  DEFINE VARIABLE dDato2 AS DATE       NO-UNDO.
  DEFINE VARIABLE iAar   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iManed AS INTEGER    NO-UNDO.
  iAar   = INT(SUBSTR(cAarmaned,1,4)).
  iManed = INT(SUBSTR(cAarmaned,5)).
  dDato1 = DATE(iManed,1,iAar).
  dDato2 = IF iManed = 12 THEN DATE(iManed,31,iAar) ELSE DATE(iManed + 1,1,iAar) - 1.
  RETURN STRING(dDato1) + "," + STRING(dDato2).   /* Function return value. */

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

