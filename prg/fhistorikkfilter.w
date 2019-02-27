&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

DEF VAR wCl               AS INT    NO-UNDO.
def var wButik            as int    no-undo.
def var wwButik           as int    no-undo.

def VAR wArtBasRecid       as recid  no-undo.
def VAR wDataObjekt        as char   no-undo.
def VAR wStTypeId          as char   no-undo.
def VAR wPerId             as char   NO-UNDO.

def var wVindusTittel as char no-undo.
DEF VAR wTitle        AS CHAR NO-UNDO.
def var wPris         as char no-undo.    
DEF VAR wKriterier    AS CHAR NO-UNDO.

DEF VAR wNoBygg       AS LOG  NO-UNDO.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH ArtBas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-fMain ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Excel CB-PerId B-Grafikk T-Tot B-Rapp ~
B-Rapp-HtmEx B-SokDato RECT-1 
&Scoped-Define DISPLAYED-OBJECTS CB-PerId FI-Filter T-Tot FI-Fltertekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.4 BY 1 TOOLTIP "Eksporter alle eller merkede tellelinjer til Excel. Alt-X.".

DEFINE BUTTON B-Grafikk 
     LABEL "&Grafikk" 
     SIZE 15 BY 1.

DEFINE BUTTON B-Rapp 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "Htm" 
     SIZE 4.4 BY 1 TOOLTIP "Html".

DEFINE BUTTON B-Rapp-HtmEx 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "HtmlExcel" 
     SIZE 4.4 BY 1 TOOLTIP "Html i Excel".

DEFINE BUTTON B-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-PerId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Statistikktype" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filter AS CHARACTER FORMAT "X(256)" 
     LABEL "Periodefilter" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FI-Fltertekst AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 156 BY 1.43.

DEFINE VARIABLE T-Tot AS LOGICAL INITIAL no 
     LABEL "Kun totaler" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Excel AT ROW 1.48 COL 126
     CB-PerId AT ROW 1.43 COL 16 COLON-ALIGNED
     FI-Filter AT ROW 1.43 COL 55 COLON-ALIGNED
     B-Grafikk AT ROW 1.48 COL 141
     T-Tot AT ROW 1.67 COL 104
     B-Rapp AT ROW 1.48 COL 131
     B-Rapp-HtmEx AT ROW 1.48 COL 136
     B-SokDato AT ROW 1.43 COL 98
     FI-Fltertekst AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 1.76.


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
         HEIGHT             = 1.76
         WIDTH              = 156.6.
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

/* SETTINGS FOR FILL-IN FI-Filter IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Fltertekst IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Fltertekst:READ-ONLY IN FRAME fMain        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.ArtBas"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel fFrameWin
ON CHOOSE OF B-Excel IN FRAME fMain /* Excel... */
DO:
    PUBLISH "ExHtmRapp" ("EX", INPUT INPUT T-Tot).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Grafikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Grafikk fFrameWin
ON CHOOSE OF B-Grafikk IN FRAME fMain /* Grafikk */
DO:
  PUBLISH "StatGrafikk".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp fFrameWin
ON CHOOSE OF B-Rapp IN FRAME fMain /* Htm */
DO:
    PUBLISH "ExHtmRapp" ("HTM", INPUT INPUT T-Tot).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp-HtmEx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp-HtmEx fFrameWin
ON CHOOSE OF B-Rapp-HtmEx IN FRAME fMain /* HtmlExcel */
DO:
    PUBLISH "ExHtmRapp" ("HTMEX", INPUT INPUT T-Tot).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokDato fFrameWin
ON CHOOSE OF B-SokDato IN FRAME fMain /* ... */
DO:   
  /* I variabelen wTekst vil ved retur fra filterprogram stå avgrensning */
  /* i en kommaseparert liste. Info ligger på formatet                   */
  /*   Fra År, Til år Fra periodelinje, Til periodelinje.                */


  case wPerId:
    when "AAR"   then run d-statfiltaar.w   (input-output wKriterier, wStTypeId).
    when "MANED" then run d-statfiltmaned.w (input-output wKriterier, wStTypeId).
    when "UKE"   then run d-statfiltuke.w   (input-output wKriterier, wStTypeId). 
    when "DAG"   then run d-statfiltdag.w   (input-output wKriterier, wStTypeId).
    otherwise         run d-statfiltdag.w   (input-output wKriterier, wStTypeId).
  end case. 
  if return-value = "AVBRYT" then
    return no-apply.

  ASSIGN
      wKriterier = wKriterier + ",0,1,1".

  RUN SaveToLokalIni IN h_dproclib ("PERID", wPerId).
  RUN SaveToLokalIni IN h_dproclib ("KRITERIER", wKriterier).

  RUN StartSok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-PerId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-PerId fFrameWin
ON VALUE-CHANGED OF CB-PerId IN FRAME fMain /* Statistikktype */
DO:

  assign 
    CB-PerId
    wPerId = CB-PerId
    .
  RUN ByttPeriodeId.
  
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Tot fFrameWin
ON VALUE-CHANGED OF T-Tot IN FRAME fMain /* Kun totaler */
DO:
  ASSIGN
      T-Tot
      .

  if T-Tot then
    wButik = 999999.
  else
    wButik = 0.

  ASSIGN
      wNoBygg = TRUE
      .
  RUN SaveToLokalIni IN h_dproclib ("VISTOT", string(T-Tot)).
  RUN StartSok.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttPeriodeId fFrameWin 
PROCEDURE ByttPeriodeId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wOldPerId     AS CHAR NO-UNDO.
DEF VAR wWeekNum      AS INT  NO-UNDO.

  ASSIGN
    wOldPerId = wPerId
    wPerId    = CB-PerId
    .
  RUN weeknum.p (TODAY, OUTPUT wWeekNum).
    
  /* I variabelen wKriterier vil ved retur fra filterprogram stå avgrensning */
  /* i en kommaseparert liste. Info ligger på formatet                       */
  /*   Fra År, Til år Fra periodelinje, Til periodelinje.                    */
  case wPerId:
    when "AAR"   then 
      do:
        ENTRY(1, wKriterier) = string(year(today) - 1).
        ENTRY(2, wKriterier) = string(year(today)).
        ENTRY(3, wKriterier) = "1".
        ENTRY(4, wKriterier) = "1".
      end.
    when "MANED" then 
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(month(today)).
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(month(today)).
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(month(today)).
          end.
        else
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(month(today)).
          end.
      end.
    when "UKE"   then 
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = SUBstring(STRING(wWeekNum,"999999"),5).
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = SUBstring(STRING(wWeekNum,"999999"),5).
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = SUBstring(STRING(wWeekNum,"999999"),5).
          end.
        else
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = SUBstring(STRING(wWeekNum,"999999"),5).
          end.
      end. 
    otherwise /* DAG */
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(today - date(01,01,YEAR(TODAY)) + 1).
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(today - date(01,01,YEAR(TODAY)) + 1).
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(today - date(01,01,YEAR(TODAY)) + 1).
          end.
        else
          do:
            ENTRY(1, wKriterier) = string(year(today) - 1).
            ENTRY(2, wKriterier) = string(year(today)).
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = string(today - date(01,01,YEAR(TODAY)) + 1).
          end.
      end. 
  end case.

  RUN SaveToLokalIni IN h_dproclib ("PERID", wPerId).
  RUN SaveToLokalIni IN h_dproclib ("KRITERIER", wKriterier).

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
  DISPLAY CB-PerId FI-Filter T-Tot FI-Fltertekst 
      WITH FRAME fMain.
  ENABLE B-Excel CB-PerId B-Grafikk T-Tot B-Rapp B-Rapp-HtmEx B-SokDato RECT-1 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
  DEF VAR pcTekst AS CHAR NO-UNDO.
                                   
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Setter sentrallager butikk */
  {syspara.i 5 1 1 wCl INT}

  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /* Leser kriterier og periodetype fra INI filen */
  DISPLAY
      T-Tot
  WITH FRAME {&FRAME-NAME}. 

  RUN InitSkjerm.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSkjerm fFrameWin 
PROCEDURE InitSkjerm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  if can-do("AAR,MANED,UKE",wPerId) then
  assign
    FI-Filter  = entry(1,wKriterier) + "/" + 
                 entry(3,wKriterier) + " - " + 
                 entry(2,wKriterier) + "/" + 
                 entry(4,wKriterier).
  else
    assign
      FI-Filter  = string(date(1,1,int(entry(1,wKriterier))) + int(entry(3,wKriterier)) - 1) + " - " +
                   string(date(1,1,int(entry(2,wKriterier))) + int(entry(4,wKriterier)) - 1).

  if entry(5,wKriterier) <> "0" then
    FI-Filter = FI-Filter + " Butikk " + entry(5,wKriterier).
  display
    FI-Filter.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVar fFrameWin 
PROCEDURE InitVar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcPerId     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pT-Tot      AS LOG  NO-UNDO.
  DEF INPUT PARAMETER piButik     AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcKriterier AS CHAR NO-UNDO.
  
  ASSIGN
      wStTypeId  = pcTekst
      wPerId     = pcPerId
      T-Tot      = pT-Tot
      wbutik     = piButik
      wKriterier = pcKriterier
      .

  DO WITH FRAME {&FRAME-NAME}:
    assign
      pcTekst = ""
      .
    for each StDef no-lock where
      StDef.StTypeId = wStTypeId:
      pcTekst = pcTekst + 
              (if pcTekst = ""
                 then ""
                 else ",") + 
              StDef.Beskrivelse + "," + StDef.PerId.                       
    end.  
    ASSIGN   
      pcTekst = IF pcTekst = "" THEN " , " ELSE pcTekst
      CB-PerId:LIST-ITEM-PAIRS = pcTekst
      CB-PerId = wPerId
      .  
    DISPLAY
        CB-PerId.
  end.

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
  DEF VAR pcFields    AS CHAR NO-UNDO.
  DEF VAR pcValues    AS CHAR NO-UNDO.
  DEF VAR pcSort      AS CHAR NO-UNDO.
  DEF VAR pcOperator  AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR piCount     AS INTE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    RUN InitSkjerm.

    ASSIGN 
        pcFeltListe = "PerId,PerLinNr,PerLinNr,Aar,Aar,TotalPost"
        .

    ASSIGN /* PerId */
        pcFields   = pcFields + 
                     (IF pcFields = "" 
                       THEN "" 
                       ELSE ",") + 
                     "PerId"
        pcValues   = pcValues + 
                     (IF pcValues = "" 
                       THEN "" 
                       ELSE chr(1)) + 
                     CB-PerId:SCREEN-VALUE
        pcOperator = pcOperator + (IF pcOperator = "" 
                       THEN "" 
                       ELSE ",") + 
                     "="
        .

    IF INPUT T-Tot THEN
    ASSIGN /* Totaler */
        pcFields   = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "TotalPost"
        pcValues   = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                     "1"
        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "="
        .

    ASSIGN /* År */
        pcFields   = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Aar,Aar"
        pcValues   = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                     entry(1,wKriterier) + chr(1) + entry(2,wKriterier)
        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=,<="
        .

    IF NUM-ENTRIES(wKriterier) < 9 THEN
    DO:
        DO WHILE NUM-ENTRIES(wKriterier) < 9:
            wKriterier = wKriterier + ",".
        END.
        ASSIGN
            ENTRY (7,wKriterier) = "1"
            ENTRY (8,wKriterier) = "1"
            ENTRY (9,wKriterier) = ""
            .            
    END.

  END.

  
  /*
  MESSAGE pcFields SKIP                  
        pcValues SKIP                  
        pcSort SKIP                    
        pcOperator SKIP                
        pcFeltListe                    
    VIEW-AS ALERT-BOX INFO BUTTONS OK. 
  */
  
  IF wNobygg = FALSE THEN
      PUBLISH "Kriterier" (wKriterier, INPUT CB-PerId).
  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).
  ASSIGN
      wNoBygg = FALSE
      .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

