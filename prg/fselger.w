&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
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
DEF VAR wTagListe AS CHAR NO-UNDO.
DEF VAR wLoop     AS INT  NO-UNDO.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 B-Koble B-Endre 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bbutikkselger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbutikkselger AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Endre 
     LABEL "Selgerid..." 
     SIZE 14 BY 1.14.

DEFINE BUTTON B-Koble 
     LABEL "Butikker..." 
     SIZE 14 BY 1.14.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Butikker" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 13.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Koble AT ROW 1.86 COL 57
     B-Endre AT ROW 3.14 COL 57
     FILL-IN-1 AT ROW 1 COL 2 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71 BY 14.48.


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
         HEIGHT             = 14.48
         WIDTH              = 71.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

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

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Endre fFrameWin
ON CHOOSE OF B-Endre IN FRAME fMain /* Selgerid... */
DO:
    
    DEFINE VARIABLE rButikkSelger AS ROWID      NO-UNDO.
    DEFINE VARIABLE cRowIdDB AS CHARACTER  NO-UNDO.
    ASSIGN cRowIdDB = ENTRY(2,ENTRY(1,DYNAMIC-FUNCTION('colValues':U IN h_dbutikkselger,
           INPUT "SelgerId" /* CHARACTER */),CHR(1))).
    IF cRowIdDB <> ? THEN DO:
        ASSIGN rButikkSelger = TO-ROWID(cRowIdDB).
        RUN d-BytSelgerId.w (rButikkSelger).
        IF NOT RETURN-VALUE = "AVBRYT" THEN
            RUN refreshRow IN h_dbutikkselger.
    END.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble fFrameWin
ON CHOOSE OF B-Koble IN FRAME fMain /* Butikker... */
DO:
  DEF BUFFER bufButikkSelger FOR ButikkSelger.

  DEFINE VARIABLE pdSelgerNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iSuggestId AS INTEGER NO-UNDO.
  DEF VAR bIBruk AS LOG NO-UNDO.
  DEF VAR bOk AS LOG NO-UNDO.
  DEF VAR piLoop AS INT NO-UNDO.

  ASSIGN
      pdSelgerNr = int(DYNAMIC-FUNCTION('getForeignValues':U IN h_dbutikkselger))
      .

  DO FOR bufButikkSelger:
    wTagListe = "".
    FOR EACH bufButikkSelger NO-LOCK WHERE
      bufButikkSelger.SelgerNr = pdSelgerNr:
      ASSIGN
        wTagListe = wTagListe +
                    (IF wTagListe <> ""
                       THEN ","
                       ELSE "") +
                    string(bufButikkSelger.ButikkNr).
    END.
  END.
  RUN d-tagbutiker.w (INPUT-OUTPUT wTagListe).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  /* Blank tagliste = Alle koblinger slettes. */
  IF wTagListe = "" THEN 
  DO:
    bOk = FALSE.
    MESSAGE 'Kobling til alle butikker vil bli tatt bort.' SKIP
            'Bekreft at dette skal gjøres.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
    IF bOk THEN
    FOR EACH bufButikkSelger EXCLUSIVE where 
        bufButikkSelger.SelgerNr = pdSelgerNr:
        DELETE bufButikkSelger.
    END.
  END.

  ELSE
    LAGRE-TAG:
    DO FOR bufButikkSelger TRANSACTION:

      /* Renser bort de som ikke skal være der. */
      FOR EACH bufButikkSelger EXCLUSIVE where 
          bufButikkSelger.SelgerNr = pdSelgerNr:
          IF NOT CAN-DO(wTagListe,STRING(bufButikkSelger.ButikkNr)) THEN
              DELETE bufButikkSelger.
      END.
      
      /* se om tidigare registrerade butikkselger har samma Id */
      /* i så fall försöker vi att lagra det på nya poster */
      iSuggestId = 0.
      NRBLOKK:
      FOR EACH bufButikkSelger WHERE 
          bufButikkSelger.SelgerNr = pdSelgerNr NO-LOCK:

          /* Plukker tidligere tildelt nr, og ser om dette er i bruk av andre i de butikkene som skal kobles. */
          bIBruk = FALSE.
          DO wLoop = 1 TO NUM-ENTRIES(wTagListe):
              IF CAN-FIND(ButikkSelger WHERE
                          ButikkSelger.butikkNr = int(ENTRY(wLoop,wTagListe)) AND
                          ButikkSelger.SelgerId = bufButikkSelger.SelgerId AND
                          ButikkSelger.SelgerNr <> bufButikkSelger.SelgerNr 
                          ) THEN
                  bIBruk = TRUE.
          END.
          IF bIBruk THEN
              NEXT NRBLOKK.
          ELSE DO:
              ASSIGN iSuggestId = bufButikkSelger.SelgerId.
              LEAVE NRBLOKK.
          END.
      END. /* NRBLOKK */
      
      /* Ingen tidligere nr tildelt som kan brukes, da forsøker vi med SelgerNr hvis dette er maks 4 siffer */
      IF iSuggestId = 0 AND pdSelgerNr <= 9999 THEN
      DO:
          bIBruk = FALSE.
          DO wLoop = 1 TO NUM-ENTRIES(wTagListe):
              IF CAN-FIND(ButikkSelger WHERE
                          ButikkSelger.butikkNr = int(ENTRY(wLoop,wTagListe)) AND
                          ButikkSelger.SelgerId = INT(pdSelgerNr) AND 
                          ButikkSelger.SelgerNr <> bufButikkSelger.SelgerNr 
                          ) THEN
                  bIBruk = TRUE.
          END.
          IF bIBruk = FALSE THEN
              iSuggestId = pdSelgerNr.
      END.

      /* Finner første ledige nr. */
      IF iSuggestId = 0 THEN
      NESTENR:
      DO:
          DO piLoop = 1 TO 9999:
              bIBruk = FALSE.
              DO wLoop = 1 TO NUM-ENTRIES(wTagListe):
                  IF CAN-FIND(ButikkSelger WHERE
                              ButikkSelger.butikkNr = int(ENTRY(wLoop,wTagListe)) AND
                              ButikkSelger.SelgerId = piLoop AND 
                              ButikkSelger.SelgerNr <> bufButikkSelger.SelgerNr 
                              ) THEN
                      bIBruk = TRUE.
              END.
              IF bIBruk = FALSE THEN
              DO:
                  iSuggestId = piLoop.
                  LEAVE NESTENR.
              END.
          END.
      END. /*NESTENR */
      
      /* Legger til eventuelle nye */
      DO wLoop = 1 TO NUM-ENTRIES(wTagListe):
        FIND FIRST bufButikkSelger EXCLUSIVE-LOCK WHERE
            bufButikkSelger.ButikkNr = int(ENTRY(wLoop,wTagListe)) AND 
            bufButikkSelger.SelgerId = iSuggestId AND
            bufButikkSelger.SelgerNr = pdSelgerNr
            NO-ERROR.
        IF NOT AVAILABLE bufButikkSelger THEN
          DO:
            RUN NyButSelger IN THIS-PROCEDURE (int(ENTRY(wLoop,wTagListe)),pdSelgerNr,iSuggestId,FALSE).
          END.
      END.
    END. /* LAGRE-TAG */

  DYNAMIC-FUNCTION('openQuery':U IN h_dbutikkselger).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

/* {lng.i &SDO = "SDO"} */

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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dbutikkselger.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedbutikkselgerOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dbutikkselger ).
       RUN repositionObject IN h_dbutikkselger ( 5.29 , 59.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/bbutikkselger.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbutikkselger ).
       RUN repositionObject IN h_bbutikkselger ( 1.86 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bbutikkselger ( 12.95 , 53.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dbutikkselger. */
       RUN addLink ( THIS-PROCEDURE , 'Data':U , h_dbutikkselger ).

       /* Links to SmartDataBrowser h_bbutikkselger. */
       RUN addLink ( h_dbutikkselger , 'Data':U , h_bbutikkselger ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bbutikkselger ,
             B-Koble:HANDLE IN FRAME fMain , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY FILL-IN-1 
      WITH FRAME fMain.
  ENABLE RECT-1 B-Koble B-Endre 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelgerSDO fFrameWin 
PROCEDURE GetSelgerSDO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER ph_dselger AS HANDLE NO-UNDO.
  /*
  ASSIGN
      ph_dselger = h_dselger.
  */
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
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_dbutikkselger) THEN
      DYNAMIC-FUNCTION('SetForeignFields':U IN h_dbutikkselger,
                   INPUT "ButikkSelger.SelgerNr,SelgerNr").
  
  SUBSCRIBE TO 'SelgerNaDis' ANYWHERE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyButSelger fFrameWin 
PROCEDURE NyButSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER piButik    AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER pdSelgerNr AS DECIMAL    NO-UNDO.
    DEFINE INPUT  PARAMETER iSuggestId AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER plOpenQuery AS LOGICAL    NO-UNDO.
   
    DEFINE        VARIABLE  piSelgerId AS INTEGER    NO-UNDO.

    IF iSuggestId <> 0 AND NOT CAN-FIND(FIRST ButikkSelger WHERE
                                    ButikkSelger.ButikkNr = piButik    AND
                                    ButikkSelger.SelgerId = iSuggestId) THEN
        ASSIGN piSelgerId = iSuggestId.

    ELSE DO:
        FIND LAST ButikkSelger WHERE
            ButikkSelger.ButikkNr = piButik USE-INDEX ButikkSelgerSelgerId NO-LOCK NO-ERROR.
        IF NOT AVAIL ButikkSelger THEN
            piSelgerId = 1.
        ELSE IF ButikkSelger.SelgerId + 1 < 10000 THEN
            piSelgerId = ButikkSelger.SelgerId + 1.
        ELSE DO piSelgerId = 1 TO 9999:
            IF NOT CAN-FIND(FIRST ButikkSelger WHERE
                                ButikkSelger.ButikkNr = piButik    AND
                                ButikkSelger.SelgerId = piSelgerId) THEN
                LEAVE.
        END.
    END.

    DO TRANSACTION:
        CREATE ButikkSelger.
        ASSIGN
          ButikkSelger.SelgerNr = pdSelgerNr
          ButikkSelger.ButikkNr = piButik
          ButikkSelger.SelgerId = piSelgerId
          NO-ERROR.
        RELEASE ButikkSelger.
    END.
    
    IF plOpenQuery THEN
        DYNAMIC-FUNCTION('openQuery':U IN h_dbutikkselger).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelgerNaDis fFrameWin 
PROCEDURE SelgerNaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iToggle AS INT NO-UNDO.

DO WITH FRAME fMain:

    CASE iToggle:
        WHEN 1 THEN ASSIGN
                      B-Koble:SENSITIVE = FALSE
                      B-Endre:SENSITIVE = FALSE.
        WHEN 2 THEN ASSIGN
                      B-Koble:SENSITIVE = TRUE
                      B-Endre:SENSITIVE = TRUE.
    END CASE.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

