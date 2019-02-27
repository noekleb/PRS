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
DEFINE VARIABLE h_dordre AS HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS B-Koble 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Koble  NO-FOCUS
     LABEL "&Koble bestillinger..." 
     SIZE 29 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Koble AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 29.2 BY 1.33.


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
         HEIGHT             = 1.33
         WIDTH              = 29.2.
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

&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble fFrameWin
ON CHOOSE OF B-Koble IN FRAME fMain /* Koble bestillinger... */
DO:
    DEFINE VARIABLE iLevNr   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iOrdreNr AS INTEGER    NO-UNDO.
    DEFINE VAR IO-Liste      AS CHAR            NO-UNDO.
    DEFINE VAR IO2-Liste     AS CHAR            NO-UNDO.
    DEFINE VAR Slett-Liste   AS CHAR            NO-UNDO.
    DEFINE VAR Nye-Liste     AS CHAR            NO-UNDO.
    DEFINE VAR wHelListe     AS CHAR            NO-UNDO.
    DEFINE VAR wBestNr       Like Butiker.Butik NO-UNDO.
    define var wLoop         as int             NO-UNDO.
    DEFINE VARIABLE cColValues AS CHARACTER  NO-UNDO.
    

    ASSIGN cColValues = DYNAMIC-FUNCTION('colValues':U IN h_dordre,
       INPUT "LevNr,OrdreNr" /* CHARACTER */).
    IF NUM-ENTRIES(cColValues,CHR(1)) <> 3 THEN
        RETURN NO-APPLY.
    ASSIGN iLevNr   = INT(ENTRY(2,cColValues,CHR(1)))
           iOrdreNr = INT(ENTRY(3,cColValues,CHR(1))).
    /*Bygger strengen med bestillinger.*/
    ASSIGN IO-Liste = DYNAMIC-FUNCTION('getKobledeBest':U IN h_dordre,
                          INPUT iLevNr /* INTEGER */,
                          INPUT iOrdreNr /* INTEGER */)
           IO2-Liste = IO-Liste. /* Tar vare på listen */

    IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
    RUN d-tagbesthode.w (INPUT-OUTPUT IO-Liste, iLevNr).
    DO wLoop = 1 TO NUM-ENTRIES(IO2-Liste):
        IF NOT CAN-DO(IO-Liste,ENTRY(wLoop,IO2-Liste)) THEN
            ASSIGN Slett-Liste = Slett-Liste + 
                                 (IF Slett-Liste = "" THEN "" ELSE ",") + 
                                 ENTRY(wLoop,IO2-Liste).
    END.
    DO wLoop = 1 TO NUM-ENTRIES(IO-Liste):
        IF NOT CAN-DO(IO2-Liste,ENTRY(wLoop,IO-Liste)) THEN
            ASSIGN Nye-Liste = Nye-Liste + 
                                 (IF Nye-Liste = "" THEN "" ELSE ",") + 
                                 ENTRY(wLoop,IO-Liste).
    END.
    IF Slett-Liste <> "" OR Nye-Liste <> "" THEN DO:
        RUN KobleBest IN h_dordre (INPUT Slett-Liste,INPUT Nye-Liste,INPUT iOrdreNr) NO-ERROR.
        PUBLISH "OpenQueryBest".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */
/* {lng.i &SDO="SDO"} */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable fFrameWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  IF VALID-HANDLE(h_dordre) THEN DO:
      /* Skal alltid være aktiv 
      ASSIGN B-Koble:SENSITIVE IN FRAME {&FRAME-NAME} =
          INT(DYNAMIC-FUNCTION('columnValue':U IN h_dordre,
     INPUT "OrdreStatus" /* CHARACTER */)) = 1.
     */
  END.
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
  ENABLE B-Koble 
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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h_dordre = DYNAMIC-FUNCTION('getDataSource':U).
  IF VALID-HANDLE(h_dordre) THEN
     RUN refreshRow IN h_dordre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

