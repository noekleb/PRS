&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_btstlinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dselger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtstlinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fhistorikkfilter AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.8 BY 21.76.


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
         HEIGHT             = 21.76
         WIDTH              = 157.8.
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
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


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
             INPUT  'sdo/dselger.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedselgerUpdateFromSourceno':U ,
             OUTPUT h_dselger ).
       RUN repositionObject IN h_dselger ( 1.00 , 144.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/dtstlinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstStLinje.DataObjekt,fuDataObjektRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtstlinjeUpdateFromSourceno':U ,
             OUTPUT h_dtstlinje ).
       RUN repositionObject IN h_dtstlinje ( 1.00 , 134.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.20 ) */

       RUN constructObject (
             INPUT  'prg/btstlinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoDataSourceNamesUpdateTargetNamesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btstlinje ).
       RUN repositionObject IN h_btstlinje ( 3.14 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_btstlinje ( 19.52 , 157.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/fhistorikkfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fhistorikkfilter ).
       RUN repositionObject IN h_fhistorikkfilter ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 156.60 ) */

       /* Links to SmartDataObject h_dselger. */
       RUN addLink ( THIS-PROCEDURE , 'Data':U , h_dselger ).

       /* Links to SmartDataObject h_dtstlinje. */
       RUN addLink ( h_dselger , 'Data':U , h_dtstlinje ).
       RUN addLink ( h_fhistorikkfilter , 'SokSdo':U , h_dtstlinje ).

       /* Links to SmartDataBrowser h_btstlinje. */
       RUN addLink ( h_dtstlinje , 'Data':U , h_btstlinje ).
       RUN addLink ( h_btstlinje , 'Update':U , h_dtstlinje ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_btstlinje ,
             h_fhistorikkfilter , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects fFrameWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pT-Tot      AS LOG  NO-UNDO.
  DEF VAR pibutik     AS INT  NO-UNDO.
  DEF VAR pcKriterier AS CHAR NO-UNDO.
  DEF VAR pcPerId     AS CHAR NO-UNDO.
  DEF VAR pcTekst     AS CHAR NO-UNDO.
                       
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DYNAMIC-FUNCTION('SetForeignFields':U IN h_dselger,
                   INPUT "Selger.SelgerNr,SelgerNr").

  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dtstlinje,
     INPUT "tStLinje.StTypeId = 'SELGER'" /* CHARACTER */).

  RUN ReadFromLokalIni IN h_dproclib ("KRITERIER", OUTPUT pcKriterier).
  IF pcKriterier = "" THEN
  DO:
    ASSIGN
      pcPerId     = "MANED"
      pcKriterier = string(year(today - 182)) + "," +
                    string(year(today)) + "," +
                    "1," + string(month(today)) + ",0,0,1,1" /* All statistikk */.
    RUN SaveToLokalIni IN h_dproclib ("VISTOT", "YES").
    RUN SaveToLokalIni IN h_dproclib ("PERID", pcPerId).
    RUN SaveToLokalIni IN h_dproclib ("KRITERIER", pcKriterier).  
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:
    RUN ReadFromLokalIni IN h_dproclib ("PERID", OUTPUT pcPerId).
    RUN ReadFromLokalIni IN h_dproclib ("VISTOT", OUTPUT pcTekst).
    IF CAN-DO("JA,TRUE,YES",pcTekst) THEN
          pT-Tot = TRUE.
      ELSE 
          pT-Tot = FALSE.
    if pT-Tot then
      piButik = 999999.
    else
      piButik = 0.
  END.

  RUN InitVar IN h_dtstlinje ("SELGER", pcPerId, pT-tot, piButik, pcKriterier).
  RUN InitVar IN h_fhistorikkfilter ("SELGER", pcPerId, pT-tot, piButik, pcKriterier).

  SUBSCRIBE PROCEDURE h_btstlinje "ExHtmRapp"   IN h_fhistorikkfilter.
  SUBSCRIBE PROCEDURE h_dtstlinje "StatGrafikk" IN h_fhistorikkfilter.
  SUBSCRIBE PROCEDURE h_dtstlinje "Kriterier"   IN h_fhistorikkfilter.
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

  ASSIGN
      ph_dselger = h_dselger.
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
  RUN initPages("4").
  RUN StartSok IN h_fhistorikkfilter.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

