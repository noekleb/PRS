&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

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

/* nedanstående block är standard */
/* för tillfället måste vi hantera det 'gamla' inputsättet */

/* &IF "{&UIB_is_Running}" = ""  &THEN                      */
/*   DEF INPUT-OUTPUT PARAMETER cColValues AS char NO-UNDO. */
/*   DEF INPUT        PARAMETER cFelt      as char NO-UNDO. */
/*   DEF INPUT        PARAMETER cVerdier   as char NO-UNDO. */
/*   DEF INPUT        PARAMETER cStart   as char NO-UNDO.   */
/* &else                                                    */
/*   DEF VAR cColValues AS char NO-UNDO.                    */
/*   DEF VAR cFelt             AS CHAR NO-UNDO.             */
/*   DEF VAR cVerdier          AS CHAR NO-UNDO.             */
/*   DEF VAR cStart          AS CHAR NO-UNDO.               */
/* &ENDIF                                                   */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wVg LIKE VarGr.Vg NO-UNDO.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wVg LIKE VarGr.Vg NO-UNDO.
&ENDIF
  DEF VAR cColValues AS char NO-UNDO.
  DEF VAR cFelt      AS CHAR NO-UNDO.
  DEF VAR cVerdier   AS CHAR NO-UNDO.
  DEF VAR cStart     AS CHAR NO-UNDO.


  
  
  
/* Local Variable Definitions ---                                       */
/* returvärdet manipuleras i destroyObject för gamla program */
DEF VAR cReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR wCBValg       AS CHAR                  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-HuvGr Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS CB-HuvGr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bvargrsmall AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvargr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-HuvGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     CB-HuvGr AT ROW 1.43 COL 28 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 12.81 COL 1
     Btn_Cancel AT ROW 12.81 COL 55
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søkeliste varegrupper"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON GO OF FRAME gDialog /* Søkeliste varegrupper */
DO:
    ASSIGN cReturn-Value = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Søkeliste varegrupper */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HuvGr gDialog
ON VALUE-CHANGED OF CB-HuvGr IN FRAME gDialog
DO:
  RUN Avgrens (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{lng.i &SDO = "SDO"}

{src/adm2/dialogmn.i}

RETURN cReturn-Value.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
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
             INPUT  'sdo/dvargr.wDB-AWARE':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvargrUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dvargr ).
       RUN repositionObject IN h_dvargr ( 11.71 , 37.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 11.20 ) */

       RUN constructObject (
             INPUT  'prg/bvargrsmall.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bvargrsmall ).
       RUN repositionObject IN h_bvargrsmall ( 2.91 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bvargrsmall ( 9.76 , 69.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataBrowser h_bvargrsmall. */
       RUN addLink ( h_dvargr , 'Data':U , h_bvargrsmall ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_bvargrsmall , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_sortsok ,
             CB-HuvGr:HANDLE , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bvargrsmall ,
             CB-HuvGr:HANDLE , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avgrens gDialog 
PROCEDURE Avgrens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cAvgrensVerdi AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cQuerySort AS CHARACTER    NO-UNDO.
   ASSIGN cQuerySort = DYNAMIC-FUNCTION('getQuerySort':U IN h_dvargr).
   DYNAMIC-FUNCTION('setQueryWhere':U IN h_dvargr,
     INPUT (IF cAvgrensVerdi = wCBValg THEN "" ELSE "Hg = " + TRIM(cAvgrensVerdi)) /* CHARACTER */).
   DYNAMIC-FUNCTION('setQuerySort':U IN h_dvargr,
     INPUT cQuerySort /* CHARACTER */).
   DYNAMIC-FUNCTION('openQuery':U IN h_dvargr).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects gDialog 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  SUBSCRIBE "MouseDblClick" IN h_bvargrsmall.

  /* Setter opp Where sats i Query.  */
  /* Legges inn der hvor det skal gjøres avgrensning I datasett for søk. */
  ASSIGN cFelt = STRING(wVg,"zzzz").
  IF cFelt <> "" THEN
  DO:
    DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dvargr,
      INPUT cFelt,    /* Comma separerte verdier  */
      INPUT cVerdier, /* CHR(1) separerte verdier */
      INPUT "EQ").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject gDialog 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF cReturn-Value <> "AVBRYT" THEN  
   ASSIGN
       cColValues = DYNAMIC-FUNCTION('colValues':U IN h_dvargr,
                      INPUT "Vg,VgBeskr" /* CHARACTER */).  
   FIND VarGr NO-LOCK WHERE VarGr.Vg = INT(ENTRY(2,cColValues,CHR(1))) NO-ERROR.
   ASSIGN cReturn-Value = IF AVAIL VarGr THEN STRING(RECID(VarGr)) ELSE "AVBRYT".
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY CB-HuvGr 
      WITH FRAME gDialog.
  ENABLE CB-HuvGr Btn_OK Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB gDialog 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    {syspara.i 1 100 1 wCBValg}
    ASSIGN cListItemPairs = wCBValg + "," + wCBValg.
    FOR EACH HuvGr NO-LOCK BY HuvGr.HgBeskr:
         ASSIGN cListItemPairs = cListItemPairs + "," + STRING(HuvGr.Hg,"zzz9") + " " + HuvGr.HgBeskr + "," + STRING(HuvGr.Hg).
    END.
    ASSIGN CB-HuvGr:LIST-ITEM-PAIRS = cListItemPairs.
  END.
  ASSIGN CB-HuvGr:SCREEN-VALUE = wCBValg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SwitchLng.
  RUN SUPER.
  RUN InitCB.
  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "Sortera" FROM h_bvargrsmall.

  DYNAMIC-FUNCTION('findRowWhere':U IN h_dvargr,
     INPUT cFelt    + (IF cFelt = "" THEN "" ELSE ",") + "Vg",
     INPUT cVerdier + (IF cVerdier = "" THEN "" ELSE CHR(1)) + cStart,
     INPUT "EQ,EQ" /* CHARACTER */).
  /* Signalerer til de andre objektene. */
  RUN dataAvailable IN h_dvargr
    ( INPUT "SAME" /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MouseDblClick gDialog 
PROCEDURE MouseDblClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "CHOOSE":U TO Btn_OK IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

