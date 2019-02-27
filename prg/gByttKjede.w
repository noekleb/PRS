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
    DEFINE INPUT-OUTPUT PARAMETER iKjede        AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iRegion       AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iDistrikt     AS INTEGER    NO-UNDO.
    DEFINE       OUTPUT PARAMETER cKjedeNavn    AS CHARACTER  NO-UNDO.
    DEFINE       OUTPUT PARAMETER cRegionNavn   AS CHARACTER  NO-UNDO.
    DEFINE       OUTPUT PARAMETER cDistriktNavn AS CHARACTER  NO-UNDO.

/* Local INPUT-OUTPUT PARAMETER Definitions ---                                       */

    DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.
DEFINE TEMP-TABLE TT_Kjede
    FIELD KjedeNr LIKE Kjederegion.KjedeNr
    FIELD RegionListItemPairs AS CHAR.
DEFINE TEMP-TABLE TT_Region
    FIELD KjedeNr LIKE Kjededistrikt.KjedeNr
    FIELD RegionNr LIKE Kjededistrikt.RegionNr
    FIELD DistriktListItemPairs AS CHAR.

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
&Scoped-Define ENABLED-OBJECTS RECT-11 Btn_OK CB-Kjede Btn_Cancel CB-Region ~
CB-Distrikt Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS CB-Kjede CB-Region CB-Distrikt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-Distrikt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Distrikt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kjede AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Kjede" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Region AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Region" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     Btn_OK AT ROW 1.24 COL 59
     CB-Kjede AT ROW 1.95 COL 13 COLON-ALIGNED
     Btn_Cancel AT ROW 2.48 COL 59
     CB-Region AT ROW 3.14 COL 13 COLON-ALIGNED
     CB-Distrikt AT ROW 4.29 COL 13 COLON-ALIGNED
     Btn_Help AT ROW 4.48 COL 59
     RECT-11 AT ROW 1.05 COL 1.6
     SPACE(18.39) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg kjedetilhørighet"
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
ON GO OF FRAME gDialog /* Velg kjedetilhørighet */
DO:
    FIND Kjede WHERE Kjede.KjedeNr = INPUT CB-Kjede NO-LOCK.
    FIND KjedeRegion WHERE KjedeRegion.KjedeNr = Kjede.KjedeNr AND
                           KjedeRegion.RegionNr = INPUT CB-Region NO-LOCK.
    FIND KjedeDistrikt WHERE KjedeDistrikt.KjedeNr = Kjede.KjedeNr AND
                             KjedeDistrikt.RegionNr = INPUT CB-Region AND
                             KjedeDistrikt.Distriktnr = INPUT CB-Distrikt NO-LOCK. 

  ASSIGN iKjede        = KjedeDistrikt.KjedeNr
         iRegion       = KjedeDistrikt.RegionNr
         iDistrikt     = KjedeDistrikt.Distriktnr
         cKjedeNavn    = Kjede.KjedeNavn
         cRegionNavn   = KjedeRegion.RegionNavn
         cDistriktNavn = KjedeDistrikt.DistriktNavn
         cReturVerdi   = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Velg kjedetilhørighet */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help gDialog
ON CHOOSE OF Btn_Help IN FRAME gDialog /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kjede
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kjede gDialog
ON VALUE-CHANGED OF CB-Kjede IN FRAME gDialog /* Kjede */
DO:
  FIND TT_Kjede WHERE TT_Kjede.KjedeNr = INPUT CB-Kjede.
  ASSIGN CB-Region:LIST-ITEM-PAIRS = TT_Kjede.RegionListItemPairs
         CB-Region:SCREEN-VALUE = ENTRY(2,CB-Region:LIST-ITEM-PAIRS).
  FIND TT_Region WHERE TT_Region.KjedeNr = TT_Kjede.KjedeNr AND
                       TT_Region.RegionNr = INPUT CB-Region.
  ASSIGN CB-Distrikt:LIST-ITEM-PAIRS = TT_Region.DistriktListItemPairs.
         CB-Distrikt:SCREEN-VALUE = ENTRY(2,CB-Distrikt:LIST-ITEM-PAIRS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Region
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Region gDialog
ON VALUE-CHANGED OF CB-Region IN FRAME gDialog /* Region */
DO:
    FIND TT_Region WHERE TT_Region.KjedeNr = INPUT CB-Kjede AND
                         TT_Region.RegionNr = INPUT CB-Region.
    ASSIGN CB-Distrikt:LIST-ITEM-PAIRS = TT_Region.DistriktListItemPairs.
           CB-Distrikt:SCREEN-VALUE = ENTRY(2,CB-Distrikt:LIST-ITEM-PAIRS).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RETURN cReturVerdi.
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
  DISPLAY CB-Kjede CB-Region CB-Distrikt 
      WITH FRAME gDialog.
  ENABLE RECT-11 Btn_OK CB-Kjede Btn_Cancel CB-Region CB-Distrikt Btn_Help 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBListItem gDialog 
PROCEDURE InitCBListItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cKjedeListItemPairs  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRegionListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDistriktListItemPairs AS CHARACTER  NO-UNDO.
/* DEFINE TEMP-TABLE TT_Kjede                     */
/*     FIELD KjedeNr LIKE Kjederegion.KjedeNr     */
/*     FIELD RegionListItemPairs AS CHAR          */
/* DEFINE TEMP-TABLE TT_Region                    */
/*     FIELD KjedeNr LIKE Kjededistrikt.KjedeNr   */
/*     FIELD RegionNr LIKE Kjededistrikt.RegionNr */
/*     FIELD DistriktListItemPairs AS CHAR        */

    FOR EACH Kjede NO-LOCK:
        ASSIGN cKjedeListItemPairs = cKjedeListItemPairs +
                    (IF cKjedeListItemPairs <> "" THEN "," ELSE "") +
                Kjede.KjedeNavn + "," + STRING(Kjede.Kjedenr)
               cRegionListItemPairs = "".
       FOR EACH KjedeRegion OF Kjede NO-LOCK.
           ASSIGN cRegionListItemPairs = cRegionListItemPairs +
                       (IF cRegionListItemPairs <> "" THEN "," ELSE "") +
                   KjedeRegion.RegionNavn + "," + STRING(KjedeRegion.Regionnr)
                  cDistriktListItemPairs = "".
           FOR EACH KjedeDistrikt OF KjedeRegion NO-LOCK.
               ASSIGN cDistriktListItemPairs = cDistriktListItemPairs +
                           (IF cDistriktListItemPairs <> "" THEN "," ELSE "") +
                       KjedeDistrikt.DistriktNavn + "," + STRING(KjedeDistrikt.Distriktnr).
           END.
           CREATE TT_Region.
           ASSIGN TT_Region.KjedeNr = Kjede.KjedeNr
                  TT_Region.RegionNr = KjedeRegion.RegionNr
                  TT_Region.DistriktListItemPairs = cDistriktListItemPairs.
       END.
       CREATE TT_Kjede.
       ASSIGN TT_Kjede.KjedeNr             = Kjede.KjedeNr
              TT_Kjede.RegionListItemPairs = cRegionListItemPairs.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-Kjede:LIST-ITEM-PAIRS = cKjedeListItemPairs
               CB-Kjede:SCREEN-VALUE    = STRING(iKjede).
    END.

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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN InitCBListItem.
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO CB-Kjede.
      ASSIGN CB-Region:SCREEN-VALUE = STRING(iRegion).
      APPLY "VALUE-CHANGED" TO CB-Region.
      ASSIGN CB-Distrikt:SCREEN-VALUE = STRING(iDistrikt).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

