&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hBrwColEANSerieAktiv AS HANDLE NO-UNDO.
DEF VAR hBrwColAntLedig      AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.

DEF VAR cTekst          AS CHAR   NO-UNDO.
DEF VAR cEANType        AS CHAR   NO-UNDO.
DEF VAR cEANLandKode    AS CHAR   NO-UNDO.
DEF VAR cAntSiffer      AS CHAR   NO-UNDO.
DEF VAR cAntSiffer8     AS CHAR   NO-UNDO.
DEF VAR cMsg            AS CHAR   NO-UNDO.
DEF VAR rRowid          AS ROWID  NO-UNDO.

DEF VAR piJBoxCompanyId LIKE jBoxCompany.iJBoxCompanyId  NO-UNDO.
DEF VAR piCL            LIKE jBoxCompany.CL NO-UNDO.

DEF TEMP-TABLE ttVerdier
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    .
DEF VAR httVerdier  AS HANDLE NO-UNDO.
httVerdier = BUFFER ttVerdier:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar RECT-1 ~
EANSerieId EANSerieAktiv EANBeskrivelse EANType EANLandKode AntSifferILevNr ~
btnPostnr CL EANLevNr FraEANArtikkelNr TilEANArtikkelNr AntLedig 
&Scoped-Define DISPLAYED-OBJECTS EANSerieId EANSerieAktiv EANBeskrivelse ~
EANType EANLandKode AntSifferILevNr CL cCompanyName EANLevNr ~
FraEANArtikkelNr TilEANArtikkelNr AntLedig 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE AntSifferILevNr AS INTEGER FORMAT "9" INITIAL 0 
     LABEL "Ant.siffer i levnr" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE EANLandKode AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "EAN landkode" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "EAN landkode 70-Norge, 73-Sverige og 20-Bedriftsintern" NO-UNDO.

DEFINE VARIABLE EANType AS INTEGER FORMAT ">9" INITIAL 13 
     LABEL "EANType" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Ean type 8, 13 eller 14." NO-UNDO.

DEFINE VARIABLE AntLedig AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Ant. ledige koder" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cCompanyName AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1.

DEFINE VARIABLE CL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE EANBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE EANLevNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "EANLevNr" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE EANSerieId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "EAN serieid" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FraEANArtikkelNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Fra EAN art.nr" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE VARIABLE TilEANArtikkelNr AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Til EAN art.nr" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 9.76.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 127 BY 25.48.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE EANSerieAktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 TOOLTIP "Akviterer eller lukker EAN nummerserien." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EANSerieId AT ROW 2.76 COL 144.6 COLON-ALIGNED
     EANSerieAktiv AT ROW 2.76 COL 164.6 HELP
          "Angir om EAN nummerserie er aktiv eller lukket."
     EANBeskrivelse AT ROW 3.76 COL 144.6 COLON-ALIGNED
     EANType AT ROW 4.71 COL 144.6 COLON-ALIGNED HELP
          "Ean type (EAN 8, 13 eller 14)"
     EANLandKode AT ROW 5.71 COL 144.6 COLON-ALIGNED HELP
          "EAN landkode"
     AntSifferILevNr AT ROW 6.71 COL 144.6 COLON-ALIGNED HELP
          "Antall siffer i leverandørnummer"
     btnPostnr AT ROW 7.67 COL 158.6 NO-TAB-STOP 
     CL AT ROW 7.71 COL 144.6 COLON-ALIGNED HELP
          "Sentrallager"
     cCompanyName AT ROW 7.71 COL 160.6 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     EANLevNr AT ROW 8.71 COL 144.6 COLON-ALIGNED
     FraEANArtikkelNr AT ROW 9.71 COL 144.6 COLON-ALIGNED
     TilEANArtikkelNr AT ROW 10.71 COL 144.6 COLON-ALIGNED
     AntLedig AT ROW 12.43 COL 144.6 COLON-ALIGNED
     rectBrowse AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 178
     RECT-1 AT ROW 2.43 COL 128.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 187 BY 27.


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
         TITLE              = "EAN nummerserier"
         HEIGHT             = 27
         WIDTH              = 187
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN cCompanyName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* EAN nummerserier */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* EAN nummerserier */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* EAN nummerserier */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cCompanyFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "jBoxCompany"
                     + ";iJBoxCompanyId|Firma;cCompanyName|Firmanavn"
                     + ";CL|Sentrallager;dModified|Sist endret"
                     ,
                   "WHERE true"
                    ,""
                    ,"cCompanyName,CL",
                    OUTPUT cCompanyFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cCompanyFieldList NE "" THEN DO WITH FRAME DEFAULT-FRAME:
    ASSIGN 
       cCompanyName:SCREEN-VALUE = ENTRY(1,cCompanyFieldList,"|")
       CL:SCREEN-VALUE           = ENTRY(2,cCompanyFieldList,"|")
       .
    APPLY "any-printable" TO CL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cHarPoster AS CHAR NO-UNDO.

  IF hBrowse:NUM-SELECTED-ROWS NE 1 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Sletting kan bare gjøre hvis én nummerserie er valgt","","").
    RETURN.
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        cHarPoster = DYNAMIC-FUNCTION("getFieldValues","EANNrListe",
                                             "WHERE EANSerieId = '" + EANSerieId:SCREEN-VALUE + "'" +
                                                " and EANKode >= ''",
                                             "EANKode")
        cHarPoster = IF cHarPoster = ? OR cHarPoster = '?' THEN '' ELSE cHarPoster.

    IF cHarPoster = '' THEN
        bOk = YES.
    ELSE DO:
        bOk = NO.
        RUN dEANNrSerieSlettAdvarsel.w (OUTPUT bOk).
        IF bOk <> TRUE THEN
            RETURN.
    END.
    IF bOk THEN
      IF NOT DYNAMIC-FUNCTION("runproc","eannrserie_slett.p",EANSerieId:SCREEN-VALUE IN FRAME {&FRAME-NAME},?) THEN
      DO:
          DYNAMIC-FUNCTIO("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
          RETURN.
      END.
  END.

  RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cHarPoster      AS CHAR NO-UNDO.
  DEF VAR cValg           AS CHAR NO-UNDO.
  DEF VAR cDisabledEvents AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      RUN initAntSifferILevNr (1).
  END.

  IF hBuffer:AVAIL THEN DO:
      IF (hBuffer:BUFFER-FIELD("AntLedig"):BUFFER-VALUE <> 0) OR
         (hBuffer:BUFFER-FIELD("EANSerieAktiv"):BUFFER-VALUE = TRUE)     
          THEN cValg = 'GenererEAN'.
      ELSE ASSIGN cValg = ''.

      ASSIGN
          cDisabledEvents = cValg.
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
  END.

  RUN SUPER.
 
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
         cHarPoster = DYNAMIC-FUNCTION("getFieldValues","EANNrListe",
                                              "WHERE EANSerieId = '" + EANSerieId:SCREEN-VALUE + "'" +
                                                 " and EANKode >= ''",
                                              "EANKode")
         cHarPoster = IF cHarPoster = ? OR cHarPoster = '?' THEN '' ELSE cHarPoster
         EANSerieId:SENSITIVE       =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
         EANType:SENSITIVE          =  (DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' OR cHarPoster = '')
         EANLandKode:SENSITIVE      =  (DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' OR cHarPoster = '')
         AntSifferILevNr:SENSITIVE  =  (int(EANType:SCREEN-VALUE) = 13) AND
                                       (DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' OR cHarPoster = '')
         CL:SENSITIVE               =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'
         EANLevNr:SENSITIVE         =  (int(EANType:SCREEN-VALUE) = 13) AND
                                       (DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' OR cHarPoster = '')
         FraEANArtikkelNr:SENSITIVE =  FALSE 
         TilEANArtikkelNr:SENSITIVE =  FALSE 
         .
     /* Bare tilgang til Aktiv knapp etter at EAN nr er generert. */
     IF NOT hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND 
         int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) = 0
         THEN EANSerieAktiv:SENSITIVE = FALSE.
     ELSE EANSerieAktiv:SENSITIVE = TRUE.

     IF DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' THEN
         RUN setEANLevNrFormat.
  END.

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
  DISPLAY EANSerieId EANSerieAktiv EANBeskrivelse EANType EANLandKode 
          AntSifferILevNr CL cCompanyName EANLevNr FraEANArtikkelNr 
          TilEANArtikkelNr AntLedig 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar RECT-1 EANSerieId EANSerieAktiv 
         EANBeskrivelse EANType EANLandKode AntSifferILevNr btnPostnr CL 
         EANLevNr FraEANArtikkelNr TilEANArtikkelNr AntLedig 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenererEANRecord C-Win 
PROCEDURE GenererEANRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      MESSAGE "Skal generering av EAN koder startes?" SKIP(1)
          (IF int(TilEANArtikkelNr:SCREEN-VALUE) > 999 
             THEN 'Generering kan ta opptil flere minutter.'
             ELSE '')
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
      IF NOT bOk THEN
          RETURN NO-APPLY.
      ELSE DO:

        IF SESSION:SET-WAIT-STATE("GENERAL") THEN.
        IF DYNAMIC-FUNCTION("runproc",
                         "eannrserie_generer.p",
                         EANSerieId:SCREEN-VALUE,
                         ?
                         ) THEN 
        DO:
            IF SESSION:SET-WAIT-STATE("") THEN.
            cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
            MESSAGE "EAN koder for nummerserie er generert og klar til bruk." SKIP(1)
                cMsg
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        END.
        ELSE DO: 
            IF SESSION:SET-WAIT-STATE("") THEN.
            cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
            MESSAGE "Feil oppsto ved generering av nummerserie. " SKIP(1)
                    cMsg
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initAntSifferILevNr C-Win 
PROCEDURE initAntSifferILevNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER piModus AS INT NO-UNDO. /* 1=Fra displayrecord, 2=Fra EventProc */

DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL THEN
  MAP_AVAIL:
  DO:
      /* Ved oppstart */
      IF (IF piModus = 1 THEN 
            int(hFieldMap:BUFFER-FIELD("EANType"):BUFFER-VALUE)
          ELSE int(EANType:screen-value)) = 13 THEN
      DO:
          ASSIGN
          AntSifferILevNr:DELIMITER = "|"
          AntSifferILevNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                            "SysPara;Parameter1",
                                                            "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 2").
          IF NOT can-do(replace(AntSifferILevNr:LIST-ITEM-PAIRS,'|',','),AntSifferILevNr:SCREEN-VALUE) THEN
              AntSifferILevNr:SCREEN-VALUE = cAntSiffer.
      END.
      /* Ved redigering. */
      ELSE /* 8 */
      DO:
          ASSIGN
          AntSifferILevNr:DELIMITER = "|"
          AntSifferILevNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                            "SysPara;Parameter1",
                                                            "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 4").
          IF NOT can-do(replace(AntSifferILevNr:LIST-ITEM-PAIRS,'|',','),AntSifferILevNr:SCREEN-VALUE) THEN
              AntSifferILevNr:SCREEN-VALUE = cAntSiffer8.
      END.
  END. /* MAP_AVAIL */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME  {&FRAME-NAME}.
  ASSIGN
    piJBoxCompanyId = int(entry(1,DYNAMIC-FUNCTION("getFieldList",
                                      "jBoxCompanyUser;iJBoxCompanyId",
                                      "WHERE cJBoxUserId = '" + USERID('SkoTex') + "'"),'|'))
    piCL            = int(DYNAMIC-FUNCTION("getFieldList",
                                      "jBoxCompany;CL",
                                      "WHERE iJBoxCompanyId = '" + string(piJBoxCompanyId) + "'"))
    cEANType = DYNAMIC-FUNCTION("getFieldList",
                                     "SysPara;Parameter2",
                                     "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 1")
    cAntSiffer = DYNAMIC-FUNCTION("getFieldList",
                                     "SysPara;Parameter2",
                                         "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 2")
    cAntSiffer8 = DYNAMIC-FUNCTION("getFieldList",
                                     "SysPara;Parameter2",
                                         "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 4")
    cEANLandKode = DYNAMIC-FUNCTION("getFieldList",
                                         "SysPara;Parameter2",
                                         "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 3")
    EANType:DELIMITER = "|"
    EANType:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                               "SysPara;Parameter1",
                                               "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 1")
    EANLandKode:DELIMITER = "|"
    EANLandKode:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                  "SysPara;Parameter1",
                                                  "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 3")
    AntSifferILevNr:DELIMITER = "|"
    AntSifferILevNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                      "SysPara;Parameter1",
                                                      "WHERE SysHId = 1 AND SysGr = 90 and ParaNr = 2")
    .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "EANNrSerie"
                    + ";EANSerieId|SerieId;EANSerieAktiv|Aktiv|*/ "  
                    + ";EANBeskrivelse;EANType|Type;EANLandKode|Landkode;AntSifferILevNr|Siffer i levnr;EANLevNr"
                    + ";+AntLedig|decimal|->>>><>>9|AntLedig|Ant.ledig"
                    + ";FraEANArtikkelNr|Fra art.nr"
                    + ";TilEANArtikkelNr|Til art.nr"
                    /* + ";+AntLedig|decimal|->>>><>>9|eannrserie_antledig.p|Ant.ledig" */
                    + ";CL;RegistrertDato;!RegistrertTid|Opprettet;!RegistrertAv;!ETid;EDato|Endret;!BrukerID"
                   ,"WHERE false"
                    ,"sort|EANSerieId").             /* Initial sort column */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "EANSerieAktiv,EANBeskrivelse,EANType,EANLandKode,AntSifferILevNr,EANLevNr,CL",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "EANSerieId,FraEANArtikkelNr,TilEANArtikkelNr,AntLedig",        /* Additional buffer and displ.fields - not updateable*/
                      "EANSerieId,FraEANArtikkelNr,TilEANArtikkelNr,AntLedig",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','eannrserie_brwcalc.p').
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"NoDeleteWarning","yes").
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + ",excel;Eksporter til E&xcel,rule"  
                    + ",OppdaterFelt;Oppdater felt på valgt linje,GenererEAN;Generer EAN koder" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

  RUN InvokeMethod (hBrowse,"OpenQuery").

  APPLY "value-changed" TO hBrowse.
  APPLY 'entry' TO EANBeskrivelse.

END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-1").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-1").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,450,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
 IF VALID-HANDLE(hBrowse) THEN APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.

  /* Legger opp default verdier på ny record. */
  IF DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New' THEN
  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          EANType:SCREEN-VALUE         = cEANType
          EANLandKode:SCREEN-VALUE     = cEANLandKode
          CL:SCREEN-VALUE              = STRING(piCL)
          EANSerieAktiv:SCREEN-VALUE   = 'no'
          EANSerieAktiv:SENSITIVE      = FALSE
          NO-ERROR.
      RUN setEANLevNrFormat.
      RUN initAntSifferILevNr (2).
      ASSIGN
          AntSifferILevNr:SCREEN-VALUE = cAntSiffer.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFeltRecord C-Win 
PROCEDURE OppdaterFeltRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  DO:
      /* Ikke initierte nummerserier er turkis. */
      IF NOT hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND 
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) = 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 11.

      /* Aktive og ledige nummerserier er grønne. */
      IF hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) > 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 10.
      
      /* Aktive fulle nummerserier er røde. */
      IF hFieldMap:buffer-field('EANSerieAktiv'):BUFFER-VALUE() AND
          int(hFieldMap:buffer-field('AntLedig'):BUFFER-VALUE()) = 0
          THEN hBrwColEANSerieAktiv:BGCOLOR = 12.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFinnes AS CHAR NO-UNDO.

  DO WITH FRAME Default-Frame:
    IF EANBeskrivelse:SCREEN-VALUE = '' THEN
    DO:
        MESSAGE "Det er ikke angitt beskrivelse for EAN nummerserien. " SKIP
                "Beskrivlese må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF int(EANLevNr:SCREEN-VALUE) = 0 AND int(EANType:SCREEN-VALUE) = 13 THEN
    DO:
        MESSAGE "Det er ikke angitt leverandørnummer for EAN nummerserien. " SKIP
                "Leverandørnummer må angis." SKIP(1)
                (IF int(EANType:SCREEN-VALUE) = 2 
                   THEN "For bedriftsinterne EAN koder, legg inn butikknummer i leverandørnummer."
                   ELSE "")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    ASSIGN
        cFinnes = DYNAMIC-FUNCTION("getFieldValues","EANNrSerie",
                                             "WHERE EANLandKode = '" + EANLandKode:SCREEN-VALUE + "'" +
                                                " and AntSifferILevNr = '"  + AntSifferILevNr:SCREEN-VALUE + "'" + 
                                                " and EANLevNr = '"  + EANLevNr:SCREEN-VALUE + "'" + 
                                                " and EANType = '"  + EANType:SCREEN-VALUE + "'" +
                                                " and EANSerieId <> '" + EANSerieId:SCREEN-VALUE + "'",
                                             "EANSerieId")
        cFinnes = IF cFinnes = ? OR cFinnes = '?' THEN '' ELSE cFinnes
        .
    IF cFinnes <> '' THEN
    DO:
        MESSAGE "Det er allerede lagt opp en EANNrSerie for denne kombinasjonen av landkode og leverandør." SKIP
                "Serieid: " + cFinnes + '.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    RUN SUPER.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEANLevNrFormat C-Win 
PROCEDURE setEANLevNrFormat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      CASE int(AntSifferILevNr:SCREEN-VALUE):
          WHEN 4 THEN DO:
              IF INT(EANLevNr:SCREEN-VALUE) > 9999 THEN
                  EANLevNr:SCREEN-VALUE = '0'.
              EANLevNr:FORMAT = '>>>9'.
          END.
          WHEN 5 THEN DO:
              IF INT(EANLevNr:SCREEN-VALUE) > 99999 THEN
                  EANLevNr:SCREEN-VALUE = '0'.
              EANLevNr:FORMAT = '>>>>9'.
          END.
          WHEN 7 THEN DO:
              IF INT(EANLevNr:SCREEN-VALUE) > 9999999 THEN
                  EANLevNr:SCREEN-VALUE = '0'.
              EANLevNr:FORMAT = '>>>>>>9'.
          END.
          OTHERWISE  DO:
              IF INT(EANLevNr:SCREEN-VALUE) > 9999 THEN
                  EANLevNr:SCREEN-VALUE = '0'.
              EANLevNr:FORMAT = '>>>9'.
          END.
      END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cField AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      CASE cField:
          WHEN 'AntSifferILevNr' THEN do: 
              RUN setEANLevNrFormat.
              IF LENGTH(EANLevNr:SCREEN-VALUE) > INT(AntSifferILevNr:SCREEN-VALUE) THEN
                  EANLevNr:SCREEN-VALUE = '0'.
          END.
          WHEN 'EANType' THEN DO:
              RUN initAntSifferILevNr (2).
              IF INT(EANType:SCREEN-VALUE) = 8 THEN
              DO:
                  IF INT(EANLandKode:SCREEN-VALUE) = 2 THEN
                      assign
                      AntSifferILevNr:SCREEN-VALUE = '0'
                      EANLevNr:SCREEN-VALUE        = '0'
                      AntSifferILevNr:SENSITIVE    = FALSE
                      EANLevNr:SENSITIVE           = FALSE
                      .
                  ELSE
                      assign
                      AntSifferILevNr:SCREEN-VALUE = '4'
                      AntSifferILevNr:SENSITIVE    = FALSE
                      EANLevNr:SENSITIVE           = FALSE
                      .
              END.
              ELSE DO:
                  ASSIGN
                      AntSifferILevNr:SCREEN-VALUE = '4'
                      AntSifferILevNr:SENSITIVE    = TRUE
                      EANLevNr:SENSITIVE           = TRUE.
              END.
              
          END.
          WHEN 'EANLandKode' THEN
          DO:
              IF INT(EANType:SCREEN-VALUE) = 8 AND int(EANLandKode:SCREEN-VALUE) = 2 THEN
              DO:
                  ASSIGN
                      AntSifferILevNr:SCREEN-VALUE = '0'
                      EANLevNr:SCREEN-VALUE        = '0'
                      AntSifferILevNr:SENSITIVE    = FALSE
                      EANLevNr:SENSITIVE           = FALSE.
              END.
              ELSE IF INT(EANType:SCREEN-VALUE) = 8 AND int(EANLandKode:SCREEN-VALUE) > 2 THEN
              DO:
                  ASSIGN
                      AntSifferILevNr:SCREEN-VALUE = '4'
                      EANLevNr:SCREEN-VALUE        = '0'
                      AntSifferILevNr:SENSITIVE    = FALSE
                      EANLevNr:SENSITIVE           = TRUE.
              END.
              ELSE
                  ASSIGN
                  AntSifferILevNr:SENSITIVE = TRUE
                  EANLevNr:SENSITIVE        = TRUE.

          END.
      END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "rectBrowse" THEN DO:  

  hBrwColEANSerieAktiv = ihBrowse:GET-BROWSE-COLUMN(2).
  hBrwColAntLedig      = ihBrowse:GET-BROWSE-COLUMN(10).

/*   ASSIGN ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 70                                           */
/*          ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 110                                          */
/*          ihBrowse:GET-BROWSE-COLUMN(24):WIDTH-PIXELS  = 40                                          */
/*          .                                                                                          */
/*                                                                                                     */
/*   DO ix = 1 TO 15:                                                                                  */
/*     ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 15. */
/*     IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "Beskr" THEN                                           */
/*       hBrwColArtBeskr = ihBrowse:GET-BROWSE-COLUMN(ix).                                             */
/*   END.                                                                                              */

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

EMPTY TEMP-TABLE ttVerdier.
hField = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.

REPEAT WHILE VALID-HANDLE(hField):
  IF CAN-DO("fill-in,combo-box",hField:TYPE) THEN DO:
    CREATE ttVerdier.
    ASSIGN ttVerdier.cNavn  = hField:NAME
           ttVerdier.cVerdi = hField:INPUT-VALUE
           .
  END.
  hField = hField:NEXT-SIBLING.
END.

RETURN httVerdier.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

