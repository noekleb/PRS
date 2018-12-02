&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE INPUT  PARAMETER dArtikkelnr    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER cBeskr         AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iVg            AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER iLevNr         AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER cLevnamn       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER lLager         AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER iStrTypeId     AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER cStrtypeTxt    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER deSalgspris    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER deTilbPris     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER daFraDato      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER daTilDato      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iCl            AS INTEGER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER dTilArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER obOk           AS LOG NO-UNDO.

DEF VAR bPassord AS LOG NO-UNDO. 

PROCEDURE SendMessageA EXTERNAL "USER32.dll":
  DEFINE INPUT PARAMETER hHWND AS LONG.
  DEFINE INPUT PARAMETER iCmd  AS LONG.
  DEFINE INPUT PARAMETER iChar AS LONG.
  DEFINE INPUT PARAMETER ulParam AS LONG.
END PROCEDURE.

PROCEDURE PostPWChar:
  DEFINE INPUT PARAMETER hHWND AS INT.
  RUN SendMessageA(hHWND, 204, ASC("*"), 0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Sok RECT-64 RECT-65 FI-Slask_ArtikkelNr ~
FI-Til_ArtikkelNr FI-Slask_Beskr FI-Til_Beskr FI-Slask_Vg FI-Til_Vg ~
FI-Slask_LevNr FI-Til_LevNr FI-Slask_lager FI-Til_lager FI-Slask_Strtypeid ~
FI-Slask_Strtypebeskr FI-Til_Strtypeid FI-Til_Strtypebeskr ~
FI-Slask_SalgsPris FI-Til_SalgsPris FI-Slask_TilbPris FI-Slask_UtsFra ~
FI-Slask_UtsTil FI-Til_TilbPris FI-Til_UtsFra FI-Til_UtsTil ED-txt Btn_OK-2 ~
Btn_Cancel-2 fi-cPassord FI-Fra FI-Til FI-OlikaLager 
&Scoped-Define DISPLAYED-OBJECTS FI-Slask_ArtikkelNr FI-Til_ArtikkelNr ~
FI-Slask_Beskr FI-Til_Beskr FI-Slask_Vg FI-Til_Vg FI-Slask_LevNr ~
FI-Slask_Levnamn FI-Til_LevNr FI-Til_Levnamn FI-Slask_lager FI-Til_lager ~
FI-Slask_Strtypeid FI-Slask_Strtypebeskr FI-Til_Strtypeid ~
FI-Til_Strtypebeskr FI-Slask_SalgsPris FI-Til_SalgsPris FI-Slask_TilbPris ~
FI-Slask_UtsFra FI-Slask_UtsTil FI-Til_TilbPris FI-Til_UtsFra FI-Til_UtsTil ~
ED-txt fi-cPassord FI-Fra FI-Til FI-OlikaLager 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Sok 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1.05 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON Btn_Cancel-2 AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK-2 AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ED-txt AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 86 BY 4.29
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cPassord AS CHARACTER FORMAT "X(256)":U 
     LABEL "Angi passord" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FI-Fra AS CHARACTER FORMAT "X(256)":U INITIAL "    Flytt denne artikkel" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-OlikaLager AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 69 BY 1.14
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Slask_ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FI-Slask_Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FI-Slask_lager AS LOGICAL FORMAT "J/N" INITIAL NO 
     LABEL "Lager" 
     VIEW-AS FILL-IN 
     SIZE 4.2 BY 1.

DEFINE VARIABLE FI-Slask_Levnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI-Slask_SalgsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Salgspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_Strtypebeskr AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_Strtypeid AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Strtype" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_TilbPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Kampanjepris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_UtsFra AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_UtsTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Slask_Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI-Til AS CHARACTER FORMAT "X(256)":U INITIAL "    Flytt til denne artikkel" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-Til_ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FI-Til_Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FI-Til_lager AS LOGICAL FORMAT "J/N" INITIAL NO 
     LABEL "Lager" 
     VIEW-AS FILL-IN 
     SIZE 4.2 BY 1.

DEFINE VARIABLE FI-Til_Levnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI-Til_SalgsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Salgspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_Strtypebeskr AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_Strtypeid AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Strtype" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_TilbPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Kampanjepris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_UtsFra AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_UtsTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Til_Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62.6 BY 12.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62.6 BY 12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Sok AT ROW 3.48 COL 106.8 NO-TAB-STOP 
     FI-Slask_ArtikkelNr AT ROW 3.43 COL 17 COLON-ALIGNED NO-TAB-STOP 
     FI-Til_ArtikkelNr AT ROW 3.43 COL 81.4 COLON-ALIGNED NO-TAB-STOP 
     FI-Slask_Beskr AT ROW 4.43 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-TAB-STOP 
     FI-Til_Beskr AT ROW 4.43 COL 81.4 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-TAB-STOP 
     FI-Slask_Vg AT ROW 5.43 COL 17 COLON-ALIGNED HELP
          "'varegruppenummer" NO-TAB-STOP 
     FI-Til_Vg AT ROW 5.43 COL 81.4 COLON-ALIGNED HELP
          "'varegruppenummer" NO-TAB-STOP 
     FI-Slask_LevNr AT ROW 6.43 COL 17 COLON-ALIGNED HELP
          "Leverandørnummer" NO-TAB-STOP 
     FI-Slask_Levnamn AT ROW 6.43 COL 27.4 COLON-ALIGNED NO-LABEL
     FI-Til_LevNr AT ROW 6.43 COL 81.4 COLON-ALIGNED HELP
          "Leverandørnummer" NO-TAB-STOP 
     FI-Til_Levnamn AT ROW 6.43 COL 91.8 COLON-ALIGNED NO-LABEL
     FI-Slask_lager AT ROW 7.43 COL 17 COLON-ALIGNED HELP
          "Artikkelen har lagerstyring." NO-TAB-STOP 
     FI-Til_lager AT ROW 7.43 COL 81.4 COLON-ALIGNED HELP
          "Artikkelen har lagerstyring." NO-TAB-STOP 
     FI-Slask_Strtypeid AT ROW 8.43 COL 17 COLON-ALIGNED NO-TAB-STOP 
     FI-Slask_Strtypebeskr AT ROW 8.43 COL 31.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-Til_Strtypeid AT ROW 8.43 COL 81.4 COLON-ALIGNED NO-TAB-STOP 
     FI-Til_Strtypebeskr AT ROW 8.43 COL 95.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-Slask_SalgsPris AT ROW 9.43 COL 17 COLON-ALIGNED NO-TAB-STOP 
     FI-Til_SalgsPris AT ROW 9.43 COL 81.4 COLON-ALIGNED NO-TAB-STOP 
     FI-Slask_TilbPris AT ROW 10.43 COL 17 COLON-ALIGNED NO-TAB-STOP 
     FI-Slask_UtsFra AT ROW 10.43 COL 34.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-Slask_UtsTil AT ROW 10.43 COL 47.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-Til_TilbPris AT ROW 10.43 COL 81.4 COLON-ALIGNED NO-TAB-STOP 
     FI-Til_UtsFra AT ROW 10.43 COL 99 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     FI-Til_UtsTil AT ROW 10.43 COL 112.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     ED-txt AT ROW 16.71 COL 23 NO-LABEL NO-TAB-STOP 
     Btn_OK-2 AT ROW 22.43 COL 97
     Btn_Cancel-2 AT ROW 22.43 COL 113
     fi-cPassord AT ROW 22.67 COL 61 COLON-ALIGNED
     FI-Fra AT ROW 2.24 COL 8 COLON-ALIGNED NO-LABEL
     FI-Til AT ROW 2.24 COL 70.2 COLON-ALIGNED NO-LABEL
     FI-OlikaLager AT ROW 14.81 COL 31 COLON-ALIGNED
     RECT-64 AT ROW 2.57 COL 2.4
     RECT-65 AT ROW 2.57 COL 66.2
     SPACE(0.99) SKIP(9.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Saner artikkel".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       ED-txt:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_ArtikkelNr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_Beskr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_lager:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-Slask_Levnamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Slask_LevNr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_SalgsPris:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_Strtypebeskr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_Strtypeid:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_TilbPris:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_UtsFra:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_UtsTil:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Slask_Vg:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_ArtikkelNr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_Beskr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_lager:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-Til_Levnamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Til_LevNr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_SalgsPris:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_Strtypebeskr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_Strtypeid:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_TilbPris:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_UtsFra:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_UtsTil:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FI-Til_Vg:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Saner artikkel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok Dialog-Frame
ON CHOOSE OF B-Sok IN FRAME Dialog-Frame /* Søk */
DO:

  RUN HentTilArt.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK-2 Dialog-Frame
ON CHOOSE OF Btn_OK-2 IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN fi-cPassord.
  IF fi-cPassord:HIDDEN = FALSE AND fi-cPassord NE DYNAMIC-FUNCTION("getFieldValues","SysPara",
                    "WHERE SysHId = 18 and SysGr = 2 and ParaNr = 1","Parameter1") THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Feil passord","Feil","").
      APPLY "ENTRY" TO fi-cPassord.
      RETURN NO-APPLY.
  END.
  ELSE DO: 
      obOk = TRUE.
      dTilArtikkelnr = INPUT FI-Til_Artikkelnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cPassord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPassord Dialog-Frame
ON ANY-PRINTABLE OF fi-cPassord IN FRAME Dialog-Frame /* Angi passord */
DO:
  RUN PostPWChar(fi-cPassord:HWND).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
    DEF VAR cTekst AS CHAR NO-UNDO.
  {syspara.i 2 4 46 cTekst}
  IF CAN-DO('1,j,y,Ja,Yes,true',cTekst) THEN
      bPassord = TRUE.
  ELSE
      bPassord = FALSE.

  RUN InitVerdier.
  RUN enable_UI.
  
  IF dTilArtikkelNr > 0 THEN 
      RUN HentInputTilArt.
  ELSE
      RUN HentTilArt.

  IF bPassord = FALSE THEN
  DO:
      fi-cPassord:HIDDEN = TRUE.
      APPLY 'ENTRY' TO Btn_Cancel-2 IN FRAME {&FRAME-NAME}. 
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FI-Slask_ArtikkelNr FI-Til_ArtikkelNr FI-Slask_Beskr FI-Til_Beskr 
          FI-Slask_Vg FI-Til_Vg FI-Slask_LevNr FI-Slask_Levnamn FI-Til_LevNr 
          FI-Til_Levnamn FI-Slask_lager FI-Til_lager FI-Slask_Strtypeid 
          FI-Slask_Strtypebeskr FI-Til_Strtypeid FI-Til_Strtypebeskr 
          FI-Slask_SalgsPris FI-Til_SalgsPris FI-Slask_TilbPris FI-Slask_UtsFra 
          FI-Slask_UtsTil FI-Til_TilbPris FI-Til_UtsFra FI-Til_UtsTil ED-txt 
          fi-cPassord FI-Fra FI-Til FI-OlikaLager 
      WITH FRAME Dialog-Frame.
  ENABLE B-Sok RECT-64 RECT-65 FI-Slask_ArtikkelNr FI-Til_ArtikkelNr 
         FI-Slask_Beskr FI-Til_Beskr FI-Slask_Vg FI-Til_Vg FI-Slask_LevNr 
         FI-Til_LevNr FI-Slask_lager FI-Til_lager FI-Slask_Strtypeid 
         FI-Slask_Strtypebeskr FI-Til_Strtypeid FI-Til_Strtypebeskr 
         FI-Slask_SalgsPris FI-Til_SalgsPris FI-Slask_TilbPris FI-Slask_UtsFra 
         FI-Slask_UtsTil FI-Til_TilbPris FI-Til_UtsFra FI-Til_UtsTil ED-txt 
         Btn_OK-2 Btn_Cancel-2 fi-cPassord FI-Fra FI-Til FI-OlikaLager 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentInputTilArt Dialog-Frame 
PROCEDURE HentInputTilArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dSokArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cProfilnr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrisVerdier AS CHARACTER  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      dSokArtikkelnr = dTilArtikkelnr.
      IF dSokArtikkelnr <> ? AND dSokArtikkelnr <> FI-Slask_ArtikkelNr THEN 
      DO:
          cVerdier = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                          "WHERE Artikkelnr = " + STRING(dSokArtikkelnr),"beskr,vg,levnr,lager,Opris,Sanertdato,Strtypeid").
          IF ENTRY(6,cVerdier,"|") <> "" THEN DO:
              MESSAGE "Artikkelen er sanert " 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
          IF ENTRY(5,cVerdier,"|") = "yes" THEN DO:
              MESSAGE "Vare med åpen pris"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              RETURN NO-APPLY.
          END.
          FI-Til_Levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Levbas",
                          "WHERE Levnr = " + ENTRY(3,cVerdier,"|"),"LevNamn").
          FI-Til_Strtypebeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Strtype",
                          "WHERE Strtypeid = " + ENTRY(7,cVerdier,"|"),"Beskrivelse").
          cProfilNr = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                          "WHERE Butik = " + STRING(iCl),"Profilnr").
          cPrisVerdier = DYNAMIC-FUNCTION("getFieldValues","ArtPris",
             "WHERE Artikkelnr = " + STRING(dSokArtikkelnr) + " AND profilnr = " + cProfilnr,"tilbud,pris;1,pris;2,TilbudFraDato,TilbudTilDato").
          
          ASSIGN FI-Til_ArtikkelNr:SCREEN-VALUE = STRING(dSokArtikkelnr)
                 FI-Til_Beskr:SCREEN-VALUE = ENTRY(1,cVerdier,"|")
                 FI-Til_vg:SCREEN-VALUE = ENTRY(2,cVerdier,"|")
                 FI-Til_LevNr:SCREEN-VALUE = ENTRY(3,cVerdier,"|")
                 FI-Til_Lager:SCREEN-VALUE = STRING(ENTRY(4,cVerdier,"|") = "yes","J/N")
                 FI-Til_Strtypeid:SCREEN-VALUE = ENTRY(7,cVerdier,"|")
                 FI-Til_SalgsPris:SCREEN-VALUE = ENTRY(2,cPrisverdier,"|") 
                 FI-Til_TilbPris:SCREEN-VALUE = ENTRY(3,cPrisverdier,"|")
                 FI-Til_UtsFra:SCREEN-VALUE = ENTRY(4,cPrisverdier,"|")
                 FI-Til_UtsTil:SCREEN-VALUE = ENTRY(5,cPrisverdier,"|")
                 .
          
          ASSIGN FI-Slask_lager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-Til_lager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-OlikaLager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-OlikaLager:SCREEN-VALUE = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 
                     "   NB! ULIK VERDI EGENSKAP: LAGERSTYRING" ELSE "".
      END.
  END.
  APPLY "ENTRY" TO fi-cPassord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentTilArt Dialog-Frame 
PROCEDURE HentTilArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dSokArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cProfilnr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrisVerdier AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      RUN d-hSok.w (OUTPUT dSokArtikkelnr,"").
      IF dSokArtikkelnr = FI-Slask_ArtikkelNr THEN DO:
          MESSAGE "Samme artikkel"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN.
      END.
      IF dSokArtikkelnr <> ? AND dSokArtikkelnr <> FI-Slask_ArtikkelNr THEN DO:
          cVerdier = DYNAMIC-FUNCTION("getFieldValues","ArtBas",
                          "WHERE Artikkelnr = " + STRING(dSokArtikkelnr),"beskr,vg,levnr,lager,Opris,Sanertdato,Strtypeid").
          IF ENTRY(6,cVerdier,"|") <> "" THEN DO:
              MESSAGE "Artikkelen er sanert " 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
          /*
          IF ENTRY(5,cVerdier,"|") = "yes" THEN DO:
              MESSAGE "Vare med åpen pris"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              RETURN NO-APPLY.
          END.
          */
          FI-Til_Levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Levbas",
                          "WHERE Levnr = " + ENTRY(3,cVerdier,"|"),"LevNamn").
          FI-Til_Strtypebeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Strtype",
                          "WHERE Strtypeid = " + ENTRY(7,cVerdier,"|"),"Beskrivelse").
          cProfilNr = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                          "WHERE Butik = " + STRING(iCl),"Profilnr").
          cPrisVerdier = DYNAMIC-FUNCTION("getFieldValues","ArtPris",
             "WHERE Artikkelnr = " + STRING(dSokArtikkelnr) + " AND profilnr = " + cProfilnr,"tilbud,pris;1,pris;2,TilbudFraDato,TilbudTilDato").
          
          ASSIGN FI-Til_ArtikkelNr:SCREEN-VALUE = STRING(dSokArtikkelnr)
                 FI-Til_Beskr:SCREEN-VALUE = ENTRY(1,cVerdier,"|")
                 FI-Til_vg:SCREEN-VALUE = ENTRY(2,cVerdier,"|")
                 FI-Til_LevNr:SCREEN-VALUE = ENTRY(3,cVerdier,"|")
                 FI-Til_Lager:SCREEN-VALUE = STRING(ENTRY(4,cVerdier,"|") = "yes","J/N")
                 FI-Til_Strtypeid:SCREEN-VALUE = ENTRY(7,cVerdier,"|")
                 FI-Til_SalgsPris:SCREEN-VALUE = ENTRY(2,cPrisverdier,"|") 
                 FI-Til_TilbPris:SCREEN-VALUE = ENTRY(3,cPrisverdier,"|")
                 FI-Til_UtsFra:SCREEN-VALUE = ENTRY(4,cPrisverdier,"|")
                 FI-Til_UtsTil:SCREEN-VALUE = ENTRY(5,cPrisverdier,"|")
                 .
          
          ASSIGN FI-Slask_lager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-Til_lager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-OlikaLager:BGCOLOR = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 12 ELSE ?
                 FI-OlikaLager:SCREEN-VALUE = IF FI-Slask_lager:SCREEN-VALUE <> FI-Til_lager:SCREEN-VALUE THEN 
                     "   NB! ULIK VERDI EGENSKAP: LAGERSTYRING" ELSE "".
      END.
  END.
  APPLY "ENTRY" TO fi-cPassord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVerdier Dialog-Frame 
PROCEDURE InitVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN FI-Slask_ArtikkelNr = dArtikkelnr
           FI-Slask_Beskr      = cBeskr
           FI-Slask_Vg         = iVg
           FI-Slask_LevNr      = iLevNr
           FI-Slask_Levnamn    = cLevnamn
           FI-Slask_lager      = lLager
           FI-Slask_Strtypeid    = INT(iStrTypeId)
           FI-Slask_Strtypebeskr = cStrtypeTxt
           FI-Slask_SalgsPris    = DECI(deSalgspris)
           FI-Slask_TilbPris     = DECI(deTilbPris) 
           FI-Slask_UtsFra       = DATE(daFraDato)  
           FI-Slask_UtsTil       = DATE(daTilDato)  
           .
    ED-txt = "EAN-koder flyttes til ny artikkel." + CHR(13) +
             "Alle transaktioner flyttes." + CHR(13) +
             "Statistik overföres.".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

