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

DEF VAR gcArtSlag AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR cgetSprak AS CHAR NO-UNDO.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 EDITOR-Hjelp RS-ArtSlag Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-Hjelp RS-ArtSlag FILL-IN-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-Hjelp AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP
     SIZE 92.6 BY 10 NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "Angi varetype" 
      VIEW-AS TEXT 
     SIZE 27.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-ArtSlag AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item 1", 0
     SIZE 35 BY 9.14 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     EDITOR-Hjelp AT ROW 1.95 COL 41.4 NO-LABEL NO-TAB-STOP 
     RS-ArtSlag AT ROW 2.33 COL 5 NO-LABEL
     Btn_OK AT ROW 12.19 COL 2.2
     Btn_Cancel AT ROW 12.19 COL 119
     FILL-IN-20 AT ROW 1.29 COL 1.4 COLON-ALIGNED NO-LABEL
     RECT-57 AT ROW 1.95 COL 2
     SPACE(93.99) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Valg av varetype"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
       EDITOR-Hjelp:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE
       EDITOR-Hjelp:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-20:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Valg av varetype */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  ASSIGN
      gcArtSlag = "AVBRYT"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN
      gcArtSlag = "0"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON GO OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN
      gcArtSlag = "0"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    cgetSprak = DYNAMIC-FUNCTION("getLanguageCode") NO-ERROR.
  RUN initVareslag.
  RUN enable_UI.
  {lng.i} 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  IF gcArtSlag <> "AVBRYT" THEN
  ASSIGN
      gcArtSlag = RS-ArtSlag:SCREEN-VALUE
      .
END.
RUN disable_UI.
RETURN gcArtSlag.

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
  DISPLAY EDITOR-Hjelp RS-ArtSlag FILL-IN-20 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-57 EDITOR-Hjelp RS-ArtSlag Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initVareslag Dialog-Frame 
PROCEDURE initVareslag PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR cExceptList AS CHAR NO-UNDO.

  PUBLISH "getVareSlagExceptList" (OUTPUT cExceptList).

  ASSIGN
      pcTekst = "Stykkvare (Stk),0" +
                 ",Vektvare (Kg),1" +
                 ",Vektvare (Hg),2" +
                 ",Metervare (m),3" + 
                 ",Kvadratmetervare (m2),4" + 
                 ",Volumvare (l),5" +
                 ",Pakkevare (Stk),7" + 
                 ",Pant (Stk),8"
      cExceptList = '6,9,10'
      .
  DO WITH FRAME {&FRAME-NAME}:
      /*
      FOR EACH SysPara NO-LOCK WHERE
          SysPara.SysHId = 2 AND
          SysPara.SysGr  = 7 AND
          SysPara.ParaNr <= 9:

          IF CAN-DO(cExceptList,SysPara.Parameter1) THEN NEXT.

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        SysPara.Beskrivelse + "," + 
                        (SysPara.Parameter1)
              .
      END.
      */
    ASSIGN
      RS-ArtSlag:RADIO-BUTTONS  = pcTekst
      RS-ArtSlag:SCREEN-VALUE   = ENTRY(2,pcTekst)
      EDITOR-Hjelp = IF cgetSprak = "SE" THEN
          "Vid försäljning av alla andra varutyper än (stk) varor kommer kassan att be om att det registreras" + CHR(10) +
          "in mängd (kg/meter/kvadratmeter/liter), eventuellt med decimaler om inte hela mängder." + CHR(10) + 
          "Om varan har fast pris (pr. kg/meter osv) multipliceras den angivna mängden med" + CHR(10) + 
          "priset och räknar ut exakt pris på den sålda varan." + CHR(10) + CHR(10) +
          "Om varan har öppet pris (diverse (kg) varor osv) registrerar kassören i tillegg till" + CHR(10) + 
          "mängden också priset per enhet. Därefter räknar kassen ut priset på den" + CHR(10) + 
          "sålda varan och lägger den in på kvittot." + CHR(10) + CHR(10) + 
          "Det är alltid pris per kg/m/kvm/l som skall lägges in på kalkylen." + CHR(10) + CHR(10) +
          "Varor av typen (kg) och (hg) är båda viktvaror, och priset i kalkylen anges pr. kg för båda." + chr(10) +  
          "Typen (kg) eller (hg) har endast betydelse för utskrift av etiketter" + CHR(10) + 
          "(lösgodis till 9,90 kr/hg i ställett för 99 kr/kg på plakatet)."
          ELSE
"Ved salg av alle andre varetyper enn (stk) varer vil kassen be om at det tastes inn mengde" + CHR(10) +
"(kg/meter/kvadratmeter/liter), eventuelt med desimaler hvis ikke hele mengder." + CHR(10) + 
"Hvis varen har fast pris (pr. kg/meter osv) vil den inntastede mengden mulitpliseres med" + CHR(10) + 
"denne prisen og regner ut eksakt pris på den solgte varen." + CHR(10) + CHR(10) +
"Hvis varen har åpen pris (diverse (kg) varer osv) vil kasserer i tillegg til å taste inn" + CHR(10) + 
"mengden også måtte taste inn pris pr. enhet. Deretter regner kassen ut prisen på den" + CHR(10) + 
"solgte varen og legger den inn i bongen." + CHR(10) + CHR(10) + 
"Det er alltid pris pr kg/m/kvm/l som skal legges inn i kalkylen." + CHR(10) + CHR(10) +
"Varer type (kg) og (hg) er begge vektvarer, og prisen i kalkylen oppgis pr. kg for begge." + chr(10) +  
"Type (kg) eller (hg) har kun betydning for utskrift av etiketter" + CHR(10) + 
"(smågodt til 9,90 kr/hg i stedet for 99 kr/kg på plakaten)."
      .    
    DISPLAY
      EDITOR-Hjelp
      WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

