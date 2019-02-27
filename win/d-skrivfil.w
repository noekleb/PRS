&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:    SJ
  Beskrivelse:  Sender en fil til standard skriver eller VS90-skriver
  Parametere:   Inp utenhet, Inp skrivernummer, Inp filnavn, inp font 
                inp uskriftsnavn, inp melding (hvis tom)
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER wValg       AS CHAR NO-UNDO. /* DIREKTE, DIALOG, VS90 */
DEF INPUT PARAMETER wUtEnhet    AS CHAR NO-UNDO.
DEF INPUT PARAMETER wSkrNr      AS INTE NO-UNDO.
DEF INPUT PARAMETER wFilNavn    AS CHAR NO-UNDO.
DEF INPUT PARAMETER wUtsNavn    AS CHAR NO-UNDO.
DEF INPUT PARAMETER wTomMelding AS CHAR NO-UNDO.

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.

DEF VAR wAvbryt AS LOGI NO-UNDO. 
DEF VAR wOk     AS LOGI NO-UNDO INIT YES.

DEF VAR wUtskrMaate  AS CHAR NO-UNDO. 
DEF VAR wSkriverNavn AS CHAR NO-UNDO.
DEF VAR wImmDisp     AS LOGI NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

/* Temp-tablle for styrekoder */
DEF TEMP-TABLE SKoder
   FIELD Lengde        AS INTE
   FIELD SKode         AS CHAR
   INDEX Lengde IS PRIMARY UNIQUE
          Lengde DESCENDING
          SKode  ASCENDING.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 BUTTON-Avbryt FILL-IN-Sender ~
FILL-IN-UtsNavn FILL-IN-PrinterName 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Progress FILL-IN-Sender ~
FILL-IN-UtsNavn FILL-IN-PrinterName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FjernStyrekoder Dialog-Frame 
FUNCTION FjernStyrekoder RETURNS CHARACTER
  ( INPUT wLinje AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-PrinterName AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 62 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Progress AS CHARACTER FORMAT "x(100)":U 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Sender AS CHARACTER FORMAT "X(256)":U INITIAL "Sender utskriften" 
      VIEW-AS TEXT 
     SIZE 62 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-UtsNavn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 62 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-Progress AT ROW 3.62 COL 1 COLON-ALIGNED NO-LABEL
     BUTTON-Avbryt AT ROW 5.05 COL 25
     FILL-IN-Sender AT ROW 1.24 COL 2 NO-LABEL
     FILL-IN-UtsNavn AT ROW 1.95 COL 2 NO-LABEL
     FILL-IN-PrinterName AT ROW 2.67 COL 2 NO-LABEL
     RECT-2 AT ROW 3.38 COL 2
     SPACE(0.00) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Skriver ut".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{functions/oserrortekst.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PrinterName IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-Progress IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Sender IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-UtsNavn IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Skriver ut */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Avbryt Dialog-Frame
ON CHOOSE OF BUTTON-Avbryt IN FRAME Dialog-Frame /* Avbryt */
DO:
  IF wUtskrMaate <> "VS90" THEN
  MESSAGE "Vil du virkelig avbryte utskrift av" SKIP
          TRIM(FILL-IN-Utsnavn:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + "?"
      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE "Avbryt utskrift" UPDATE wAvbryt.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ASSIGN 
   wImmDisp = SESSION:IMMEDIATE-DISPLAY
   SESSION:IMMEDIATE-DISPLAY = YES.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wValg = "DIALOG" OR wUtEnhet = "F" THEN DO: /* Hvis formatert for fil skal alltid dialog vises */
     IF wUtEnhet <> "F" AND wFilNavn <> "<tom>" THEN DO:
        RUN d-utskrmaate.w.
        IF NOT CAN-DO("1,2",RETURN-VALUE) THEN LEAVE MAIN-BLOCK.
        IF RETURN-VALUE = "2" THEN DO:
           SYSTEM-DIALOG PRINTER-SETUP UPDATE wOk.
           IF NOT wOk THEN LEAVE MAIN-BLOCK.
           ASSIGN wUtskrMaate = "".
        END.  
        ELSE ASSIGN wUtskrMaate = "VS90". 
     END.   
     ELSE DO:
        SYSTEM-DIALOG PRINTER-SETUP UPDATE wOk.
        IF NOT wOk THEN LEAVE MAIN-BLOCK.
        ASSIGN wUtskrMaate = "".
     END.
  END.
  
  ELSE 
  
  IF wValg = "VS90" THEN ASSIGN wUtskrMaate = "VS90". 
  
  ASSIGN wSkrivernavn = SESSION:PRINTER-NAME.
  
  ASSIGN 
     FILL-IN-Sender      = FILL(" ",INT((FILL-IN-Sender:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (FILL-IN-Sender)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + FILL-IN-Sender
     FILL-IN-UtsNavn     = wUtsNavn
     FILL-IN-UtsNavn     = FILL(" ",INT((FILL-IN-UtsNavn:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (FILL-IN-UtsNavn)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + FILL-IN-UtsNavn
     FILL-IN-PrinterName = "til " + wSkriverNavn 
     FILL-IN-PrinterName = FILL(" ",INT((FILL-IN-PrinterName:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (FILL-IN-PrinterName)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + FILL-IN-PrinterName.

  {lng.i} RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN FILL-IN-UtsNavn:SENSITIVE = NO.
  END.   
  RUN SkrivFil.
  IF wUtskrMaate = "VS90" THEN 
      WAIT-FOR CHOOSE OF BUTTON-Avbryt IN FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
ASSIGN SESSION:IMMEDIATE-DISPLAY = wImmDisp.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateSKoder Dialog-Frame 
PROCEDURE CreateSKoder :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter tempfile for skriverens styrekoder sortert synkende på
               lengde. Tempfilen benyttes sendere for å fjerne styrekoder før
               utskrift. Den lengste fjernes alltid først.
  Parameters:  Inp styrekode
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wSKode AS CHAR NO-UNDO.
   IF wSKode <> "" AND NOT CAN-FIND(SKoder WHERE SKoder.Lengde = LENGTH(wSKode) AND
                                                 SKoder.SKode  = wSKode)
   THEN DO:
      CREATE SKoder.
      ASSIGN 
         SKoder.Lengde = LENGTH(wSKode)
         SKoder.SKode  = wSKode.    
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-Progress FILL-IN-Sender FILL-IN-UtsNavn FILL-IN-PrinterName 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-2 BUTTON-Avbryt FILL-IN-Sender FILL-IN-UtsNavn 
         FILL-IN-PrinterName 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCreateSKoder Dialog-Frame 
PROCEDURE InitCreateSKoder :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter tempfile for skriverens styrekoder sortert synkende på
               lengde. Tempfilen benyttes sendere for å fjerne styrekoder før
               utskrift. Den lengste fjernes alltid først.
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   /*
   RUN CreateSKoder(SkrType.SkrInit      ). 
   RUN CreateSKoder(SkrType.SkrReset     ).
   RUN CreateSKoder(SkrType.SkrSideLenPre).
   RUN CreateSKoder(SkrType.SkrSideLenSuf).
   RUN CreateSKoder(SkrType.SkrNorm      ).
   RUN CreateSKoder(SkrType.SkrComp      ).
   RUN CreateSKoder(SkrType.SkrCompAv    ).
   RUN CreateSKoder(SkrType.SkrExp       ).
   RUN CreateSKoder(SkrType.SkrExpAv     ).
   RUN CreateSKoder(SkrType.SkrDblBr     ).
   RUN CreateSKoder(SkrType.SkrDblBrAv   ).
   RUN CreateSKoder(SkrType.SkrDblHo     ).
   RUN CreateSKoder(SkrType.SkrDblHoAv   ).
   RUN CreateSKoder(SkrType.SkrDblBrHo   ).
   RUN CreateSKoder(SkrType.SkrDblBrHoAv ).
   RUN CreateSKoder(SkrType.SkrHalvHo    ).
   RUN CreateSKoder(SkrType.SkrHalvHoAv  ).
   RUN CreateSKoder(SkrType.SkrSub       ).
   RUN CreateSKoder(SkrType.SkrSubAv     ).
   RUN CreateSKoder(SkrType.SkrSuper     ).
   RUN CreateSKoder(SkrType.SkrSuperAv   ).
   RUN CreateSKoder(SkrType.SkrBold      ).
   RUN CreateSKoder(SkrType.SkrBoldAv    ).
   RUN CreateSKoder(SkrType.SkrKursiv    ).
   RUN CreateSKoder(SkrType.SkrKursivAv  ).
   RUN CreateSKoder(SkrType.SkrUnder     ).
   RUN CreateSKoder(SkrType.SkrUnderAv   ).
   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivFil Dialog-Frame 
PROCEDURE SkrivFil :
/*------------------------------------------------------------------------------
  Purpose:     Skriver ut
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &SCOP MaxWidth 60
   DEF VAR wLinje         AS CHAR NO-UNDO.
   DEF VAR wBytesTot      AS INTE NO-UNDO.
   DEF VAR wBytesTotLest  AS INTE NO-UNDO.
   DEF VAR wBytesPrevLest AS INTE NO-UNDO.

   IF wUtskrMaate <> "VS90" THEN 
   WindowsDriver: DO WITH FRAME {&FRAME-NAME}:
   
      OUTPUT STREAM Ut TO PRINTER.
      IF wFilNavn = "<tom>" THEN
         PUT STREAM Ut UNFORMATTED wUtsNavn SKIP(1) wTomMelding SKIP.
      /*
      ELSE DO:   
         IF wUtEnhet = "P" THEN DO:
            FIND Skriver WHERE Skriver.SkrNr = wSkrNr NO-LOCK NO-ERROR.
            IF AVAIL Skriver THEN DO: 
               FIND SkrType OF Skriver NO-LOCK NO-ERROR.
               IF AVAIL SkrType AND NOT CAN-FIND(FIRST SKoder) THEN
                  RUN InitCreateSKoder. /* Initierer tempfile med styrekoder */
            END.   
         END.
            
         INPUT STREAM Inn FROM VALUE(wFilNavn) NO-ECHO NO-MAP.
         SEEK STREAM Inn TO END.
         ASSIGN wBytesTot = SEEK(Inn).
         SEEK STREAM Inn TO 0.
      
         SkrivUt:
         REPEAT WITH FRAME f-skriv:
            ASSIGN wLinje = "".
            IMPORT STREAM Inn UNFORMATTED wLinje.
            PROCESS EVENTS.
      
            IF wAvbryt THEN LEAVE.
      
            ASSIGN 
                wBytesTotLest = SEEK(Inn).
          
            IF wBytesTotLest - wBytesPrevLest >=  wBytesTot / {&MaxWidth} THEN
               ASSIGN FILL-IN-Progress:WIDTH = FILL-IN-Progress:WIDTH + 1
                      wBytesPrevLest = wBytesTotLest.
            
            /* Fjern eventuelle styretegn hvis utskriften er formatert for skriver */
            IF wLinje <> "" AND wUtEnhet = "P" AND AVAIL SkrType THEN  
                ASSIGN wLinje = FjernStyreKoder(wLinje).
                        
            DISPLAY STREAM Ut wLinje FORMAT "X(250)" WITH
                FRAME f-skriv NO-LABELS NO-BOX STREAM-IO USE-TEXT WIDTH 255.
         END. /* SkrivUt */
      END.   
      */
      OUTPUT STREAM ut CLOSE.
      ASSIGN FILL-IN-Progress:width = {&MaxWidth}.   
      
   END. /* WindowsDriver */
   
   ELSE
   
   VS90utskrift: DO:
      DO WITH FRAME {&FRAME-NAME}:
         ASSIGN 
             BUTTON-Avbryt:LABEL           = "OK"
             BUTTON-AVBRYT:SENSITIVE       = NO
             FILL-IN-Progress:WIDTH        = {&MaxWidth}
             FILL-IN-Progress:SCREEN-VALUE = FILL(" ",36) + "Vennligst vent...".
      END.   

      /*OS-COMMAND SILENT tocp850 value(wFilnavn) > value(Skriver.SkrUtenhet). */

      DO WITH FRAME {&FRAME-NAME}:
         ASSIGN      
            FRAME {&FRAME-NAME}:TITLE        = "Utskriving ferdig"
            RECT-2:HIDDEN                    = YES
            FILL-IN-Progress:BGCOLOR         = ?
            FILL-IN-Progress:FGCOLOR         = ?
            FILL-IN-Sender:SCREEN-VALUE      = FILL-IN-UtsNavn:SCREEN-VALUE
            FILL-IN-UtsNavn:SCREEN-VALUE     = ""
            FILL-IN-PrinterName:SCREEN-VALUE = "Resultat fra utskriftskommando:" 
            FILL-IN-PrinterName:SCREEN-VALUE = FILL(" ",INT((FILL-IN-PrinterName:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (FILL-IN-PrinterName:SCREEN-VALUE)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + FILL-IN-PrinterName:SCREEN-VALUE
            FILL-IN-Progress:SCREEN-VALUE    = IF OS-ERROR = 0 THEN "Utskriften er sendt til " else OsErrorTekst(OS-ERROR)
            FILL-IN-Progress:SCREEN-VALUE    = FILL(" ",INT((FILL-IN-Progress:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (FILL-IN-Progress:SCREEN-VALUE)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + FILL-IN-Progress:SCREEN-VALUE
            BUTTON-Avbryt:SENSITIVE          = YES.
            
      END.      
   END. /* VS90utskrift */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FjernStyrekoder Dialog-Frame 
FUNCTION FjernStyrekoder RETURNS CHARACTER
  ( INPUT wLinje AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Fjerner eventuelle styrekoder fra lest linje i ascii-fil
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR wStartPos AS INTE NO-UNDO. /* Startposisjon for styretegn  */
   DEF VAR wSluttPos AS INTE NO-UNDO. /* Sleuttposisjon for styretegn */
   FOR EACH SKoder:
   
      DO WHILE wLinje MATCHES "*" + SKoder.SKode + "*":
         ASSIGN 
            wStartPos = INDEX(wLinje,SKoder.SKode)
            wSluttPos = wStartPos + SKoder.Lengde - IF wStartPos = 1 THEN 1 ELSE 0
            wLinje    = SUBSTR(wLinje,1,wStartPos - 1) + SUBSTR(wLinje,wSluttPos + 1).
      END.      
   END.

   RETURN wLinje.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

