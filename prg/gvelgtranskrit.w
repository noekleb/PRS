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
DEFINE INPUT  PARAMETER cButListItemP  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cTTIdListItemP AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cAlle          AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cButik         AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cTTId          AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER dDatofra       AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER dDatoil        AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER cKasseNr       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cForsNr        AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cSelger        AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER lNeglager      AS LOGICAL    NO-UNDO.
DEFINE        VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Dato FI-DatoTil B-Transtyper ~
B-KasseNrBlank Btn_OK BUTTON-SokBut B-ForsNrBlank Btn_Cancel CB-Butik ~
B-SelgerNrBlank CB-TTId TG-Neglager B-TranstyperBlank B-KasseNr ~
BUTTON-SokDatoTil B-SelgerNr B-ForsNr BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil FI-KasseNr FI-Butikker ~
FI-ForsNr CB-Butik FI-SelgerNr CB-TTId TG-Neglager FI-Transtyper 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ForsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-ForsNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KasseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KasseNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SelgerNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SelgerNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Transtyper  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-TranstyperBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Butik AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "","1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "TransTypeId" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "","0"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-ForsNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kasserer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasseNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kassenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SelgerNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Selger" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Transtyper AS CHARACTER FORMAT "X(10)":U 
     LABEL "Transtyper" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE TG-Neglager AS LOGICAL INITIAL no 
     LABEL "Neg. lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Dato AT ROW 1.19 COL 15 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.19 COL 37.2 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     B-Transtyper AT ROW 5.19 COL 32.4 NO-TAB-STOP 
     FI-KasseNr AT ROW 1.19 COL 71 COLON-ALIGNED
     B-KasseNrBlank AT ROW 1.19 COL 92.6
     Btn_OK AT ROW 1.24 COL 104.6
     BUTTON-SokBut AT ROW 2.19 COL 33.6 NO-TAB-STOP 
     FI-Butikker AT ROW 2.19 COL 15 COLON-ALIGNED
     FI-ForsNr AT ROW 2.19 COL 71 COLON-ALIGNED
     B-ForsNrBlank AT ROW 2.19 COL 92.6
     Btn_Cancel AT ROW 2.48 COL 104.6
     CB-Butik AT ROW 3.19 COL 15 COLON-ALIGNED HELP
          "Butikknummer"
     FI-SelgerNr AT ROW 3.19 COL 71 COLON-ALIGNED
     B-SelgerNrBlank AT ROW 3.19 COL 92.6
     CB-TTId AT ROW 4.19 COL 15 COLON-ALIGNED HELP
          "TransaksjonstypensID"
     TG-Neglager AT ROW 4.19 COL 73
     FI-Transtyper AT ROW 5.19 COL 15 COLON-ALIGNED
     B-TranstyperBlank AT ROW 5.19 COL 37.6
     B-KasseNr AT ROW 1.19 COL 87.6 NO-TAB-STOP 
     BUTTON-SokDatoTil AT ROW 1.19 COL 55.8
     B-SelgerNr AT ROW 3.19 COL 87.6 NO-TAB-STOP 
     B-ForsNr AT ROW 2.19 COL 87.6 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 1.19 COL 33.6
     SPACE(84.59) SKIP(4.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg avgrensing"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ForsNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KasseNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SelgerNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Transtyper IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Velg avgrensing */
DO:
  ASSIGN cReturVerdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg avgrensing */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForsNr Dialog-Frame
ON CHOOSE OF B-ForsNr IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cForsaljRowIdList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cForsaljIdList     AS CHARACTER  NO-UNDO.
    IF FI-ForsNr:PRIVATE-DATA <> "" AND FI-ForsNr:PRIVATE-DATA <> ? THEN
        ASSIGN cForsaljRowIdList = FI-ForsNr:PRIVATE-DATA.
     RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Forsalj;ForsNr;FoNamn",
                         "where CAN-DO('" + cTillgKasserer + "',STRING(Forsalj.ForsNr))",
                         INPUT-OUTPUT cForsaljRowIdList,
                         "ForsNr",
                         INPUT-OUTPUT cForsaljIdList,
                         "","",
                         OUTPUT bOK).
  IF bOK = FALSE THEN
        RETURN NO-APPLY.
  assign
    FI-ForsNr:SCREEN-VALUE = IF cForsaljIdList = ""
                      then cAlle
                    else "( " + STRING(NUM-ENTRIES(cForsaljIdList,"|")) + " )"
    FI-ForsNr     = if cForsaljIdList = ""
                      then "*"
                      else REPLACE(cForsaljIdList,"|",",")
    FI-ForsNr:TOOLTIP = IF FI-ForsNr = "*" THEN "" ELSE FI-ForsNr
    FI-ForsNr:PRIVATE-DATA = cForsaljRowIdList.
/*   def var IO-Liste as char no-undo.                                  */
/*                                                                      */
/* /*   if wVareGrupper = cAlle then */                                 */
/* /*     RUN InitVaregrupper.       */                                 */
/*                                                                      */
/*   assign                                                             */
/*     IO-Liste = if FI-ForsNr:SCREEN-VALUE = cAlle                     */
/*                  then ""                                             */
/*                  else FI-ForsNr.                                     */
/*                                                                      */
/*   run d-tagforsalj.w (input-output IO-Liste).                        */
/*   IF RETURN-VALUE = "Avbryt" THEN                                    */
/*         RETURN NO-APPLY.                                             */
/*   assign                                                             */
/*     FI-ForsNr:SCREEN-VALUE = if IO-Liste = ""                        */
/*                       then cAlle                                     */
/*                     else "( " + STRING(NUM-ENTRIES(IO-Liste)) + " )" */
/*     FI-ForsNr     = if IO-Liste = ""                                 */
/*                       then "*"                                       */
/*                       else IO-Liste                                  */
/*     FI-ForsNr:TOOLTIP = IF FI-ForsNr = "*" THEN "" ELSE FI-ForsNr.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ForsNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForsNrBlank Dialog-Frame
ON CHOOSE OF B-ForsNrBlank IN FRAME Dialog-Frame /* Blank */
DO:
    IF FI-ForsNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-ForsNr:SCREEN-VALUE = cAlle
               FI-ForsNr              = "*"
               FI-ForsNr:TOOLTIP      = ""
               FI-ForsNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasseNr Dialog-Frame
ON CHOOSE OF B-KasseNr IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Kasse;KasseNr;Navn;Aktiv;!ButikkNr",
                        "WHERE ButikkNr = " + CB-Butik:SCREEN-VALUE,
                        INPUT-OUTPUT cRowIdList,
                        "KasseNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-KasseNr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-KasseNr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-KasseNr:TOOLTIP = IF FI-KasseNr = "*" THEN "" ELSE FI-KasseNr.
        IF FI-KasseNr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = "".
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasseNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasseNrBlank Dialog-Frame
ON CHOOSE OF B-KasseNrBlank IN FRAME Dialog-Frame /* Blank */
DO:
    IF FI-KasseNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-KasseNr:SCREEN-VALUE = cAlle
               FI-KasseNr              = "*"
               FI-KasseNr:TOOLTIP      = ""
               FI-KasseNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SelgerNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SelgerNr Dialog-Frame
ON CHOOSE OF B-SelgerNr IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cSelgerRowIdList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSelgerIdList     AS CHARACTER  NO-UNDO.
    IF FI-SelgerNr:PRIVATE-DATA <> "" AND FI-SelgerNr:PRIVATE-DATA <> ? THEN
        ASSIGN cSelgerRowIdList = FI-SelgerNr:PRIVATE-DATA.
     RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Selger;SelgerNr;Navn",
                         "where CAN-DO('" + cTillgSelger + "',STRING(Selger.SelgerNr))",
                         INPUT-OUTPUT cSelgerRowIdList,
                         "SelgerNr",
                         INPUT-OUTPUT cSelgerIdList,
                         "","",
                         OUTPUT bOK).
  IF bOK = FALSE THEN
        RETURN NO-APPLY.
  assign
    FI-SelgerNr:SCREEN-VALUE = IF cSelgerIdList = ""
                      then cAlle
                    else "( " + STRING(NUM-ENTRIES(cSelgerIdList,"|")) + " )"
    FI-SelgerNr     = if cSelgerIdList = ""
                      then "*"
                      else REPLACE(cSelgerIdList,"|",",")
    FI-SelgerNr:TOOLTIP = IF FI-SelgerNr = "*" THEN "" ELSE FI-SelgerNr
    FI-SelgerNr:PRIVATE-DATA = cSelgerRowIdList.
    
    
/*   def var IO-Liste as char no-undo.                                      */
/*                                                                          */
/* /*   if wVareGrupper = cAlle then */                                     */
/* /*     RUN InitVaregrupper.       */                                     */
/*                                                                          */
/*   assign                                                                 */
/*     IO-Liste = if FI-SelgerNr:SCREEN-VALUE = cAlle                       */
/*                  then ""                                                 */
/*                  else FI-SelgerNr.                                       */
/*                                                                          */
/*   run d-tagselger.w (input-output IO-Liste).                             */
/*   IF RETURN-VALUE = "Avbryt" THEN                                        */
/*         RETURN NO-APPLY.                                                 */
/*   assign                                                                 */
/*     FI-SelgerNr:SCREEN-VALUE = if IO-Liste = ""                          */
/*                       then cAlle                                         */
/*                     else "( " + STRING(NUM-ENTRIES(IO-Liste)) + " )"     */
/*     FI-SelgerNr     = if IO-Liste = ""                                   */
/*                       then "*"                                           */
/*                       else IO-Liste                                      */
/*     FI-SelgerNr:TOOLTIP = IF FI-SelgerNr = "*" THEN "" ELSE FI-SelgerNr. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SelgerNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SelgerNrBlank Dialog-Frame
ON CHOOSE OF B-SelgerNrBlank IN FRAME Dialog-Frame /* Blank */
DO:
    IF FI-SelgerNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-SelgerNr:SCREEN-VALUE = cAlle
               FI-SelgerNr              = "*"
               FI-SelgerNr:TOOLTIP      = ""
               FI-SelgerNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Transtyper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Transtyper Dialog-Frame
ON CHOOSE OF B-Transtyper IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Transtype;TTId;Beskrivelse",
                        "WHERE TTId < 12",
                        INPUT-OUTPUT cRowIdList,
                        "TTId",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Transtyper:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Transtyper     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Transtyper:TOOLTIP = IF FI-Transtyper = "*" THEN "" ELSE FI-Transtyper.
        IF FI-Transtyper <> "*" THEN DO:
            ASSIGN SELF:PRIVATE-DATA     = cRowIdList + CHR(1) + cIdList
                   FI-Transtyper:BGCOLOR = 11
                   CB-TTId:SCREEN-VALUE = " ".
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-Transtyper:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TranstyperBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TranstyperBlank Dialog-Frame
ON CHOOSE OF B-TranstyperBlank IN FRAME Dialog-Frame /* Blank */
DO:
    IF FI-Transtyper:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Transtyper:SCREEN-VALUE = cAlle
               FI-Transtyper            = "*"
               FI-Transtyper:TOOLTIP      = ""
               FI-Transtyper:BGCOLOR      = ?
               B-Transtyper:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    IF INPUT FI-Dato > INPUT FI-DatoTil THEN DO:
      MESSAGE "Feil dato, fra dato > til dato"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-DatoTIl.
      RETURN NO-APPLY.
    END.
    ASSIGN cButik    = IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE CB-Butik:SCREEN-VALUE
           cTTId     = IF FI-Transtyper <> "*" THEN FI-Transtyper ELSE CB-TTId:SCREEN-VALUE
           dDatofra  = INPUT FI-Dato 
           dDatoil   = INPUT FI-DatoTil
           cKasseNr  = FI-KasseNr
           cForsNr   = FI-ForsNr
           cSelger   = FI-SelgerNr
           lNeglager = TG-Neglager:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut Dialog-Frame
ON CHOOSE OF BUTTON-SokBut IN FRAME Dialog-Frame /* ... */
or F10 of BUTTON-SokBut
DO:
/*    DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO. */
/*    DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO. */
   DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;ButNamn",
                        "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik))",
                        INPUT-OUTPUT cButikerRowIdList,
                        "Butik",
                        INPUT-OUTPUT cButikerIdList,
                        "","",
                        OUTPUT bOK).
/*     "where can-do('" + cTillgButikker + "',string(butik))" , */
    IF bOK THEN DO:
        RUN FixButikVis.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato Dialog-Frame
ON CHOOSE OF BUTTON-SokDato IN FRAME Dialog-Frame /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Feil dato, > Til dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil Dialog-Frame
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME Dialog-Frame /* ... */
or F10 of FI-DatoTil
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato < INPUT FI-Dato THEN DO:
            MESSAGE "Feil dato, < Fra dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-DatoTil:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butik Dialog-Frame
ON VALUE-CHANGED OF CB-Butik IN FRAME Dialog-Frame /* Butikk */
DO:
    RUN SetTillgKassSelger.
    APPLY "CHOOSE" TO B-SelgerNrBlank.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId Dialog-Frame
ON VALUE-CHANGED OF CB-TTId IN FRAME Dialog-Frame /* TransTypeId */
DO:
    IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN
        APPLY "CHOOSE" TO B-TranstyperBlank.
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
    ASSIGN cListItemPairs = entry(1,cButListItemP,CHR(1))
           CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
           CB-Butik = entry(2,cButListItemP,CHR(1))
           CB-TTId:LIST-ITEM-PAIRS  = entry(1,cTTIdListItemP,CHR(1))
           CB-TTId  = entry(2,cTTIdListItemP,CHR(1))
           FI-Dato = TODAY.
  RUN enable_UI.
  ASSIGN FI-TransTyper  = "*"
         FI-TransTyper:SCREEN-VALUE = cAlle
         FI-KasseNr  = "*"
         FI-KasseNr:SCREEN-VALUE    = cAlle
         FI-ForsNr  = "*"
         FI-ForsNr:SCREEN-VALUE   = cAlle
         FI-SelgerNr  = "*"
         FI-SelgerNr:SCREEN-VALUE = cAlle.
APPLY "VALUE-CHANGED" TO CB-Butik.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturVerdi.

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
  DISPLAY FI-Dato FI-DatoTil FI-KasseNr FI-Butikker FI-ForsNr CB-Butik 
          FI-SelgerNr CB-TTId TG-Neglager FI-Transtyper 
      WITH FRAME Dialog-Frame.
  ENABLE FI-Dato FI-DatoTil B-Transtyper B-KasseNrBlank Btn_OK BUTTON-SokBut 
         B-ForsNrBlank Btn_Cancel CB-Butik B-SelgerNrBlank CB-TTId TG-Neglager 
         B-TranstyperBlank B-KasseNr BUTTON-SokDatoTil B-SelgerNr B-ForsNr 
         BUTTON-SokDato 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikVis Dialog-Frame 
PROCEDURE FixButikVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF cButikerIdList <> "" THEN
          ASSIGN CB-Butik:LIST-ITEM-PAIRS = ",INGEN"
                 FI-Butikker = cButikerIdList
                 FI-Butikker:BGCOLOR = 15
                 FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"
                 FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")
                 CB-Butik:SCREEN-VALUE = "INGEN"
                 CB-Butik:SENSITIVE    = FALSE.
      ELSE
          ASSIGN FI-Butikker:BGCOLOR = ?
                 FI-Butikker = ""
                 FI-Butikker:SCREEN-VALUE = ""
                 FI-Butikker:TOOLTIP = ""
                 CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
                 CB-Butik:SCREEN-VALUE    = entry(2,cButListItemP,CHR(1))
                 CB-Butik:SENSITIVE    = TRUE.
                 .
     APPLY "VALUE-CHANGED" TO CB-Butik.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetTillgKassSelger Dialog-Frame 
PROCEDURE SetTillgKassSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* cTillgKasserer */
    /* Tillgängliga selgere för den butik som är vald   */
    ASSIGN cTillgSelger   = ""
           cTillgKasserer = "".
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH ButikkSelger WHERE ButikkSelger.butikknr = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgSelger = cTillgSelger + (IF cTillgSelger <> "" THEN "," ELSE "")
                                               + STRING(ButikkSelger.SelgerNr).
        END.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH ButikkSelger WHERE ButikkSelger.butikknr = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgSelger = cTillgSelger + (IF cTillgSelger <> "" THEN "," ELSE "")
                                               + STRING(ButikkSelger.SelgerNr).
        END.
        FOR EACH butikkforsalj WHERE butikkforsalj.butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgKasserer = cTillgKasserer + (IF cTillgKasserer <> "" THEN "," ELSE "")
                                               + STRING(butikkforsalj.ForsNr).
        END.
    END.
    APPLY "CHOOSE" TO B-ForsNrBlank.
    APPLY "CHOOSE" TO B-SelgerNrBlank.
    APPLY "CHOOSE" TO B-KasseNrBlank.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

