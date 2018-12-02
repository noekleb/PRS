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

DEFINE VARIABLE wTittel           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAnropButiker AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-SokBut FI-Butikker CB-ButikkTeam ~
CB-PerId FI-Dato1 FI-FraAar FI-LinjeNr1 FI-TilAar FI-Dato2 FI-LinjeNr2 ~
BUTTON-SokDato1 BUTTON-SokDato2 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikker CB-ButikkTeam CB-PerId ~
FI-Dato1 FI-FraAar FI-LinjeNr1 FI-TilAar FI-Dato2 FI-LinjeNr2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE CB-PerId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Periodetype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Fra år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr2 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Til år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-SokBut AT ROW 1.19 COL 31.8 NO-TAB-STOP 
     FI-Butikker AT ROW 1.19 COL 14 COLON-ALIGNED
     CB-ButikkTeam AT ROW 2.19 COL 14 COLON-ALIGNED
     CB-PerId AT ROW 3.19 COL 14 COLON-ALIGNED
     FI-Dato1 AT ROW 4.19 COL 14 COLON-ALIGNED
     FI-FraAar AT ROW 4.19 COL 14 COLON-ALIGNED
     FI-LinjeNr1 AT ROW 4.19 COL 23 COLON-ALIGNED NO-LABEL
     FI-TilAar AT ROW 5.19 COL 14 COLON-ALIGNED
     FI-Dato2 AT ROW 5.19 COL 14 COLON-ALIGNED
     FI-LinjeNr2 AT ROW 5.19 COL 23 COLON-ALIGNED NO-LABEL
     BUTTON-SokDato1 AT ROW 4.19 COL 31.8 NO-TAB-STOP 
     BUTTON-SokDato2 AT ROW 5.19 COL 31.8 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36.4 BY 6.14.


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
         HEIGHT             = 6.14
         WIDTH              = 36.4.
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

ASSIGN 
       FI-Butikker:READ-ONLY IN FRAME fMain        = TRUE.

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

&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut fFrameWin
ON CHOOSE OF BUTTON-SokBut IN FRAME fMain /* ... */
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
    IF bOK THEN DO:
        RUN FixButikVis.
        PUBLISH "runrun".
/*         MESSAGE "Echoose" CB-butikkteam:SCREEN-VALUE */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.       */
/*         PROCESS EVENTS.                              */
/*         RETURN NO-APPLY. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato1 fFrameWin
ON CHOOSE OF BUTTON-SokDato1 IN FRAME fMain /* ... */
or F10 of FI-Dato1
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato1 = date(FI-Dato1:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato1
      &Program     = kalender.w
      &Frame       = fMain
      &ExtraParam  = "input wTittel"
    }   
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato2 fFrameWin
ON CHOOSE OF BUTTON-SokDato2 IN FRAME fMain /* ... */
or F10 of FI-Dato2
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato2 = date(FI-Dato2:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato2
      &Program     = kalender.w
      &Frame       = fMain
      &ExtraParam  = "input wTittel"
    } 
    return no-apply.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam fFrameWin
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME fMain /* Butikkteam */
DO:
  ASSIGN CB-ButikkTeam:TOOLTIP = IF CB-ButikkTeam:SCREEN-VALUE = "INGEN" THEN "" ELSE IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN
      ENTRY(1,CB-ButikkTeam:SCREEN-VALUE,";") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-PerId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-PerId fFrameWin
ON VALUE-CHANGED OF CB-PerId IN FRAME fMain /* Periodetype */
DO:
    RUN InitFillIns (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

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
  DISPLAY FI-Butikker CB-ButikkTeam CB-PerId FI-Dato1 FI-FraAar FI-LinjeNr1 
          FI-TilAar FI-Dato2 FI-LinjeNr2 
      WITH FRAME fMain.
  ENABLE BUTTON-SokBut FI-Butikker CB-ButikkTeam CB-PerId FI-Dato1 FI-FraAar 
         FI-LinjeNr1 FI-TilAar FI-Dato2 FI-LinjeNr2 BUTTON-SokDato1 
         BUTTON-SokDato2 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikVis fFrameWin 
PROCEDURE FixButikVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF cButikerIdList <> "" THEN DO:
          ASSIGN 
                 CB-ButikkTeam:LIST-ITEM-PAIRS = " ,INGEN"
                 FI-Butikker = cButikerIdList
                 FI-Butikker:BGCOLOR = 15
                 FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"
                 FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")
                 CB-ButikkTeam:SCREEN-VALUE = "INGEN"
                 CB-ButikkTeam:SENSITIVE    = FALSE.
      END.
      ELSE DO:
          ASSIGN FI-Butikker:BGCOLOR = ?
                 FI-Butikker = ""
                 FI-Butikker:SCREEN-VALUE = ""
                 FI-Butikker:TOOLTIP = ""
                 CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
                 CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cListItemPairs,cUserDefaultBut) THEN 
                                                    cUserDefaultBut ELSE ENTRY(2,cListItemPairs)
                 CB-ButikkTeam:SENSITIVE    = TRUE.
      END.
     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB fFrameWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cStTypeId  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButString        AS CHARACTER  NO-UNDO.
    FOR EACH StDef WHERE StDef.StTypeId = cStTypeId NO-LOCK:
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
            StDef.Beskrivelse + "," + StDef.PerId.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-PerId:LIST-ITEM-PAIRS = cListItemPairs
               CB-PerId:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
        APPLY "VALUE-CHANGED" TO CB-PerId.
    END.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cListItemPairs = "".
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                              ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
        ASSIGN cButString = "".
        FOR EACH ButikkKobling OF ButikkTeam.
            ASSIGN cButString = cButString + (IF cButString = "" THEN "" ELSE CHR(1)) 
                              + STRING(ButikkKobling.butik).
        END.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                             ButikkTeam.Beskrivelse + "," + cButString.
    END.
    ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
           CB-ButikkTeam:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
    APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB2 fFrameWin 
PROCEDURE InitCB2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStTypeId    AS CHARACTER INIT "AVDELING" NO-UNDO.
    DEFINE VARIABLE cButString   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOkButiker   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButStringListe AS CHARACTER  NO-UNDO.
    FOR EACH StDef WHERE StDef.StTypeId = cStTypeId NO-LOCK:
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
            StDef.Beskrivelse + "," + StDef.PerId.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-PerId:LIST-ITEM-PAIRS = cListItemPairs
               CB-PerId:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
        APPLY "VALUE-CHANGED" TO CB-PerId.
    END.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cUserDefaultBut = STRING(Bruker.ButikkNr)
           cListItemPairs  = "".
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                              ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
        ASSIGN cButString = "".
        FOR EACH ButikkKobling OF ButikkTeam.
            ASSIGN cButString = cButString + (IF cButString = "" THEN "" ELSE CHR(1)) 
                              + STRING(ButikkKobling.butik).
            FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
            IF NOT AVAIL TT_TillgButikker THEN DO:
                    CREATE TT_TillgButikker.
                    ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
            END.
        END.
        IF NUM-ENTRIES(cButString,CHR(1)) > 25 THEN DO:
            CREATE TT_BigListItem.
            ASSIGN TT_BigListItem.Butiker = cButString
                   cButString = "(" + STRING(NUM-ENTRIES(cButString,CHR(1))) + ");" + STRING(ROWID(TT_BigListItem)).
            RELEASE TT_BigListItem.
        END.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                             ButikkTeam.Beskrivelse + "," + cButString
               cButStringListe = cButStringListe + (IF cButStringListe <> "" THEN "," ELSE "") + cButString.
    END.
    FOR EACH TT_TillgButikker:
        ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.
    /* Om vi har fått en butiklista genom proc SetIpButiker skall vi kontrollera att listan */
    /* innehåller butiker vi har tillgång till och ev strippa bort andra butiker */
    IF cAnropButiker <> "" AND cAnropButiker <> cUserDefaultBut THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cAnropButiker,"|"):
            IF CAN-DO(cTillgButikker,ENTRY(iCount,cAnropButiker,"|")) THEN
                ASSIGN cOkButiker = cOkButiker + (IF cOkButiker <> "" THEN "|" ELSE "") + ENTRY(iCount,cAnropButiker,"|").
        END.
        /* om cOkButiker finns i teamlistan i combo väljer vi det teamet som cUserdefault */
/*         MESSAGE "cTillgbutikker  : " cTillgbutikker SKIP              */
/*                 "cOkButiker     : " cOkButiker SKIP                   */
/*                 "cButStringListe:" cButStringListe                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*         IF cOkButiker <> "" THEN DO:                                  */
/*             IF CAN-DO(cButStringListe,cOkButiker) THEN                */
/*                 ASSIGN cUserDefaultBut = REPLACE(cOkButiker,"|",","). */
/*             ELSE                                                      */
/*                 cButStringListe = cOkButiker.                         */
/*         END.                                                          */
        IF cOkButiker <> "" THEN DO:
            ASSIGN cButikerIdList = cOkButiker.
            DO iCount = 1 TO NUM-ENTRIES(cOkButiker,"|"):
                FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCOunt,cOkButiker,"|")) NO-LOCK NO-ERROR.
                IF AVAIL Butiker THEN
                   ASSIGN cButikerRowIdList = cButikerRowIdList + (IF cButikerRowIdList <> "" THEN "," ELSE "") + STRING(ROWID(Butiker)).
            END.
        END.
    END.
    RUN FixButikVis.
/*     ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs                                                           */
/*            CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN */
/*                                              cUserDefaultBut ELSE ENTRY(2,cListItemPairs).                          */
/*     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.                                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFillIns fFrameWin 
PROCEDURE InitFillIns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cPerId AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-FraAar:HIDDEN       = TRUE
               FI-TilAar:HIDDEN       = TRUE
               FI-LinjeNr1:HIDDEN     = TRUE
               FI-LinjeNr2:HIDDEN     = TRUE
               FI-Dato1:HIDDEN        = TRUE
               FI-Dato2:HIDDEN        = TRUE
               BUTTON-SokDato1:HIDDEN = TRUE
               BUTTON-SokDato2:HIDDEN = TRUE.
        CASE cPerId:
            WHEN "AAR" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE = STRING(YEAR(TODAY))
                       FI-FraAar:HIDDEN       = FALSE
                       FI-TilAar:HIDDEN       = FALSE.
            END.
            WHEN "MANED" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-LinjeNr1:SCREEN-VALUE = "1"
                       FI-LinjeNr2:SCREEN-VALUE = "12"
                       FI-FraAar:HIDDEN         = FALSE
                       FI-TilAar:HIDDEN         = FALSE
                       FI-LinjeNr1:HIDDEN       = FALSE
                       FI-LinjeNr2:HIDDEN       = FALSE.
            END.
            WHEN "UKE" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-LinjeNr1:SCREEN-VALUE = "1"
                       FI-LinjeNr2:SCREEN-VALUE = "53"
                       FI-FraAar:HIDDEN         = FALSE
                       FI-TilAar:HIDDEN         = FALSE
                       FI-LinjeNr1:HIDDEN       = FALSE
                       FI-LinjeNr2:HIDDEN       = FALSE.
            END.
            WHEN "DAG" THEN DO:
                ASSIGN FI-Dato1:SCREEN-VALUE  = STRING(DATE(1,1,YEAR(TODAY)))
                       FI-Dato2:SCREEN-VALUE  = STRING(TODAY)
                       FI-Dato1:HIDDEN        = FALSE
                       FI-Dato2:HIDDEN        = FALSE
                       BUTTON-SokDato1:HIDDEN = FALSE
                       BUTTON-SokDato2:HIDDEN = FALSE.
            END.
            OTHERWISE DO:
            END.
        END CASE.
    END.
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
  RUN InitCB2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetIpButiker fFrameWin 
PROCEDURE SetIpButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipButiker AS CHARACTER  NO-UNDO.
        ASSIGN cAnropButiker = ipButiker.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKriterier fFrameWin 
PROCEDURE ValiderKriterier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CASE CB-PerId:SCREEN-VALUE:
        WHEN "AAR" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "MANED" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar OR 
               INPUT FI-LinjeNr1 < 1 OR 
               INPUT FI-LinjeNr1 > 12 OR
               INPUT FI-LinjeNr2 < 1 OR 
               INPUT FI-LinjeNr2 > 12 OR 
               (INPUT FI-FraAar = INPUT FI-TilAar AND 
                      INPUT FI-LinjeNr1 > INPUT FI-LinjeNr2) 
                THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "UKE" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar OR 
               INPUT FI-LinjeNr1 < 1 OR 
               INPUT FI-LinjeNr1 > 53 OR
               INPUT FI-LinjeNr2 < 1 OR 
               INPUT FI-LinjeNr2 > 53 OR 
               (INPUT FI-FraAar = INPUT FI-TilAar AND 
                      INPUT FI-LinjeNr1 > INPUT FI-LinjeNr2) 
                THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "DAG" THEN DO:
            IF INPUT FI-Dato1 > TODAY OR
               INPUT FI-Dato2 > TODAY OR 
               INPUT FI-Dato1 > INPUT FI-Dato2 THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        OTHERWISE DO:
            IF INPUT FI-Dato1 > TODAY OR
               INPUT FI-Dato2 > TODAY OR 
               INPUT FI-Dato1 > INPUT FI-Dato2 THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
    END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
  RUN ValiderKriterier.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN FALSE.   /* Function return value. */
  ELSE DO WITH FRAME {&FRAME-NAME}:
      CASE CB-PerId:SCREEN-VALUE:
          WHEN "AAR" THEN DO:
              ASSIGN cKriterier = "AAR" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + "," + "1" + "," + "1".
          END.
          WHEN "MANED" THEN DO:
              ASSIGN cKriterier = "MANED" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + ","  +
                                                 STRING(INPUT FI-LinjeNr1) + "," +
                                                 STRING(INPUT FI-LinjeNr2).
          END.
          WHEN "UKE" THEN DO:
              ASSIGN cKriterier = "UKE" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + "," +
                                                 STRING(INPUT FI-LinjeNr1) + "," +
                                                 STRING(INPUT FI-LinjeNr2).
          END.
          WHEN "DAG" THEN DO:
              ASSIGN cKriterier = "DAG" + "," + STRING(INPUT FI-Dato1) + "," +
                                                 STRING(INPUT FI-Dato2).
          END.
          OTHERWISE DO:
              ASSIGN cKriterier = CB-PerId:SCREEN-VALUE + "," + STRING(INPUT FI-Dato1) + "," +
                                                 STRING(INPUT FI-Dato2).
          END.
      END CASE.
      IF FI-Butikker = "" THEN DO:
          IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN DO:
              FIND TT_BigListItem WHERE ROWID(TT_BigListItem) = TO-ROWID(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,";")).
              ASSIGN cButiker = REPLACE(TT_BigListItem.Butiker,CHR(1),",").
          END.
          ELSE
              ASSIGN cButiker = REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
      END.
      ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE cButiker) + CHR(1) + cKriterier.
/*       ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")) + CHR(1) + cKriterier. */
      RETURN TRUE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

