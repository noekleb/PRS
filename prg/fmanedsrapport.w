&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpLevBas NO-UNDO LIKE LevBas.


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

DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.


/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter = "Kontant,BankPose,BankKort,Visa,Eurocard,Amex,Diners,SenterGavekort,DiverseKort,KontKjopKasse,TilgodeBruktEgne,TilgodeBruktAndre,GavekortBruktEgne,GavekortBruktAndre,TilgodeUt,GavekortUt,SumInnbutikk,OmsetningEksKred,DiffKasse,Kreditsalg,InnbetaltKunde"

cFieldDefs = "ButikkNr;Butikk;;1," +
             "Dato;Dato;;," +
             "Ukedag;Dag;;," +
             "BokfNr;BokfNr;;," +
             "Kontant;Kontant;2;1," +
             "BankPose;BankPose;2;1," +
             "BankKort;BankKort;2;1," +
             "Visa;Visa;2;1," +
             "Eurocard;Eurocard;2;1," +
             "Amex;Amex;2;1," +
             "Diners;Diners;2;1," +
             "SenterGavekort;SenterGavekort;2;1," +
             "DiverseKort;DiverseKort;2;1," +
             "KontKjopKasse;KontKjopKasse;2;1," +
             "Beskrivelse;Beskrivelse;;,"   +
             "TilgodeBruktEgne;TilgodeBruktEgne;2;1," +
             "TilgodeBruktAndre;TilgodeBruktAndre;2;1," +
             "GavekortBruktEgne;GavekortBruktEgne;2;1," +
             "GavekortBruktAndre;GavekortBruktAndre;2;1," +
             "TilgodeUt;TilgodeUt;2;1," +
             "GavekortUt;GavekortUt;2;1," +
             "SumInnbutikk;SumInnbutikk;2;1," +
             "OmsetningEksKred;OmsetningEksKred;2;1," +
             "DiffKasse;DiffKasse;2;1," +
             "Kreditsalg;Kreditsalg;2;1," +
             "InnbetaltKunde;InnbetaltKunde;2;1".
             
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

{runlib.i}

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
&Scoped-Define ENABLED-OBJECTS FI-Dato FI-DatoTil B-Aktiver CB-Butik ~
BUTTON-SokBut BUTTON-SokDatoTil BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil FI-Butikker CB-Butik 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

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

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Dato AT ROW 1.19 COL 14 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.19 COL 32 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     FI-Butikker AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 47
     CB-Butik AT ROW 3.19 COL 14 COLON-ALIGNED HELP
          "Butikknummer"
     BUTTON-SokBut AT ROW 2.19 COL 29.2 NO-TAB-STOP 
     BUTTON-SokDatoTil AT ROW 1.19 COL 47.6
     BUTTON-SokDato AT ROW 1.19 COL 29.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 6.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpLevBas T "NEW SHARED" NO-UNDO skotex LevBas
   END-TABLES.
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
         HEIGHT             = 6.29
         WIDTH              = 155.
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

/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
    DEFINE VARIABLE TTH_Table AS HANDLE.
    DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
    DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cParaString AS CHARACTER  NO-UNDO.
/*     Dessa är flyttade till definitionsblocket för att kunna hanteras om vi har valt avancerat */
/*     eller kommer från artikkelutvalg */
/*     DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO. */
/*     DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    IF INPUT FI-Dato > INPUT FI-DatoTil THEN DO:
        MESSAGE "Feil dato, fra dato > til dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTIl.
        RETURN NO-APPLY.
    END.
    ETIME(TRUE).
    RUN StartSok.
    RUN SetFilterParam (OUTPUT ocButiker).
/*     iTime3 = ETIME.                           */
/*     MESSAGE "Polygon test, skal fjernes" SKIP */
/*     iTime1 SKIP iTime2 SKIP iTime3            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
    PUBLISH "VisTxtBox" ("Søker data......").

/*     RUN TransLoggToTTNY IN h_dtranslogg                                                                  */
/*       ( OUTPUT TTH,pcFeltListe,pcVerdier,?,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil). */
    cParaString = REPLACE(ocButiker,"|",",") + ";" + STRING(INPUT FI-Dato) + ";" + (IF INPUT FI-DatoTil = ? THEN STRING(INPUT FI-Dato) ELSE STRING(INPUT FI-DatoTil)) + "|".
    TTH_Table = DYNAMIC-FUNCTION("getTempTable","get_manedsrapp.p",cParaString,?).
    TTH = TTH_Table:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY qh.
    qh:SET-BUFFERS(TTH).
    qh:QUERY-PREPARE("for each " + TTH:NAME).
    qh:QUERY-OPEN().

  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("for each " + TTH:NAME,"",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

  DO:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,5).  /* 3 = antall frozen cols  */
      PUBLISH "AlignCol" (LOOKUP("Storl",cFelter),8). /* högerjustert */
      DO:
          ASSIGN cSumCols   = getSumFelter(cSummerFelter)
                 cSumString = getSumFelter("BokfNr") + ",SUM" .
          PUBLISH "Summer" (cSumCols,cSumString).
      END.

  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
/*     "where can-do('" + cTillgButikker + "',string(butik))" , */
    IF bOK THEN DO:
        RUN FixButikVis.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato fFrameWin
ON CHOOSE OF BUTTON-SokDato IN FRAME fMain /* ... */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil fFrameWin
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME fMain /* ... */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF
   {lng.i &SDO = "SDO"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktiver fFrameWin 
PROCEDURE Aktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButikkNr AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER dTransDat AS DATE    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-DO(cTillgButikker,STRING(iButikkNr)) THEN DO:
            FIND Butiker WHERE Butiker.butik = iButikkNr NO-LOCK NO-ERROR.
            IF AVAIL Butiker THEN DO:
                CB-Butik:ADD-LAST(Butiker.Butnamn,STRING(iButikkNr)).
            END.
        END.
        ASSIGN CB-Butik:SCREEN-VALUE = STRING(iButikkNr)
               FI-Dato = dTransDat
               FI-Dato:SCREEN-VALUE = STRING(FI-Dato)
               CB-Butik:SENSITIVE   = FALSE
               FI-Dato:SENSITIVE    = FALSE
               FI-DatoTil:SENSITIVE = FALSE
               BUTTON-SokDato:SENSITIVE    = FALSE
               BUTTON-SokDatoTil:SENSITIVE = FALSE.
    END.
    PROCESS EVENTS.
    APPLY "VALUE-CHANGED" TO CB-Butik.
    APPLY "CHOOSE" TO B-Aktiver.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort fFrameWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  fLockvindu(TRUE).
  run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avancerat fFrameWin 
PROCEDURE Avancerat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN tmpUtvalg IN h_Window (THIS-PROCEDURE).

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
  DISPLAY FI-Dato FI-DatoTil FI-Butikker CB-Butik 
      WITH FRAME fMain.
  ENABLE FI-Dato FI-DatoTil B-Aktiver CB-Butik BUTTON-SokBut BUTTON-SokDatoTil 
         BUTTON-SokDato 
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
                 CB-Butik:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN 
                                                    cUserDefaultBut ELSE ENTRY(2,cListItemPairs)
                 CB-Butik:SENSITIVE    = TRUE.
                 .
     APPLY "VALUE-CHANGED" TO CB-Butik.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStrings fFrameWin 
PROCEDURE FixStrings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    /* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
    ASSIGN cFelter = FILL(",",NUM-ENTRIES(cFieldDefs) - 1)
           cLabels = cFelter
           cDecimaler = cFelter
           cRightCols = cFelter.
    DO iCount = 1 TO NUM-ENTRIES(cFieldDefs):
        ASSIGN ENTRY(iCount,cFelter) = ENTRY(1,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cLabels) = ENTRY(2,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefs),";").
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos fFrameWin 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO. */
    DO WITH FRAME {&FRAME-NAME}:

        /* Butiker start */
        FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
        ASSIGN cListItemPairs  = "".
        /* Leta upp unika förekomster av butiker genom brukers team */
        FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                  ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
            FOR EACH ButikkKobling OF ButikkTeam.
                FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
                IF NOT AVAIL TT_TillgButikker THEN DO:
                        CREATE TT_TillgButikker.
                        ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
                END.
            END.
        END.
        FOR EACH TT_TillgButikker:
            FIND Butiker OF TT_TillgButikker NO-LOCK NO-ERROR.
            IF NOT AVAIL Butiker THEN
                NEXT.
            ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(Butiker.Butik)
                   cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                                    Butiker.butnamn + "," + STRING(Butiker.Butik).
                   cUserDefaultBut = IF Butiker.Butik = Bruker.ButikkNr THEN STRING(Butiker.Butik) ELSE cUserDefaultBut.
        END.
        ASSIGN cUserDefaultBut = IF TRIM(cUserDefaultBut) = "" THEN TRIM(ENTRY(2,cListItemPairs)) ELSE cUserDefaultBut
               CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
               CB-Butik:SCREEN-VALUE = cUserDefaultBut.
        /* Butiker end */

/* gammal butikshantering */
/*       FOR EACH Butiker /* WHERE CAN-FIND(FIRST Kasse WHERE Kasse.Butik = Butiker.Butik) */ NO-LOCK:                                           */
/*           ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") + Butiker.ButNamn + "," + STRING(Butiker.Butik). */
/*       END.                                                                                                                                    */
/*       IF NUM-ENTRIES(cListItemPairs) > 2 THEN                                                                                                 */
/*           ASSIGN cListItemPairs = cAlle + ",," + cListItemPairs.                                                                              */
/*       ASSIGN CB-Butik:LIST-ITEM-PAIRS = cListItemPairs                                                                                        */
/*              CB-Butik:SCREEN-VALUE = IF NUM-ENTRIES(cListItemPairs) > 2 THEN " " ELSE ENTRY(2,cListItemPairs).                                */
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
  ASSIGN h_Window = SOURCE-PROCEDURE.
  RUN FixStrings.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  ASSIGN FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridmaned.txt".
  APPLY "VALUE-CHANGED" TO CB-Butik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFilterValues fFrameWin 
PROCEDURE SendFilterValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFilterVerdier AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cColAlign      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR.
      ASSIGN cFilterVerdier = "Butikk: " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") + CHR(10) +
             "Dato: " + FI-Dato:SCREEN-VALUE
             cColAlign = cRightCols.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilterParam fFrameWin 
PROCEDURE SetFilterParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER ocButiker AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN ocButiker = IF fi-Butikker <> "" THEN fi-Butikker ELSE CB-Butik:SCREEN-VALUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount      AS INTE NO-UNDO.
  DEF VAR iTst        AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
/*     ASSIGN pcFeltListe = "Dato,Butik,LevNr,TTId". */
    ASSIGN pcFeltListe = "Dato,Butik".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "Dato" THEN DO:
                IF INPUT FI-DatoTil = ? OR INPUT FI-DatoTil = INPUT FI-Dato THEN
                  ASSIGN
                  pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                  pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                             FI-Dato:SCREEN-VALUE
                  pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
                ELSE DO:
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   FI-Dato:SCREEN-VALUE
                        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                  FI-DatoTil:SCREEN-VALUE
                        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
                END.
            END.
            WHEN "Butik" THEN DO:
/*                 IF CB-Butik:SCREEN-VALUE <> "" THEN */
                IF FI-Butikker = "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Butik"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Butik:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
        END CASE.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArtDyn fFrameWin 
PROCEDURE StartSokArtDyn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER qhArtSok        AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal     AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE qh AS HANDLE     NO-UNDO.
  DEFINE VARIABLE TTH            AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cKriterier     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ipKriterierTot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFraAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTilAarPer   AS INTEGER    NO-UNDO.
/*   DEFINE VARIABLE pcFeltListe  AS CHARACTER  NO-UNDO. */
  DEFINE VARIABLE iPeriode     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cDataobjekt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lEkstern     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE datofra      AS DATE       NO-UNDO.
  DEFINE VARIABLE datotil      AS DATE       NO-UNDO.
  DEFINE VARIABLE cButik       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cttid        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKassenr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cForsnr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSelger   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lneglager AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ocTTId    AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      DO:
          /* Här kommer vi från artikkelutvalgsbrowsern */
          ASSIGN FI-Dato:SCREEN-VALUE = STRING(datofra) 
                 FI-DatoTil:SCREEN-VALUE = STRING(datotil) 
                 FI-Butikker = IF NUM-ENTRIES(cButik) > 1 THEN cButik ELSE ""
                 CB-Butik:SCREEN-VALUE = IF NUM-ENTRIES(cButik) > 1 THEN CB-Butik:SCREEN-VALUE ELSE STRING(cButik).
          RUN Startsok.
/*           RUN SetFilterParam (OUTPUT ocButiker,OUTPUT ocTTId). */
/*           RUN SetFilterParam. */
      END.
      RUN SetFilterParam (OUTPUT ocButiker,OUTPUT ocTTId).
      PUBLISH "VisTxtBox" ("Søker data......").
/* för test */
/*       MESSAGE "NY TEST Ja/Nej"                                                                                        */
/*           VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lsvar AS logi.                                                 */
/*       IF lsvar = TRUE THEN DO:                                                                                        */
/*           ETIME(TRUE).                                                                                                */
/*           RUN TransLoggToTTNY IN h_dtranslogg                                                                         */
/*             ( OUTPUT TTH,pcFeltListe,pcVerdier,qhArtSok,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil). */
/*       END.                                                                                                            */
/*       ELSE DO:                                                                                                        */
/*           ETIME(TRUE).                                                                                                */
/*           RUN TransLoggToTT IN h_dtranslogg                                                                           */
/*             ( OUTPUT TTH,pcFeltListe,pcVerdier,qhArtSok).                                                             */
/*       END.                                                                                                            */
/*       MESSAGE ETIME                                                                                                   */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                          */
/* end test */


      CREATE QUERY qh.
      qh:SET-BUFFERS(TTH).
      qh:QUERY-PREPARE("for each TT_TransLogg").
      qh:QUERY-OPEN().

  /*   RUN rappgenqry.p ("Translogg",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dtranslogg),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,?). */
  /*   RUN rappgenqry.p ("Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,DYNAMIC-FUNCTION('getQueryHandle':U IN h_dtranslogg)). */
    PUBLISH "VisTxtBox" ("Leser ut data......").
    RUN rappgenqry.p ("TT_Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

    PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
    PUBLISH "LoadGrid" (cFileName,5).  /* 3 = antall frozen cols  */
    PUBLISH "AlignCol" (LOOKUP("Storl",cFelter),8). /* högerjustert */
    DO:
        PUBLISH "Summer" (getSumFelter(cSummerFelter),getSumFelter("Bongtekst") + ",SUM").
    END.

  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  DELETE OBJECT TTH.
  PUBLISH "VisTxtBox" ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject fFrameWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "ClearGrid" (cLabels).
  APPLY "ENTRY" TO FI-Dato IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltNumListe AS CHARACTER  NO-UNDO.
  ASSIGN cFeltNumListe = FILL(",",NUM-ENTRIES(cFeltnavnListe) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cFeltnavnListe):
      ASSIGN ENTRY(iCount,cFeltNumListe) = STRING(LOOKUP(ENTRY(iCount,cFeltnavnListe),cFelter)).
  END.
  RETURN cFeltNumListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

