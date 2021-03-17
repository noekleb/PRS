&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTransFelter  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGetTransVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hGrid AS HANDLE      NO-UNDO.
DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lButikkBruker  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cSkomodus AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVisFelterTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTTdata AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cWStyp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lWSaktiv AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.

/*        cTidFelter = "PostertTid,Tid,". */
{syspara.i 1 1 54 cSkomodus}
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */

DEFINE TEMP-TABLE tt_weborder NO-UNDO
    FIELD Dato      AS DATE
    FIELD Butikknr  AS INTE
    FIELD KOrdre_Id AS DECI
    FIELD Bongnr    AS INTE
    FIELD Levstatus AS CHAR
    FIELD StatusTxt AS CHAR
    FIELD Onorder   AS INTE
    FIELD Bought    AS INTE
    FIELD Other     AS INTE
    FIELD Kassenr   AS INTE
    FIELD Bongdatum AS DATE
    FIELD Bongbelopp AS DECI
    FIELD KOrdreBelopp AS DECI
    INDEX DBK IS PRIMARY UNIQUE Dato ButikkNr KOrdre_id.


ASSIGN cTransFelter = "ButikkNr,KasseNr,Bongdatum,Bongnr,KOrdre_Id"
       cSummerFelter = "Antall,SumNetto,Mva,SumVk,SumDBKr".

/* cVisFelterTxt = "ButikkNr,KasseNr,Dato,VareGr,LopeNr,Storrelse,BongTekst,LevNr,LevNavn,Antall,FeilKode,FeilKodeTekst,LinjeRab,Nettokr,LinjeSum". */

/* IF cSkomodus = "1" THEN */
cFieldDefs = 
    "Dato;Dato;;1," +
    "ButikkNr;Butikk;;1," +
    "KOrdre_Id;KOrdre_Id;;1," +
    "KOrdreBelopp;Orderbelopp;2;1," +
    "BongNr;BongNr;;1," +
    "Bongdatum;Kvittodatum;;1," +
    "Bongbelopp;Kvittobelopp;2;1," +
    "LevStatus;LevStatus;;1," +
    "StatusTxt;StatusTxt;;1," +
    "OnOrder;På order;;1," +
    "Bought;Köpt;;1," + 
    "Kassenr;Kassa;;1"    .


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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Dato FI-DatoTil B-Aktiver CB-Typ ~
BUTTON-SokDatoTil BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil CB-Typ 

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

DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Transtyper  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-TranstyperBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.14.

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

DEFINE VARIABLE CB-Typ AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

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

DEFINE VARIABLE FI-Transtyper AS CHARACTER FORMAT "X(10)":U 
     LABEL "Transtyper" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Dato AT ROW 1.19 COL 14 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.19 COL 32 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     B-Transtyper AT ROW 5.19 COL 31.4 NO-TAB-STOP 
     FI-Butikker AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 47.8
     CB-Butik AT ROW 3.19 COL 14 COLON-ALIGNED HELP
          "Butikknummer"
     CB-Typ AT ROW 3.38 COL 46 COLON-ALIGNED NO-LABEL
     CB-TTId AT ROW 4.19 COL 14 COLON-ALIGNED HELP
          "TransaksjonstypensID"
     B-VisTrans AT ROW 4.57 COL 47
     FI-Transtyper AT ROW 5.19 COL 14 COLON-ALIGNED
     B-TranstyperBlank AT ROW 5.19 COL 36.6
     B-Rapport AT ROW 5.76 COL 47
     B-Artikkelkort AT ROW 5.76 COL 63
     BUTTON-SokBut AT ROW 2.19 COL 29.2 NO-TAB-STOP 
     BUTTON-SokDatoTil AT ROW 1.19 COL 47.8
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Artikkelkort IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Artikkelkort:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Rapport IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Rapport:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Transtyper IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Transtyper:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-TranstyperBlank IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-TranstyperBlank:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-VisTrans IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-VisTrans:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokBut IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SokBut:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Butik IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CB-Butik:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-TTId IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CB-TTId:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-Butikker:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN FI-Transtyper IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-Transtyper:HIDDEN IN FRAME fMain           = TRUE.

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
    DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
    DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTime1 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTime2 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTime3 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ocTTId    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButikLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDatolista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTTIdlista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRabattLista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cVisFelterTxtTmp AS CHARACTER   NO-UNDO.
/*     Dessa är flyttade till definitionsblocket för att kunna hanteras om vi har valt avancerat */
/*     eller kommer från artikkelutvalg */
/*     DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO. */
/*     DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVisFelterNr AS CHARACTER   NO-UNDO.
    cTTdata = "".
    ASSIGN INPUT FI-Dato FI-DatoTil.

    IF FI-Dato = ? THEN DO:
        MESSAGE "Registrer dato fra"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-Dato.
        RETURN NO-APPLY.
    END.
    ELSE IF FI-DatoTil = ? THEN DO:
        MESSAGE "Registrer dato til"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTil.
        RETURN NO-APPLY.
    END.
    ELSE IF FI-Dato > FI-DatoTil THEN DO:
        MESSAGE "Feil dato, fra dato > til dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTIl.
        RETURN NO-APPLY.
    END.

/*     cButikLista = IF FI-Butikker = "" THEN CB-Butik:SCREEN-VALUE ELSE REPLACE(FI-Butikker,"|",","). */
    cDatolista  = FI-Dato:SCREEN-VALUE + "," + FI-DatoTil:SCREEN-VALUE.

/*     cTTIdlista  = IF FI-Transtyper = "*" AND TRIM(CB-TTId:SCREEN-VALUE) = "" THEN "1,3,10" ELSE     */
/*                   IF TRIM(CB-TTId:SCREEN-VALUE) <> "" THEN CB-TTId:SCREEN-VALUE ELSE FI-Transtyper. */
/*     cRabattLista = "".                                                                              */
    
    PUBLISH "VisTxtBox" ("Søker data......").

    RUN KOrdreToTT IN THIS-PROCEDURE.
    TTH = BUFFER TT_weborder:HANDLE.
    CREATE QUERY qh.
    qh:SET-BUFFERS(TTH).
    qh:QUERY-PREPARE("for each tt_weborder").
    qh:QUERY-OPEN().

  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("tt_weborder","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

  DO:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,6).  /* 3 = antall frozen cols  */
/*       PUBLISH "AlignCol" (LOOKUP("Storrelse",cFelter),8). /* högerjustert */ */
/*       IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN DO:                              */
/*           ASSIGN cSumCols   = getSumFelter(cSummerFelter)                    */
/*                  cSumString = getSumFelter("Lopnr") + ",SUM" .               */
/*           PUBLISH "Summer" (cSumCols,cSumString).                            */
/*       END.                                                                   */
/*       cVisFelterNr = getSumFelter(cVisFelterTxt).                            */
/*       PUBLISH "VisKun" (cVisFelterNr,"SKJUL").                               */
  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort fFrameWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
  RUN ArtikkelKort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport fFrameWin
ON CHOOSE OF B-Rapport IN FRAME fMain /* Rapport */
DO:
    DEFINE VARIABLE pcRappFil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCol AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cData AS LONGCHAR     NO-UNDO.
    ASSIGN iCol = LOOKUP("IdLinjeNr",cFelter).
    RUN getVisibelColValues IN hGrid (iCol,OUTPUT cData).
    RUN orsaksrabatter_reklamasjonsliste.p (cData,OUTPUT pcRappFil).
    IF SEARCH(pcRappFil) <> ? THEN
        RUN browse2pdf\viewxmldialog.w (pcRappFil, 'Polygon Retail Solutions').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Transtyper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Transtyper fFrameWin
ON CHOOSE OF B-Transtyper IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Transtype;TTId;Beskrivelse",
                        "WHERE TTId = 1 or TTId = 3 or TTId = 10",
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TranstyperBlank fFrameWin
ON CHOOSE OF B-TranstyperBlank IN FRAME fMain /* Blank */
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


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans fFrameWin
ON CHOOSE OF B-VisTrans IN FRAME fMain /* Vis transaksjon */
DO:
    DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cYMD         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato        AS CHARACTER  NO-UNDO.

    PUBLISH "FeltVerdier" (OUTPUT cVerdier,cGetTransVerdier,"SAME"). 
    IF cVerdier = "" THEN
        RETURN.
/*     MESSAGE cVerdier                       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    ASSIGN cYMD = ENTRY(3,cVerdier).
    IF SESSION:DATE-FORMAT = "ymd" THEN
        ASSIGN cDato = cYMD.
    ELSE IF SESSION:DATE-FORMAT = "mdy" THEN
        ASSIGN cDato = ENTRY(2,cYMD,"/") + "/" + ENTRY(3,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
    ELSE IF SESSION:DATE-FORMAT = "dmy" THEN
        ASSIGN cDato = ENTRY(3,cYMD,"/") + "/" + ENTRY(2,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
    IF ENTRY(4,cVerdier) <> "0" THEN /* bongnr */
        RUN gviskvittokopi2.w (INT(ENTRY(1,cVerdier)),1,INT(ENTRY(2,cVerdier)),DATE(cDato),INT(ENTRY(4,cVerdier)),THIS-PROCEDURE).
    ELSE /* här skall vi kunna visa kundorderraderna */
        .
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


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId fFrameWin
ON VALUE-CHANGED OF CB-TTId IN FRAME fMain /* TransTypeId */
DO:
  IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN
      APPLY "CHOOSE" TO B-TranstyperBlank.
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
  IF lButikkBruker = TRUE THEN
      RUN ArtBasVisTime.w (THIS-PROCEDURE,artbas.artikkelnr).
  ELSE
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
  DISPLAY FI-Dato FI-DatoTil CB-Typ 
      WITH FRAME fMain.
  ENABLE FI-Dato FI-DatoTil B-Aktiver CB-Typ BUTTON-SokDatoTil BUTTON-SokDato 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecord fFrameWin 
PROCEDURE GetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iButikkNr LIKE BongHode.ButikkNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
    DEFINE OUTPUT PARAMETER dDato     LIKE BongHode.Dato     NO-UNDO.
    DEFINE OUTPUT PARAMETER iBongNr   LIKE BongHode.BongNr   NO-UNDO.
    DEFINE VARIABLE cYMD         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cVerdier,cGetTransVerdier,cRettning). 
        IF cVerdier = "" THEN
            RETURN.
        ASSIGN cYMD = ENTRY(3,cVerdier).
        IF SESSION:DATE-FORMAT = "ymd" THEN
            ASSIGN cDato = cYMD.
        ELSE IF SESSION:DATE-FORMAT = "mdy" THEN
            ASSIGN cDato = ENTRY(2,cYMD,"/") + "/" + ENTRY(3,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
        ELSE IF SESSION:DATE-FORMAT = "dmy" THEN
            ASSIGN cDato = ENTRY(3,cYMD,"/") + "/" + ENTRY(2,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
        ASSIGN iButikkNr = INT(ENTRY(1,cVerdier))
               iGruppeNr = 1
               iKasseNr  = INT(ENTRY(2,cVerdier))
               dDato     = DATE(cDato)
               iBongNr   = INT(ENTRY(4,cVerdier)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo fFrameWin 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  cListItemPairs = cAlle + ",0".

  IF NOT lWSaktiv THEN
      RETURN.
  {syspara.i 150 1 20 cTmp}

  IF cTmp <> "2" THEN
      RETURN.

  {syspara.i 150 17 1 cTmp}

  IF cTmp = "1" THEN /* reservationsweb -> Jarmeus */
      RETURN.

  cListItemPairs = cAlle + ",0,Vanlig weborder,1,Hämta Express,2".

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
        /* Transtype start */
        CB-TTId:LIST-ITEM-PAIRS = " " + cAlle + ",".
        FOR EACH Transtype NO-LOCK:
            IF CAN-DO('1,3,10',STRING(TransType.TTId)) THEN 
                CB-TTId:ADD-LAST(STRING(Transtype.TTId,"zz9") + "   " + 
                           REPLACE(TransType.Beskrivelse,","," "),string(Transtype.TTId)).
        END.
        CB-TTId:SCREEN-VALUE = "1".
        /* Transtype end */

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
  ASSIGN hGrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND Bruker.BrukerType = 1 THEN
      lButikkBruker = FALSE.
  ELSE
      lButikkBruker = TRUE.
  RUN FixStrings.

{syspara.i 1 100 1 cAlle}
IF cAlle = "" THEN
  ASSIGN cAlle = "[Alle]".

{syspara.i 150 1 1 cTmp}

lWSaktiv = cTmp = "1".

  RUN InitCombo.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */
/*   RUN initCombos. */
  RUN InitVerdier.
  DO WITH FRAME {&FRAME-NAME}:
      CB-Typ:LIST-ITEM-PAIRS = cListItemPairs.
      CB-Typ:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cListItemPairs),cListItemPairs).

      ASSIGN FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
      ASSIGN FI-DatoTil:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
      ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridweboanalys.txt"
             FI-TransTyper = "*"
             FI-TransTyper:SCREEN-VALUE = cAlle.
  END.
  RUN viewObject.

  APPLY "VALUE-CHANGED" TO CB-Butik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVerdier fFrameWin 
PROCEDURE InitVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cTransFelter):
        ASSIGN cGetTransVerdier = cGetTransVerdier + (IF cGetTransVerdier = "" THEN "" ELSE ",") + STRING(LOOKUP(ENTRY(iCount,cTransFelter),cFelter)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KOrdreToTT fFrameWin 
PROCEDURE KOrdreToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLevStatusTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntBest AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntBought AS INTEGER     NO-UNDO.
FOR EACH KOrdreHode WHERE KOrdreHode.RegistrertDato >= FI-Dato AND KOrdreHode.RegistrertDato <= FI-DatoTil NO-LOCK.
    IF KOrdreHode.Opphav <> 10 THEN
        NEXT.
    CASE KOrdreHode.LevStatus:
        WHEN "30" THEN cLevStatusTxt = "Ej bekräftad".
        WHEN "45" THEN cLevStatusTxt = "Klar att hämtas".
        WHEN "50" THEN cLevStatusTxt = "Betald".
        WHEN "60" THEN cLevStatusTxt = "Makulerad".
        OTHERWISE cLevStatusTxt = "Okänd".
    END CASE.
    iAntBest = 0.
    FOR EACH KOrdreLinje OF KOrdreHode WHERE KOrdreLinje.PlukkButikk > 0 NO-LOCK.
        iAntBest = iAntBest + 1.
    END.
        iAntBought = 0.
    IF CB-Typ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" AND KOrdreHode.Butik > 0 THEN
        NEXT.
    IF CB-Typ:SCREEN-VALUE = "2" AND KOrdreHode.Butik < 1 THEN
        NEXT.
    IF KOrdreHode.LevStatus = "50" THEN DO:
        FIND FIRST Bonghode WHERE Bonghode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-LOCK NO-ERROR.
        IF NOT AVAIL BongHode THEN
            NEXT.
        FOR EACH Bonglinje WHERE Bonglinje.b_id = bonghode.b_id NO-LOCK.
            IF Bonglinje.TTId = 1 AND Bonglinje.Makulert = FALSE THEN DO:
                iAntBought = iAntBought + Bonglinje.antall.
            END.
        END.
    END.

    CREATE tt_weborder.
    ASSIGN tt_weborder.Dato      = KOrdreHode.RegistrertDato
           tt_weborder.Butikknr  = KOrdreHode.Butik
           tt_weborder.KOrdre_Id = KOrdreHode.KOrdre_Id
           tt_weborder.Bongnr    = IF AVAIL Bonghode THEN Bonghode.bongnr ELSE 0
           tt_weborder.Levstatus = KOrdreHode.LevStatus
           tt_weborder.StatusTxt = cLevStatusTxt
           tt_weborder.Onorder   = iAntBest
           tt_weborder.Bought    = iAntBought
           tt_weborder.Kassenr   = IF AVAIL BongHode THEN Bonghode.kassenr ELSE 0
           tt_weborder.Bongdatum = IF AVAIL BongHode THEN Bonghode.dato ELSE ?
           tt_weborder.KOrdreBelopp = KOrdreHode.Totalt
           tt_weborder.Bongbelopp   = IF AVAIL Bonghode THEN Bonghode.belop ELSE 0.
/*            tt_weborder.Other     = . */
    RELEASE Bonghode.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext fFrameWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.

    ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,cRettning).
        IF cArtikkelNr = "" THEN
          RETURN.
        PUBLISH "ByttArtikkel" (DECI(cArtikkelNr)).
    END.
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
DEFINE        VARIABLE  cTTId          AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF CB-TTId:SCREEN-VALUE = "" THEN
          ASSIGN cTTId = TRIM(TRIM(cAlle,"["),"]").
      ELSE DO iCount = 1 TO NUM-ENTRIES(CB-TTId:SCREEN-VALUE):
          FIND TransType WHERE TransType.TTId = INT(ENTRY(iCount,CB-TTId:SCREEN-VALUE)) NO-LOCK NO-ERROR.
          IF AVAIL TransType THEN
              ASSIGN cTTId = cTTId + (IF cTTId = "" THEN "" ELSE ",") + TransType.Beskrivelse.
      END.
      ASSIGN cFilterVerdier = "Butikk: " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") + CHR(10) +
             "Dato: " + FI-Dato:SCREEN-VALUE + CHR(10) +
             "TransType: " + cTTId
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
  DEFINE OUTPUT PARAMETER ocTTId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN ocButiker = IF fi-Butikker <> "" THEN fi-Butikker ELSE CB-Butik:SCREEN-VALUE
             ocTTId    = IF FI-Transtyper = "*" AND TRIM(CB-TTId:SCREEN-VALUE) = "" THEN "1,3,10" ELSE
                         IF TRIM(CB-TTId:SCREEN-VALUE) <> "" THEN CB-TTId:SCREEN-VALUE ELSE FI-Transtyper.
      ASSIGN pcFeltListe = "Feilkode," + pcFeltListe + (IF fi-Butikker <> "" THEN "," + "Butik" ELSE "") +
                           (IF FI-Transtyper <> "" THEN "," + "TTId" ELSE "")
             pcVerdier   = FILL(CHR(1),NUM-ENTRIES(pcFeltListe) - 1).
      DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "Butik" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = REPLACE(FI-Butikker,"|",",").
            WHEN "TTId" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = REPLACE(FI-TransTyper,"|",",").
        END CASE.
      END.
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
    ASSIGN pcFeltListe = "Dato,Butik,TTId".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "Dato" THEN DO:
                IF INPUT FI-DatoTil = ? OR INPUT FI-DatoTil = INPUT FI-Dato THEN
                  ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                         pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                             FI-Dato:SCREEN-VALUE.
                ELSE DO:
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   FI-Dato:SCREEN-VALUE.
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                  FI-DatoTil:SCREEN-VALUE.
                END.
            END.
            WHEN "Butik" THEN DO:
/*                 IF CB-Butik:SCREEN-VALUE <> "" THEN */
                IF FI-Butikker = "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Butik"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Butik:SCREEN-VALUE.
            END.
            WHEN "TTId" THEN DO:
                IF CB-TTId:SCREEN-VALUE <> "" THEN
                    ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "TTId"
                           pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   CB-TTId:SCREEN-VALUE.
                ELSE IF CB-TTId:SCREEN-VALUE = "" AND FI-Transtyper <> "*" AND NUM-ENTRIES(FI-Transtyper) = 1 THEN
                    ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "TTId"
                           pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   FI-Transtyper.
            END.
        END CASE.
    END.
  END.
/*   RUN SokSdo IN h_dtranslogg (pcFields,     */
/*                               pcValues,     */
/*                               pcSort,       */
/*                               pcOperator,   */
/*                               pcFeltListe). */
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

