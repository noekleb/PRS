&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_fstperiode AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_dstlinje   AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs2 AS CHARACTER  NO-UNDO.

DEFINE VARIABLE tmp_bh         AS HANDLE     NO-UNDO.
/* DEFINE VARIABLE tmp_lLocal     AS LOGICAL    NO-UNDO. */
/* DEFINE VARIABLE tmp_cButiker   AS CHARACTER  NO-UNDO. */



/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi" */
/*        cLabels = "Butikk,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr"                         */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2"                                                                                                                                                                                                           */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* Fält som skall högerjust i XPrint */                                                                                                                                                         */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
"AntSolgt,BruttoSolgt,VerdiSolgt,MvaVerdi,DbKr,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi," +
"SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi".
ASSIGN cFieldDefs = 
        /*  1 */ "DataObjekt;Kampanje;;1," +
        /*  2 */ "Beskrivelse;Beskrivelse;;," +
        /*  3 */ "PerLinTxt;Periode;;," +
        /*  4 */ "AntSolgt;Solgt;3;1," +
        /* 4b */ "BruttoSolgt;Solgt brutto;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto;2;1," +
        /*  6 */ "Solgt%;Solgt%;2;1," +
        /*  7 */ "MvaVerdi;Mva verdi;2;1," +
        /*  8 */ "DbKr;DbKr;2;1," +
        /*  9 */ "Db%;Db%;2;1," +
        /* 10 */ "AntRabatt;Rabatter;;1," +
        /* 11 */ "VerdiRabatt;Rabatt kr;2;1," +
        /* 12 */ "Rab%;Rab%;2;1," +
        /* 13 */ "VVarekost;VVarekost;2;1"
/*         /* 14 */ "ReklAnt;Kunderekl;;1," +         */
/*         /* 15 */ "ReklVerdi;Kunderekl kr;;1," +    */
/*         /* 16 */ "ReklLAnt;Levrekl;;1," +          */
/*         /* 17 */ "ReklLVerdi;Levrekl kr;;1," +     */
/*         /* 18 */ "SvinnAnt;Svinn;;1," +            */
/*         /* 19 */ "SvinnVerdi;Svinn kr;;1," +       */
/*         /* 20 */ "GjenkjopAnt;Gjenkjøp;;1," +      */
/*         /* 21 */ "GjenkjopVerdi;Gjenkjøp kr;;1," + */
/*         /* 22 */ "AntTilbSolgt;Tilbud;;1," +       */
/*         /* 23 */ "VerdiTilbSolgt;Tilbud kr;;1," +  */
/*         /* 24 */ "BrekkAnt;Brekkasje;;1," +        */
/*         /* 25 */ "BrekkVerdi;Brekkasje kr;;1," +   */
/*         /* 26 */ "Adresse1;Adresse1;;," +          */
/*         /* 27 */ "Adresse2;Adresse2;;," +          */
/*         /* 28 */ "Postnr;Postnr;;," +              */
/*         /* 29 */ "PostAdr;Postadr;;," +            */
/*         /* 30 */ "Telefon;Telefon;;," +            */
/*         /* 31 */ "EMail;Email;;"                   */
                 .

/* ASSIGN cFelter = "Aar,AntSolgt,VerdiSolgt,MvaVerdi,DbKr,Db%,AntRabatt,AntTilbSolgt,Beskrivelse,BrekkAnt,BrekkVerdi,BrukerID,Butik,DataObjekt,Diverse,DiverseAnt,Diverseverdi,EDato,ETid,GjenkjopAnt,GjenkjopVerdi,Hg,IntAnt,IntVerdi,JustAnt,JustVerdi,KjopAnt,KjopVerdi,LagerAnt,LagerVerdi,NedAnt,NedVerdi,OmlHast,OvAnt,OvVerdi,PerId,PerLinNr,PerLinTxt,PrimoAnt,Primoverdi,RegistrertAv,RegistrertDato,RegistrertTid,ReklAnt,ReklLAnt,ReklLVerdi,ReklVerdi,StTypeId,SvinnAnt,SvinnVerdi,TilbMvaVerdi,TilbVVarekost,TotalPost,Utsolgt%,VerdiRabatt,VerdiTilbSolgt,VisBut,VVarekost"                                                                                                                                                                  */
/*        cLabels = "År,Antall solgt,Verdi solgt,Mva verdi,DbKr,Db%,Antall rabatt,Antall solgt på tilbud,Beskrivelse,Brekkasje,Verdi av brekasje,Bruker,Butikknummer,Dataobjekt,Diverse,,,Endret,Endret tid,Gjenkjøp fra kunde,Verdi av gjenkjøpte varer,,Internt forbruk,Verdi av internt forbruk,Justert antall,Justert verdi,Innkjopt antall,Verdi kjøpt,,,Nedskrevet antall,Verdi nedskrevet,,Overført antall,Verdi av overførte varer,PeriodeId,PeriodeLinje,,,,Registrert av,Registrert dato,Registreringstidspunkt,Kundereklamasjoner,Rekl.lev.antall,Verdi av leveerandørreklamasjoner,Verdi kundereklamasjoner,Statistikktype,Antall svinn,Svinn verdi,Tilb Mva verdi,Varekost tilbudssalg,,,Verdi rabatt,Verdi solgt på tilbud,,Vektet varekost". */

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StLinje Kunde

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH StLinje NO-LOCK, ~
      EACH Kunde WHERE TRUE /* Join to StLinje incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH StLinje NO-LOCK, ~
      EACH Kunde WHERE TRUE /* Join to StLinje incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain StLinje Kunde
&Scoped-define FIRST-TABLE-IN-QUERY-fMain StLinje
&Scoped-define SECOND-TABLE-IN-QUERY-fMain Kunde


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Tg-VisPeriode B-Tilbudstype RS-Kampanjetype ~
B-Aktiver B-KampanjeBlank B-TilbudstypeBlank Tg-VisPerBut TG-AvFilter ~
B-Kampanje B-KampEier 
&Scoped-Define DISPLAYED-OBJECTS Tg-VisPeriode FI-KampEier RS-Kampanjetype ~
FI-Kampanje FI-Tilbudstype Tg-VisPerBut TG-AvFilter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

DEFINE BUTTON B-Kampanje  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KampanjeBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KampEier  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Tilbudstype  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-TilbudstypeBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE VARIABLE FI-Kampanje AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kampanje" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KampEier AS CHARACTER FORMAT "X(20)":U 
     LABEL "Kampanjeeier" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tilbudstype AS CHARACTER FORMAT "X(10)":U 
     LABEL "Tilbudstype" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Kampanjetype AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Kampanje", 1,
"Tilbud", 2,
"Artikel", 3
     SIZE 21 BY 3 NO-UNDO.

DEFINE VARIABLE TG-AvFilter AS LOGICAL INITIAL no 
     LABEL "Avansert filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE Tg-VisPerBut AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Tg-VisPeriode AS LOGICAL INITIAL no 
     LABEL "Vis periodelinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      StLinje, 
      Kunde SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Tg-VisPeriode AT ROW 1.19 COL 1
     B-Tilbudstype AT ROW 3.19 COL 78.8 NO-TAB-STOP 
     FI-KampEier AT ROW 1.19 COL 62.2 COLON-ALIGNED
     RS-Kampanjetype AT ROW 1.48 COL 26 NO-LABEL
     B-Aktiver AT ROW 2.19 COL 1
     FI-Kampanje AT ROW 2.19 COL 62.2 COLON-ALIGNED
     B-KampanjeBlank AT ROW 2.19 COL 83.8
     FI-Tilbudstype AT ROW 3.19 COL 62.2 COLON-ALIGNED
     B-TilbudstypeBlank AT ROW 3.19 COL 83.8
     Tg-VisPerBut AT ROW 4.91 COL 1
     TG-AvFilter AT ROW 5.86 COL 1
     B-Kampanje AT ROW 2.19 COL 78.8 NO-TAB-STOP 
     B-KampEier AT ROW 1.19 COL 97 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.6 BY 5.71.


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
         HEIGHT             = 5.67
         WIDTH              = 148.6.
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

/* SETTINGS FOR FILL-IN FI-Kampanje IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KampEier IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tilbudstype IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.StLinje,SkoTex.Kunde WHERE skotex.StLinje ..."
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExtraLabel AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iFrozen AS INTEGER    NO-UNDO.
  IF NOT DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
     OUTPUT cKriterier /* CHARACTER */) THEN
      RETURN.
  IF TG-AvFilter:CHECKED AND NOT VALID-HANDLE(tmp_bh) THEN DO:
      RUN Avancerat.
      RETURN NO-APPLY.
  END.
  PUBLISH "VisTxtBox" ("Søker data......").
  RUN StartSok (ENTRY(2,cKriterier,CHR(1))).

  RUN StLinjeToTTKampanje IN h_dstlinje
    ( OUTPUT TTH,cStTypeId,ENTRY(1,cKriterier,CHR(1)),pcFeltListe + ";" + pcVerdier,IF Tg-VisPerBut:CHECKED THEN CHR(1) + "J" ELSE "",Tg-VisPeriode:CHECKED,FI-Kampanje,FI-Tilbudstype,tmp_bh).
  cExtraLabel = "".
  tmp_bh = ?.
  IF RS-Kampanjetype:SCREEN-VALUE <> "1" THEN
      ASSIGN cExtraLabel = IF RS-Kampanjetype:SCREEN-VALUE = "2" THEN ",KampTilbid;Kamptilbud;;,Kamptilbbeskr;Beskr;;,Kamptypebeskr;Type;;" ELSE 
                            ",KampTilbid;Kamptilbud;;,Kamptilbbeskr;Beskr;;,Kamptypebeskr;Type;;,Artikkelnr;Artikkelnr;;,ArtBeskr;Beskr;;".
  IF Tg-VisPerBut:CHECKED THEN
      cExtraLabel = cExtraLabel + ",Butik;Butikk;;,Butnamn;Navn;;".
  IF cExtralabel <> "" THEN DO:
        ASSIGN cTmpFieldDefs       = cFieldDefs
           ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + cExtralabel.
    RUN FixStrings.
    ASSIGN cFieldDefs = cTmpFieldDefs.
  END.
  ELSE
      RUN FixStrings.
  CREATE QUERY qh.
  qh:SET-BUFFERS(TTH).
  qh:QUERY-PREPARE("for each TT_StLinje").
  qh:QUERY-OPEN().
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("TT_StLinje",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dstlinje),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
  PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
  iFrozen = 3.
  iFrozen = iFrozen + IF Tg-VisPerBut:CHECKED THEN 1 ELSE 0.
  iFrozen = iFrozen + IF RS-Kampanjetype:SCREEN-VALUE = "2" THEN 3 ELSE 0.
  iFrozen = iFrozen + IF RS-Kampanjetype:SCREEN-VALUE = "3" THEN 5 ELSE 0.
  PUBLISH "LoadGrid" (cFileName,iFrozen).  /* 3 = antall frozen cols  */
  /* getSumFelter ger colnr för resp fält */
  ASSIGN cSumCols = getSumFelter(cSummerFelter)
         /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
         cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                   + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt")
         /* Col för SummaRadTxt, SUM = txt  */
         cSumString = getSumFelter("PerLinTxt") + ",SUM" .
  /* nästa rad måste stå före 'Summer' */
  PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).
  PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).
  qh:QUERY-CLOSE().
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  DELETE OBJECT TTH NO-ERROR.
  DELETE OBJECT qh NO-ERROR.
  ASSIGN TTH = ?
         qh  = ?.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kampanje fFrameWin
ON CHOOSE OF B-Kampanje IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "KampanjeMixMatch;KampId;KampNavn",
                        "WHERE KampanjeMixMatch.KampEierId = " + TRIM(ENTRY(1,FI-KampEier,":")),
                        INPUT-OUTPUT cRowIdList,
                        "KampId",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Kampanje:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Kampanje     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Kampanje:TOOLTIP = IF FI-Kampanje = "*" THEN "" ELSE FI-Kampanje.
        IF FI-Kampanje <> "*" THEN DO:
/*             APPLY "CHOOSE" TO B-AvdelingBlank. */
/*             APPLY "CHOOSE" TO B-HgBlank.       */
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Kampanje:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Kampanje:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KampanjeBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KampanjeBlank fFrameWin
ON CHOOSE OF B-KampanjeBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Kampanje:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Kampanje:SCREEN-VALUE = cAlle
               FI-Kampanje              = "*"
               FI-Kampanje:TOOLTIP      = ""
               FI-Kampanje:BGCOLOR      = ?
               B-Kampanje:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KampEier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KampEier fFrameWin
ON CHOOSE OF B-KampEier IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK          AS LOGICAL    NO-UNDO.
    DEF    VAR      cLookupValue AS CHAR NO-UNDO.

/*     IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN          */
/*         ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))  */
/*                cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)). */
/*                                                                */

    /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
    /*          Param2: <Where sats> m/Join                                              */
    /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
    /* Kalkulerte felt kan også benyttes, label, format o.l..       */
    cLookupValue = "KampEierId,KampEierNavn".
    RUN JBoxDLookup.w ("KampanjeEier;KampEierId;KampEierNavn","where true",INPUT-OUTPUT cLookupValue).
    IF cLookupValue = "" THEN
        RETURN.
    FI-KampEier:SCREEN-VALUE = REPLACE(cLookupValue,"|"," : ").
    FI-KampEier = FI-KampEier:SCREEN-VALUE.
/*     RUN JBoxDSelector.w (THIS-PROCEDURE,0,                                    */
/*                         "SaSong;Sasong;SasBeskr",                             */
/*                         "WHERE TRUE",                                         */
/*                         INPUT-OUTPUT cRowIdList,                              */
/*                         "Sasong",                                             */
/*                         INPUT-OUTPUT cIdList,                                 */
/*                         "","",                                                */
/*                         OUTPUT bOK).                                          */
/*     IF bOK THEN DO:                                                           */
/*         assign                                                                */
/*           FI-KampEier:SCREEN-VALUE = if cIdList = ""                            */
/*                             then cAlle                                        */
/*                           else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )" */
/*           FI-KampEier     = if cIdList = ""                                     */
/*                             then "*"                                          */
/*                             else REPLACE(cIdList,"|",",")                     */
/*           FI-KampEier:TOOLTIP = IF FI-KampEier = "*" THEN "" ELSE FI-KampEier.      */
/*         IF FI-KampEier <> "*" THEN                                              */
/*             ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList          */
/*                FI-KampEier:BGCOLOR      = 11.                                   */
/*         ELSE                                                                  */
/*             ASSIGN SELF:PRIVATE-DATA = ""                                     */
/*                FI-KampEier:BGCOLOR      = ?.                                    */
/*      END.                                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tilbudstype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tilbudstype fFrameWin
ON CHOOSE OF B-Tilbudstype IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "KampanjeTilbType;KampTilbTypeId;KampTilbTypeNavn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "KampTilbTypeId",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Tilbudstype:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Tilbudstype     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Tilbudstype:TOOLTIP = IF FI-Tilbudstype = "*" THEN "" ELSE FI-Tilbudstype.
        IF FI-Tilbudstype <> "*" THEN DO:
/*             APPLY "CHOOSE" TO B-AvdelingBlank. */
/*             APPLY "CHOOSE" TO B-HgBlank.       */
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Tilbudstype:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Tilbudstype:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TilbudstypeBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TilbudstypeBlank fFrameWin
ON CHOOSE OF B-TilbudstypeBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Tilbudstype:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Tilbudstype:SCREEN-VALUE = cAlle
               FI-Tilbudstype              = "*"
               FI-Tilbudstype:TOOLTIP      = ""
               FI-Tilbudstype:BGCOLOR      = ?
               B-Tilbudstype:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Kampanjetype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Kampanjetype fFrameWin
ON VALUE-CHANGED OF RS-Kampanjetype IN FRAME fMain
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "1" THEN
            cSttypeid = "KAMPANJE".
        WHEN "2" THEN
            cSttypeid = "KAMPTILB".
        WHEN "3" THEN
            cSttypeid = "KAMPART".
    END CASE.
    TG-AvFilter:SENSITIVE = SELF:SCREEN-VALUE = "3".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AvFilter fFrameWin
ON VALUE-CHANGED OF TG-AvFilter IN FRAME fMain /* Avansert filter */
DO:
  ASSIGN tmp_bh = ?.
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
  DISPLAY Tg-VisPeriode FI-KampEier RS-Kampanjetype FI-Kampanje FI-Tilbudstype 
          Tg-VisPerBut TG-AvFilter 
      WITH FRAME fMain.
  ENABLE Tg-VisPeriode B-Tilbudstype RS-Kampanjetype B-Aktiver B-KampanjeBlank 
         B-TilbudstypeBlank Tg-VisPerBut TG-AvFilter B-Kampanje B-KampEier 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
  RUN FixStrings.
  RUN SUPER.
  APPLY "VALUE-CHANGED" TO RS-KampanjeType IN FRAME {&FRAME-NAME}.
  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN
      ASSIGN h_fstperiode = DYNAMIC-FUNCTION('geth_fstperiode':U IN h_Window)
             h_dstlinje   = DYNAMIC-FUNCTION('geth_dstlinje':U IN h_Window).

  FIND FIRST KampanjeEier NO-LOCK NO-ERROR.
  IF AVAIL KampanjeEier THEN DO:
      FI-KampEier:SCREEN-VALUE = STRING(KampanjeEier.KampEierId) + " : " + KampanjeEier.KampEierNavn.
      FI-KampEier = FI-KampEier:SCREEN-VALUE.
  END.
  ELSE DO:
      FI-KampEier:SCREEN-VALUE =  "0 : --".
      FI-KampEier = FI-KampEier:SCREEN-VALUE.
  END.
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt".
      APPLY "CHOOSE" TO B-KampanjeBlank.
      APPLY "CHOOSE" TO B-TilbudstypeBlank.
      APPLY "VALUE-CHANGED" TO TG-Avfilter.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFeltInfo fFrameWin 
PROCEDURE SendFeltInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cField#List AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cColAlign   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
  ASSIGN cField#List = getSumFelter(cFeltListe).
         cColAlign   = FILL(",",NUM-ENTRIES(cField#List) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cField#List):
      IF ENTRY(INT(ENTRY(iCount,cField#List)),cRightCols) = "1" THEN 
          ASSIGN ENTRY(iCount,cColAlign) = "1".
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
DEFINE        VARIABLE cFraAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilAar         AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cFraPerLinNr    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE cTilPerLinNr    AS CHARACTER  NO-UNDO.

  DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
       OUTPUT cFstPeriode /* CHARACTER */).
/*   ASSIGN cButikker   = "Butikker: " + ENTRY(1,cFstPeriode,CHR(1)) */
/*          cPeriodeTmp = ENTRY(2,cFstPeriode,CHR(1)).               */
  RELEASE Butiker.
  IF NUM-ENTRIES(ENTRY(1,cFstPeriode,CHR(1))) = 1 THEN
      FIND Butiker WHERE Butiker.Butik = INT(ENTRY(1,cFstPeriode,CHR(1))) NO-LOCK NO-ERROR.
  ASSIGN cButikker   = (IF AVAIL Butiker THEN "Butikk: " ELSE "Butikker: ") + IF AVAIL Butiker THEN Butiker.Butnamn ELSE
                                         ENTRY(1,cFstPeriode,CHR(1))
         cPeriodeTmp = ENTRY(2,cFstPeriode,CHR(1)).
  CASE ENTRY(1,cPeriodeTmp):
      WHEN "AAR" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                            ENTRY(2,cPeriodeTmp) + ":" + ENTRY(4,cPeriodeTmp) + "-" +
                            ENTRY(3,cPeriodeTmp) + ":" + ENTRY(5,cPeriodeTmp).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                             ENTRY(2,cPeriodeTmp) + "-" +
                             ENTRY(3,cPeriodeTmp).
      END.
      OTHERWISE DO:
          ASSIGN cFilterVerdier = "Periodetype: " + ENTRY(1,cPeriodeTmp) + CHR(10) +
                               ENTRY(2,cPeriodeTmp) + "-" +
                               ENTRY(3,cPeriodeTmp).
      END.
  END CASE.
  ASSIGN cFilterVerdier = cButikker + CHR(10) + cFilterVerdier
         cColAlign = cRightCols.
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
  DEFINE INPUT  PARAMETER ipKriterier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  CASE ENTRY(1,ipKriterier):
      WHEN "AAR" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "MANED" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "UKE" THEN DO:
          ASSIGN cFraAar      = ENTRY(2,ipKriterier)
                 cTilAar      = ENTRY(3,ipKriterier)
                 cFraPerLinNr = ENTRY(4,ipKriterier)
                 cTilPerLinNr = ENTRY(5,ipKriterier).
      END.
      WHEN "DAG" THEN DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,ipKriterier)))).
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,ipKriterier)))).
                 cFraPerLinNr = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1)).
                 cTilPerLinNr = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1)).
      END.
      OTHERWISE DO:
          ASSIGN cFraAar      = STRING(YEAR(DATE(ENTRY(2,ipKriterier))))
                 cTilAar      = STRING(YEAR(DATE(ENTRY(3,ipKriterier))))
                 cFraPerLinNr = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1))
                 cTilPerLinNr = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1)).
      END.
  END CASE.
  ASSIGN cQryString = 
      "FOR EACH StLinje WHERE SUBSTBUTIK AND StTypeId = '&1' AND PerId = '&2' AND AarPerLinNr >= &3 AND AarPerLinNr <= &4 "
         cQryString = cQryString + IF FI-Kampanje = "*" THEN "" ELSE "and Dataobjekt "
      + (IF RS-Kampanjetype:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN "= 'SUBSTKAMPANJE' no-lock" ELSE "begins 'SUBSTKAMPANJE' no-lock") + " use-index AarPerLinNr "
      cFraAarPer = cFraAar + STRING(INT(cFraPerLinNr),"999")
      cTilAarPer = cTilAar + STRING(INT(cTilPerLinNr),"999")
      cQryString = SUBSTITUTE(cQryString,cStTypeId,ENTRY(1,ipKriterier),cFraAarPer,cTilAarPer).
  DYNAMIC-FUNCTION('setQueryString':U IN h_dstlinje,
     INPUT cQryString /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Startsokartdyn fFrameWin 
PROCEDURE Startsokartdyn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER qh         AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal     AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.

  IF VALID-HANDLE(qh) THEN
      tmp_bh = qh:GET-BUFFER-HANDLE(1).
  IF tmp_bh:FIND-FIRST("where artikkelnr > 0") THEN
      APPLY "CHOOSE" TO B-Aktiver IN FRAME {&FRAME-NAME}.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

