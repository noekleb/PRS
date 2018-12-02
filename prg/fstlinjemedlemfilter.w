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
DEFINE VARIABLE cVisFelterTxt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterNr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hGrid AS HANDLE      NO-UNDO.
DEF VAR hTTMedlem  AS HANDLE NO-UNDO.
/* DEFINE TEMP-TABLE TT_Medlem NO-UNDO */
/*     FIELD Feltverdier AS CHAR       */
/*     FIELD Rad1 AS CHAR              */
/*     FIELD Rad2 AS CHAR              */
/*     FIELD Rad3 AS CHAR              */
/*     FIELD Rad4 AS CHAR              */
/*     INDEX Rad1 IS PRIMARY Rad1.     */
                
/* ASSIGN cFelter = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,VerdiSolgt,Solgt%,MvaVerdi,DbKr,Db%,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi" */
/*        cLabels = "Butikk,Beskrivelse,Periode,Solgt,Verdi solgt,Solgt%,Mva verdi,DbKr,Db%,Rabatter,Rabatt kr,VVarekost,Kunderekl,Kunderekl kr,Levrekl,Levrekl kr,Svinn,Svinn kr,Gjenkjøp,Gjenkjøp kr,Tilbud,Tilbud kr,Brekkasje,Brekkasje kr"                         */
/*        cDecimaler = ",,,,2,1,2,2,1,,2,2,,2,,2,,2,,2,,2,,2"                                                                                                                                                                                                           */
/*        cRightCols = "1,,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* Fält som skall högerjust i XPrint */                                                                                                                                                         */
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
"AntSolgt,BruttoSolgt,VerdiSolgt,MvaVerdi,DbKr,AntRabatt,VerdiRabatt,VVarekost,ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi," +
"SvinnAnt,SvinnVerdi,GjenkjopAnt,GjenkjopVerdi,AntTilbSolgt,VerdiTilbSolgt,BrekkAnt,BrekkVerdi".
ASSIGN cFieldDefs = 
        /*  1 */ "DataObjekt;Medlem;;1," +
        /*  2 */ "Beskrivelse;Beskrivelse;;," +
        /*  3 */ "PerLinTxt;Periode;;," +
        /*  4 */ "AntSolgt;Solgt;3;1," +
        /* 4b */ "BruttoSolgt;Solgt brutto;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto;2;1," +
        /*  6 */ "Solgt%;Solgt%;2;1," +
        /*  7 */ "MvaVerdi;Mva verdi;2;1," +
        /*  8 */ "DbKr;DbKr;2;1," +
        /*  9 */ "Db%;Db%;1;1," +
        /* 10 */ "AntRabatt;Rabatter;;1," +
        /* 11 */ "VerdiRabatt;Rabatt kr;2;1," +
        /* 12 */ "Rab%;Rab%;2;1," +
        /* 13 */ "VVarekost;VVarekost;2;1," +
        /* 14 */ "ReklAnt;Kunderekl;3;1," +
        /* 15 */ "ReklVerdi;Kunderekl kr;2;1," +
        /* 16 */ "ReklLAnt;Levrekl;3;1," +
        /* 17 */ "ReklLVerdi;Levrekl kr;2;1," +
        /* 18 */ "SvinnAnt;Svinn;;1," +
        /* 19 */ "SvinnVerdi;Svinn kr;;1," +
        /* 20 */ "GjenkjopAnt;Returer;3;1," +
        /* 21 */ "GjenkjopVerdi;Returer kr;2;1," +
        /* 22 */ "AntTilbSolgt;Tilbud;;1," +
        /* 23 */ "VerdiTilbSolgt;Tilbud kr;2;1," +
        /* 24 */ "BrekkAnt;Brekkasje;;1," +
        /* 25 */ "BrekkVerdi;Brekkasje kr;;1," +
        /* 26 */ "Fornavn;Fornavn;;," +
        /* 27 */ "Etternavn;Etternavn;;," +
        /* 28 */ "Adresse1;Adresse1;;," +
        /* 29 */ "Adresse2;Adresse2;;," +
        /* 30 */ "Postnr;Postnr;;," +
        /* 31 */ "PostAdr;Postadr;;," +
        /* 32 */ "Telefon;Telefon;;," +
        /* 33 */ "MobilTlf;Mobil;;," +
        /* 34 */ "EMail;Email;;,"      +
        /* 35 */ "Kilde;Kilde;;,"      +
        /* 36 */ "TilgKilde;TilgKilde;;," +
        /* 37 */ "MedGruppe;MedGruppe;;," +
        /* 38 */ "MedType;MedType;;," + 
        /* 39 */ "KundeNr;KundeNr;;," +
        /* 40 */ "MKlubbId;Medelmsklubb;;"
                 .

/* ASSIGN cFelter = "Aar,AntSolgt,VerdiSolgt,MvaVerdi,DbKr,Db%,AntRabatt,AntTilbSolgt,Beskrivelse,BrekkAnt,BrekkVerdi,BrukerID,Butik,DataObjekt,Diverse,DiverseAnt,Diverseverdi,EDato,ETid,GjenkjopAnt,GjenkjopVerdi,Hg,IntAnt,IntVerdi,JustAnt,JustVerdi,KjopAnt,KjopVerdi,LagerAnt,LagerVerdi,NedAnt,NedVerdi,OmlHast,OvAnt,OvVerdi,PerId,PerLinNr,PerLinTxt,PrimoAnt,Primoverdi,RegistrertAv,RegistrertDato,RegistrertTid,ReklAnt,ReklLAnt,ReklLVerdi,ReklVerdi,StTypeId,SvinnAnt,SvinnVerdi,TilbMvaVerdi,TilbVVarekost,TotalPost,Utsolgt%,VerdiRabatt,VerdiTilbSolgt,VisBut,VVarekost"                                                                                                                                                                  */
/*        cLabels = "År,Antall solgt,Verdi solgt,Mva verdi,DbKr,Db%,Antall rabatt,Antall solgt på tilbud,Beskrivelse,Brekkasje,Verdi av brekasje,Bruker,Butikknummer,Dataobjekt,Diverse,,,Endret,Endret tid,Gjenkjøp fra kunde,Verdi av gjenkjøpte varer,,Internt forbruk,Verdi av internt forbruk,Justert antall,Justert verdi,Innkjopt antall,Verdi kjøpt,,,Nedskrevet antall,Verdi nedskrevet,,Overført antall,Verdi av overførte varer,PeriodeId,PeriodeLinje,,,,Registrert av,Registrert dato,Registreringstidspunkt,Kundereklamasjoner,Rekl.lev.antall,Verdi av leveerandørreklamasjoner,Verdi kundereklamasjoner,Statistikktype,Antall svinn,Svinn verdi,Tilb Mva verdi,Varekost tilbudssalg,,,Verdi rabatt,Verdi solgt på tilbud,,Vektet varekost". */

DEFINE TEMP-TABLE tt_Dataobjekt NO-UNDO
    FIELD Dataobjekt AS CHAR.

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
&Scoped-Define ENABLED-OBJECTS Tg-VisPeriode CB-Medlemsklubb B-AdressSpar ~
B-MedlemsNr B-Aktiver B-MedlemsNrBlank B-Medlemskort FI-Kilde B-KildeBlank ~
FI-TilgKilde B-TilgKildeBlank B-Etiketter FI-OmsFra FI-OmsTil Tg-VisPerBut 
&Scoped-Define DISPLAYED-OBJECTS Tg-VisPeriode CB-Medlemsklubb FI-MedlemsNr ~
FI-Kilde FI-TilgKilde FI-OmsFra FI-OmsTil Tg-VisPerBut 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InputOk fFrameWin 
FUNCTION InputOk RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AdressSpar 
     LABEL "Adressuppdatering" 
     SIZE 27 BY 1.14.

DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Etiketter 
     LABEL "Etiketter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-KildeBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Medlemskort 
     LABEL "Medlemskort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-MedlemsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-MedlemsNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-TilgKildeBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE VARIABLE CB-Medlemsklubb AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Medlemsklubb" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 35.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kilde AS CHARACTER FORMAT "X(30)" 
     LABEL "Kilde" 
     VIEW-AS FILL-IN 
     SIZE 34.2 BY 1.

DEFINE VARIABLE FI-MedlemsNr AS CHARACTER FORMAT "X(256)" 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OmsFra AS DECIMAL FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Brutto omsettn. fra/til" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OmsTil AS DECIMAL FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilgKilde AS CHARACTER FORMAT "X(30)" 
     LABEL "Tilg.kilde" 
     VIEW-AS FILL-IN 
     SIZE 34.2 BY 1.

DEFINE VARIABLE Tg-VisPerBut AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Tg-VisPeriode AS LOGICAL INITIAL no 
     LABEL "Vis periodelinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Tg-VisPeriode AT ROW 1.19 COL 1
     CB-Medlemsklubb AT ROW 1.38 COL 38.8 COLON-ALIGNED
     B-AdressSpar AT ROW 1.48 COL 88
     B-MedlemsNr AT ROW 2.52 COL 55.8 NO-TAB-STOP 
     B-Aktiver AT ROW 2.19 COL 1
     FI-MedlemsNr AT ROW 2.52 COL 38.8 COLON-ALIGNED HELP
          "Medlemsnummer"
     B-MedlemsNrBlank AT ROW 2.52 COL 60.8
     B-Medlemskort AT ROW 3.52 COL 1
     FI-Kilde AT ROW 3.52 COL 38.8 COLON-ALIGNED HELP
          "Hvor kommer kunden fra."
     B-KildeBlank AT ROW 3.52 COL 75.2
     FI-TilgKilde AT ROW 4.52 COL 38.8 COLON-ALIGNED HELP
          "Hvilken tilknyttning har kunden."
     B-TilgKildeBlank AT ROW 4.52 COL 75.2
     B-Etiketter AT ROW 4.86 COL 1
     FI-OmsFra AT ROW 5.52 COL 38.8 COLON-ALIGNED
     FI-OmsTil AT ROW 5.52 COL 56.6 COLON-ALIGNED NO-LABEL
     Tg-VisPerBut AT ROW 6.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.4 BY 6.67.


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
         HEIGHT             = 6.67
         WIDTH              = 116.4.
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-MedlemsNr IN FRAME fMain
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

&Scoped-define SELF-NAME B-AdressSpar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AdressSpar fFrameWin
ON CHOOSE OF B-AdressSpar IN FRAME fMain /* Adressuppdatering */
DO:
    DEFINE VARIABLE pcRappFil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCol AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cData AS LONGCHAR     NO-UNDO.
    ASSIGN iCol = LOOKUP("DataObjekt",cFelter).
    RUN getVisibelColValues IN hGrid (iCol,OUTPUT cData).
    IF cData <> "" THEN DO:
        RUN d-runUpdateSpar.w (cData).
/*         MESSAGE "Det är valt " + STRING(NUM-ENTRIES(cData)) + " medlemmar för adressuppdatering" SKIP */
/*                 "mot SPAR. Kontrollera ditt urval och tryck 'Ja' för uppdatering." SKIP               */
/*                 "OBS! Beroende på antalet så kan uppdateringen ta lång tid."                          */
/*             VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lSvar AS LOG.                                */
/*         IF lSvar THEN DO:                                                                             */
/*            {sww.i}                                                                                    */
/*             RUN BatchUpdateMedlemSPAR.p (cData) NO-ERROR.                                             */
/*            {swn.i}                                                                                    */
/*             MESSAGE "Uppdatering klar"                                                                */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
/*         END.                                                                                          */
    END.
    ELSE DO:
        MESSAGE "Ingen medlem är vald för adressuppdatering."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DEFINE VARIABLE cExtraFelt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilleggsFelter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  ASSIGN INPUT CB-Medlemsklubb.
  IF NOT DYNAMIC-FUNCTION('getKriterier':U IN h_fstperiode,
     OUTPUT cKriterier /* CHARACTER */) THEN
      RETURN.
  ASSIGN INPUT FI-OmsFra
         INPUT FI-OmsTil.
  ASSIGN FI-Kilde
         FI-TilgKilde.
  IF NOT InputOk() THEN DO:
      APPLY "ENTRY" TO FI-OmsFra.
      RETURN NO-APPLY.
  END.
  IF FI-MedlemsNr <> "*" or FI-Kilde <> "" or FI-TilgKilde <> "" then do:
      ASSIGN pcFeltListe = "MedlemsNr"
             pcVerdier   = FI-MedlemsNr.
    IF FI-Kilde <> "" THEN
        ASSIGN pcFeltListe = pcFeltListe + "," + "Kilde"
               pcVerdier   = pcVerdier   + CHR(1) + FI-Kilde.
    IF FI-TilgKilde <> "" THEN
        ASSIGN pcFeltListe = pcFeltListe + "," + "TilgKilde"
               pcVerdier   = pcVerdier   + CHR(1) + FI-TilgKilde.
  END.
  IF CB-Medlemsklubb <> -1 THEN
      ASSIGN pcFeltListe = pcFeltListe + "," + "Medlemsklubb"
             pcVerdier   = pcVerdier   + CHR(1) + STRING(CB-Medlemsklubb).

/*   ASSIGN pcFeltListe = "MedlemsNr" */
/*          pcVerdier   = FI-MedlemsNr. */
  IF FI-OmsFra > 0 OR FI-OmsTil > 0 THEN DO:
      IF pcFeltListe = "" THEN
          ASSIGN pcFeltListe = "MedlemsNr"
                 pcVerdier   = FI-MedlemsNr.
      ASSIGN pcVerdier = pcVerdier + CHR(2) + STRING(FI-OmsFra) + "," + STRING(FI-OmsTil).
  END.
  cTilleggsFelter = "Butik,Butnamn".
  PUBLISH "VisTxtBox" ("Søker data......").
  RUN StartSok (ENTRY(2,cKriterier,CHR(1))).
  RUN StLinjeToTT IN h_dstlinje
    ( OUTPUT TTH,cStTypeId,ENTRY(1,cKriterier,CHR(1)),pcFeltListe + ";" + pcVerdier,IF Tg-VisPerBut:CHECKED THEN CHR(1) + "J" ELSE "",Tg-VisPeriode:CHECKED).
IF Tg-VisPerBut:CHECKED THEN DO:
  ASSIGN cTmpFieldDefs       = cFieldDefs
         ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
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
  PUBLISH "LoadGrid" (cFileName,IF Tg-VisPerBut:CHECKED THEN 4 ELSE 3).  /* 3 = antall frozen cols  */
  /* getSumFelter ger colnr för resp fält */
  ASSIGN cSumCols = getSumFelter(cSummerFelter)
         /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
         cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                   + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt")
         /* Col för SummaRadTxt, SUM = txt  */
         cSumString = getSumFelter("PerLinTxt") + ",SUM" .
  IF cVisFelterTxt <> "" THEN DO:
      cExtrafelt = "".
      DO ii = 1 TO NUM-ENTRIES(cTilleggsFelter):
          IF CAN-DO(cFelter,ENTRY(ii,cTilleggsFelter)) THEN
              cExtraFelt = cExtraFelt + "," + ENTRY(ii,cTilleggsFelter).
      END.
      cVisFelterNr = getSumFelter(cVisFelterTxt + cExtraFelt).
  END.
  /* nästa rad måste stå före 'Summer' */
  PUBLISH "X%Solgt" ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")).
  PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).
  IF cVisFelterNr <> "" THEN
      PUBLISH "VisKun" (cVisFelterNr,"SKJUL").
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


&Scoped-define SELF-NAME B-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Etiketter fFrameWin
ON CHOOSE OF B-Etiketter IN FRAME fMain /* Etiketter */
DO:
  RUN Etikettliste.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KildeBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KildeBlank fFrameWin
ON CHOOSE OF B-KildeBlank IN FRAME fMain /* Blank */
DO:
    ASSIGN FI-Kilde = "".
           FI-Kilde:SCREEN-VALUE = "".
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Medlemskort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Medlemskort fFrameWin
ON CHOOSE OF B-Medlemskort IN FRAME fMain /* Medlemskort */
DO:
  RUN Medlemskort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MedlemsNr fFrameWin
ON CHOOSE OF B-MedlemsNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    this-procedure:current-window:sensitive = no.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Medlem;MedlemsNr;EtterNavn;ForNavn;Kilde;TilgKilde",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "MedlemsNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    this-procedure:current-window:sensitive = yes.
    IF bOK THEN DO:
        assign
          FI-MedlemsNr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-MedlemsNr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-MedlemsNr:TOOLTIP = IF FI-MedlemsNr = "*" THEN "" ELSE FI-MedlemsNr.
        IF FI-MedlemsNr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-MedlemsNr:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-MedlemsNr:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-MedlemsNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MedlemsNrBlank fFrameWin
ON CHOOSE OF B-MedlemsNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-MedlemsNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-MedlemsNr:SCREEN-VALUE = cAlle
               FI-MedlemsNr              = "*"
               FI-MedlemsNr:TOOLTIP      = ""
               FI-MedlemsNr:BGCOLOR      = ?
               B-MedlemsNr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TilgKildeBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TilgKildeBlank fFrameWin
ON CHOOSE OF B-TilgKildeBlank IN FRAME fMain /* Blank */
DO:
    ASSIGN FI-TilgKilde = "".
           FI-TilgKilde:SCREEN-VALUE = "".
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
  DISPLAY Tg-VisPeriode CB-Medlemsklubb FI-MedlemsNr FI-Kilde FI-TilgKilde 
          FI-OmsFra FI-OmsTil Tg-VisPerBut 
      WITH FRAME fMain.
  ENABLE Tg-VisPeriode CB-Medlemsklubb B-AdressSpar B-MedlemsNr B-Aktiver 
         B-MedlemsNrBlank B-Medlemskort FI-Kilde B-KildeBlank FI-TilgKilde 
         B-TilgKildeBlank B-Etiketter FI-OmsFra FI-OmsTil Tg-VisPerBut 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etikettliste fFrameWin 
PROCEDURE Etikettliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCols        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDataCols      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cMedlemsNr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cRad1       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRad2       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRad3       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRad4       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFV         AS CHARACTER   NO-UNDO.
DEF VAR hQuery              AS HANDLE NO-UNDO.

  ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cMedlemsNr,cGetVerdier,"SAME").
  IF cMedlemsNr = "" THEN
    RETURN.

cDataCols = "DataObjekt,Fornavn,Etternavn,Adresse1,Adresse2,Postnr,PostAdr".
ihBuffer = hTTMedlem:DEFAULT-BUFFER-HANDLE.
hTTMedlem:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

DO ii = 1 TO NUM-ENTRIES(cDataCols):
    cCols = cCols + (IF cCols <> "" THEN "," ELSE "") + getSumFelter(ENTRY(ii,cDataCols)).
END.
PUBLISH  "FillTTFelter" (hTTMedlem:DEFAULT-BUFFER-HANDLE,"feltverdier",cCols).
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(ihBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hTTMedlem:DEFAULT-BUFFER-HANDLE:NAME + " NO-LOCK").
  hQuery:QUERY-OPEN().
  
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      cFV = ihBuffer:BUFFER-FIELD('feltverdier'):BUFFER-VALUE.
      ASSIGN cRad1 = LEFT-TRIM(ENTRY(1," ",CHR(1)),"0")
/*              cRad1 = LEFT-TRIM(ENTRY(1,cFV,CHR(1)),"0") */
             cRad2 = LEFT-TRIM(ENTRY(2,cFV,CHR(1)),"0") + " " + LEFT-TRIM(ENTRY(3,cFV,CHR(1)),"0")
             cRad3 = IF ENTRY(4,cFV,CHR(1)) <> "" THEN ENTRY(4,cFV,CHR(1)) ELSE ENTRY(5,cFV,CHR(1))
             cRad4 = ENTRY(6,cFV,CHR(1)) + " " + ENTRY(7,cFV,CHR(1)).
      ihBuffer:BUFFER-FIELD('Rad1'):BUFFER-VALUE = cRad1.
      ihBuffer:BUFFER-FIELD('Rad2'):BUFFER-VALUE = cRad2.
      ihBuffer:BUFFER-FIELD('Rad3'):BUFFER-VALUE = cRad3.
      ihBuffer:BUFFER-FIELD('Rad4'):BUFFER-VALUE = cRad4.

      hQuery:GET-NEXT().
  END.
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  RUN skriv_A4_etikettPDF.p ("16",ihBuffer).
/* FOR EACH tt_medlem:                */
/*     DISPLAY tt_medlem.feltverdier. */
/* END.                               */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCombo fFrameWin 
PROCEDURE initCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER   NO-UNDO.

    cListItemPairs = cAlle + ",-1".

    FOR EACH MedlemsKlubb NO-LOCK. 
        cListItemPairs = cListItemPairs + "," + MedlemsKlubb.MKlubbBeskrivelse + "," + STRING(MedlemsKlubb.MKlubbId).
    END.
    CB-Medlemsklubb:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.
    CB-Medlemsklubb = -1.
    CB-Medlemsklubb:SCREEN-VALUE = STRING(CB-Medlemsklubb).
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
  DEFINE VARIABLE cSpar AS CHARACTER   NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN FixStrings.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN
      ASSIGN h_fstperiode = DYNAMIC-FUNCTION('geth_fstperiode':U IN h_Window)
             h_dstlinje   = DYNAMIC-FUNCTION('geth_dstlinje':U IN h_Window)
             hGrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  {syspara.i 14 1 8 cSpar}
  {syspara.i 1 100 1 cAlle}
  {syspara.i 220 1 1 cVisFelterTxt}
  B-AdressSpar:HIDDEN IN FRAME {&FRAME-NAME} = cSpar = "".
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombo.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cStTypeId = "MEDLEM"
             cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt"
             FI-MedlemsNr  = "*"
             FI-MedlemsNr:SCREEN-VALUE = cAlle.
  END.
  RUN SkapaTTMedlem.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Medlemskort fFrameWin 
PROCEDURE Medlemskort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cMedlemsNr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cMedlemsNr,cGetVerdier,"SAME").                         
  IF cMedlemsNr = "" THEN
    RETURN.
  FIND Medlem WHERE Medlem.MedlemsNr = DECI(cMedlemsNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL medlem THEN 
      RETURN.
  fLockvindu(TRUE).
  run w-vmedlem.w (input recid(Medlem), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).
  
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
    DEFINE VARIABLE cMedlemsNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.

    ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cMedlemsNr,cGetVerdier,cRettning).      
        IF cMedlemsNr = "" THEN
          RETURN.
        PUBLISH "ByttMedlem" (DECI(cMedlemsNr)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTMedlem fFrameWin 
PROCEDURE SkapaTTMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE TEMP-TABLE hTTMedlem.
    hTTMedlem:ADD-NEW-FIELD("feltverdier","char").
    hTTMedlem:ADD-NEW-FIELD("rad1","char").
    hTTMedlem:ADD-NEW-FIELD("rad2","char").
    hTTMedlem:ADD-NEW-FIELD("rad3","char").
    hTTMedlem:ADD-NEW-FIELD("rad4","char").
    hTTMedlem:ADD-NEW-INDEX("rad1",NO).
    hTTMedlem:ADD-INDEX-FIELD("rad1","rad1").
    hTTMedlem:TEMP-TABLE-PREPARE("hTTMedlem").

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
      "FOR EACH StLinje WHERE SUBSTBUTIK AND StTypeId = '&1' AND PerId = '&2' AND AarPerLinNr >= &3 AND AarPerLinNr <= &4 use-index AarPerLinNr no-lock"
      cFraAarPer = cFraAar + STRING(INT(cFraPerLinNr),"999")
      cTilAarPer = cTilAar + STRING(INT(cTilPerLinNr),"999")
      cQryString = SUBSTITUTE(cQryString,cStTypeId,ENTRY(1,ipKriterier),cFraAarPer,cTilAarPer).
                 
  DYNAMIC-FUNCTION('setQueryString':U IN h_dstlinje,
     INPUT cQryString /* CHARACTER */).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockVindu fFrameWin 
FUNCTION fLockVindu RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InputOk fFrameWin 
FUNCTION InputOk RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF (FI-OmsFra = FI-OmsTil) OR (FI-OmsFra < FI-OmsTil) THEN
          RETURN TRUE.
      ELSE DO:
          MESSAGE "Omsettning fra > omsettning til"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN FALSE.
      END.

  END.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

