&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xvpisputpakk.p
    Purpose     : Utpakking av vareinndeling/grupperinger fra StorePoint.

    Syntax      :

    Description : Fra StorePont leveres en fil som inneholder:
                    1. Artikkelinformasjon
                    2. Avdelinger
                    3. Hovedgrupper
                    4. Varegrupper
                    5. Leverandører
                    6. Produsenter
                  Disse pakkes opp og oppdateres direkte inn i motsvarende
                  tabeller i SkoTex databasen.

    Author(s)   : Tom Nøkleby
    Created     : 1/12-03
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR lLapTop       AS LOG  NO-UNDO.
DEF VAR iProfilNr     AS INT  NO-UNDO.
DEF VAR iAntNye       AS INT  NO-UNDO.
DEF VAR iAntUppdat    AS INT  NO-UNDO.
DEF VAR iAntNyaAvd    AS INT  NO-UNDO.
DEF VAR iAntNyaHg     AS INT  NO-UNDO.
DEF VAR iAntFel       AS INT  NO-UNDO.
DEF VAR bError        AS LOG  NO-UNDO.
DEF VAR cErrorLog     AS CHAR NO-UNDO.
DEF VAR iAntEntries   AS INT  NO-UNDO.

DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.
DEF VAR pcSep         AS CHAR   INITIAL "|" NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}

DEF VAR piAntFeil   AS INT  NO-UNDO.
DEF VAR bProfitBase AS LOG  NO-UNDO.

DEF TEMP-TABLE TT_Avdeling
    FIELD AvdelingNr   LIKE Avdeling.AvdelingNr
    FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
    INDEX AvdelingNr IS PRIMARY UNIQUE AvdelingNr
    .
DEF TEMP-TABLE TT_HuvGr
    FIELD HG      LIKE HuvGr.Hg
    FIELD HgBeskr LIKE HuvGr.HgBeskr
    FIELD AvdelingNr LIKE HuvGr.AvdelingNr
    INDEX Hg IS PRIMARY UNIQUE Hg
    .
DEF TEMP-TABLE TT_VarGr
    FIELD Vg      LIKE VarGr.Vg
    FIELD VgBeskr LIKE VarGr.VgBeskr
    FIELD MomsKod LIKE VarGr.MomsKod
    FIELD Hg      LIKE VarGr.Hg
    FIELD Kost_Proc LIKE VarGr.Kost_Proc
    INDEX Vg IS PRIMARY UNIQUE Vg
    .
DEF TEMP-TABLE TT_LevBas
    FIELD LevNr   LIKE LevBas.LevNr
    FIELD LevNamn LIKE LevBas.LevNamn
    INDEX LevNr IS PRIMARY UNIQUE LevNr
    .
DEF TEMP-TABLE TT_Produsent
    FIELD ProdNr      LIKE Produsent.ProdNr
    FIELD Beskrivelse LIKE Produsent.Beskrivelse
    INDEX ProdNr IS PRIMARY UNIQUE ProdNr
    .
DEF TEMP-TABLE TT_Artikkel
    FIELD ArtikkelNr AS DEC 
    FIELD EAN        AS CHAR  
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD BongTekst  LIKE ArtBas.BongTekst
    FIELD KostPris   AS DEC
    FIELD Pris       AS DEC
    FIELD Mva%       AS DEC 
    FIELD LevNr      LIKE ArtBas.LevNr
    FIELD ProdNr     LIKE ArtBas.ProdNr
    FIELD Hg         LIKE ArtBas.Hg
    FIELD Vg         LIKE ArtBas.Vg
    FIELD OPris      LIKE ArtBas.OPris
    FIELD LevKod     LIKE ArtBas.LevKod
    FIELD SalgsEnhet LIKE ArtBas.SalgsEnhet
    FIELD LinkVareNr LIKE ArtBas.LinkVareNr
    INDEX Artikkel IS PRIMARY ArtikkelNr
    .

DEF TEMP-TABLE TT_Strekkode
    FIELD ArtikkelNr AS DEC
    FIELD EAN        AS CHAR
    INDEX EAN IS PRIMARY EAN
    INDEX Artikkel ArtikkelNr
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Centrallager */
{syspara.i 5 1 1 iCl INT}
/* MESSAGE "g.jansbo"                     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
FOR EACH tt_Error:
  DELETE tt_Error.
END.

SUBSCRIBE TO 'PBR' ANYWHERE.
{syspara.i 50 200 1 cTekst}
IF TRIM(cTekst) = "1" THEN
  bProfitBase = TRUE.
ELSE 
  bProfitBase = FALSE.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Sjekker om det kjøres på en LapTop */
if valid-handle(h_dproclib) then
  run SjekkLapTop in h_dproclib (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
IF NOT AVAILABLE Butiker THEN
DO:
    MESSAGE "Sentrallager " + STRING(iCL) + " er ikke lagt inn i butikkregister."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .


/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ASSIGN
    cEDB-System = EkstVPILev.KortNavn
    .

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* /* Er datasettet ferdig oppdatert? */                                           */
/* IF AVAILABLE VPIDatasett THEN                                                   */
/* DO:                                                                             */
/*     IF VPIDatasett.DatasettStatus >= 3 AND                                      */
/*        VPIDatasett.DatasettStatus <= 4 THEN                                     */
/*     DO:                                                                         */
/*         MESSAGE "Forrige datasett er ikke oppdatert." SKIP                      */
/*                 "Det må være ferdig oppdatert før ny import av VPI kan gjøres." */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
/*         RETURN.                                                                 */
/*     END.                                                                        */
/* END.                                                                            */

/* Sjekker at default verdier er lagt inn i databasen. */
RUN SkapaDiverse.

/* Pakker ut varestrukturen */
RUN UtpakkVarestruktur.

/* Oppdaterer varestruktur i databasen. */
RUN OpprettVarestruktur.

/* Utpakking av Gruppeinndelinger.                       */
/* Her kommer også avdleinger, hovedgrupper og mvakoder. */

RUN UtpakkVPI.

/* /* Kjører oppdatering av faste registre til ProfitBase. */ */
/* IF bProfitBase THEN                                        */
/* PROFITBASE:                                                */
/* DO:                                                        */
/*   RUN pfxoppdatfastereg.p.                                 */
/* END. /* PROFITBASE */                                      */

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

/* Legger opp ErrorLog i Notepad hvis det er logget noen feil. */
IF bError AND SEARCH(cErrorLog) <> ? THEN
    RUN VisErrorLog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ArtError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtError Procedure 
PROCEDURE ArtError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  DEF VAR plDec  AS DEC NO-UNDO.

  IF cErrorLog = "" THEN
      RUN InitErrorLog.

  OUTPUT TO VALUE(cErrorLog) APPEND.
    PUT UNFORMATTED 
        "Linje: " VPIFilLinje.LinjeNr
        SKIP.

    PUT UNFORMATTED
        "       " VPIFilLinje.StorTekst
        SKIP.
    DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT UNFORMATTED
            "       "  ERROR-STATUS:GET-MESSAGE(piLoop)
            SKIP.            
    END.

    IF AVAILABLE TT_Strekkode THEN
    DO:
        ASSIGN
            cTekst = "Tandem EAN opprettet - Artikkel/EAN: " + ENTRY(1,VPIFilLinje.StorTekst,pcSep) + " " + ENTRY(2,VPIFilLinje.StorTekst,pcSep).
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "3").
        PUT UNFORMATTED
          "       Tandem EAN opprettet - Artikkel/EAN: " ENTRY(1,VPIFilLinje.StorTekst,pcSep) " " ENTRY(2,VPIFilLinje.StorTekst,pcSep)
          SKIP.
    END.
    IF iAntEntries <> NUM-ENTRIES(VPIFilLinje.StorTekst,pcSep) THEN
    DO:
        ASSIGN
            cTekst = "** Feil antall elementer på linje. Skal være 34. Det er: " + string(NUM-ENTRIES(VPIFilLinje.StorTekst,pcSep)) + " Separator: " + pcSep.
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "3").
        PUT UNFORMATTED
          "       ** Feil antall elementer på linje: " NUM-ENTRIES(VPIFilLinje.StorTekst,pcSep) pcSep 
          SKIP.
    END.
    IF length(TRIM(entry( 2,VPIFilLinje.StorTekst,pcSep))) > 13 THEN
        PUT UNFORMATTED
          "       ** For stor EAN kode: " TRIM(entry( 2,VPIFilLinje.StorTekst,pcSep)) 
          SKIP.

    ASSIGN
        plDec = DEC(entry( 2,VPIFilLinje.StorTekst,pcSep)) 
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        PUT UNFORMATTED
          "       ** Feil i EAN kode: " TRIM(entry( 2,VPIFilLinje.StorTekst,pcSep))
          SKIP.


  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKalkyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKalkyle Procedure 
PROCEDURE FixKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cInprisLev    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cBerUtpPerBut AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wSkjerm               AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wFeltListe            AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE wFeltNr               AS INTEGER    NO-UNDO.
   ASSIGN wFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                 "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                 "FI-Mva,FI-Mva%,Pris,EU-Pris".

   FIND VarGr WHERE VarGr.Vg = ArtBas.Vg NO-LOCK.
   FIND Moms OF VarGr NO-LOCK.
   FIND ArtPris OF ArtBas WHERE ArtPris.Profilnr = 1 NO-LOCK NO-ERROR.
   IF AVAIL ArtPris THEN DO:
   if valid-handle(h_PrisKo) then
     run InitKalkyle in h_PrisKo
          (RECID(ArtBas), 
           1, /* prisprofil */
           input-output wSkjerm,
           Moms.MomsProc,
           1, /* Valuta.ValKurs */
           wFeltNr,
           FALSE).
       ASSIGN wSkjerm = wSkjerm + "0;;;0;;;0;0;".
   END.
   ELSE
       ASSIGN wSkjerm = "0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;;;0;;;0;0;". 
/* 760;760;0;0;0;0;0;0;0;0;0;0;760;380;25;760;50;1900;285;no;11/10/01;0;;;0;0;no */

   /* Kalkyle for artikkler med åpen pris og tidskrifter og blader, hentes fra varegruppe */
   IF (ArtBas.ArtikkelNr >= 7388000000000 AND ArtBas.ArtikkelNr <= 7388999999999) OR
      ArtBas.OPris = TRUE THEN
       ASSIGN wFeltNr = LOOKUP("ValPris",wFeltListe) /* fält 1 */
              ENTRY(1,wSkjerm,";")  = STRING(VarGr.Kost_Proc)
              ENTRY(2,wSkjerm,";")  = STRING(VarGr.Kost_Proc)
              ENTRY(17,wSkjerm,";") = STRING(Moms.MomsProc)
              ENTRY(18,wSkjerm,";") = string(100.0 + Moms.MomsProc).
   ELSE
       ASSIGN wFeltNr = LOOKUP("ValPris",wFeltListe) /* fält 1 */
              ENTRY(1,wSkjerm,";")  = cInprisLev
              ENTRY(2,wSkjerm,";")  = cInprisLev
              ENTRY(17,wSkjerm,";") = STRING(Moms.MomsProc)
              ENTRY(18,wSkjerm,";") = cBerUtpPerBut.

   RUN Omregning IN h_PrisKo
        (RECID(ArtBas), 
         1, /* PrisProfil.ProfilNr */
         input-output wSkjerm,
         Moms.MomsProc,
         1, /* Valuta.ValKurs */
         wFeltNr,
         FALSE).
   ASSIGN wFeltNr = LOOKUP("Pris",wFeltListe) /* fält 1 */
         wSkjerm = wSkjerm + "0;;;0;;;0;0;".
   RUN Omregning IN h_PrisKo
        (RECID(ArtBas), 
         1, /* PrisProfil.ProfilNr */
         input-output wSkjerm,
         Moms.MomsProc,
         1, /* Valuta.ValKurs */
         wFeltNr,
         FALSE).
   ASSIGN wSkjerm = wSkjerm + "0;;;0;;;0;0;"
          ENTRY(20,wSkjerm,";") = "no"
          ENTRY(21,wSkjerm,";") = STRING(TODAY)
          ENTRY(27,wSkjerm,";") = "no".

   run LagreArtPris in h_PrisKo
       (RECID(ArtBas), 
        1, /* Profilnr*/
        INPUT-OUTPUT wSkjerm,
        FALSE, /* tilbud */
        TRUE,  /* plDirekte */
        TRUE,
        ?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitErrorLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitErrorLog Procedure 
PROCEDURE InitErrorLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cTekst AS CHAR NO-UNDO.

  DEF VAR bFilFinnes AS LOG  NO-UNDO.

  ASSIGN
      iAntEntries = 34
      cErrorLog   = "ErrorVPI-" + 
                    STRING(TODAY,"99-99-99") + 
                    ".Txt"
      .

  IF SEARCH(cErrorLog) <> "" THEN
      bFilFinnes = TRUE.

  OUTPUT TO VALUE(cErrorLog) APPEND.
    IF bFilFinnes THEN
        PUT UNFORMATTED "  " SKIP(1).

    PUT UNFORMATTED
      cTekst FORMAT "x(78)" SKIP
      "Utpakking " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Antall entries pr linje skal være: " + STRING(iAntEntries) SKIP
      "(Felt separator er - " + pcSep + ")" SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
      "---------------------------------------------------------------------------------------" SKIP
      .
  OUTPUT CLOSE.

END PROCEDURE.


  /*
  
/*------------------------------------------------------------------------------
  Purpose:     Avslutter logging til ErrorLog.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE("Error.Txt") APPEND.
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.

END PROCEDURE.


  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  def INPUT        PARAMETER pcFraFelt  as char no-undo.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm   AS CHAR NO-UNDO.
  
  def var pcFeltListe as char no-undo.
  def var piFeltNr    as int  no-undo.
  DEF VAR lTilbud     AS LOG  NO-UNDO.
  DEF VAR lDirekte    AS LOG  NO-UNDO.

  DEF BUFFER bArtBas FOR VPIArtBas.
  DEF BUFFER bVarGr  FOR VarGr.
  DEF BUFFER bMoms   FOR Moms.
  DEF BUFFER bValuta FOR Valuta.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  assign
    lTilbud     = FALSE
    lDirekte    = FALSE
    pcFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                  "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                  "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  assign
    /*pcFraFelt = substring(pcFraFelt,4)*/
    piFeltNr  = lookup(pcFraFelt,pcFeltListe)
    .

  /* Ukjent felt. */  
  if piFeltNr = 0 then
    do:
      message "Ukjent felt!" view-as alert-box title "Kalkylefeil".
      return no-apply.  
    end.

  /* Henter nødvendige buffere */
  FIND bArtBas NO-LOCK WHERE
      RECID(bArtBas) = rArtBasRecid NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent artikkel."
      .
    RETURN "AVBRYT".
  END.
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe."
      .
    RETURN "AVBRYT".
  END.
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
  DO:
    MESSAGE "Ukjent bMoms"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bValuta OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent valuta."
      .
    RETURN "AVBRYT".
  END.

  /* Starter omkalkulering.                         */
  run Omregning in h_PrisKo
       (input rArtBasRecid, 
        input iProfilNr,
        input-output pcSkjerm,
        input bMoms.MomsProc,
        input bValuta.ValKurs, 
        input piFeltNr,
        INPUT lTilbud).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LogError Procedure 
PROCEDURE LogError PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.

  OUTPUT TO VALUE(cErrorLog) APPEND.
    PUT UNFORMATTED 
        "Linje: " VPIFilLinje.LinjeNr 
        SKIP.

    IF iAntEntries <> NUM-ENTRIES(VPIFilLinje.StorTekst,pcSep) THEN
        PUT UNFORMATTED
          "       ** Feil antall elementer på linje: " NUM-ENTRIES(VPIFilLinje.StorTekst,pcSep) pcSep
          SKIP.

    PUT UNFORMATTED
        "       " VPIFilLinje.StorTekst
        SKIP.
    DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT UNFORMATTED
            "       "  ERROR-STATUS:GET-MESSAGE(piLoop)
            SKIP.            
    END.

  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettVarestruktur) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettVarestruktur Procedure 
PROCEDURE OpprettVarestruktur :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:
         
        TT_VarGr.
        TT_HuvGr.
        TT_Avdeling.
        TT_LevBas.
        TT_Produsent.
  
------------------------------------------------------------------------------*/
/* Varegrupper */
FOR EACH TT_VarGr:
    FIND VarGr EXCLUSIVE-LOCK WHERE
        VarGr.Vg = TT_VarGr.Vg NO-ERROR.
/*     IF NOT AVAILABLE VarGr THEN */
/*         CREATE VarGr.           */
    BUFFER-COPY TT_VarGr TO VarGr.
    FOR EACH Kategori:
        IF NOT CAN-FIND(VgKat NO-LOCK WHERE
                        VgKat.Vg    = VarGr.Vg AND
                        VgKat.VgKat = Kategori.KatNr AND
                        VgKat.KatNr = Kategori.KatNr) THEN
        DO:
            CREATE VgKat.
            ASSIGN
                VgKat.Vg    = VarGr.Vg      
                VgKat.VgKat = Kategori.KatNr 
                VgKat.KatNR = Kategori.KatNr
                .
        END.
    END.
    RELEASE VarGr.
END.

/* Hovedgruppe */
FOR EACH TT_HuvGr:
    FIND HuvGr EXCLUSIVE-LOCK WHERE
        HuvGr.Hg = TT_HuvGr.Hg NO-ERROR.
/*     IF NOT AVAILABLE HuvGr THEN */
/*         CREATE HuvGr.           */
    BUFFER-COPY TT_HuvGr TO HuvGr.
    RELEASE HuvGr.
END.

/* Avdelinger */
FOR EACH TT_Avdeling:
    FIND Avdeling EXCLUSIVE-LOCK WHERE
        Avdeling.AvdelingNr = TT_Avdeling.AvdelingNr NO-ERROR.
/*     IF NOT AVAILABLE Avdeling THEN */
/*         CREATE Avdeling.           */
    BUFFER-COPY TT_Avdeling TO Avdeling.
    RELEASE Avdeling.
END.

/* Leverandør */
FOR EACH TT_LevBas:
    FIND LevBas EXCLUSIVE-LOCK WHERE
         LevBas.LevNr = TT_LevBas.LevNr NO-ERROR.
/*     IF NOT AVAILABLE LevBas THEN */
/*         CREATE LevBas.           */
    BUFFER-COPY TT_LevBas TO LevBas.
    RELEASE LevBas.
END.

/* Produsent */
FOR EACH TT_Produsent:
    FIND Produsent EXCLUSIVE-LOCK WHERE
         Produsent.ProdNr = TT_Produsent.ProdNr NO-ERROR.
/*     IF NOT AVAILABLE Produsent THEN */
/*         CREATE Produsent.           */
    BUFFER-COPY TT_Produsent TO Produsent.
    RELEASE Produsent.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PBR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PBR Procedure 
PROCEDURE PBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrisError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrisError Procedure 
PROCEDURE PrisError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  DEF VAR plDec  AS DEC NO-UNDO.

  OUTPUT TO VALUE(cErrorLog) APPEND.
    PUT UNFORMATTED 
        "Linje: " VPIFilLinje.LinjeNr " ** Null (0) i kostpris og/eller utpris."
        SKIP.

    PUT UNFORMATTED
        "       " VPIFilLinje.StorTekst
        SKIP.
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaDiverse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaDiverse Procedure 
PROCEDURE SkapaDiverse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMomsKod AS CHARACTER INIT "1,4,6,7" NO-UNDO.
    DEFINE VARIABLE cMoms%   AS CHARACTER INIT "25,0,12,6" NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    /* Detta finns i PGR-fil */
    DO iCount = 1 TO NUM-ENTRIES(cMomsKod):
        FIND Moms WHERE Moms.MomsKod = INT(ENTRY(iCount,cMomsKod)) NO-ERROR.
        IF NOT AVAIL Moms THEN DO:
            CREATE Moms.
            ASSIGN Moms.MomsKod = INT(ENTRY(iCount,cMomsKod)).
        END.
        ASSIGN Moms.MomsProc    = DECI(ENTRY(iCount,cMoms%))
               Moms.Beskrivelse = IF ENTRY(iCount,cMoms%) = "0" THEN
                                  "Ingen moms" ELSE
                                  ENTRY(iCount,cMoms%) + "% moms" .
    END.
    IF NOT CAN-FIND(StrType WHERE StrType.StrTypeId = 1) THEN DO:
        CREATE StrType.
        ASSIGN StrType.StrTypeId   = 1
               StrType.KortNavn    = "*DEF*"
               StrType.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Rabatt WHERE Rabatt.RabKod = 0) THEN DO:
        CREATE Rabatt.
        ASSIGN Rabatt.RabKod   = 0
               Rabatt.RabProc  = 0
               Rabatt.RabBeskr = "Default".
    END.
    IF NOT CAN-FIND(Prov WHERE Prov.ProvKod = 0) THEN DO:
        CREATE Prov.
        ASSIGN Prov.ProvKod   = 0
               Prov.ProvProc  = 0
               Prov.ProvBeskr = "Default".
    END.
    IF NOT CAN-FIND(Kategori WHERE Kategori.KatNr = 1) THEN DO:
        CREATE Kategori.
        ASSIGN Kategori.KatNr       = 1
               Kategori.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Prisprofil WHERE Prisprofil.ProfilNr = 1) THEN DO:
        CREATE Prisprofil.
        ASSIGN Prisprofil.ProfilNr    = 1
               Prisprofil.KortNavn    = "*DEF*"
               Prisprofil.Beskrivelse = "Default"
               Prisprofil.Merknad     = "Skapad import".                
    END.
    IF NOT CAN-FIND(Fylke WHERE Fylke.FylkesNr = "1") THEN DO:
        CREATE Fylke.
        ASSIGN Fylke.FylkesNr = "1"
               Fylke.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Kommune WHERE Kommune.FylkesNr = "1" AND 
                                  Kommune.KommNr   = "1") THEN DO:
        CREATE Kommune.
        ASSIGN Kommune.FylkesNr    = "1"
               Kommune.KommNr      = "1"
               Kommune.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Farg WHERE Farg.Farg = 0) THEN DO:
        CREATE Farg.
        ASSIGN Farg.Farg     = 0
               Farg.KFarge   = "*DEF*"
               Farg.FarBeskr = "Default".
    END.
    IF NOT CAN-FIND(Sasong WHERE SaSong.Sasong = 0) THEN DO:
        CREATE SaSong.
        ASSIGN SaSong.Sasong   = 0
               SaSong.SasBeskr = "Default".
    END.
    IF NOT CAN-FIND(Valuta WHERE Valuta.ValKod = "SEK") THEN DO:
        CREATE Valuta.
        ASSIGN Valuta.ValKod   = "SEK"
               Valuta.ValDatum = TODAY
               Valuta.ValKurs  = 1
               Valuta.ValLand  = "Default".
    END.
    IF NOT CAN-FIND(Material WHERE Material.Matkod = 0) THEN DO:
        CREATE Material.
        ASSIGN Material.MatKod   = 0
               Material.MatBeskr = "Default".
    END.
    IF NOT CAN-FIND(LevBas WHERE
                    LevBas.LevNr = 901) THEN
    DO:
        CREATE LevBas.
        ASSIGN
            LevBas.LevNr   = 901
            LevBas.LevNamn = "Ukjent leverandør"
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtpakkVarestruktur) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtpakkVarestruktur Procedure 
PROCEDURE UtpakkVarestruktur :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piVg         LIKE VarGr.Vg            NO-UNDO.
DEF VAR piHg         LIKE HuvGr.Hg            NO-UNDO.
DEF VAR piAvdelingNr LIKE Avdeling.AvdelingNr NO-UNDO.
DEF VAR plMva%       AS   DEC                 NO-UNDO.
DEF VAR piLevNr      LIKE LevBas.LevNr        NO-UNDO.
DEF VAR piProdNr     LIKE Produsent.ProdNr    NO-UNDO.

ASSIGN
    bError = FALSE
    .
        
/* Behandler fillinjene */
VPIFILLINJE:
FOR EACH VPIFilLinje OF VPIFilHode NO-LOCK:
    IF AVAILABLE TT_VarGr THEN
        RELEASE TT_VarGr.
    IF AVAILABLE TT_HuvGr THEN
        RELEASE TT_HuvGr.
    IF AVAILABLE TT_Avdeling THEN
        RELEASE TT_Avdeling.
    IF AVAILABLE TT_LevBas THEN
        RELEASE TT_LevBas.
    IF AVAILABLE TT_Produsent THEN
        RELEASE TT_Produsent.

    IF VPIFilLinje.LinjeNr MODULO 100 = 0 THEN
        STATUS DEFAULT "Behandler linje " + STRING(VPIFilLinje.LinjeNr) + " av " + 
                       STRING(VPIFilHode.AntLinjer) + ".". 
    ASSIGN
        plMva%        = dec(replace(TRIM(entry(13,VPIFilLinje.StorTekst,pcSep)),".",","))
        piVg          = int(entry(14,VPIFilLinje.StorTekst,pcSep))
        piLevNr       = int(entry(15,VPIFilLinje.StorTekst,pcSep))
        piProdNr      = int(entry(16,VPIFilLinje.StorTekst,pcSep))
        piHg          = int(entry(23,VPIFilLinje.StorTekst,pcSep))
        piAvdelingNr  = int(entry(25,VPIFilLinje.StorTekst,pcSep))
        NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
    DO:
        IF bError = FALSE THEN
        DO:
            RUN InitErrorLog ("V A R E S T R U K T U R").
            bError = TRUE.
        END.
        RUN LogError.
        NEXT VPIFILLINJE.
    END.

    FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc = plMva% NO-ERROR.

    /* Avdeling */
    IF NOT CAN-FIND(TT_Avdeling WHERE
                    TT_Avdeling.AvdelingNr = piAvdelingNr) THEN
    DO:
        CREATE TT_Avdeling.
        ASSIGN
            TT_Avdeling.AvdelingNr  = piAvdelingNr
            TT_Avdeling.AvdelingNavn = TRIM(entry(24,VPIFilLinje.StorTekst,pcSep))
            .
    END.

    /* Hovedgrupper */
    IF NOT CAN-FIND(TT_HuvGr WHERE
                    TT_HuvGr.Hg = piHg) THEN
    DO:
        CREATE TT_HuvGr.
        ASSIGN
            TT_HuvGr.Hg      = piHg
            TT_HuvGr.HgBeskr = TRIM(entry(24,VPIFilLinje.StorTekst,pcSep))
            TT_HuvGr.AvdelingNr = piAvdelingNr.
    END.

    /* Varegruppe */
    IF NOT CAN-FIND(TT_VarGr WHERE
                    TT_VarGr.Vg = piVg) THEN 
    DO:
        CREATE TT_VarGr.
        ASSIGN
            TT_VarGr.Vg      = piVg
            TT_VarGr.VgBeskr = TRIM(entry(21,VPIFilLinje.StorTekst,pcSep))
            TT_VarGr.Hg      = piHg
            TT_VarGr.MomsKod = IF AVAILABLE Moms
                                 THEN Moms.MomsKod
                                 ELSE 0
            TT_VarGr.Kost_Proc = 100 - dec(replace(entry(32,VPIFilLinje.StorTekst,pcSep),".",","))
            .
    END.

    /* Leverandør */
    IF NOT CAN-FIND(TT_LevBas WHERE
                    TT_LevBas.LevNr = piLevNr) THEN 
    DO:
        CREATE TT_LevBas.
        ASSIGN
            TT_LevBas.LevNr = piLevNr
            TT_LevBas.LevNamn = TRIM(entry(10,VPIFilLinje.StorTekst,pcSep))
            .
    END.

    /* Produsent */
    FIND TT_Produsent WHERE
         TT_Produsent.ProdNr = piProdNr NO-ERROR.
    IF NOT AVAILABLE TT_Produsent THEN
    DO:
        CREATE TT_Produsent.
        ASSIGN
            TT_Produsent.ProdNr      = piProdNr
            TT_Produsent.Beskrivelse = TRIM(entry(11,VPIFilLinje.StorTekst,pcSep))
            .
    END.
/*     ASSIGN                                                                     */
/*         TT_Produsent.Beskrivelse = TRIM(entry(10,VPIFilLinje.StorTekst,pcSep)) */
        .
END. /* VPIFILLINJE */

STATUS DEFAULT "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtpakkVPI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtpakkVPI Procedure 
PROCEDURE UtpakkVPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcHg          AS CHAR NO-UNDO.
DEF VAR pcVg          AS CHAR NO-UNDO.
DEF VAR pcUgr         AS CHAR NO-UNDO.
DEF VAR pcMva%        AS CHAR NO-UNDO.
DEF VAR piTid         AS INT  NO-UNDO.

DEF VAR piHg          AS INT  NO-UNDO.
DEF VAR piVg          AS INT  NO-UNDO.
DEF VAR piAvdelingNr  AS INT  NO-UNDO.
DEF VAR piMomsKod     AS INT  NO-UNDO.
DEF VAR piHovedGr     AS INT  NO-UNDO.
DEF VAR pbStatus      AS LOG  NO-UNDO.
DEF VAR piAntKoblet   AS INT  NO-UNDO.
DEF VAR piAntOppdat   AS INT  NO-UNDO.
DEF VAR iTstNr        AS INT  NO-UNDO.
DEF VAR dTst          AS DECI NO-UNDO.
DEF VAR iTotAnt       AS INT  NO-UNDO.
DEF VAR iAntIkke      AS INT  NO-UNDO.

DEF VAR dTest         AS DECI NO-UNDO.

/*
DEF VAR piFarg        AS INT  NO-UNDO.
DEF VAR pcVareNr      AS CHAR NO-UNDO.
DEF VAR piTilbud      AS INT  NO-UNDO.
DEF VAR piAntArtikler AS INT  NO-UNDO.
*/
DEF VAR piLoop1       AS INT  NO-UNDO.

DEF BUFFER bMoms FOR Moms.

ASSIGN
    bError  = FALSE
    piTid   = TIME
    cTekst  = "Starter utpakking av VPI.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").


ASSIGN
    pbStatus = FALSE /* Aldri tilbud */
    iTotAnt  = VPIFilHode.AntLinjer
    .

/* Standard Mva */
FIND FIRST Moms NO-LOCK WHERE 
    Moms.Momsproc > 0 NO-ERROR.
IF AVAILABLE Moms THEN
    piMomsKod = Moms.MomsKod.

/* Standard hovedgruppe */
FIND FIRST HuvGr NO-LOCK WHERE
    HuvGr.Hg > 0 NO-ERROR.
IF AVAILABLE HuvGr THEN
    piHovedGr = HuvGr.Hg.

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT AVAILABLE VPIDatasett THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

ASSIGN
    iAntNye     = 0
    iAntFel     = 0
    iAntIkke    = 0
/*     cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel" */
    .

/* Behandler fillinjene */
VPIFILLINJE:
FOR EACH VPIFilLinje OF VPIFilHode TRANSACTION:
    IF bError = FALSE THEN
    DO:
        RUN InitErrorLog ("V P I  I M P O R T").
        bError = TRUE.
    END.
    IF AVAILABLE TT_Strekkode THEN
        RELEASE TT_Strekkode.

    /* Logger alle strekkoder. */
    FIND FIRST TT_Strekkode WHERE
        TT_Strekkode.EAN = TRIM(entry(2,VPIFilLinje.StorTekst,pcSep)) NO-ERROR.
    IF NOT AVAILABLE TT_Strekkode THEN
    DO:
        CREATE TT_Strekkode.
        ASSIGN
            TT_Strekkode.ArtikkelNr = DEC(entry(1,VPIFilLinje.StorTekst,pcSep))
            TT_Strekkode.EAN        = TRIM(entry(2,VPIFilLinje.StorTekst,pcSep))
            .
    END.
    IF AVAILABLE TT_Strekkode THEN
        RELEASE TT_Strekkode.

    /* Varer som ikke ligger på hovedleverandør, skal ikke leses inn.   */
    IF TRIM(entry(31,VPIFilLinje.StorTekst,pcSep)) = "0" THEN /* 0 = dubletter av samma vara */
    DO:
        iAntIkke = iAntIkke + 1.
        NEXT VPIFILLINJE.
    END.

    /* Logger ny artikkel */
    FIND FIRST TT_Artikkel WHERE 
        TT_Artikkel.ArtikkelNr = DEC(entry(1,VPIFilLinje.StorTekst,pcSep)) NO-ERROR.
    IF NOT AVAILABLE TT_Artikkel THEN
        CREATE TT_Artikkel.

    IF VPIFilLinje.LinjeNr MODULO 100 = 0 THEN
        STATUS DEFAULT "Behandler artikkler " + STRING(VPIFilLinje.LinjeNr) + " av " + 
                       STRING(VPIFilHode.AntLinjer) + ".". 

    /* Kontrollerer lengde på EAN kode. DUN nummer stoppes. */
    IF LENGTH(TRIM(entry( 2,VPIFilLinje.StorTekst,pcSep))) > 13 THEN
    DO:
        RUN ArtError.
        iAntFel = iAntFel + 1.
        NEXT VPIFILLINJE.
    END.
    ASSIGN
        TT_Artikkel.ArtikkelNr = dec(entry( 1,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.EAN        = TRIM(entry( 2,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.Beskr      =     TRIM(entry( 3,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.BongTekst  =     TRIM(entry( 4,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.KostPris   = dec(REPLACE(entry( 6,VPIFilLinje.StorTekst,pcSep),".",","))
        TT_Artikkel.Pris       = dec(REPLACE(entry( 5,VPIFilLinje.StorTekst,pcSep),".",","))
        TT_Artikkel.Mva%       = dec(replace(entry(13,VPIFilLinje.StorTekst,pcSep),".",","))
        TT_Artikkel.LevNr      = int(entry(15,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.ProdNr     = int(entry(16,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.Hg         = int(entry(23,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.Vg         = int(entry(14,VPIFilLinje.StorTekst,pcSep))
        TT_Artikkel.OPris      = IF int(entry(29,VPIFilLinje.StorTekst,pcSep)) = 1 AND TT_Artikkel.Pris = 0
                                   THEN TRUE
                                   ELSE FALSE
        TT_Artikkel.LevKod     = (entry( 9,VPIFilLinje.StorTekst,pcSep)) 
        TT_Artikkel.SalgsEnhet = IF TRIM(entry(28,VPIFilLinje.StorTekst,pcSep)) = "1"
                                   THEN "Kg"
                                   ELSE "Stk"
        TT_Artikkel.LinkVareNr = dec(REPLACE(entry(33,VPIFilLinje.StorTekst,pcSep),".",","))
        NO-ERROR.
    /* Håndterer eventuell feil i EAN kode. */
    IF ERROR-STATUS:ERROR THEN
    DO:
        RUN ArtError.
        iAntFel = iAntFel + 1.
        NEXT VPIFILLINJE.
    END.
    /* Sjekker om varegruppen er gyldig */
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = TT_Artikkel.Vg NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
    DO:
        RUN ArtError.
        iAntFel = iAntFel + 1.
        NEXT VPIFILLINJE.
    END.

    /* Sjekker om artikkelen finnes fra før.             */
    FIND ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.ArtikkelNr = TT_Artikkel.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
    NYARTBAS:
    DO:
        IF NOT AVAIL ArtBas THEN DO:
            /* Artikkel */
            CREATE ArtBas.
            ASSIGN 
                ArtBas.ArtikkelNr = TT_Artikkel.ArtikkelNr
                ArtBas.Vg         = TT_Artikkel.Vg
                ArtBas.LopNr      = ?
                iAntNye           = iAntNye + 1
                .
            DO WHILE TRUE:
                ASSIGN ArtBas.LopNr = SetLopeNr() NO-ERROR. /* Må stå i egen ASSIGN */
                IF ERROR-STATUS:ERROR THEN
                    MESSAGE "NY ART:" VPIFilLinje.LinjeNr SKIP
                            VPIFilLinje.StorTekst
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                IF ERROR-STATUS:ERROR = FALSE 
                    THEN LEAVE.
            END.
        END.
        /* Vi kan få en ny artikel som har streckkod som har tillhört en gammal artikel */
        /* därför tar vi bort den existerande streckkoden */
/* ! */ FIND Strekkode WHERE StrekKode.Kode = trim(STRING(TT_Artikkel.EAN)) NO-ERROR.
        IF AVAIL Strekkode THEN
            DELETE Strekkode.
        /* Strekkode */
        ASSIGN dTest = DECI(TT_Artikkel.EAN) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND dTest > 0 THEN DO: /* Test */
            CREATE Strekkode.
            ASSIGN                      /* Kodetype är 0 för plu och strkode = 0 plu KANSKE UPP TILL 6 SIFFROR */
                Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
                Strekkode.Kode       = trim(STRING(TT_Artikkel.EAN))
                Strekkode.StrKode    = 1
                Strekkode.KodeType   = IF LENGTH(trim(Strekkode.Kode)) <= 5
                                         THEN 3 /* PLU */
                                         ELSE 1 /* EAN */
                Strekkode.HovedNr    = YES
                Strekkode.IKasse     = YES
                .
        END.
    END. /* NYARTBAS */

    /* Håndtering av bytting av varegruppe på artikkel. */
    IF ArtBas.Vg > 0 AND ArtBas.Vg <> TT_Artikkel.Vg THEN
    DO:
        ASSIGN 
            ArtBas.Vg    = TT_Artikkel.Vg
            ArtBas.LopNr = ?
            .
        DO WHILE TRUE:
            ASSIGN ArtBas.LopNr = SetLopeNr() NO-ERROR. /* Må stå i egen ASSIGN */
            IF ERROR-STATUS:ERROR THEN
                MESSAGE "BYTTE VG:" VPIFilLinje.LinjeNr SKIP
                        VPIFilLinje.StorTekst
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            IF ERROR-STATUS:ERROR = FALSE 
                THEN LEAVE.
        END.
    END.

    /* Oppdaterer artikkelinformasjonen. */
    ASSIGN
        ArtBas.Hg         = IF AVAILABLE VarGr
                              THEN VarGr.Hg
                              ELSE TT_Artikkel.Hg
        ArtBas.Beskr      = TT_Artikkel.Beskr                   
        ArtBas.BongTekst  = TT_Artikkel.BongTekst
        ArtBas.LevNr      = TT_Artikkel.LevNr
        ArtBas.LevKod     = TT_Artikkel.LevKod
        ArtBas.VgKat      = 1
        ArtBas.StrTypeId  = 2
        ArtBas.Lager      = FALSE
        ArtBas.Storrelser = TRUE
        ArtBas.Valkod     = "SEK"
        ArtBas.SalgsEnhet = TT_Artikkel.SalgsEnhet
        ArtBas.OPris      = TT_Artikkel.OPris
        ArtBas.LinkVareNr = TT_Artikkel.LinkVareNr
        .
    /* Oppretter STrekkode */
    FOR EACH TT_Strekkode WHERE
        TT_STrekkode.ArtikkelNr = ArtBas.ArtikkelNr:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = TT_Strekkode.EAN NO-ERROR.
        /* Här hanterar vi streckkoder som har flyttats till annan artikel */
        /* Vi tar helt enkelt bort den gamla */
/* ! */ IF AVAIL Strekkode AND Strekkode.ArtikkelNr <> ArtBas.ArtikkelNr THEN DO:
            FIND CURRENT Strekkode EXCLUSIVE.
            DELETE StrekKode.
        END.
        /* Här skall det INTE !! stå ELSE */
        IF NOT AVAILABLE Strekkode THEN
        DO:
            ASSIGN dTest = DECI(TT_Artikkel.EAN) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR AND dTest > 0 THEN DO: /* Test */
                CREATE Strekkode.
                ASSIGN
                    Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
                    Strekkode.Kode       = TT_Strekkode.EAN
                    Strekkode.StrKode    = 1
                    Strekkode.KodeType   = 1
                    Strekkode.VareId     = ArtBas.ArtikkelNr
                    Strekkode.HovedNr    = (IF TT_Artikkel.EAN = TT_Strekkode.EAN
                                              THEN TRUE
                                              ELSE FALSE)
                    Strekkode.iKasse     = TRUE
                    .
            END.
        END.
    END.

    /* Overstyrer åpen pris flagg på tidskrifter og blader. */
    IF (ArtBas.ArtikkelNr >= 7388000000000 AND ArtBas.ArtikkelNr <= 7388999999999) THEN
        ASSIGN
        ArtBas.Lager      = FALSE
        ArtBas.Storrelser = TRUE
        ArtBas.OPris      = TRUE
        .

    /* Oppretter/oppdaterer kalkylen */
    RUN FixKalkyle (STRING(TT_Artikkel.KostPris),STRING(TT_Artikkel.Pris)).
END. /* TRANSACTION - VPIFILLINJE */
ASSIGN pbStatus = TRUE. /* skall vi göra på något annat sätt ? */
IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
/*       VPIDatasett.DatasettStatus = 3 /* Gruppeinfo mottatt og oppdatert lokalt */ */
      VPIDatasett.DatasettStatus = 5 /* Her MÅ vi ferdigstille med en gang     */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .
  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.

END. /* TRANSACTION */

STATUS DEFAULT "".

ASSIGN
    cTekst = "Lest " + STRING(iTotAntLinjer) + " VPI poster. " + 
             "Antall linjer med ukjente koder " + STRING(iAntFel) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Antall linjer med på sekundær leverandør " + STRING(iAntIkke) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Antall behandlede varelinjer " + STRING(iAntNye) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av VPI ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VisErrorLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisErrorLog Procedure 
PROCEDURE VisErrorLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF SEARCH(cErrorLog) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cErrorLog),
                                  "",
                                  1,
                                  OUTPUT hInstance).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var pcTekst   as char no-undo.
  def var pdAktDato as date no-undo.
  DEF VAR plTilbud AS LOG  NO-UNDO.
  DEF VAR plManuel AS LOG  NO-UNDO.
  
  assign
    plTilbud = false
    plManuel = FALSE
    pdAktDato =  TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ string(dcValPris) + ";" +
      /*string(input FI-InnPris) */ string(dcInnPris) + ";" +
      /*string(input FI-Rab1)    */ string(dcRabatt)  + ";" +
      /*string(input FI-Rab1%)   */ "0" + ";" +
      /*string(input FI-Rab2)    */ "0" + ";" +
      /*string(input FI-Rab2%)   */ "0" + ";" +
      /*string(input FI-Frakt)   */ "0" + ";" +
      /*string(input FI-Frakt%)  */ "0" + ";" +
      /*string(input FI-DivKost) */ "0" + ";" +
      /*string(input FI-DivKost%)*/ "0" + ";" +
      /*string(input FI-Rab3)    */ "0" + ";" +
      /*string(input FI-Rab3%)   */ "0" + ";" +
      /*string(input FI-VareKost)*/ "0" + ";" +
      /*string(input FI-Mva)     */ "0" + ";" +
      /*string(input FI-Mva%)    */ "0" + ";" +
      /*string(input FI-DB)      */ "0" + ";" +
      /*string(input FI-DB%)     */ "0" + ";" +
      /*string(input FI-Pris)    */ STRING(dcUtpris) + ";" +
      /*string(input FI-EUPris)  */ "0" + ";" +
      /*plManuel                 */ "no" + ";"
       .
  /* Normal aktiveringsdag/tid */                 
  ASSIGN  
    cEndelse = cEndelse +              
             (if pdAktDato <> ?
                then string(pdAktDato)
                else "") + ";" +
              "0;"
    cEndelse = cEndelse + 
             ";0;;0;no"
    pcTekst = pcTekst + cEndelse
    .
  
  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wLoop as int no-undo.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  repeat wLoop = 1 to 10000:

    /* BUG Hider */
    if wLoop = 0 then
      next FINN-NESTE.
      
    if can-find(bufArtBas no-lock where
      bufArtBas.Vg    = ArtBas.Vg and
      bufArtBas.LopNr = wLoop) then
      do:
        next FINN-NESTE.
      end.
    else
      leave FINN-NESTE.          
  end. /* FINN-NESTE */
  
  if wLoop > 9999 then
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

