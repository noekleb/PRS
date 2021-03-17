&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xbxCPksdlInnles.p
    Purpose     :

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR iIntLinjeId   AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR lHK           AS LOG  NO-UNDO.
DEF VAR iUkjentVg     AS INT  NO-UNDO.
DEF VAR lFactor       AS DEC NO-UNDO.
DEF VAR cStorl        AS CHAR NO-UNDO.
DEF VAR iHKKorrLogg   AS INT  NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.
DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR piLevNr       AS INT  NO-UNDO.
DEF VAR lEuKurs       AS DEC  NO-UNDO.
DEF VAR lPkSdl        AS LOG  NO-UNDO. /* Dette flagget settes hvis det leses inn pakkseddel */
DEF VAR dVarebehnr LIKE Varebehhode.varebehnr NO-UNDO. /* Varebok som nye ordre skal legges i */
DEF VAR lLeggInnArt   AS INT  NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR cLoggFil      AS CHAR NO-UNDO.
DEF VAR iForrige      AS INT  NO-UNDO.
DEF VAR ipksdlNr      AS INT  NO-UNDO.

DEF VAR cDelimiter    AS CHAR INITIAL ";" NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  INDEX LinjeNr LinjeNr
  .

{windows.i}
{ttOrdre.i}

DEF TEMP-TABLE tt_Ordre NO-UNDO LIKE ttOrdre.    
DEFINE TEMP-TABLE ttoPkSdlHode NO-UNDO LIKE PkSdlHode.
DEFINE TEMP-TABLE ttoPkSdlLinje NO-UNDO LIKE PkSdlLinje.
    
/*     FIELD EkstVPILevNr AS INT  */
/*     FIELD LinjeNr      AS INT. */
DEFINE BUFFER BufTT_Ordre FOR tt_Ordre.
DEFINE TEMP-TABLE tt_BestLst 
    FIELD BestNr LIKE BestHode.BestNr
    .
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER pkPKSDLLinje FOR PKSDLLinje.
DEFINE BUFFER pkPKSdlPris  FOR PkSdlPris.

DEFINE STREAM Logg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&IF DEFINED(EXCLUDE-settLoggFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settLoggFil Procedure 
FUNCTION settLoggFil RETURNS CHARACTER
  ( OUTPUT pcLoggFil AS CHAR )  FORWARD.

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
         HEIGHT             = 28.95
         WIDTH              = 62.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{syspara.i 2 1 1 lFactor DEC}
{syspara.i 22 10 3 iHKKorrLogg INT}
{syspara.i 21 3 100 lLeggInnArt INT}
{syspara.i 1 1 18 cTekst}
lHK = CAN-DO("1,yes,Ja,true",cTekst).
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.
{syspara.i 2 1 1 lEuKurs DEC}
IF lEuKurs = 0 THEN lEuKurs = 1.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    .

RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Pakkseddelimport ' + cFilNavn + ' ' + string(TIME,"HH:MM:SS")).

FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILEvNr NO-ERROR.
ASSIGN
    piLevNr = EkstVPILev.LevNr.

/* Starter procedure bibliotek. */
IF NOT VALID-HANDLE(h_prisko) THEN
    RUN  prisko.p PERSISTENT SET h_prisko.

PUBLISH "SkrivTilDataMottaksLogg" (";xbxCPksdlInnles.p:;" + "** LesInnFil").
RUN LesInnFil.

/* Stopper innlesningsprogram for håndterminalfil. */
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

  PUBLISH "SkrivTilDataMottaksLogg" (";xbxCPksdlInnles.p:;" + "** MainBlock ferdig").

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

/* Kun filer med feil skal logges. */
IF NOT CAN-FIND(FIRST tt_Error) THEN
    RETURN.

IF AVAILABLE VPIFilHode THEN 
  DO:
  ASSIGN
    cFilNavn = VPIFilHode.FilNavn
    cFilNavn = REPLACE(cFilNavn,".csv",".err")
    pcTekst = "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    .  
  END.
ELSE 
  pcTekst = "Ukjent/slettet VPI fil (xbxCPksdlInnles.p).".
    
  OUTPUT TO VALUE(cFilNavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      pcTekst SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
/*   IF SEARCH("Error.Txt") <> ? THEN                     */
/*   DO:                                                  */
/*     DEF VAR hInstance AS INT.                          */
/*                                                        */
/*     RUN ShellExecute{&A} IN hpApi(0,                   */
/*                                   "open",              */
/*                                   "notepad.exe",       */
/*                                   SEARCH("Error.Txt"), */
/*                                   "",                  */
/*                                   1,                   */
/*                                   OUTPUT hInstance).   */
/*                                                        */
/*   END.                                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr     AS INT  NO-UNDO.
  DEF VAR piAntFeil     AS INT  NO-UNDO.
  DEF VAR pcBkuFil      AS CHAR NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.
  DEF VAR pcStorl       AS CHAR NO-UNDO.
  DEF VAR cDato         AS CHAR NO-UNDO.
  DEF VAR cTid          AS CHAR NO-UNDO.
  DEF VAR cTekst        AS CHAR NO-UNDO.
  DEF VAR iRecType      AS INT  NO-UNDO.
  DEF VAR iSeqNr        AS INT  NO-UNDO.
  DEF VAR iDummy        AS INT  NO-UNDO.
  DEF VAR iButik        AS INT  NO-UNDO.
  DEF VAR cEAN          AS CHAR NO-UNDO.
  DEF VAR piStrKode     AS INTEGER NO-UNDO.
  
  DEF VAR ipksdlOrdreNr AS INT  NO-UNDO.
  DEF VAR dpksdlDato    AS DATE NO-UNDO.
  DEF VAR cpksdlTid     AS CHAR NO-UNDO.
  DEF VAR cpksdlBruker  AS CHAR NO-UNDO.
  DEF VAR ipksdlLinjeNr AS INT  FORMAT ">>>>>9" NO-UNDO.
  DEF VAR cLinje        AS CHAR FORMAT "x(250)" NO-UNDO.
  DEF VAR ipksdlLevert  AS DEC  NO-UNDO.
  DEF VAR iPkSdlLevNr   AS INT  NO-UNDO.
  DEF VAR cPkSdlLevNamn AS CHAR NO-UNDO.

  DEFINE VARIABLE dTest AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE cErrorArtnr AS CHARACTER  NO-UNDO.
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH tt_Ordre:
      DELETE tt_Ordre.
  END.

  RUN TellOppLinjer.

  IF AVAILABLE ttOrdre THEN
      DELETE ttOrdre.

  ASSIGN
      iButik        = 1
      piLinjeNr     = 1
      iAntLinjer    = 0
      /* Pakkseddelinfo */
      ipksdlOrdreNr = 0
      dpksdlDato    = ?
      cpksdlTid     = ""
      cpksdlBruker  = ""
      ipksdlNr      = 0
      cLinje        = ""
      ipksdlLinjeNr = 0
      ipksdlLevert  = 0
      iPkSdlLevNr   = 0
      cPkSdlLevNamn = ""
      cStorl        = ''
      piStrKode     = 0
      /* Pakkseddelinfo */
            iRecType      = 99
            ipksdlOrdreNr = TIME 
            ipksdlNr      = TIME 
            cTekst        = 'HT ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS")
            cDato         = STRING(TODAY)
            cpksdlTid     = STRING(TIME,"HH:MM:SS")
            dpksdlDato    = DATE(cDato)
            cpksdlBruker  = USERID("SkoTex")
            lPkSdl        = TRUE
      .

  RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Pakkseddelimport pakkseddel ' + cLinje + ' ' + string(TIME,"HH:MM:SS")).

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    IF AVAILABLE StrKonv THEN RELEASE StrKonv.
    IF AVAILABLE ttOrdre THEN DELETE ttOrdre.
    
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iAntLinjer = iAntLinjer + 1
        iSeqNr     = iSeqNr + 1.

    /* Ta bort oönskade fnuttar */
    cLinje = REPLACE(cLinje,'"','').

    ASSIGN
        cEAN   = TRIM(ENTRY(5,cLinje,";")).
    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
    FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cEAN NO-ERROR.
    IF NOT AVAILABLE Strekkode THEN 
    DO:
      RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Ukjent strekkode ' + cLinje + ' ' + string(TIME,"HH:MM:SS")).
      NEXT LESERLINJER.
    END.
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN 
    DO:
      RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Ukjent artikkel ' + cLinje + ' ' + string(TIME,"HH:MM:SS")).
      NEXT LESERLINJER.
    END.
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
    DO:
      RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Ukjent artpris ' + cLinje + ' ' + string(TIME,"HH:MM:SS")).
      NEXT LESERLINJER.
    END.
    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    ASSIGN
        iPkSdlLevNr   = ArtBas.LevNr
        cPkSdlLevNamn = IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE "".
    
    /* TN 20/9-10 Størrelsen som kommer fra ERP skal gjelde. */
    SETT_STRKODE:
    DO:
      FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
      IF NOT AVAILABLE StrKonv THEN
      DO: 
        cStorl = ' 1'.
        FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = 1 NO-ERROR.
      END.
      ELSE 
      ASSIGN
        cStorl    = StrKonv.Storl
        piStrKode = Strekkode.StrKode.
    END. /* SETT_STRKODE */

    /* Antall linjer på pakkseddelen. */
    ASSIGN
        ipksdlLinjeNr = ipksdlLinjeNr + 1.

      /* Record buffer å lese inn filen i */
      CREATE ttOrdre.
      ASSIGN
      ttOrdre.RecType       = iRecType
      ttOrdre.OrdreNr       = 0  
      ttOrdre.LevNr         = ArtBas.LevNr  
      ttORdre.LevKod        = ArtBas.LevKod
      
      ttOrdre.EkstId        = ''       
      ttOrdre.BestNr        = 0  
      ttOrdre.OrdreNr       = IF ttOrdre.BestNr = 0 THEN 0 ELSE ttOrdre.OrdreNr
      ttOrdre.ArtikkelNr    = ArtBas.ArtikkelNr
      ttOrdre.StrKode       = piStrKode
      ttOrdre.Storl         = cStorl 
      ttOrdre.DirekteLev    = TRUE
      ttOrdre.Beskr         = ArtBas.Beskr       
      ttOrdre.ValPris       = ArtPris.ValPris[1]  
      ttOrdre.InnkjopsPris  = ArtPris.InnkjopsPris[1]  
      ttOrdre.Rab1Kr        = ArtPris.Rab1Kr[1]  
      ttOrdre.Rab1%         = ArtPris.Rab1%[1]  
      ttOrdre.Pris          = ArtPris.Pris[1]  
      ttOrdre.Butik         = 1  
      ttOrdre.Strekkode     = Strekkode.Kode       
      ttOrdre.Varekost      = ArtPris.VareKost[1]
      /* Pakkseddelinfo */
      ttOrdre.Bestilt       = ttOrdre.Bestilt + DECIMAL(ENTRY(2,cLinje,";"))  
      ipksdlLevert          = ttOrdre.Bestilt
      ttOrdre.pksdlLevert   = ttOrdre.pksdlLevert + ipksdlLevert
      ttOrdre.pksdlOrdreNr  = ipksdlOrdreNr 
      ttOrdre.pksdlDato     = dpksdlDato    
      ttOrdre.pksdlTid      = cpksdlTid     
      ttOrdre.pksdlBruker   = cpksdlBruker  
      ttOrdre.pksdlNr       = ipksdlNr      
      ttOrdre.pksdlLinjeNr  = ipksdlLinjeNr /* Linjenummrering ved innlesning */
      ttOrdre.pksdlLinjeSeq = iSeqNr
      ttOrdre.PkSdlLevNr    = iPkSdlLevNr   
      ttOrdre.PkSdlLevNamn  = cPkSdlLevNamn 
      ttOrdre.SendtDato     = TODAY 
      ttOrdre.LevDato       = TODAY 
      .

    IF ERROR-STATUS:ERROR THEN
      RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Pakkseddelimport FEIL i assign ' + "** (1 LesInnFil)Feil på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop) + ' ' + string(TIME,"HH:MM:SS")).          

    /* Tar vare på ordrelinje/størrelse i temptabell */
    CREATE tt_Ordre.
    BUFFER-COPY ttOrdre TO tt_Ordre.
    DELETE ttOrdre.
    RELEASE tt_Ordre.
    
    IF ERROR-STATUS:ERROR THEN
    DO:
      RUN bibl_logg.p ('BXPKSDLImport', 'xbxCPksdlInnles.p: Pakkseddelimport FEIL : ' + tt_Error.Tekst + ' ' + string(TIME,"HH:MM:SS")).          
      NEXT LESERLINJER. 
    END.

    /* Regner om enhet */
    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  RUN lesPakkseddel.        /* Posterer pakkseddelinformasjon.                             */

  /* Stempler posten som utpakket. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* PAKKSEDDELFILEN */
  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesPakkseddel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesPakkseddel Procedure 
PROCEDURE lesPakkseddel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR fPkSdlId      AS DEC  NO-UNDO.
  DEF VAR fPkSdlLinjeId AS DEC  NO-UNDO.
  DEF VAR cEkstId       AS CHAR NO-UNDO.
  DEF VAR fMvaKr        AS DEC  NO-UNDO.
  DEF VAR fDbKr         AS DEC  NO-UNDO.
  DEF VAR fAntBest      AS DEC NO-UNDO.
  DEF VAR fAntLevert    AS DEC NO-UNDO.
  DEF VAR piCL          AS INT  NO-UNDO.

  ASSIGN
      iIntLinjeId = 0
      .
  /* Leser pakkseddellinjer */                           
  PAKKSEDDEL:
  FOR EACH tt_Ordre WHERE
    tt_Ordre.RecType = 99
    BREAK BY tt_Ordre.RecType
          BY tt_Ordre.EkstId /* TN 29/7-08 WMS systemet legger ut to pakkseddler med samme nummer. tt_Ordre.pksdlNr */
          BY tt_Ordre.pksdlLinjeSeq:

    ASSIGN
        iIntLinjeId = iIntLinjeId + 1.

    IF FIRST-OF(tt_Ordre.EkstId) THEN
    OPPRETT_HODE:
    DO:
        FIND butiker WHERE butiker.butik = tt_Ordre.Butik NO-LOCK.
        piCL = IF Butiker.clButikkNr = 0 THEN tt_Ordre.Butik ELSE Butiker.clButikkNr.

        /* Allt skal inn  - TN 30/11-11
        FIND PkSdlHode EXCLUSIVE-LOCK WHERE
            PkSdlHode.EkstId  = tt_Ordre.EkstId /* string(tt_Ordre.pksdlOrdreNr) */ AND
            PkSdlHode.PkSdlNr = string(tt_Ordre.pksdlNr) NO-ERROR.
        */
        IF AVAILABLE PkSdlHode THEN RELEASE PkSdlHode.
        
        IF NOT AVAILABLE PkSdlHode THEN
        DO:
            CREATE PkSdlHode.
            ASSIGN 
              PkSdlHode.EkstId         = tt_Ordre.EkstId /*string(tt_Ordre.pksdlOrdreNr)*/
              cEkstId                  = tt_Ordre.EkstId /* PkSdlHode.EkstId */
              PkSdlHode.PkSdlNr        = STRING(ipksdlNr)
              PkSdlHode.PkSdlStatus    = 10
              PkSdlHode.PkSdlOpphav    = 2 /* Import fra ERP */
              PkSdlHode.SendtDato      = TODAY 
              fPkSdlId                 = PkSdlHode.PkSdlId
              PkSdlHode.Merknad        = "Fra BxCentral " + STRING(PkSdlHode.SendtDato) + " " + tt_Ordre.pksdlTid + " av " + tt_Ordre.pksdlBruker
              PkSdlHode.CL             = piCL
              PkSdlHode.LevNr          = tt_Ordre.PkSdlLevNr
              PkSdlHode.LevNamn        = tt_Ordre.PkSdlLevNamn
              .
            /* Logger for opprettelse av ordre og bestilling. */  
            CREATE ttoPkSdlHode.
            BUFFER-COPY pkSdlHode TO ttoPkSdlHode.
        END.
        /* Pakkseddel er allerede innlest. Innlesning av brytes */
        ELSE DO:
            LEAVE PAKKSEDDEL.
        END.

        FIND LAST PkSdlLinje NO-LOCK
             WHERE PkSdlLinje.PkSdlId = fPkSdlId
             USE-INDEX PkSdlLinje NO-ERROR.
        fPkSdlLinjeId = IF AVAIL PkSdlLinje 
                          THEN PkSdlLinje.PkSdlLinjeId + 1 
                          ELSE 1.
    END. /* OPPRETT_HODE */

    /*
       = ipksdlLevert
    ttOrdre.pksdlOrdreNr  = ipksdlOrdreNr 
      = ipksdlLinjeNr /* Linjenummrering ved innlesning */
    */

    /* ----------------------------------------------- */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = tt_Ordre.ArtikkelNr NO-ERROR.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = tt_Ordre.butik NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        FIND Butiker WHERE Butiker.Butik = iCL NO-ERROR.

    CREATE PkSdlLinje.
    ASSIGN
        PkSdlLinje.PkSdlId       = fPkSdlId
        PkSdlLinje.PkSdlLinjeId  = iIntLinjeId
        .
    ASSIGN PkSdlLinje.ArtikkelNr    = tt_Ordre.ArtikkelNr
           PkSdlLinje.BestNr        = tt_Ordre.BestNr 
           PkSdlLinje.OrdreNr       = tt_Ordre.OrdreNr
           PkSdlLinje.Beskr         = tt_Ordre.Beskr
           PkSdlLinje.LevFargKod    = IF AVAILABLE ArtBas
                                        THEN ArtBas.LevFargKod
                                        ELSE ""
           PkSdlLinje.Antall        = tt_Ordre.Bestilt
           PkSdlLinje.AntLevert     = IF ArtBas.Pakke THEN 0 ELSE tt_Ordre.pksdlLevert
           PkSdlLinje.AntRest       = IF ArtBas.Pakke THEN 0 ELSE (PkSdlLinje.Antall - PkSdlLinje.AntLevert)
           PkSdlLinje.LevKod        = IF AVAILABLE ArtBas
                                        THEN ArtBas.LevKod
                                        ELSE ""
           PkSdlLinje.LevNr         = IF AVAILABLE Artbas
                                        THEN ArtBas.LevNr
                                        ELSE 0
           PkSdlLinje.StrKode       = tt_Ordre.StrKode
           PkSdlLinje.Kode          = tt_Ordre.Strekkode
           PkSdlLinje.Salgsenhet    = IF AVAILABLE ArtBas
                                        THEN ArtBas.SalgsEnhet
                                        ELSE ""
           PkSdlLinje.Linjenr       = tt_Ordre.pksdlLinjeNr
           PkSdlLinje.ButikkNr      = tt_Ordre.Butik
           PkSdlLinje.Pakke         = ArtBas.Pakke
           PkSdlLinje.PakkeNr       = ArtBas.PakkeNr
           fPkSdlLinjeId            = fPkSdlLinjeId + 1
           .      
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr
           AND ArtPris.ProfilNr   = Butiker.ProfilNr
         NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
        FIND FIRST ArtPris NO-LOCK
             WHERE ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.

    PRIS:
    DO:
      FIND PkSdlPris EXCLUSIVE-LOCK WHERE
          PkSdlPris.PkSdlId    = fPkSdlId AND
          PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE PkSdlPris THEN
      DO:
          CREATE PkSdlPris.
          ASSIGN
              PkSdlPris.PkSdlId        = fPkSdlId
              PkSdlPris.ArtikkelNr     = PkSdlLinje.ArtikkelNr
              .
          ASSIGN
              /*PkSdlPris.OverstyrPris   = bStdPrisOverf*/
              PkSdlPris.Beskr          = PkSdlLinje.Beskr
              PkSdlPris.LevKod         = PkSdlLinje.LevKod
              PkSdlPris.LevFargKod     = PkSdlLinje.LevFargKod

              /* Den nye prisen som kommer inn og bruker kan velge å overstyre med. */
              PkSdlPris.NyInnkjopsPris = tt_Ordre.InnkjopsPris
              /*PkSdlPris.NyRab1%        = (tt_Ordre.Rab1Kr * 100) / tt_Ordre.InnkjopsPris */
              PkSdlPris.NyRab1%          = tt_Ordre.Rab1%
              PkSdlPris.NyRab1%        = IF PkSdlPris.NyRab1% <> ?
                                           THEN PkSdlPris.NyRab1%
                                           ELSE 0
              PkSdlPris.NyFrakt        = tt_Ordre.Frakt
              PkSdlPris.NyVarekost     = PkSdlPris.NyInnkjopsPris - ((PkSdlPris.NyInnkjopspris * PkSdlPris.NyRab1%) / 100)
              PkSdlPris.NyPris         = tt_Ordre.Pris
              /* Beregner ny DB% */
              fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
              fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
              PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
              PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%

              /* Henter inn pris fra ordrebekreftelsen.                                          */
              /* TN 16/5-08 Her skal det hentes opp BestPris og info fra Bestpris skal benyttes. */
              PkSdlPris.InnkjopsPris   = tt_Ordre.InnkjopsPris
              PkSdlPris.Rab1%          = tt_Ordre.Rab1%
              PkSdlPris.Frakt          = tt_Ordre.Frakt
              PkSdlPris.Varekost       = PkSdlPris.InnkjopsPris - ((PkSdlPris.Innkjopspris * PkSdlPris.Rab1%) / 100)
              PkSdlPris.Pris           = tt_Ordre.Pris
              fMvaKr                   = PkSdlPris.Pris - (PkSdlPris.Pris / (1 + (ArtPris.Mva%[1] / 100)))
              fDbKr                    = PkSdlPris.Pris - fMvaKr - PkSdlPris.Varekost
              PkSdlPris.DB%            = ROUND((fDbKr * 100) / (PkSdlPris.Varekost + fDbKr),2)
              PkSdlPris.DB%            = IF PkSdlPris.DB% = ? THEN 0 ELSE PkSdlPris.DB%
              .

          IF ArtPris.Pris[1] = PkSdlPris.NyPris AND
              ArtPris.InnkjopsPris[1] = PkSdlPris.NyInnkjopsPris 
              THEN PkSdlPris.OverstyrPris = NO.
      END.

    END. /* PRIS */

    /* Her slipper vi recorden. Slik at DB trigger får slippe til. */
    IF AVAILABLE PkSdlHode  THEN RELEASE PkSdlHode.
    IF AVAILABLE PkSdlLinje THEN RELEASE PkSdlLinje.
    IF AVAILABLE PkSdlPris  THEN RELEASE PkSdlPris.

    IF FIRST-OF(tt_Ordre.EkstId) THEN
    OPPRETT_HODE:
    DO:
        IF NOT AVAILABLE PkSdlHode THEN
        FIND PkSdlHode EXCLUSIVE-LOCK WHERE
            PkSdlHode.EkstId  = tt_Ordre.EkstId /*string(tt_Ordre.pksdlOrdreNr)*/ AND
            PkSdlHode.PkSdlNr = string(tt_Ordre.pksdlNr) NO-ERROR.
        IF AVAILABLE PkSdlHode THEN
            PkSdlHode.PkSdlStatus = 10.
    END. /* OPPRETT_HODE */
  END. /* PAKKSEDDEL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-settLoggFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settLoggFil Procedure 
FUNCTION settLoggFil RETURNS CHARACTER
  ( OUTPUT pcLoggFil AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN
      pcLoggFil = "xPRSPkSdlInnles" + replace(STRING(TODAY),"/","-") + ".csv"
      .
  RETURN pcLoggFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

