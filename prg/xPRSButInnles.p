&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSButInnles.p
    Purpose     : Innlesning av data til butikkregister

    Syntax      : xPRSButInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i butikkregisteret. Setter opp standardverdier. 
                  Oppretter 2 kasser pr. butikk samt en kundeordre kasse.

    Author(s)   : Tom Nøkleby
    Created     : 27/07/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE BUFFER bButiker FOR Butiker.
DEFINE BUFFER bKasse   FOR Kasse.
DEFINE TEMP-TABLE ttButiker LIKE Butiker
  FIELD OpprettKasse AS LOG.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.
RUN PosterData.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

IF CAN-FIND(FIRST ttError) THEN
  RUN ErrorLogg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p ('PRSButImport', 'xPRSButInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 58 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 58 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    CREATE ttButiker.

    RUN AssignInt(1,OUTPUT ttButiker.Butik, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(20,OUTPUT ttButiker.ProfilNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(22,OUTPUT ttButiker.clButikkNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(38,OUTPUT ttButiker.FakturaLayout, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(39,OUTPUT ttButiker.FakturaKopi, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(40,OUTPUT ttButiker.FaktTekstNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(41,OUTPUT ttButiker.FGMomsKod, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(42,OUTPUT ttButiker.PGMomsKod, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(46,OUTPUT ttButiker.Plukkbutikk, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(47,OUTPUT ttButiker.PrioPlukket, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(49,OUTPUT ttButiker.Kampanje, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(50,OUTPUT ttButiker.VPI, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(52,OUTPUT ttButiker.KasseNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(53,OUTPUT ttButiker.BELayout, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(25,OUTPUT ttButiker.NedlagtDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(48,OUTPUT ttButiker.KommisjonsdatoStart, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    RUN AssignDec(33,OUTPUT ttButiker.StdVeksel, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(43,OUTPUT ttButiker.Fakturagebyr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(44,OUTPUT ttButiker.Purregebyr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(45,OUTPUT ttButiker.KundeNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    ASSIGN
        ttButiker.ButNamn           = ENTRY(2,pcLinje,';')
        ttButiker.KortNavn          = ENTRY(3,pcLinje,';')
        ttButiker.EksterntId        = ENTRY(4,pcLinje,';')
        ttButiker.ButFirmaNavn      = ENTRY(5,pcLinje,';')
        ttButiker.BuKon             = ENTRY(6,pcLinje,';')
        ttButiker.LevKontakt        = ENTRY(7,pcLinje,';')
        ttButiker.OrganisasjonsNr   = ENTRY(8,pcLinje,';')
        ttButiker.BuAdr             = ENTRY(9,pcLinje,';')
                                    /*= ENTRY(10,pcLinje,';')*/ /* Adresse2 finnes ikke i db */
        ttButiker.BuPoNr            = ENTRY(11,pcLinje,';')
        ttButiker.BuPAdr            = ENTRY(12,pcLinje,';')
        ttButiker.ButLand           = ENTRY(13,pcLinje,';')
        ttButiker.LevAdresse1       = ENTRY(14,pcLinje,';')
        ttButiker.LevAdresse2       = ENTRY(15,pcLinje,';')
        ttButiker.LevPostBoks       = ENTRY(16,pcLinje,';')
        ttButiker.LevPostNr         = ENTRY(17,pcLinje,';')
        ttButiker.BuTel             = ENTRY(18,pcLinje,';')

        ttButiker.Telefaks          = ENTRY(19,pcLinje,';')
        ttButiker.Sentrallager      = CAN-DO('Ja,Yes,J,1,True',ENTRY(20,pcLinje,';'))
        ttButiker.harButikksystem   = CAN-DO('Ja,Yes,J,1,True',ENTRY(23,pcLinje,';'))
        ttButiker.LANButikk         = CAN-DO('Ja,Yes,J,1,True',ENTRY(26,pcLinje,';'))
        ttButiker.MinusButikk       = CAN-DO('Ja,Yes,J,1,True',ENTRY(27,pcLinje,';'))
        ttButiker.VaarRef           = ENTRY(28,pcLinje,';')
        ttButiker.BankKonto         = ENTRY(29,pcLinje,';')
        ttButiker.PostGiro          = ENTRY(30,pcLinje,';')

        ttButiker.ePostAdresse      = ENTRY(31,pcLinje,';')
        ttButiker.URLAdresse        = ENTRY(32,pcLinje,';')
        ttButiker.FalckMedlNr       = ENTRY(34,pcLinje,';')
        ttButiker.DirFakturautskrift  = CAN-DO('Ja,Yes,J,1,True',ENTRY(35,pcLinje,';'))
        ttButiker.FaktKopiRappSkriver = CAN-DO('Ja,Yes,J,1,True',ENTRY(36,pcLinje,';'))
        ttButiker.IntFaktOverforing   = CAN-DO('Ja,Yes,J,1,True',ENTRY(37,pcLinje,';'))
        ttButiker.StatistikkOppdatering = CAN-DO('Ja,Yes,J,1,True',ENTRY(51,pcLinje,';'))
        ttButiker.EODRapporter          = CAN-DO('Ja,Yes,J,1,True',ENTRY(54,pcLinje,';'))
        ttButiker.EODBokforingsbilag    = CAN-DO('Ja,Yes,J,1,True',ENTRY(55,pcLinje,';'))
        ttButiker.EODFinansrapport      = CAN-DO('Ja,Yes,J,1,True',ENTRY(56,pcLinje,';'))
        ttButiker.EDOJournal            = CAN-DO('Ja,Yes,J,1,True',ENTRY(57,pcLinje,';'))
        ttButiker.OpprettKasse          = CAN-DO('Ja,Yes,J,1,True',ENTRY(58,pcLinje,';'))
        NO-ERROR.
    /* Det har vært feil i en eller flere kolonner */
    IF b2Ok = FALSE THEN 
      NEXT LESERLINJER.
      
    /* Validering og overstyringer */
    IF ttButiker.ProfilNr = 0 THEN 
    DO:
      FIND FIRST PrisProfil NO-LOCK NO-ERROR.
      IF AVAILABLE PrisProfil THEN 
        ttButiker.ProfilNr = PrisProfil.ProfilNr.
      ELSE 
        ttButiker.ProfilNr = 1.
    END.

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-PosterData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData Procedure
PROCEDURE PosterData:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  
  FOR EACH ttButiker TRANSACTION:
    FIND Butiker EXCLUSIVE-LOCK WHERE 
      Butiker.Butik = ttButiker.Butik NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
    DO:
      CREATE Butiker.
      ASSIGN
        Butiker.Butik = ttButiker.Butik.
    END.

    IF TRIM(ENTRY(20,pcLinje,';')) <> '' THEN Butiker.ProfilNr = ttButiker.ProfilNr.
    IF TRIM(ENTRY(22,pcLinje,';')) <> '' THEN Butiker.clButikkNr = ttButiker.clButikkNr. 
    IF TRIM(ENTRY(38,pcLinje,';')) <> '' THEN Butiker.FakturaLayout = ttButiker.FakturaLayout. 
    IF TRIM(ENTRY(39,pcLinje,';')) <> '' THEN Butiker.FakturaKopi = ttButiker.FakturaKopi. 
    IF TRIM(ENTRY(40,pcLinje,';')) <> '' THEN Butiker.FaktTekstNr = ttButiker.FaktTekstNr. 
    IF TRIM(ENTRY(41,pcLinje,';')) <> '' THEN Butiker.FGMomsKod = ttButiker.FGMomsKod. 
    IF TRIM(ENTRY(42,pcLinje,';')) <> '' THEN Butiker.PGMomsKod = ttButiker.PGMomsKod. 
    IF TRIM(ENTRY(46,pcLinje,';')) <> '' THEN Butiker.Plukkbutikk = ttButiker.Plukkbutikk. 
    IF TRIM(ENTRY(47,pcLinje,';')) <> '' THEN Butiker.PrioPlukket = ttButiker.PrioPlukket. 
    IF TRIM(ENTRY(49,pcLinje,';')) <> '' THEN Butiker.Kampanje = ttButiker.Kampanje. 
    IF TRIM(ENTRY(50,pcLinje,';')) <> '' THEN Butiker.VPI = ttButiker.VPI. 
    IF TRIM(ENTRY(52,pcLinje,';')) <> '' THEN Butiker.KasseNr = ttButiker.KasseNr. 
    IF TRIM(ENTRY(53,pcLinje,';')) <> '' THEN Butiker.BELayout = ttButiker.BELayout. 

    IF TRIM(ENTRY(24,pcLinje,';')) <> '' THEN Butiker.ApningsDato = ttButiker.ApningsDato. 
    IF TRIM(ENTRY(25,pcLinje,';')) <> '' THEN Butiker.NedlagtDato = ttButiker.NedlagtDato. 
    IF TRIM(ENTRY(48,pcLinje,';')) <> '' THEN Butiker.KommisjonsdatoStart = ttButiker.KommisjonsdatoStart. 

    IF TRIM(ENTRY(33,pcLinje,';')) <> '' THEN Butiker.StdVeksel = ttButiker.StdVeksel. 
    IF TRIM(ENTRY(43,pcLinje,';')) <> '' THEN Butiker.Fakturagebyr = ttButiker.Fakturagebyr. 
    IF TRIM(ENTRY(44,pcLinje,';')) <> '' THEN Butiker.Purregebyr = ttButiker.Purregebyr. 
    IF TRIM(ENTRY(45,pcLinje,';')) <> '' THEN Butiker.KundeNr = ttButiker.KundeNr. 

    IF TRIM(ENTRY(2,pcLinje,';')) <> '' THEN Butiker.ButNamn = ttButiker.ButNamn. 
    IF TRIM(ENTRY(3,pcLinje,';')) <> '' THEN Butiker.KortNavn = ttButiker.KortNavn. 
    IF TRIM(ENTRY(4,pcLinje,';')) <> '' THEN Butiker.EksterntId = ttButiker.EksterntId. 
    IF TRIM(ENTRY(5,pcLinje,';')) <> '' THEN Butiker.ButFirmaNavn = ttButiker.ButFirmaNavn. 
    IF TRIM(ENTRY(6,pcLinje,';')) <> '' THEN Butiker.BuKon = ttButiker.BuKon. 

    IF TRIM(ENTRY(7,pcLinje,';')) <> '' THEN Butiker.LevKontakt = ttButiker.LevKontakt. 
    IF TRIM(ENTRY(8,pcLinje,';')) <> '' THEN Butiker.OrganisasjonsNr = ttButiker.OrganisasjonsNr. 
    IF TRIM(ENTRY(9,pcLinje,';')) <> '' THEN Butiker.BuAdr = ttButiker.BuAdr. 
    IF TRIM(ENTRY(11,pcLinje,';')) <> '' THEN Butiker.BuPoNr = ttButiker.BuPoNr. 

    IF TRIM(ENTRY(12,pcLinje,';')) <> '' THEN Butiker.BuPAdr = ttButiker.BuPAdr. 
    IF TRIM(ENTRY(13,pcLinje,';')) <> '' THEN Butiker.ButLand = ttButiker.ButLand. 
    IF TRIM(ENTRY(14,pcLinje,';')) <> '' THEN Butiker.LevAdresse1 = ttButiker.LevAdresse1. 
    IF TRIM(ENTRY(15,pcLinje,';')) <> '' THEN Butiker.LevAdresse2 = ttButiker.LevAdresse2. 
    IF TRIM(ENTRY(16,pcLinje,';')) <> '' THEN Butiker.LevPostBoks = ttButiker.LevPostBoks. 

    IF TRIM(ENTRY(17,pcLinje,';')) <> '' THEN Butiker.LevPostNr = ttButiker.LevPostNr. 
    IF TRIM(ENTRY(18,pcLinje,';')) <> '' THEN Butiker.BuTel = ttButiker.BuTel. 
    IF TRIM(ENTRY(19,pcLinje,';')) <> '' THEN Butiker.Telefaks = ttButiker.Telefaks. 
    IF TRIM(ENTRY(20,pcLinje,';')) <> '' THEN Butiker.Sentrallager = ttButiker.Sentrallager. 
    IF TRIM(ENTRY(23,pcLinje,';')) <> '' THEN Butiker.harButikksystem = ttButiker.harButikksystem. 
        
    IF TRIM(ENTRY(26,pcLinje,';')) <> '' THEN Butiker.LANButikk = ttButiker.LANButikk. 
    IF TRIM(ENTRY(27,pcLinje,';')) <> '' THEN Butiker.MinusButikk = ttButiker.MinusButikk. 
    IF TRIM(ENTRY(28,pcLinje,';')) <> '' THEN Butiker.VaarRef = ttButiker.VaarRef. 
    IF TRIM(ENTRY(29,pcLinje,';')) <> '' THEN Butiker.BankKonto = ttButiker.BankKonto. 
    IF TRIM(ENTRY(30,pcLinje,';')) <> '' THEN Butiker.PostGiro = ttButiker.PostGiro. 

    IF TRIM(ENTRY(31,pcLinje,';')) <> '' THEN Butiker.ePostAdresse = ttButiker.ePostAdresse. 
    IF TRIM(ENTRY(32,pcLinje,';')) <> '' THEN Butiker.URLAdresse = ttButiker.URLAdresse. 
    IF TRIM(ENTRY(34,pcLinje,';')) <> '' THEN Butiker.FalckMedlNr = ttButiker.FalckMedlNr. 
    IF TRIM(ENTRY(35,pcLinje,';')) <> '' THEN Butiker.DirFakturautskrift = ttButiker.DirFakturautskrift. 
    IF TRIM(ENTRY(36,pcLinje,';')) <> '' THEN Butiker.FaktKopiRappSkriver = ttButiker.FaktKopiRappSkriver. 

    IF TRIM(ENTRY(37,pcLinje,';')) <> '' THEN Butiker.IntFaktOverforing = ttButiker.IntFaktOverforing. 
    IF TRIM(ENTRY(51,pcLinje,';')) <> '' THEN Butiker.StatistikkOppdatering = ttButiker.StatistikkOppdatering. 
    IF TRIM(ENTRY(54,pcLinje,';')) <> '' THEN Butiker.EODRapporter = ttButiker.EODRapporter. 
    IF TRIM(ENTRY(55,pcLinje,';')) <> '' THEN Butiker.EODBokforingsbilag = ttButiker.EODBokforingsbilag. 
    IF TRIM(ENTRY(56,pcLinje,';')) <> '' THEN Butiker.EODFinansrapport = ttButiker.EODFinansrapport. 

    IF TRIM(ENTRY(57,pcLinje,';')) <> '' THEN Butiker.EDOJournal = ttButiker.EDOJournal. 
    
    /* Default prisprofil. */
    IF Butiker.ProfilNr = 0 THEN
    DO:
      FIND FIRST PrisProfil NO-LOCK NO-ERROR.
      IF AVAILABLE PrisProfil
        THEN Butiker.ProfilNr = PrisProfil.ProfilNr.
      ELSE Butiker.ProfilNr = 1.
    END.
    
    RELEASE Butiker.
    
    /* Oppretter kasser */
    IF ttButiker.OpprettKasse THEN 
    DO:
      IF NOT CAN-FIND(FIRST Kasse WHERE 
                            Kasse.ButikkNr = ttButiker.Butik AND 
                            Kasse.GruppeNr = 1 AND 
                            Kasse.KasseNr  < 99) THEN 
      KASSE:
      DO:
        CREATE Kasse.
        ASSIGN
            Kasse.ButikkNr          = ttButiker.Butik
            Kasse.GruppeNr          = 1
            Kasse.KasseNr           = 1
            Kasse.Navn              = "PRS POS 1"
            Kasse.ModellNr          = 5
            Kasse.FakturaLayout     = 1
            Kasse.ElJournal[1]      = 'PRSJou'
            Kasse.ElJournalOperand  = 2
            Kasse.ElJournal[2]      = STRING(ttButiker.Butik)
            Kasse.ElJournalId       = '*'
            Kasse.ElJournalKatalog  = '.\kom\in'
            Kasse.ElJournalKonv     = FALSE 
            Kasse.ElJournalAktiv    = TRUE
            Kasse.ElJournalInnles   = 'xinnprseljournal'
            Kasse.ElJournalBehandle = 'dummy'
            Kasse.FakturaKopi       = 3
            Kasse.Aktiv             = TRUE 
            Kasse.ElJournalId       = STRING(ttButiker.Butik) + ";1"
            .
                
        /* Standard kasse for kundeordre */
        IF NOT CAN-FIND(bKasse WHERE
                        bKasse.ButikkNr = Kasse.ButikkNr AND
                        bKasse.GruppeNr = 1 AND
                        bKasse.KasseNr  = 99) THEN
        DO:
          CREATE bKasse.
          BUFFER-COPY Kasse TO bKasse
          ASSIGN
              bKasse.KasseNr     = 99
              bKasse.Navn        = "Kasse 99 Kundeordre"
              bKasse.ElJournalId = STRING(ttButiker.Butik) + ";99"
              bKasse.ElJournal[1]      = ''
              bKasse.ElJournalOperand  = 2
              bKasse.ElJournal[2]      = ''
              bKasse.ElJournalId       = ''
              bKasse.ElJournalKatalog  = ''
              bKasse.Aktiv             = TRUE 
              bKasse.ElJournalKonv     = FALSE 
              bKasse.ElJournalAktiv    = TRUE  
              bKasse.ElJournalInnles   = ''
              bKasse.ElJournalBehandle = ''
              .
          RELEASE bKasse.
        END.
        RELEASE Kasse.
      END. /* KASSE */
    END.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

