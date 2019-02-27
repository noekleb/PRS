&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR iProfilNr     AS INT  NO-UNDO.
DEF VAR iLinjeNr      AS INT  NO-UNDO.
DEF VAR iTid          AS INT  NO-UNDO.
DEF VAR iTelleNr      AS INT  NO-UNDO.
DEF VAR dDato         AS DATE NO-UNDO.
DEF VAR bOk           AS LOG  NO-UNDO.

DEF VAR bHk           AS LOG    NO-UNDO.
DEF VAR bStatus       AS LOG INITIAL TRUE NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT
  .
{windows.i}
{xppt880.i &NEW="new" 
           &SHARED="shared" 
           &FIELD="FIELD ErrFlag as log"}

DEF VAR lDec          AS DEC NO-UNDO.
DEF VAR piAntFeil     AS INT  NO-UNDO.
DEF VAR plEAN         AS DEC  NO-UNDO.
DEF VAR pcVareNavn    AS CHAR NO-UNDO.

DEF VAR piLoop1       AS INT  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

DEF STREAM Ut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue Procedure 
FUNCTION getValue RETURNS DATE
  ( INPUT pcField AS CHAR /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 23.19
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

FOR EACH tt_Error:
  DELETE tt_Error.
END.

/* Profilnr for sentrallager. */
FIND clButiker NO-LOCK WHERE
    clButiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = clButiker.ProfilNr
    iTid      = TIME
    bStatus   = TRUE
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

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT can-find(FIRST VPIDatasett WHERE 
                VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr) THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    RELEASE VPIDataSett.
END. /* TRANSACTION */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  RELEASE VPIDAtasett.
END. /* TRANSACTION */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.

STATUS DEFAULT "Bygger temp-tabell (tmpPDA)....".
/* Leser opp filen i temp tabell og gjør linjevalidering av alle felt. */
RUN ByggTmpTabell.

/* Sjekker om det er dubletter på linjene                         */
/* Hvis tidspunkt på alle linjene er lik 0, kobles kontrollen ut. */
RUN dublettKontroll.

/* Transaksjonstypekontroll */
/* Det er kun telletransaksjoner som skal komme inn foreløpig. Annet forkastes. */
run transtypeKontroll.

STATUS DEFAULT "Kjører registervalidering (tmpPDA)....".
/* Validerer filens innhold mot aktuelle registre. */
RUN Registervalidering.

/* Utpakking av VPI til VPI bufferet. */
IF CAN-FIND(FIRST tt_Error WHERE tt_Error.Gradering < 10) THEN
DO:
    STATUS DEFAULT "Markerer fil som har feilet...".
    RUN FeilVpi.
END.
ELSE  DO:
    STATUS DEFAULT "Pakker ut VPI (ttPriKat)....".
    RUN UtpakkVpi (OUTPUT bOk).
    /* Denne gir ALLTID feil ????
    IF bOk = FALSE THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            tt_Error.LinjeNr   = iLinjeNr
            tt_Error.Tekst     = "* Feil ved utpakking av fil." 
            tt_Error.Gradering = 1
            .
        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
    END.
    */
END.

IF CAN-FIND(FIRST tt_Error) THEN
DO:
    RUN ErrorLogg.
    RETURN "ERROR".
END.
ELSE
    RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabell Procedure 
PROCEDURE ByggTmpTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
DEFINE {&NEW} {&SHARED} TEMP-TABLE tmpPDA
    FIELD Butnr      AS INT  FORMAT ">>>>>9"
    FIELD Ean        AS DEC  FORMAT "->>>>>>>>>>>>9"
    FIELD Dato       AS DATE FORMAT "99/99-99"
    FIELD Tid        AS INT  FORMAT ">>>>9"
    FIELD Loggnr     AS INT  FORMAT ">>>>>9"
    FIELD Transtype  AS INT  FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(30)"
    FIELD Brukerid   AS CHAR FORMAT "x(12)"
    FIELD Antall     AS DEC  FORMAT "->>,>>9.999"   
    FIELD Kostpris   AS DEC  FORMAT "->>>,>>9.99"   
    FIELD Salgssum   AS DEC  FORMAT "->,>>>,>>9.99" 
    FIELD Nylagant   AS DEC  FORMAT "->>>,>>9.999"  
    FIELD Gmlagant   AS DEC  FORMAT "->>>,>>9.999"  

------------------------------------------------------------------------------*/
/* Tømmer feillogg */
FOR EACH tt_Error:
    DELETE tt_error.
END.

/* Tømmer temp-tabell hvis den inneholder noe */
FOR EACH tmpPDA:
    DELETE tmpPDA.
END.

iLinjeNr = 0.
LESLINJER:
FOR EACH VPIFilLinje NO-LOCK WHERE 
    VPIFilLinje.FilId = VPIFilHode.FilId:

    STATUS DEFAULT "Bygger temp-tabell - FilId/FilLinje: " + string(VPIFilLinje.FilId) + "/" + STRING(VPIFilLinje.LinjeNr) + ".".

    CREATE tmpPDA.
    ASSIGN
        iLinjeNr          = iLinjeNr + 1
        tmpPDA.Butnr      = int(trim(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Ean        = dec(trim(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Dato       = date(trim(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Tid        = int(trim(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Loggnr     = int(trim(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Transtype  = int(trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Transtekst = trim(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"')
        tmpPDA.Brukerid   = trim(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"')
        tmpPDA.Antall     = dec(trim(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Kostpris   = dec(trim(ENTRY(10,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Salgssum   = dec(trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Nylagant   = dec(trim(ENTRY(12,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.Gmlagant   = dec(trim(ENTRY(13,VPIFilLinje.StorTekst,";"),'"'))
        tmpPDA.LinjeNr    = INT(trim(ENTRY(14,VPIFilLinje.StorTekst,";"),'"'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            tt_Error.LinjeNr   = iLinjeNr
            tt_Error.Tekst     = "* Feil på linje (ByggTmpTabell)" + STRING(iLinjeNr) + "." 
            tt_Error.Gradering = 1.
        IF AVAILABLE tmpPDA THEN DELETE tmpPDA.
        NEXT LESLINJER.
    END.
END. /* LESLINJER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dublettKontroll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dublettKontroll Procedure 
PROCEDURE dublettKontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

/* Er ikke tidspunkt angitt på noen linjer, skippes kontrollen. */           
FIND FIRST tmpPDA WHERE
    tmpPDA.Tid > 0 NO-ERROR.
IF NOT AVAILABLE tmpPDA THEN
    RETURN.

FOR EACH tmpPDA WHERE
    CAN-FIND(Butiker WHERE
             Butiker.Butik = tmpPDA.ButNr)
    BREAK BY tmpPDA.ButNr
          BY tmpPDA.EAN
          BY tmpPDA.TransType
          BY tmpPDA.Dato
          BY tmpPDA.Tid:
    IF FIRST-OF(tmpPDA.Tid) THEN
        piLoop = 1.
    ELSE
        piLoop = piLoop + 1.
    IF piLoop > 1 THEN
    DUBLETT:
    DO:
        CREATE tt_Error.
        ASSIGN
            tmpPDA.ErrFlag     = TRUE
            tt_Error.LinjeNr   = tmpPDA.LinjeNr
            tt_Error.Gradering = 52 /* Gradering under 50 stopper opprettelse av lokasjonsliste. */
            tt_Error.Tekst     = "* Dubletter av EAN linjenr: " + 
                                 STRING(tmpPDA.LinjeNr) + " EAN: " +
                                 string(tmpPDA.EAN,"9999999999999") + " Dato: " +
                                 STRING(tmpPDA.Dato) + " Kl: " +
                                 STRING(tmpPDA.Tid,"HH:MM:SS") + " Antall: " + 
                                 STRING(tmpPDA.Antall)
            .
        /* Tar bort dublett. */
        delete tmpPDA.    
    END. /* DUBLETT */             
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFilnavn AS CHAR NO-UNDO.

  FIND FIRST tmpPDA NO-LOCK NO-ERROR.
  ASSIGN
      cFilnavn = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + VPIFilHode.FilNavn
                 + ".Txt".
  
  OUTPUT TO VALUE(cFilnavn).
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
      .
    IF AVAILABLE tmpPDA THEN DO:
        PUT UNFORMATTED
            "Lok.liste " STRING(tmpPDA.Dato) " " tmpPDA.BrukerId " " STRING(tmpPDA.Tid,"HH:MM:SS") skip(1).
    END.
    
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 1) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER (ErrorLogg) ***" SKIP
            "   Linjer som inneholder ugyldige verdier." SKIP
            "   Innlesning av filen er avbrutt fordi den inneholder linjer med ugyldige verdier." SKIP
            "   Filen må sees igjennom og rettes før den kan leses inn." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 1:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 3) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UGYLDIGE VERDIER (ErrorLogg 2) - FEIL VED OPPRETTELSE AV TELLELISTE ***" SKIP
            "   Den eller flere linjer inneholder ugyldige verdier." SKIP
            "   Klarte ikke å opprette telleliste." SKIP
            "   Filen må kontrolleres og rettes. Deretter kan den leses inn igjen." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 3:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 4) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENT BUTIKKNR (ErrorLogg 2) - FEIL VED OPPRETTELSE AV TELLELISTE ***" SKIP
            "   Den eller flere linjer inneholder ugyldige butikknr." SKIP
            "   Klarte ikke å opprette telleliste." SKIP
            "   Filen må kontrolleres og rettes. Deretter kan den leses inn igjen." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 4:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    
    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 50) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENTE EAN KODER ***" SKIP
            "   Det er oppdaget ukjente EAN koder i filen fra håndterminalen. Disse EAN kodene" SKIP
            "   er vanskelig å identifisere når man ikke har varen EAN koden kommer fra." SKIP
            "   Det er derfor å anbefale at man legger unna varer med ukjente EAN koder og " SKIP
            "   registrerer inn disse manuelt. Eventuelt prøver å slå opp i VPI registeret for " SKIP
            "   å se om de ligger der." SKIP
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 50:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 51) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** UKJENTE TRANSAKSJONSTYPER ***" SKIP
            "   Det er oppdaget ukjente transaksjonstyper i filen fra håndterminalen. Linjene" SKIP
            "   som inneholder ukjente transaksjonstyper leses ikke inn. Kun de linjene som " skip
            "   inneholder telletransaksjoner blir lest inn." skip
            "   (Normalt skyldes dette at det er innlest en varetran fil fra en av kassene)." skip
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 51:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.

    IF CAN-FIND(FIRST tt_Error WHERE
                tt_Error.Gradering = 52) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** DUBLETTER ***" SKIP
            "   Det ligger dubletter i filen." SKIP
            "   Håndterminalen har levert de samme data flere ganger inn i samme fil. " SKIP
            "   Kun de linjene som ikke er dubletter, er innlest. " skip
            "   Dubletter oppstår hvis ikke håndterminalen har nullstillt tellefil ved " skip
            "   tømming. Symbol PPT8800 terminalen feiler noen ganger på dette." skip
            "   **************************" SKIP(1)
            .
        FOR EACH tt_Error WHERE
            tt_Error.Gradering = 52:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_Error.
        END.
    END.
    
    IF CAN-FIND(FIRST tt_Error) THEN
    DO:
        PUT UNFORMATTED SKIP(1)
            "   *** ØVRIGE FEIL ***" SKIP(1)
            .
        FOR EACH tt_Error:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
          DELETE tt_error.
        END.
    END.

  OUTPUT CLOSE.
  IF SEARCH(cFilnavn) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilnavn),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FeilVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FeilVpi Procedure 
PROCEDURE FeilVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 9 /* Feil */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 9
      .

  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettLokasjonsliste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettLokasjonsliste Procedure 
PROCEDURE opprettLokasjonsliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
     DEFINE {&NEW} {&SHARED} TEMP-TABLE tmpPDA
    FIELD Butnr      AS INT  FORMAT ">>>>>9"
    FIELD Ean        AS DEC  FORMAT "->>>>>>>>>>>>9"
    FIELD Dato       AS DATE FORMAT "99/99-99"
    FIELD Tid        AS INT  FORMAT ">>>>9"
    FIELD Loggnr     AS INT  FORMAT ">>>>>9"
    FIELD Transtype  AS INT  FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(30)"
    FIELD Brukerid   AS CHAR FORMAT "x(12)"
    FIELD Antall     AS DEC  FORMAT "->>,>>9.999"   
    FIELD Kostpris   AS DEC  FORMAT "->>>,>>9.99"   
    FIELD Salgssum   AS DEC  FORMAT "->,>>>,>>9.99" 
    FIELD Nylagant   AS DEC  FORMAT "->>>,>>9.999"  
    FIELD Gmlagant   AS DEC  FORMAT "->>>,>>9.999"  

------------------------------------------------------------------------------*/
            
DEF OUTPUT PARAMETER piTelleNr AS INT NO-UNDO.
DEF OUTPUT PARAMETER bOk       AS LOG NO-UNDO.

DEF VAR piLoop      AS INT NO-UNDO.
DEF VAR piTransType AS INT NO-UNDO.

ASSIGN
    bOk = FALSE.

BLOKKEN:
DO TRANSACTION:
    TELLEHODE:
    DO WHILE TRUE:
      FIND LAST Tellehode USE-INDEX TelleHode NO-LOCK NO-ERROR.
      IF AVAILABLE TelleHode THEN
          piTelleNr = piTelleNr + 1.
      ELSE piTelleNr = 1.

      /* Sjekker om det finnes noen ledig ehull i serien. */
      IF piTelleNr > 99999999 THEN
      DO piLoop = 1 TO 99999999:
          IF NOT CAN-FIND(TelleHode WHERE
                          TelleHode.TelleNr = piLoop) THEN
          DO:
              piTelleNr = piLoop.
              LEAVE TELLEHODE.
          END.
          ELSE DO:
              piTelleNr = ?.
              LEAVE TELLEHODE.
          END.
      END.

      IF NOT CAN-FIND(TelleHode WHERE
                      TelleHode.TelleNr = piTelleNr) THEN
          LEAVE TELLEHODE.
    END. /* TELLEHODE */

    /* Setter transtype: */
    CASE tmpPDA.TransType:
        WHEN 1 THEN piTransType =  2. /* Brekkasje */
        WHEN 2 THEN piTransType = 11. /* Internt forbruk */
        WHEN 3 THEN piTransType = 10. /* Gjennkjøp/retur */
        WHEN 4 THEN piTransType =  4. /* Reklamasjon (Lager reklamasjon) */
        WHEN 5 THEN piTransType =  5. /* Varemottak */
        WHEN 6 THEN piTransType =  7. /* Lagerjustering */
        WHEN 7 THEN piTransType =  9. /* Svinn/varetelling */
        WHEN 8 THEN piTransType =  9. /* IKKE I BRUK */
        WHEN 9 THEN piTransType =  6. /* Internt salg/overføring */
        OTHERWISE piTransType        =  9. /* Default er Svinn */
    END CASE.

    CREATE TelleHode.
    ASSIGN
        TelleHode.TelleNr     = piTelleNr
        TelleHode.TelleType   = 2 /* Lokasjonsliste */
        TelleHode.TTID        = piTransType
        TelleHode.TBId        = 1
        TelleHode.StartDato   = TODAY
        TelleHode.ButikkListe = STRING(tmpPDA.Butnr)
        TelleHode.Beskrivelse = "Lok.liste " + 
                                tmpPDA.BrukerID + " " + 
                                (IF tmpPDA.Dato = ? THEN "?" ELSE string(tmpPDA.Dato)) + " " + 
                                STRING(tmpPDA.Tid,"HH:MM:SS")
        TelleHode.LokasjonsID = tmpPDA.Brukerid
        TelleHode.BrukerIdPD  = tmpPDA.BrukerID
        TelleHode.FilDatoPDA  = VPIFilHode.Dato
        TelleHode.FilTidPDA   = int(entry(1,VPIFilHode.Kl,":")) * 3600 +
                                int(entry(2,VPIFilHode.Kl,":")) * 60 +
                                int(entry(3,VPIFilHode.Kl,":")) * 1
        TelleHode.FilId       = VPIFilHode.FilId
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            bStatus            = FALSE
            tt_Error.LinjeNr   = iLinjeNr
            tt_Error.Tekst     = "* Feil på linje " + STRING(tmpPDA.LinjeNr) + "." 
            tt_Error.Gradering = 3.
    END.
    ELSE bOk = TRUE.
END. /* TRANSACTION */
IF AVAILABLE TelleHode THEN RELEASE TelleHode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-posterVaretelling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterVaretelling Procedure 
PROCEDURE posterVaretelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
        
  DEFINE {&NEW} {&SHARED} TEMP-TABLE tmpPDA
    FIELD Butnr      AS INT  FORMAT ">>>>>9"
    FIELD Ean        AS DEC  FORMAT "->>>>>>>>>>>>9"
    FIELD Dato       AS DATE FORMAT "99/99-99"
    FIELD Tid        AS INT  FORMAT ">>>>9"
    FIELD Loggnr     AS INT  FORMAT ">>>>>9"
    FIELD Transtype  AS INT  FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(30)"
    FIELD Brukerid   AS CHAR FORMAT "x(12)"
    FIELD Antall     AS DEC  FORMAT "->>,>>9.999"   
    FIELD Kostpris   AS DEC  FORMAT "->>>,>>9.99"   
    FIELD Salgssum   AS DEC  FORMAT "->,>>>,>>9.99" 
    FIELD Nylagant   AS DEC  FORMAT "->>>,>>9.999"  
    FIELD Gmlagant   AS DEC  FORMAT "->>>,>>9.999"  
    
    Tabellene
      tmpPDA og TelleHode er tilgjengelige her.

------------------------------------------------------------------------------*/

FIND Butiker NO-LOCK WHERE
    Butiker.butik = tmpPDA.ButNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN
    RETURN.
                                                         
LINJEPOSTERING:               
DO TRANSACTION:
  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = string(tmpPDA.Ean,"9999999999999") NO-ERROR.
  IF NOT AVAILABLE Strekkode THEN
      LEAVE LINJEPOSTERING.
  FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      LEAVE LINJEPOSTERING.
  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
  FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
      ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      LEAVE LINJEPOSTERING.
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
      Lager.Butik      = tmpPDA.ButNr NO-ERROR.
  FIND ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
      ArtLag.Butik      = tmpPDA.ButNr AND
      ArtLag.StrKode    = Strekkode.StrKode NO-ERROR.

  IF NOT AVAILABLE StrKonv THEN
      LEAVE LINJEPOSTERING.
  FIND StrTStr NO-LOCK WHERE
      StrTStr.StrTypeId = ArtBAs.StrTypeId AND
      StrTStr.SoStorl = StrKonv.Storl NO-ERROR.

  FIND TelleLinje EXCLUSIVE-LOCK WHERE
      TelleLinje.TelleNr        = iTelleNr AND
      TelleLinje.ArtikkelNr     = ArtBas.ArtikkelNr AND
      TelleLinje.Butik          = tmpPDA.ButNr AND
      TelleLinje.Storl          = StrKonv.Storl 
      NO-ERROR.
  IF NOT AVAILABLE TelleLinje THEN
  DO:
      CREATE TelleLinje.
      ASSIGN
          TelleLinje.TelleNr    = iTelleNr           
          TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr  
          TelleLinje.Butik      = tmpPDA.ButNr       
          TelleLinje.Storl      = StrKonv.Storl         
          .
  END.

  /* Henter varekost */
  IF AVAILABLE Lager THEN
      ASSIGN TelleLinje.VVareKost = Lager.VVareKost.
  IF (TelleLinje.VVarekost = 0 OR
      TelleLinje.VVareKost = ?) THEN
      TelleLinje.VVareKost = ArtPris.VareKost[1].
  IF TelleLinje.VVareKost = ? THEN
      TelleLinje.VVareKost = 0.

  IF AVAILABLE ArtLag THEN
      ASSIGN
      TelleLinje.AntallPar      = ArtLag.LagAnt
      TelleLinje.OpprVerdi      = ArtLag.LagAnt * TelleLinje.VVareKost
      .
  ASSIGN    
      TelleLinje.OpprAntalTalt  = TelleLinje.OpprAntalTalt + tmpPDA.Antall
      TelleLinje.AntallTalt     = TelleLinje.AntallTalt + tmpPDA.Antall
      TelleLinje.OpptVerdi      = TelleLinje.OpprAntalTalt * TelleLinje.VVareKost
      TelleLinje.AntallDiff     = TelleLinje.AntallPar - TelleLinje.AntallTalt
      TelleLinje.VerdiDiff      = TelleLinje.AntallDiff * TelleLinje.VVareKost

      TelleLinje.LevNr          = ArtBas.LevNr
      TelleLinje.Farg           = ArtBas.Farg
      TelleLinje.Sasong         = ArtBas.Sasong
      TelleLinje.MatKod         = ArtBas.MatKod
      TelleLinje.VgLopNr        = string(ArtBas.Vg) + "/" + 
                                   (IF ArtBas.LopNr = ? THEN "?" ELSE string(ArtBas.LopNr))
      TelleLinje.Beskr          = ArtBas.Beskr
      TelleLinje.SeqNr          = IF AVAILABLE StrTStr
                                    THEN StrTStr.SeqNr
                                    ELSE 0
      TelleLinje.LevFargKod     = ArtBas.LevFargKod
      TelleLinje.Vg             = ArtBas.Vg
      TelleLinje.LopNr          = ArtBas.LopNr
      TelleLinje.LevKod         = ArtBas.LevKod
      .

  /* Disse feltene oppdateres ikke her. 
      TelleLinje.AntallPar      
      TelleLinje.RabKr          
      TelleLinje.Kode           
      TelleLinje.Merknad        
      TelleLinje.Nedskrevet     
      TelleLinje.OpprVerdi      
      TelleLinje.VerdiDiff      
      TelleLinje.AntallDiff     
  */
      
  IF AVAILABLE TelleLinje THEN
      RELEASE TelleLinje.
END. /* LINJEPOSTERING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Registervalidering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registervalidering Procedure 
PROCEDURE Registervalidering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    DEFINE {&NEW} {&SHARED} TEMP-TABLE tmpPDA
    FIELD Butnr      AS INT  FORMAT ">>>>>9"
    FIELD Ean        AS DEC  FORMAT "->>>>>>>>>>>>9"
    FIELD Dato       AS DATE FORMAT "99/99-99"
    FIELD Tid        AS INT  FORMAT ">>>>9"
    FIELD Loggnr     AS INT  FORMAT ">>>>>9"
    FIELD Transtype  AS INT  FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(30)"
    FIELD Brukerid   AS CHAR FORMAT "x(12)"
    FIELD Antall     AS DEC  FORMAT "->>,>>9.999"   
    FIELD Kostpris   AS DEC  FORMAT "->>>,>>9.99"   
    FIELD Salgssum   AS DEC  FORMAT "->,>>>,>>9.99" 
    FIELD Nylagant   AS DEC  FORMAT "->>>,>>9.999"  
    FIELD Gmlagant   AS DEC  FORMAT "->>>,>>9.999"  
  
------------------------------------------------------------------------------*/
DEF VAR pcTekst AS CHAR NO-UNDO.

VALIDER:
FOR EACH tmpPDA NO-LOCK where
  tmpPDA.TransType = 7:
    STATUS DEFAULT "Registervalidering - fillinje: " + STRING(tmpPDA.LinjeNr) + ".".
    /* Kontroll av varegruppe */
    IF NOT CAN-FIND(Strekkode NO-LOCK WHERE
                    Strekkode.Kode = string(tmpPDA.EAN,"9999999999999")) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            tmpPDA.ErrFlag     = TRUE
            tt_Error.LinjeNr   = tmpPDA.LinjeNr
            tt_Error.Gradering = 50
            tt_Error.Tekst     = "* Ukjent EAN Linjenr: " + 
                                 STRING(tmpPDA.LinjeNr) + " EAN: " +
                                 string(tmpPDA.EAN,"9999999999999") + " Antall: " +
                                 STRING(tmpPDA.Antall)
            .
    END.

    /* Kontroll av varegruppe */
    IF NOT CAN-FIND(Butiker NO-LOCK WHERE
                    Butiker.Butik = tmpPDA.ButNr) THEN
    DO:
        CREATE tt_Error.
        ASSIGN
            tmpPDA.ErrFlag     = TRUE
            tt_Error.LinjeNr   = tmpPDA.LinjeNr
            tt_Error.Gradering = 4
            tt_Error.Tekst     = "* Ukjent Butikknr. Linjenr: " + 
                                 STRING(tmpPDA.LinjeNr) + " EAN: " +
                                 string(tmpPDA.ButNr) + " Antall: " +
                                 STRING(tmpPDA.Antall)
            .
    END.
END. /* VALIDER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transtypeKontroll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transtypeKontroll Procedure 
PROCEDURE transtypeKontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

FOR EACH tmpPDA /*WHERE
    CAN-FIND(Butiker WHERE
             Butiker.Butik = tmpPDA.ButNr) */:

    if not can-do('7',string(tmpPda.TransType)) then
    UGYLDIG_TRANSTYPE:
    DO:
        CREATE tt_Error.
        ASSIGN
            tmpPDA.ErrFlag     = TRUE
            tt_Error.LinjeNr   = tmpPDA.LinjeNr
            tt_Error.Gradering = 51 /* Gradering større eller lik 50 stopper ikke opprettelse av lokasjonsliste. */
            tt_Error.Tekst     = "* Linjer med ukjent transaksjonstype: " + 
                                 STRING(tmpPDA.LinjeNr) + " EAN: " +
                                 string(tmpPDA.EAN,"9999999999999") + " Transtype: " +
                                 string(tmpPda.Transtype) + " Dato: " + 
                                 STRING(tmpPDA.Dato) + " Kl: " +
                                 STRING(tmpPDA.Tid,"HH:MM:SS") + " Antall: " + 
                                 STRING(tmpPDA.Antall)
            .
    END. /* UGYLDIG_TRANSTYPE */ 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtPakkVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtPakkVpi Procedure 
PROCEDURE UtPakkVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER obOk AS LOG NO-UNDO.
                                                                 
DEF VAR piAntall    AS INT  NO-UNDO.
DEF VAR piTotAntall AS INT  NO-UNDO.
DEF VAR piAntFeil   AS INT NO-UNDO.
DEF VAR piLoop      AS INT  NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR cStrek      AS CHAR NO-UNDO.
DEF VAR ocReturn    AS CHAR NO-UNDO.

ASSIGN
    piLoop1 = 0
    cTekst  = "Starter utpakking av varefil.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Behandler linjene i filen. */
VPIFILLINJE:
FOR EACH tmpPDA where
  tmpPDA.TransType = 7
    BREAK BY tmpPDA.ButNr
          BY tmpPDA.TransType
          BY tmpPDA.LinjeNr: 
    
    piTotAntall = piTotAntall + 1.

    IF tmpPDA.ErrFlag = TRUE THEN
        piAntFeil = piAntFeil + 1.

    /* Oppretter lokasjonsliste som varelinjene skal posteres på. */
    IF FIRST-OF(tmpPDA.TransType) THEN DO:
        RUN opprettLokasjonsliste (OUTPUT iTelleNr, OUTPUT obOk).
        IF obOk = FALSE THEN
            RETURN.
        ELSE
            FIND TelleHode NO-LOCK WHERE
                TelleHode.TelleNr = iTelleNr.
    END.

    STATUS DEFAULT "Pakker ut varelinje: " + STRING(tmpPDA.LinjeNr) + ".".

    /* Behandler godkjente varelinjer */
    IF tmpPDA.ErrFlag = FALSE THEN
    POSTERLINJE:
    DO:
        ASSIGN piAntall = piAntall + 1.
        CASE tmpPDA.TransType:
            WHEN 7 THEN RUN posterVaretelling.
        END CASE.

    END. /* POSTERLINJE */

    /* Oppretter lokasjonsliste som varelinjene skal posteres på. */
    IF LAST-OF(tmpPDA.TransType) THEN DO:
        ASSIGN dDato = tmpPDA.Dato.
        RUN tellehode_oppdatersum.p (string(ROWID(TelleHode)),
                                     ?,
                                     "",
                                     OUTPUT ocReturn,
                                     OUTPUT obOk).
    END.


END. /* VPIFILLINJE */

IF bStatus = TRUE THEN
DO:
  DO TRANSACTION:
    FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
    ASSIGN
      VPIDatasett.DatasettStatus = 5 /* VPI mottatt og behandlet */
      .
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .
  END. /* TRANSACTION */
  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.
  bOk = TRUE.
END.
ELSE RUN FeilVpi.    
    

STATUS DEFAULT "".

ASSIGN
    cTekst = "Behandlet " + STRING(piTotAntall) + " Vareposter.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Av disse har " + STRING(piAntall) + " ukjent EAN kode.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av varefil ferdig.Tidsbruk " + STRING(TIME - iTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue Procedure 
FUNCTION getValue RETURNS DATE
  ( INPUT pcField AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN dDato.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

