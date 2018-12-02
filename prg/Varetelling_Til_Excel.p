/* Varetelling_Til_Excel.p           */
/* Eksport av varetelling til Excel. */

/* For Excel eksport */
{runlib.i}
{windows.i}
DEF STREAM Eksport.
DEF STREAM sExportFile.
{methodexcel.i}
/* Excel eksport slutt. */

  DEFINE INPUT PARAMETER iTelleNr AS INTEGER NO-UNDO.
  
  DEF VAR wtmpFileName AS CHAR NO-UNDO.
  DEF VAR hQuery       AS HANDLE NO-UNDO.
  DEFINE VARIABLE lOK AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iRapportValg AS INTEGER    NO-UNDO.
  DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
  DEF VAR cKunde           AS CHAR  NO-UNDO.
  DEF VAR cSkoTex          AS CHAR  NO-UNDO.
  DEFINE VARIABLE iCLProfilNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE lPris       AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
  DEFINE VARIABLE lTilbPris   AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
  
  DEFINE BUFFER bufTellelinje FOR TelleLinje.
  DEFINE BUFFER clButiker     FOR Butiker.
  
  FIND TelleHode NO-LOCK WHERE
    TelleHode.TelleNr = iTelleNr NO-ERROR.
  IF NOT AVAILABLE TelleHode THEN
    RETURN.
  {syspara.i 1 4   1 cExcEkstent}
  cExcEkstent = IF cExcEkstent = "" THEN "csv" ELSE cExcEkstent.
     
  {syspara.i 1 1 100 cKunde}
  {syspara.i 1 1 101 cSkoTex}
  
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = INTEGER(TelleHode.ButikkListe) NO-ERROR.
  FIND clButiker NO-LOCK WHERE
    clButiker.Butik = INTEGER(TelleHode.ButikkListe) NO-ERROR.
  iclProfilNr = clButiker.ProfilNr.
  
  RUN gvelgutskrtell.w (OUTPUT iRapportValg).
  IF iRapportValg = 0 THEN
    RETURN NO-APPLY.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(BUFFER bufTellelinje:HANDLE).
  hQuery:QUERY-PREPARE('FOR EACH bufTellelinje NO-LOCK WHERE bufTellelinje.tellenr = ' + STRING(TelleHode.tellenr)).
  hQuery:QUERY-OPEN().

  /* Henter temporært filnavn. */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle (INPUT "varetell", INPUT cExcEkstent, OUTPUT wtmpFileName).
  FIND Butiker WHERE Butiker.Butik = int(TelleHode.ButikkListe) NO-LOCK NO-ERROR.
  FIND TransType WHERE TransType.TTId = TelleHode.TTId NO-LOCK NO-ERROR.
  OUTPUT stream Eksport to value(wtmpFileName).

  {sww.i}
  EXPORT STREAM Eksport DELIMITER ";" "TELLING-" + IF AVAIL TransType THEN TransType.Beskrivels ELSE "???".
  EXPORT STREAM Eksport DELIMITER ";" "Butikk: " + IF AVAIL Butiker THEN Butiker.butnamn ELSE "???".
  EXPORT STREAM Eksport DELIMITER ";" " ".
  EXPORT STREAM Eksport DELIMITER ";"
    "Butikk"
    "Artikkelnr"
    "Vg/LøpNr"
    "Lev.art.Nr"
    "Varetekst"
    "Lev.farge"
    "Str"
    "AntPar"
    "AntTalt"
    (IF TelleHode.TTId = 8 THEN "Nedskrevet" ELSE "Salgsverdi")
    "VVAreKost"
    "Rabatt"
    "AntDiff"
    "VerdiDiff"
    "Pris"
    "Tilb.pris"
    "Oppdatert"
    "Strekkode"
    "OpprVerdi"
    "OpptaltVerdi"
    "Sesong"
    "Avd"
    "Avd beskr"
    "Hg"
    "Hg beskr"
    "Vg"
    "Vg beskr"
    "Levnr"
    "Leverandør"
    "EndretDato"
    "BrukerId".
  
  hQuery:GET-FIRST().

  TELLELINJE:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
       ASSIGN lOK = TRUE.
      /* iRapportValg: 10=alla,11=inga 0-0,20=bara negativ diff ,30=bara positiv diff */
      IF iRapportValg = 11 AND bufTellelinje.AntallPar = 0 AND bufTellelinje.AntallTalt = 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 20 AND bufTellelinje.AntallDiff >= 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 30 AND bufTellelinje.AntallDiff <= 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 40 AND bufTellelinje.AntallPar = bufTellelinje.AntallTalt THEN
          lOK = FALSE.
     
/*     /* Tar bare med endrede linjer. */                               */
/*     if (bufTellelinje.AntallDiff = 0 and bufTellelinje.VerdiDiff = 0) then */
/*       next TELLELINJE.                                               */
    IF lOK = TRUE THEN DO:
        ASSIGN
          lPris     = 0
          lTilbPris = 0.
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = bufTellelinje.ArtikkelNr NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
            FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
            FIND Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
                               ArtPris.ProfilNr   = Butiker.ProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAIL ArtPris THEN
                FIND Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                   ArtPris.ProfilNr   = iCLProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAIL ArtPris THEN
                FIND FIRST Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            RELEASE Avdeling.
            RELEASE HuvGr.
            RELEASE VarGr.
        END.
        IF TelleHode.TTId <> 8 THEN DO:
            IF AVAILABLE ArtPris THEN
                ASSIGN
                lPris = (ArtPris.Pris[1])
                lTilbPris = (ArtPris.Pris[2])
                .
            ELSE
                ASSIGN
                    lPris = 0
                    lTilbPris = 0
                    .
        END.
        EXPORT STREAM Eksport DELIMITER ";"
          (IF AVAIL butiker THEN butiker.butik ELSE 0)
          bufTellelinje.ArtikkelNr
          bufTellelinje.VgLopNr
          bufTellelinje.LevKod
          (IF AVAILABLE ArtBas
             THEN ArtBas.Beskr
             ELSE "")
          (IF AVAILABLE ArtBas
             THEN ArtBas.LevFargKod
             ELSE "")
          bufTellelinje.Storl
          bufTellelinje.AntallPar
          bufTellelinje.AntallTalt
          (IF TelleHode.TTId = 8 THEN bufTellelinje.Nedskrevet ELSE 
              IF AVAIL ArtPris THEN
              bufTellelinje.AntallTalt * Artpris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] ELSE ?)
          bufTellelinje.VVareKost
          bufTellelinje.RabKr
          bufTellelinje.AntallDiff
          bufTellelinje.VerdiDiff
          lPris
          lTilbPris
          STRING(bufTellelinje.Oppdatert,"Ja/Nei")
          bufTellelinje.Kode
          bufTellelinje.OpprVerdi
          bufTellelinje.OpptVerdi
          (IF AVAILABLE ArtBas THEN ArtBas.SaSong ELSE 0)
            (IF AVAILABLE Avdeling
                 THEN Avdeling.AvdelingNr
                 ELSE 0)
           (IF AVAILABLE Avdeling
                 THEN Avdeling.AvdelingNavn
                 ELSE "")
            (IF AVAILABLE HuvGr
                 THEN HuvGr.Hg
                 ELSE 0)
           (IF AVAILABLE HuvGr
                 THEN HuvGr.HgBeskr
                 ELSE "")
            (IF AVAILABLE VarGr
                 THEN VarGr.Vg
                 ELSE 0)
           (IF AVAILABLE VarGr
                 THEN VarGr.VgBeskr
                 ELSE "")
          bufTellelinje.LevNr
          (IF AVAILABLE LevBas
             THEN LevBas.LevNamn
             ELSE "")
          bufTellelinje.EDato
          bufTellelinje.BrukerId
          .
    END.
     hQuery:GET-NEXT().
  END. /* TELLELINJE */
  {swn.i}
  OUTPUT stream Eksport close.
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  hQuery = ?.
  
  IF VALID-HANDLE(wLibHandle) THEN
      RUN OpenExcelDocument IN wLibHandle (wtmpFileName, " ").
