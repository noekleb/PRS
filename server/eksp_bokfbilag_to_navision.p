/* Utlegg av bokføringsbilag til navision.
   Rutinen er avhengig av et forprogram som putter temp-table som skal eksporteres.
   Parametere:   Bokf.nr i parametersteng og temp-tabell med feltene i som er nødvendige eller 
              eller
                 Liste over rowid's med bokføringsbilag i parameterstreng:
                   <ButikkNr>,<"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over Bokføringsnr i parameterstreng:
                   <ButikkNr>,<"ARTNR">,<BokfNr,BokfNr..>
   
   TN 28/11-05               
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR iNumErrors  AS INT NO-UNDO.
DEF VAR dDeci       AS DEC NO-UNDO.
DEF VAR iTelleNr    AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.

DEF VAR iCl         AS INT NO-UNDO.
DEF VAR wVVAreKost  AS DEC NO-UNDO.
DEF VAR wStorl      AS CHAR NO-UNDO.
DEF VAR wTabell     AS CHAR NO-UNDO INIT "ArtBas".
DEF VAR wNedskriv   AS LOG NO-UNDO.
DEF VAR wTekst      AS CHAR NO-UNDO.
DEF VAR wEDB-System AS CHAR NO-UNDO.

{runlib.i}

DEF BUFFER     clButiker FOR Butiker.
DEF TEMP-TABLE TT_ArtLag NO-UNDO LIKE ArtLag
    FIELD Kode AS CHAR FORMAT "x(25)".

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:ADD-LIKE-FIELD("Vg","ArtBas.Vg").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
  ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
END.

iTelleNr = INT(ENTRY(1,icParam)).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

FIND TelleHode 
     WHERE TelleHode.TelleNr = INT(iTelleNr)
     NO-LOCK NO-ERROR.
IF NOT AVAIL TelleHode THEN DO:
  ocReturn = "Ugyldig TelleNr: " + STRING(iTelleNr).
  RETURN.
END.

{syspara.i 5 1 1 iCL INT}
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  ocReturn = "Finner ikke sentral-lager: " + STRING(iCL).
  RETURN.
END.
{syspara.i 4 1 2 wTekst}
IF TelleHode.TTId = INT(wTekst) THEN 
  wNedskriv = TRUE.
ELSE 
  wNedskriv = FALSE.

{syspara.i 1 2 4 wEDB-System}

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND ArtBas
       WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
       NO-LOCK NO-ERROR.
  IF AVAIL ArtBas 
     /*AND ArtBas.LopNr NE ? */
     AND Artbas.Lager = TRUE
     AND NOT ArtBas.pakke
     AND NOT ArtBas.OPris
     THEN DO:

    FIND Farg OF ArtBas NO-LOCK NO-ERROR.

    /* Legger opp artikkelen i listen for alle valgte butikker. */
    BUTIKKER:
    DO ix = 1 TO NUM-ENTRIES(TelleHode.ButikkListe) TRANSACTION: /* vi har bara 1 butik */

      FIND Butiker 
           WHERE Butiker.Butik = INT(ENTRY(ix,TelleHode.ButikkListe))
           NO-LOCK NO-ERROR.
      IF NOT AVAIL Butiker THEN NEXT BUTIKKER.

      /* Skal det oppdateres som varesalg, må pris hentes. */
      IF TelleHode.TTId = 1 THEN DO:
        FIND ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
             ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

        /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
        IF NOT AVAILABLE ArtPris THEN
          FIND ArtPris NO-LOCK WHERE
               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
               ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN
          wVVAreKost = ArtPris.Pris[if ArtPris.Tilbud THEN 2 else 1].
      END.

      /* Här skapar vi TT_ArtLag från artikkelens ArtLag           */
      /* I tillägg lägger vi till ev. strl som inte har artlag     */
      /* Detta löser problemet när vi skall ta ut ikke lagerstyrda */
      EMPTY TEMP-TABLE TT_ArtLag.
      FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                    ArtLag.Butik = INT(ENTRY(ix,TelleHode.ButikkListe)).
        BUFFER-COPY ArtLag TO TT_ArtLag.
        RELEASE TT_ArtLag.
      END.

      FOR EACH StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND
                                         StrekKode.StrKode  > 0:
        FIND StrKonv WHERE StrKonv.StrKode = StrekKode.StrKode NO-LOCK NO-ERROR.
        IF AVAIL StrKonv THEN DO:
          ASSIGN wStorl = StrKonv.Storl.
          IF AVAILABLE ArtBas THEN DO:
/*             RUN StrTypeSjekk in wLibHandle (wStorl,ArtBas.StrTypeId). */
/*             IF RETURN-VALUE <> "AVBRYT" THEN DO:                      */
              FIND TT_ArtLag WHERE TT_ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                   TT_ArtLag.storl = wStorl       AND                           
                                   TT_ArtLag.Butik = INT(ENTRY(ix,TelleHode.ButikkListe)) NO-ERROR.
              IF NOT AVAIL TT_Artlag THEN DO ON ERROR UNDO, LEAVE:
                CREATE TT_ArtLag.
                ASSIGN 
                       TT_ArtLag.ArtikkelNr = StrekKode.ArtikkelNr
                       TT_ArtLag.Vg         = ArtBas.Vg
                       TT_ArtLag.LopNr      = ArtBas.Lopnr
                       TT_ArtLag.storl      = wStorl
                       TT_ArtLag.Butik      = INT(ENTRY(ix,TelleHode.ButikkListe))
                       TT_ArtLag.StrKode    = StrekKode.StrKode
                       TT_ArtLag.Kode       = Strekkode.Kode NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                DO:
                    IF AVAILABLE TT_Artlag THEN
                        DELETE TT_Artlag.
                END.
                IF AVAILABLE TT_Artlag THEN
                    RELEASE TT_ArtLag.
              END.
          END.
        END.
      END.

      /* Supplerer de artikler som ikke har Artlag. */
      FOR EACH StrTStr NO-LOCK WHERE
          StrTStr.StrTypeId = ArtBas.StrTypeId:
          FIND FIRST StrKonv WHERE 
              StrKonv.Storl = StrTStr.SoStorl NO-LOCK NO-ERROR.
          IF AVAIL StrKonv THEN DO:
            ASSIGN wStorl = StrKonv.Storl.
            IF AVAILABLE ArtBas THEN DO:
                FIND TT_ArtLag WHERE TT_ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                     TT_ArtLag.storl = wStorl       AND                           
                                     TT_ArtLag.Butik = INT(ENTRY(ix,TelleHode.ButikkListe)) NO-ERROR.
                IF NOT AVAIL TT_Artlag THEN DO ON ERROR UNDO, LEAVE:
                  CREATE TT_ArtLag.
                  ASSIGN 
                         TT_ArtLag.ArtikkelNr = ArtBas.ArtikkelNr
                         TT_ArtLag.Vg         = ArtBas.Vg
                         TT_ArtLag.LopNr      = ArtBas.Lopnr
                         TT_ArtLag.storl      = wStorl
                         TT_ArtLag.Butik      = INT(ENTRY(ix,TelleHode.ButikkListe))
                         TT_ArtLag.StrKode    = StrKonv.StrKode
                         TT_ArtLag.Kode       = "" NO-ERROR.
                  IF ERROR-STATUS:ERROR = TRUE THEN
                  DO:
                      IF AVAILABLE TT_Artlag THEN
                          DELETE TT_Artlag.
                  END.
                  IF AVAILABLE TT_Artlag THEN
                      RELEASE TT_ArtLag.
                END.
            END. /* AVAILABLE ArtBas */
          END. /* AVAIL StrKonv */
      END. /* For each */

      /* Oppstandelsen - Oppstandelse pr. størrelse. */      
      ARTLAG:
      FOR EACH TT_ArtLag NO-LOCK WHERE
          TT_ArtLag.ArtikkelNr = ArtBas.ArtikkelNr and
          TT_ArtLag.Butik = INT(ENTRY(ix,TelleHode.ButikkListe))
          break by TT_ArtLag.ArtikkelNr
                by TT_ArtLag.Butik:

        /* Setter opp tellelås for butikken og henter lagerpost for butikken. */
        IF FIRST-OF(TT_ArtLag.Butik) THEN 
        DO:
          /* Setter VVareKost. */
          FIND Lager NO-LOCK WHERE
               Lager.ArtikkelNr = ArtBas.ArtikkelNr and
               Lager.Butik      = TT_ArtLag.Butik NO-ERROR.
          /* Setter varekost */
          IF TelleHode.TTId <> 1 THEN DO: /* Ikke for varekjøp */
            IF AVAILABLE Lager THEN
              wVVareKost = Lager.VVareKost.
            ELSE   
              wVVareKost = 0.

            IF wVVareKost = 0 THEN DO:
                FIND Butiker NO-LOCK WHERE
                    Butiker.Butik = TT_ArtLag.Butik NO-ERROR.
                IF NOT AVAILABLE Butiker THEN
                    FIND Butiker WHERE
                    Butiker.Butik = iCl NO-ERROR.
                FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                IF AVAILABLE ArtPris THEN
                    wVVareKost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].
          END.
        END.

          IF NOT CAN-FIND(FIRST KonvReg WHERE
             KonvReg.EDB-System = wEDB-System AND 
             KonvReg.Tabell     = wTabell     AND 
             KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr) + "," + 
                                  STRING(TT_ArtLag.Butik) AND  
             KonvReg.InterntId  = STRING(ArtBas.ArtikkelNr) + "," + 
                                  STRING(TT_ArtLag.Butik)) THEN DO:
            CREATE KonvReg.
            ASSIGN
              KonvReg.EDB-System = wEDB-System 
              KonvReg.Tabell     = wTabell     
              KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr) + "," + 
                                   STRING(TT_ArtLag.Butik)
              KonvReg.InterntId  = STRING(ArtBas.ArtikkelNr) + "," + 
                                   STRING(TT_ArtLag.Butik)
              KonvReg.DivTekst   = STRING(TelleHode.TelleNr).
          END. 
          /* TN 29/12-04 Denne kontrollen skal ikke gjøres.
          ELSE NEXT BUTIKKER. /* Artikkel er på telling allerede */
          */
        END.

        if wNedskriv THEN
          wStorl = "".
        ELSE
          wStorl = TT_ArtLag.Storl.

            /* Denne kontrollen skal ikke gjøres. TN 29/12-04.
        /* ArtikkelenLegges ikke opp hvis den finnes fra før. */
        IF wNedskriv = FALSE THEN DO:
          IF CAN-FIND(FIRST TelleLinje of TelleHode WHERE        
             TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr and
             TelleLinje.Butik      = INT(ENTRY(ix,TelleHode.ButikkListe)) and
             TelleLinje.Storl      = wStorl) THEN
            NEXT ARTLAG.  
          END.
        ELSE 
            */
        FIND TelleLinje of TelleHode exclusive-lock WHERE        
             TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr and
             TelleLinje.Butik      = INT(ENTRY(ix,TelleHode.ButikkListe)) and
             TelleLinje.Storl      = wStorl NO-ERROR.

        IF NOT AVAILABLE TelleLinje THEN 
        DO:
            CREATE TelleLinje.

            /* Setter index. */
            ASSIGN
              TelleLinje.TelleNr    = TelleHode.TelleNr 
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
              Tellelinje.Beskr      = ArtBas.Beskr
              TelleLinje.Vg         = ArtBas.Vg
              TelleLinje.LopNr      = ArtBas.LopNr
              TelleLinje.Butik      = INT(ENTRY(ix,TelleHode.ButikkListe))
              TelleLinje.Storl      = wStorl
              TelleLinje.AntallPar  = TT_ArtLag.Lagant
              TelleLinje.OpprVerdi  = TT_ArtLag.LagAnt * wVVareKost
              .
/*               wNyFLagg              = TRUE. */
        END.

        /* Øvrig informasjon. */
        ASSIGN
          TelleLinje.Vg         = ArtBas.Vg 
          TelleLinje.LopNr      = ArtBas.LopNr
          TelleLinje.LevKod     = ArtBas.LevKod
          TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> ""
                                    THEN ArtBas.LevFargKod
                                  ELSE IF AVAILABLE Farg
                                    THEN Farg.FarBeskr
                                  ELSE ""
          TelleLinje.VVareKost  = wVVAreKost
          TelleLinje.NedSkrevet = wVVAreKost
          TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
          TelleLinje.VerdiDiff  = (IF wNedskriv 
                                     THEN (TelleLinje.AntallDiff * (TelleLinje.VVareKost - TelleLinje.Nedskrevet))
                                     ELSE (TelleLinje.AntallDiff * TelleLinje.VVareKost)
                                  )
          TelleLinje.LevNr      = ArtBas.LevNr
          TelleLinje.Sasong     = ArtBas.SaSong
          TelleLinje.Farg       = ArtBas.Farg
          TelleLinje.MatKod     = ArtBas.MatKod
          TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg)) + "/" + TRIM(STRING(ArtBas.LopNr))
          TelleLinje.Kode       = TT_ArtLAg.Kode.
        /* Sjekk pris på nedskrivinger */
        IF wNedskriv THEN DO:
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
          IF TelleLinje.NedSkrevet = 0 and AVAILABLE ArtPris THEN DO:
            ASSIGN
              TelleLinje.VVareKost  = ArtPris.VareKost[if ArtPris.Tilbud 
                                                         THEN 2
                                                         ELSE 1]
              TelleLinje.NedSkrevet = ArtPris.VareKost[if ArtPris.Tilbud 
                                                         THEN 2
                                                         ELSE 1].
          END.
        END.

        RELEASE TelleLinje. /* Denne MÅ få stå */

      END. /* ARTLAG */

    END. /* BUTIKKER TRANSACTION */

  END.

  hQuery:GET-NEXT().
  IF iNumErrors > 20 THEN DO:
    ocReturn = "Ingen oppdatering ble utført pga for mange feil: " + CHR(10) + CHR(10) + ocReturn.
    UNDO, LEAVE.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

DELETE OBJECT hQuery.
