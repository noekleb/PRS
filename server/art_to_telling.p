/* Overføring av artikler til varetelling 
   Parametere:   TelleNr i parametersteng og temp-tabell med feltene Artikkelnr og Vg eller 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <Tellenr>,<"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <Tellenr>,<"ARTNR">,<Artnr1,Artnr2..>
   
   Opprettet: 29.07.04 av BHa. Kode er sakset fra prosedyre ByggUtvalg i d-byggtelleliste.w                
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
DEFINE VARIABLE cIdListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE iakttid AS INTEGER    NO-UNDO.
DEFINE VARIABLE cKritListe AS CHARACTER NO-UNDO.

{runlib.i}

DEF BUFFER     clButiker FOR Butiker.
DEF TEMP-TABLE TT_ArtLag NO-UNDO 
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Vg         LIKE ArtBas.Vg
    FIELD LopNr      AS INT FORMAT ">>>>>9"
    FIELD storl      LIKE StrKonv.Storl
    FIELD Butik      LIKE Butiker.Butik
    FIELD StrKode    LIKE Strekkode.StrKode 
    FIELD Lagant     LIKE ArtLag.Lagant
    FIELD Kode AS CHAR FORMAT "x(25)".
iakttid = TIME.

iTelleNr = INT(ENTRY(1,icParam,'|')).

/* ID liste */
IF NUM-ENTRIES(icParam,'|') > 1 THEN 
  ASSIGN 
    cIdListe  = ENTRY(2,icParam,'|').
ELSE 
    cIdListe = ''.

IF NUM-ENTRIES(icParam,'|') > 2 THEN 
  ASSIGN 
    cKritListe  = ENTRY(3,icParam,'|').
ELSE 
    cKritListe = ''.

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
FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR.
IF NOT AVAIL Butiker THEN DO:
    ocReturn = "Finner ikke butikk: " + TelleHode.ButikkListe.
    RETURN.
END.
{syspara.i 4 1 2 wTekst}
IF TelleHode.TTId = INT(wTekst) THEN 
  wNedskriv = TRUE.
ELSE 
  wNedskriv = FALSE.
  
  IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(cIdListe) > 1 AND cIdListe <> '' THEN DO:
      /* Her skal hele ArtBas legges inn i listen. Derfor peker vi på tabellen ArtBas. */
      IF ENTRY(1,cIdListe) = "ALLE" THEN 
      DO:
          ihBuffer = BUFFER ArtBas:HANDLE.
      END.
      /* Her skal ale artikler i en eller flere varegrupper inn i listen */
      ELSE IF ENTRY(1,cIdListe) = "VG" THEN 
      DO:
          CREATE TEMP-TABLE httTable.
          httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
          httTable:ADD-LIKE-FIELD("Vg","ArtBas.Vg").
          httTable:TEMP-TABLE-PREPARE("ttArtBas").
          ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
          
          DO ix = 1 TO NUM-ENTRIES(cKritListe):
            FOR EACH ArtBas WHERE ArtBas.Vg = INT(ENTRY(ix,cKritListe)) NO-LOCK:
              IF AVAIL ArtBas THEN 
              DO:
                ihBuffer:BUFFER-CREATE().
                ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
            END.
          END.
      END.
      /* Her skal alle artiklene på en eller flere leverandører inn */
      ELSE IF ENTRY(1,cIdListe) = "LEV" THEN 
      DO:
          CREATE TEMP-TABLE httTable.
          httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
          httTable:ADD-LIKE-FIELD("Vg","ArtBas.Vg").
          httTable:TEMP-TABLE-PREPARE("ttArtBas").
          ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
          
          DO ix = 1 TO NUM-ENTRIES(cKritListe):
            FOR EACH ArtBas WHERE ArtBas.LevNr = INT(ENTRY(ix,cKritListe)) NO-LOCK:
              IF AVAIL ArtBas THEN 
              DO:
                ihBuffer:BUFFER-CREATE().
                ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
            END.
          END.
      END.      
      /* Her skal artiklene som ligger i listen inn */
      ELSE DO:
          CREATE TEMP-TABLE httTable.
          httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
          httTable:ADD-LIKE-FIELD("Vg","ArtBas.Vg").
          httTable:TEMP-TABLE-PREPARE("ttArtBas").
          ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
          IF ENTRY(1,cIdListe) = "ROWID" THEN
            DO ix = 2 TO NUM-ENTRIES(cIdListe):
              FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,cIdListe)) NO-LOCK NO-ERROR.
              IF AVAIL ArtBas THEN DO:
                ihBuffer:BUFFER-CREATE().
                ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
            END.
          ELSE
            DO ix = 1 TO NUM-ENTRIES(cIdListe):
              FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,cIdListe)) NO-LOCK NO-ERROR.
              IF AVAIL ArtBas THEN DO:
                ihBuffer:BUFFER-CREATE().
                ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
            END.
      END.
  END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


{syspara.i 1 2 4 wEDB-System}
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:


  FIND ArtBas
       WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
       NO-LOCK NO-ERROR.

  IF AVAIL ArtBas 
     /*AND ArtBas.LopNr NE ? */
     /*AND Artbas.Lager = TRUE */
     AND NOT ArtBas.pakke
     AND NOT ArtBas.OPris
     THEN DO:
    /* Legger opp artikkelen i listen for alle valgte butikker. */
    BUTIKKER:
    DO TRANSACTION:
       FIND Farg OF ArtBas NO-LOCK NO-ERROR.
       
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
          wVVAreKost = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1].
      END.
      /* Når vi oppdaterer varekjøp må også pris hentes. */
      ELSE IF (TelleHode.TTId = 5) THEN DO:
        FIND ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
             ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
          Lager.Butik = Butiker.butik NO-ERROR.

        /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
        IF NOT AVAILABLE ArtPris THEN
          FIND ArtPris NO-LOCK WHERE
               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
               ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        IF (AVAILABLE ArtPris AND AVAILABLE Lager) THEN
          wVVAreKost = (if (Lager.Lagant > 0 and Lager.VVareKost > 0 and Lager.VVAreKost <> ?)
                          then Lager.VVarekost
                          else ArtPris.Varekost[1]).
      END.

      /* Här skapar vi TT_ArtLag från artikkelens ArtLag           */
      /* I tillägg lägger vi till ev. strl som inte har artlag     */
      /* Detta löser problemet när vi skall ta ut ikke lagerstyrda */
      EMPTY TEMP-TABLE TT_ArtLag.
      FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                    ArtLag.Butik = Butiker.Butik AND ArtLag.lagant <> 0.
        BUFFER-COPY ArtLag TO TT_ArtLag.
        
        FIND LAST StrekKode NO-LOCK WHERE 
            StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND 
            StrekKode.StrKode    = tt_ArtLag.StrKode NO-ERROR.
        IF AVAILABLE StrekKode THEN 
            TT_ArtLag.Kode = StrekKode.Kode.
        RELEASE TT_ArtLag.
      END.

      DO:
        /* Setter VVareKost. */
          FIND Lager NO-LOCK WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                                   Lager.Butik      = Butiker.Butik NO-ERROR.
          /* Setter varekost */
          IF TelleHode.TTId <> 1 THEN DO: /* Ikke for varesalg */
              IF AVAILABLE Lager THEN
                  wVVareKost = Lager.VVareKost.
              ELSE   
                  wVVareKost = 0.

              IF (wVVareKost = 0 OR wVVareKost = ? OR ArtBas.Lager = FALSE) THEN DO:
                  FIND ArtPris NO-LOCK WHERE
                       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                       ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                  IF AVAILABLE ArtPris THEN
                       wVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
              END.
          END.
      END.

      /* Ukjent varekost */
      IF wVVareKost = ? THEN
          wVVareKost = 0.

      /* Oppstandelsen - Oppstandelse pr. størrelse. */      
      ARTLAG:
      FOR EACH TT_ArtLag NO-LOCK:
        IF wNedskriv THEN
          wStorl = "".
        ELSE
          wStorl = TT_ArtLag.Storl.
        FIND TelleLinje OF TelleHode EXCLUSIVE-LOCK WHERE        
             TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
             TelleLinje.Butik      = Butiker.butik AND
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
              TelleLinje.Butik      = Butiker.butik
              TelleLinje.Storl      = wStorl
              TelleLinje.AntallPar  = TT_ArtLag.Lagant
              TelleLinje.OpprAntalTalt = IF CAN-DO("6",STRING(TelleHode.TTId)) 
                                        THEN TT_ArtLag.LagAnt /* Overføringer */
                                        ELSE TelleLinje.AntallTalt 
              TelleLinje.AntallTalt = IF CAN-DO("6",STRING(TelleHode.TTId)) 
                                        THEN TT_ArtLag.LagAnt /* Overføringer */
                                        ELSE TelleLinje.AntallTalt 
              TelleLinje.OpprVerdi  = TT_ArtLag.LagAnt * wVVareKost
              TelleLinje.OpptVerdi  =  TelleLinje.AntallTalt * wVVarekost
              TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> "" THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.Farbeskr ELSE ""
              .
/*               wNyFLagg              = TRUE. */
            /* Øvrig informasjon. */
            ASSIGN
              TelleLinje.Vg         = ArtBas.Vg 
              TelleLinje.LopNr      = ArtBas.LopNr
              TelleLinje.LevKod     = ArtBas.LevKod
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
              TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>9")) + "/" + TRIM(STRING(ArtBas.LopNr,">>>>>9"))
              TelleLinje.Kode       = TT_ArtLAg.Kode.
              .              
            /* Sjekk pris på nedskrivinger */
            IF wNedskriv THEN DO:
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
              IF TelleLinje.NedSkrevet = 0 AND AVAILABLE ArtPris THEN DO:
                ASSIGN
                  TelleLinje.VVareKost  = ArtPris.VareKost[IF ArtPris.Tilbud 
                                                             THEN 2
                                                             ELSE 1]
                  TelleLinje.NedSkrevet = ArtPris.VareKost[IF ArtPris.Tilbud 
                                                             THEN 2
                                                             ELSE 1].
              END.
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
DELETE OBJECT httTable NO-ERROR.
DELETE OBJECT hQuery.
/* MESSAGE "TIDSÅTGÅNG" STRING(TIME - iakttid,"HH:MM:SS") */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
