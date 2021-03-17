/* Oppdaterer lagerantall på postene i tellelisten.
   
   Opprettet: 24.06.04 av TN. Kode er kopiert fra Art_To_Telling.p                
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
DEF VAR wTekst      AS CHAR NO-UNDO.
DEF VAR wEDB-System AS CHAR NO-UNDO.
DEFINE VARIABLE cIdListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE iakttid AS INTEGER    NO-UNDO.

{runlib.i}

DEF BUFFER     clButiker FOR Butiker.
iakttid = TIME.

iTelleNr = INT(ENTRY(1,icParam,'|')).
IF NUM-ENTRIES(icParam,'|') > 1 THEN 
  ASSIGN 
    icParam  = ENTRY(2,icParam,'|').

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

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN 
DO:
  IF ENTRY(1,icParam) = "ROWID" THEN
    DO ix = 2 TO NUM-ENTRIES(icParam):
      FIND Tellelinje WHERE ROWID(TelleLinje) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL TelleLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER TelleLinje:HANDLE).
      END.
    END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
  FIND ArtBas
       WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
       NO-LOCK NO-ERROR.


  IF AVAIL ArtBas THEN 
  ARTIKKEL:
  DO:
    /* Setter VVareKost. */
    FIND Lager NO-LOCK WHERE 
         Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
         Lager.Butik      = Butiker.Butik NO-ERROR.
    /* Setter varekost */          
    IF AVAILABLE Lager THEN
        wVVareKost = Lager.VVareKost.
    ELSE   
        wVVareKost = 0.

    IF (wVVareKost = 0 OR wVVareKost = ? OR ArtBas.Lager = FALSE) THEN 
    DO:
        FIND ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
             ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN
             wVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
    END.
    IF wVVarekost = ? THEN wVVarekost = 0.
          
    BUTIKKER:
    DO TRANSACTION:
      FIND TelleLinje OF Tellehode EXCLUSIVE-LOCK WHERE 
           TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
           TelleLinje.Butik      = Butiker.Butik AND 
           TelleLinje.Storl      = ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE.

      IF AVAILABLE TelleLinje THEN 
      DO:
        FIND FIRST ArtLag NO-LOCK WHERE 
          ArtLag.ArtikkelNr = TelleLinje.ArtikkelNr AND
          ArtLag.Storl      = TelleLinje.Storl AND
          ArtLag.Butik      = TelleLinje.Butik NO-ERROR.
        IF AVAILABLE ArtLag THEN 
        LAGERKORR:
        DO:
          IF ArtLag.LagAnt = TelleLinje.AntallPar THEN 
            LEAVE LAGERKORR.
          ELSE DO:
            ASSIGN
                /* Nye verdier */
                TelleLinje.VVAreKost  = wVVAreKost
                TelleLinje.AntallPar  = ArtLag.Lagant
                TelleLinje.OpprVerdi  = ArtLag.LagAnt * wVVareKost
                /* Omregning */
                TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
                TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * wVVareKost
                .              
            END.  
        END. /* LAGERKORR */
      END.
      IF AVAILABLE TelleLinje THEN 
        RELEASE TelleLinje. /* Denne MÅ få stå */
    END. /* BUTIKKER TRANSACTION */

  END. /* ARTIKKEL */
  hQuery:GET-NEXT().
END.

IF ocReturn = "" THEN obOk = TRUE.
DELETE OBJECT httTable NO-ERROR.
DELETE OBJECT hQuery.
