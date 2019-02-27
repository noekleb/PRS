DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iTelleNr    AS INT  NO-UNDO.
DEF VAR dDato       AS DATE NO-UNDO.
DEF VAR dFraDato    AS DATE NO-UNDO.
DEF VAR dTilDato    AS DATE NO-UNDO.
DEF VAR iFraTid     AS INT  NO-UNDO.
DEF VAR iTilTid     AS INT  NO-UNDO.
DEF VAR cTTId       AS CHAR NO-UNDO.
DEF VAR iTTId       AS INT  NO-UNDO.
DEF VAR iEntry      AS INT  NO-UNDO.
DEF VAR iantalltalt AS INT  NO-UNDO.
DEFINE VARIABLE cDato1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDato2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTid1  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTid2  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE bStrBytte AS LOG NO-UNDO.

DEFINE TEMP-TABLE TT_TransKorr NO-UNDO
    FIELD Butik      AS INTEGER 
    FIELD artikkelnr AS DECI
    FIELD storl      AS CHAR
    FIELD antalltalt AS DECI
    FIELD tellenr    AS INTEGER
    FIELD LevNr      LIKE ArtBas.LevNr
    FIELD Farg       LIKE ArtBas.Farg
    FIELD Sasong     LIKE ArtBas.Sasong
    FIELD MatKod     LIKE ArtBas.MatKod
    FIELD LinjeNr    AS INTEGER 
    FIELD LevFargKod LIKE ArtBas.LevFargKod
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD LevKod     LIKE ArtBas.LevKod
    FIELD VgLopNr    LIKE TelleLinje.VgLopNr
    FIELD Vg         LIKE ArtBAs.Vg
    FIELD LopNr      LIKE ArtBas.LopNr
    INDEX artstorl   IS PRIMARY UNIQUE artikkelnr storl.

iTelleNr = INT(ENTRY(1,icParam)).
/* cTTId = "1,5,6,7,9,10". */
/*cTTId = "1,5,6,10".*/ 
cTTId = "1,2,3,4,5,6,7,9,10,11". 

/* salg,overföring,lagerjustering,svinn,vareköp */

FIND TelleHode WHERE TelleHode.TelleNr = INT(iTelleNr) NO-LOCK NO-ERROR.
IF NOT AVAIL TelleHode THEN
    RETURN.
FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN
    RETURN.
IF NUM-ENTRIES(TelleHode.Notat,"-") <> 2 THEN
    RETURN.

cDato1 = ENTRY(1,ENTRY(1,TelleHode.Notat,"-")," ").
cDato2 = ENTRY(1,ENTRY(2,TelleHode.Notat,"-")," ").
cTid1  = ENTRY(2,ENTRY(1,TelleHode.Notat,"-")," ").
cTid2  = ENTRY(2,ENTRY(2,TelleHode.Notat,"-")," ").
dFraDato = DATE(cDato1).
dTilDato = DATE(cDato2).
iFraTid  = (INT(ENTRY(1,cTid1,":")) * 3600) + (INT(ENTRY(2,cTid1,":")) * 60) + (INT(ENTRY(3,cTid1,":"))).
iTilTid  = (INT(ENTRY(1,cTid2,":")) * 3600) + (INT(ENTRY(2,cTid2,":")) * 60) + (INT(ENTRY(3,cTid2,":"))).

/* dato ttid butik */
DATOLOOP: 
DO dDato = dFraDato TO dTilDato:
    TTIDLOOP:
    DO iEntry = 1 TO NUM-ENTRIES(cTTId):
    
        /* Aktuell transaksjonstype som skal behandles */
        iTTId = INT(ENTRY(iEntry,cTTId)).
        
        LES_TRANSTYPE_TTID:
        FOR EACH translogg WHERE 
          TransLogg.Dato = dDato AND 
          translogg.ttid = iTTId NO-LOCK:
            /* Ikke oppdaterte transaksjoner skal ikke med. */
            IF translogg.postert = FALSE THEN
                NEXT.
            /* På startdato, skal transaksjoner utenfor angitt tid ikke tas med. */    
            IF Translogg.Dato = dFraDato AND translogg.tid < iFraTid THEN
                NEXT.
            /* På sluttdato, skal transaksjoenre utenfor angitt tid ikke tas med */
            IF Translogg.Dato = dTilDato AND translogg.tid > iTilTid THEN
                NEXT.
            /* Fra andre butikker, er det bare overføringstransaksjonen det skal tas hensyn til.      */
            /* Hvis det ikke er en overføring, og transen er fra en annen butikk, skal den ignoreres. */    
            IF NOT CAN-DO("6",STRING(Translogg.ttid)) AND translogg.butik <> Butiker.butik THEN
                NEXT.
            /* Skipper overføringer som ikke gjelder butikken som behandles. */
            IF translogg.ttid = 6 AND 
               translogg.butik   <> butiker.butik AND /* Fra en annen butikk */ 
               translogg.ovbutik <> butiker.butik /* IKKE overføring til denne butikken. */
               THEN NEXT.
            /* Skrot som ikke kan behandles. */
            IF translogg.artikkelnr = 0 OR TRIM(translogg.storl) = "" THEN
                NEXT.
            /* Ukjent artikkel på transen. */    
            FIND artbas WHERE artbas.artikkelnr = translogg.artikkelnr NO-LOCK NO-ERROR.
            IF NOT AVAIL artbas OR artbas.lager = FALSE THEN
                NEXT.
            /* Sanerte artikler skal ikke være med. */
            IF ArtBas.SanertDato <> ? THEN 
              NEXT.             
            
            ASSIGN 
              iantalltalt = 0
              bStrBytte   = FALSE.
            /* Behandler de ulike transaksjonstyper. */  
            CASE translogg.ttid:
                WHEN 1 THEN DO: /* Varesalg */
                    iantalltalt = -1 * translogg.antall.
                END.
                WHEN 2 THEN DO: /* Brekkasje */
                    iantalltalt = -1 * translogg.antall.
                END.
                WHEN 3 THEN DO: /* Kundereklamasjon */
                    iantalltalt = translogg.antall.
                END.
                WHEN 4 THEN DO: /* Lagerreklamasjon */
                    iantalltalt = -1 * translogg.antall.
                END.
                WHEN 5 THEN DO: /* Varekjøp */
                    iantalltalt = translogg.antall.
                END.
                WHEN 6 THEN DO: /* Overføring */
                    iantalltalt = (IF translogg.butik = butiker.butik THEN -1 ELSE 1) * translogg.antall.
                    /* Overføring til en annen størrelse. Slike transaksjoner gjelder alltid butikk */
                    IF TransLogg.Butik = TransLogg.OvButik THEN 
                      ASSIGN bStrBytte = TRUE.
                    ELSE 
                      ASSIGN bStrBytte = FALSE.
                END.
                WHEN 7 THEN DO: /* Lagerjustering */
                    iantalltalt = -1 * translogg.antall.
                END.
                WHEN 8 THEN DO:
                  /* Nedskrivning  - påvirker ikke lagerteller. */
                END.
                WHEN 9 THEN DO: /* Svinn */
                    iantalltalt = -1 * translogg.antall.
                END.
                WHEN 10 THEN DO:
                    /* Fortegn på Retur står motsatt i Translogg. */
                    iantalltalt = -1 * translogg.antall.
                END.
            END CASE.
            IF iantalltalt = 0 THEN
                NEXT.
            /* Logger korreksjonspost. */    
            FIND TT_TransKorr WHERE TT_TransKorr.artikkelnr = translogg.artikkelnr AND
                                    TT_TransKorr.storl      = translogg.storl NO-ERROR.
            IF NOT AVAIL TT_TransKorr THEN 
            POSTER_I_LOGG:
            DO:
                iLinjeNr = iLinjeNr + 1.
                CREATE TT_TransKorr.
                ASSIGN TT_TransKorr.artikkelnr = translogg.artikkelnr
                       TT_TransKorr.storl      = translogg.storl
                       TT_TransKorr.tellenr    = iTellenr
                       TT_TransKorr.LevNr      = ArtBas.LevNr
                       TT_TransKorr.Farg       = ArtBas.Farg
                       TT_TransKorr.Sasong     = ArtBas.Sasong
                       TT_TransKorr.MatKod     = ArtBas.MatKod
                       TT_TransKorr.LinjeNr    = iLinjeNr 
                       TT_TransKorr.LevFargKod = ArtBas.LevFargKod
                       TT_TransKorr.Beskr      = ArtBas.Beskr
                       TT_TransKorr.LevKod     = ArtBas.LevKod
                       TT_TransKorr.VgLopNr    = TRIM(STRING(ArtBas.Vg,'>>>>>9')) + '/' + (IF ArtBas.LopNr = ? THEN '?' ELSE TRIM(STRING(ArtBas.LopNr,'>>>>>9')))
                       TT_TransKorr.Vg         = ArtBas.Vg
                       TT_TransKorr.LopNr      = ArtBas.LopNr
                       TT_TransKorr.butik      = Butiker.Butik
                       .
            END. /* POSTER_I_LOGG */
            TT_TransKorr.antalltalt = TT_TransKorr.antalltalt + iantalltalt.
            
            /* Bytte av størrelse - overføring på samme butikk.        */
            /* Da skal butikken tilføres en par på en annen størrelse. */
            IF (bStrBytte AND TransLogg.TTId = 6) THEN 
            STØRRELSESBYTTE:
            DO:
              FIND TT_TransKorr WHERE TT_TransKorr.artikkelnr = translogg.artikkelnr AND
                                      TT_TransKorr.storl      = translogg.TilStorl NO-ERROR.
              IF NOT AVAIL TT_TransKorr THEN DO:
                iLinjeNr = iLinjeNr + 1.
                CREATE TT_TransKorr.
                ASSIGN TT_TransKorr.artikkelnr = translogg.artikkelnr
                       TT_TransKorr.storl      = translogg.TilStorl
                       TT_TransKorr.tellenr    = iTellenr
                       TT_TransKorr.LevNr      = ArtBas.LevNr
                       TT_TransKorr.Farg       = ArtBas.Farg
                       TT_TransKorr.Sasong     = ArtBas.Sasong
                       TT_TransKorr.MatKod     = ArtBas.MatKod
                       TT_TransKorr.LinjeNr    = iLinjeNr 
                       TT_TransKorr.LevFargKod = ArtBas.LevFargKod
                       TT_TransKorr.Beskr      = ArtBas.Beskr
                       TT_TransKorr.LevKod     = ArtBas.LevKod
                       TT_TransKorr.VgLopNr    = TRIM(STRING(ArtBas.Vg,'>>>>>9')) + '/' + (IF ArtBas.LopNr = ? THEN '?' ELSE TRIM(STRING(ArtBas.LopNr,'>>>>>9')))
                       TT_TransKorr.Vg         = ArtBas.Vg
                       TT_TransKorr.LopNr      = ArtBas.LopNr
                       TT_TransKorr.butik      = Butiker.Butik
                       bStrBytte               = FALSE 
                       .
              END.
              TT_TransKorr.antalltalt = TT_TransKorr.antalltalt + (iantalltalt * -1).
            END. /* STØRRELSESBYTTE */
        END. /* LES_TRANSTYPE_TTID */
    END. /* TTIDLOOP */
END. /* DATOLOOP */
FOR EACH TT_TransKorr WHERE TT_Transkorr.antalltalt <> 0:
    CREATE tellelinje.
    BUFFER-COPY TT_TransKorr TO tellelinje NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        DELETE tellelinje.
    RELEASE tellelinje.
END.
