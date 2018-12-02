/* Aktivering av varer til etikettkø
   Parameter:  
   Opprettet: 28.01.10 av BHa              
   Endret:    22.09.10 av BHa
            - Kan ta imot parameter som styrer om varer med/uten lokal pris skal behandles      
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery             AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Prisko           AS HANDLE NO-UNDO.
DEFINE VARIABLE bVarerUtenEgenPris AS LOG    NO-UNDO INIT YES.
DEFINE VARIABLE bVarerMedEgenPris  AS LOG    NO-UNDO INIT YES.
DEFINE VARIABLE bBehandle          AS LOG    NO-UNDO.

IF NUM-ENTRIES(icParam,"|") > 1 THEN
  ASSIGN bVarerUtenEgenPris = LOGICAL(ENTRY(1,icParam,"|"))
         bVarerMedEgenPris  = LOGICAL(ENTRY(2,icParam,"|"))
         NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    /* Henter databaserecorden. */
    FIND PrisKo EXCLUSIVE-LOCK WHERE
        PrisKo.ArtikkelNr    = DECI(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) AND
        Prisko.ProfilNr      = INT(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) AND 
        PrisKo.AktiveresDato = DATE(string(ihBuffer:BUFFER-FIELD("AktiveresDato"):BUFFER-VALUE)) AND 
        PrisKo.AktiveresTid  = INT(ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE) AND
        PrisKo.Tilbud        = LOGICAL(ihBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE) NO-ERROR.

    /* Overfører til etikettkø. */
    IF AVAILABLE PrisKo THEN 
    ETIKETT:
    DO:
      bBehandle = YES.
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = PrisKo.ArtikkelNr
             AND ArtPris.ProfilNr   = PrisKo.ProfilNr
           NO-ERROR.
      IF AVAIL ArtPris AND ArtPris.Pris[1] NE PrisKo.Pris AND NOT bVarerMedEgenPris THEN bBehandle = NO.
      IF AVAIL ArtPris AND ArtPris.Pris[1] = PrisKo.Pris AND NOT bVarerUtenEgenPris THEN bBehandle = NO.

      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
        
      IF AVAILABLE ArtBas AND bBehandle THEN     
      FOR EACH Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = PrisKo.ArtikkelNr /*AND
        LENGTH(Strekkode.Kode) > 5*/:
        
        BUTIKKER_LOOP:
        FOR EACH Butiker NO-LOCK WHERE 
          Butiker.ProfilNr        = PrisKo.ProfilNr AND
          Butiker.ApningsDato    <= TODAY AND
          Butiker.harButikksystem = TRUE AND 
          Butiker.NedlagtDato     = ?:

            FIND FIRST Etikettko EXCLUSIVE-LOCK WHERE
              Etikettko.ButikkNr    = Butiker.Butik AND
              EtikettKo.Kode        = Strekkode.Kode AND
              EtikettKo.StrKode     = Strekkode.StrKode AND
              EtikettKo.UtskriftsNr = 0 NO-ERROR.
            IF NOT AVAILABLE EtikettKo THEN 
            DO:
                CREATE Etikettko.
                BUFFER-COPY ArtBas TO Etikettko
                ASSIGN 
                    Etikettko.ButikkNr    = Butiker.Butik 
                    EtikettKo.Kode        = Strekkode.Kode 
                    EtikettKo.StrKode     = Strekkode.StrKode 
                    EtikettKo.UtskriftsNr = 0 
                    NO-ERROR.
            END. 

            ASSIGN 
              Etikettko.Pris        = PrisKo.Pris
              EtikettKo.EtikettAntHylleplasser = 1
              EtikettKo.JamforEnhet = ArtBas.JamforEnhet
              EtikettKo.Mengde      = ArtBas.Mengde
              PrisKo.EtikettStatus  = 1
              .
            IF Etikettko.EtikettAntHylleplasser = 0 THEN
              Etikettko.EtikettAntHylleplasser = 1.
            IF Etikettko.Etikettekst1 = "" THEN
              Etikettko.Etikettekst1 = ArtBas.Beskr.        
        END. /* BUTIKKER_LOOP */

      END.
    END. /* ETIKETT */

  /* Neste post. */
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".

