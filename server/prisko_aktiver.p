/* Aktivering av varer i priskø
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

DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Prisko AS HANDLE NO-UNDO.
DEFINE VARIABLE bVarerUtenEgenPris AS LOG    NO-UNDO INIT YES.
DEFINE VARIABLE bVarerMedEgenPris  AS LOG    NO-UNDO INIT YES.
DEFINE VARIABLE bBehandle          AS LOG    NO-UNDO.

IF NUM-ENTRIES(icParam,"|") > 1 THEN
  ASSIGN bVarerUtenEgenPris = LOGICAL(ENTRY(1,icParam,"|"))
         bVarerMedEgenPris  = LOGICAL(ENTRY(2,icParam,"|"))
         NO-ERROR.

IF NOT VALID-HANDLE(h_Prisko) THEN 
  RUN Prisko.p PERSISTENT SET h_Prisko.

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
        PrisKo.AktiveresDato = DATE(STRING(ihBuffer:BUFFER-FIELD("AktiveresDato"):BUFFER-VALUE)) AND 
        PrisKo.AktiveresTid  = INT(ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE) AND
        PrisKo.Tilbud        = LOGICAL(ihBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE) NO-ERROR.

    /* Aktivering av priskøpost. */
    IF AVAILABLE PrisKo THEN 
    BEHANDLE:
    DO:    
      bBehandle = YES.
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = PrisKo.ArtikkelNr
             AND ArtPris.ProfilNr   = PrisKo.ProfilNr
           NO-ERROR.
      IF AVAIL ArtPris AND ArtPris.Pris[1] NE PrisKo.Pris AND NOT bVarerMedEgenPris THEN bBehandle = NO.
      IF AVAIL ArtPris AND ArtPris.Pris[1] = PrisKo.Pris AND NOT bVarerUtenEgenPris THEN bBehandle = NO.
      IF bBehandle THEN DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.

        /* Er artikkelen slettet, skal køposten bare fjernes.                        */
        /* Er posten behandlet (Prisko.KlargjorStatus > 0) -- og                     */
        /* etikettflagget også satt (PrisKo.EtikettStatus > 0), skal posten slettes. */        
        IF NOT AVAILABLE ArtBas /*OR 
           (PrisKo.KlargjorStatus > 0 AND 
            PrisKo.EtikettStatus > 0)*/ THEN 
           DO:
             DELETE PrisKo.
             LEAVE BEHANDLE.
           END.
        /* Aktiverer posten */
        ELSE DO:
          RUN KlargjorPrisKoEn IN h_PrisKo (ROWID(ArtBas)).
        END.
      END.
    END. /* BEHANDLE */

  /* Neste post. */
  hQuery:GET-NEXT().
END. /* BLOKKEN */

IF VALID-HANDLE(h_PrisKo) THEN
  DELETE PROCEDURE h_PrisKo.

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".
