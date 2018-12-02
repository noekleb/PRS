/* Oppdater pakkseddelhoder slik at de blir distribuert (på nytt) fra HK
   Parameter:  
   Opprettet: 24.06.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery    AS HANDLE  NO-UNDO.
DEFINE VARIABLE iWebLager AS INTEGER NO-UNDO.

{syspara.i 150 1 3 iWebLager INT}
FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iWebLager NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST PkSdlHode NO-LOCK
        WHERE PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
        NO-ERROR.

    IF AVAILABLE PkSdlHode THEN 
        FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
            RUN flaggArtikkel(PkSdlPris.ArtikkelNr, iWebLager).            
        END.         
    hQuery:GET-NEXT().
END.
DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE flaggArtikkel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ibutNr      AS INTEGER NO-UNDO.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEF BUFFER ArtBas        FOR ArtBas.

FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
FIND Lager NO-LOCK WHERE 
    Lager.ArtikkelNr = lArtikkelNr AND 
    Lager.Butik      = iButNr NO-ERROR.

/* Betingelser for å logge endring. */        
IF NOT AVAILABLE Butiker OR 
   NOT AVAILABLE ArtBas  OR 
   iButNr = 0            OR 
   lArtikkelNr = 0       OR
   NOT AVAILABLE Lager   THEN 
    RETURN.

IF ArtBas.WebButikkArtikkel THEN 
DO:
  FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
  IF AVAILABLE trgEkstEDBSystem THEN
  WEBBUTIKK:
  DO:
      
    /* Logger utlegg av prisposter for butikken. */  
    IF NOT CAN-FIND(FIRST ELogg WHERE 
         ELogg.TabellNavn     = "ArtBas" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Lager.ArtikkelNr) AND 
         ELogg.EndringsType   = 1) THEN
    ARTIKKEL_LOGG: 
    DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtBas"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Lager.ArtikkelNr)
               ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DELETE ELogg.
        RELEASE ELogg.
    END. /* ARTIKKEL_LOGG */
      
    /* Logger utlegg av lagerposter for butikken. */  
    IF NOT CAN-FIND(FIRST ELogg WHERE 
         ELogg.TabellNavn     = "Lager" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                + chr(1) + string(Lager.butik) AND 
         ELogg.EndringsType   = 1) THEN
    LAGERLOGG: 
    DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Lager"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                + chr(1) + string(Lager.butik)
               ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DELETE ELogg.
        RELEASE ELogg.
    END. /* LAGERLOGG */
  END. /* WEBBUTIKK */
END.

END PROCEDURE.
