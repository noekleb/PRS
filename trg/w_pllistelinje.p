TRIGGER PROCEDURE FOR WRITE OF PlListeLinje OLD BUFFER oldPlListeLinje.

{trg\c_w_trg.i &Fil=PlListeLinje &TYPE=W}

DEFINE BUFFER trgplListeHode     FOR plListeHode.
DEFINE BUFFER trgplListeArtikkel FOR plListeArtikkel.
DEFINE BUFFER trgplListeModell   FOR plListeModell.
DEFINE BUFFER trgArtBas          FOR ArtBas.

DEFINE VARIABLE trglArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

/* Henter ordreforslagshode. */
FIND trgplListeHode OF plListeLinje NO-LOCK NO-ERROR.
IF NOT AVAILABLE trgplListeHode THEN 
  RETURN.

/* Sjekker at det er av riktig type. */
IF trgplListeHode.plLType = 1 THEN 
  RETURN.

/* Henter ArtBas. */
IF plListeLinje.ArtikkelNr > 0 THEN 
FIND trgArtBas NO-LOCK WHERE
  trgArtBas.ArtikkelNr = plListeLinje.ArtikkelNr NO-ERROR.
IF NOT AVAILABLE trgArtBas THEN 
  RETURN.
  
/* MODELL: Artikkler som inngr i modell :). */
IF trgArtBas.ModellFarge > 0 AND NOT CAN-FIND(FIRST trgplListeModell WHERE
  trgplListeModell.plListeId   = plListeLinje.plListeId AND
  trgplListeModell.ModellFarge = trgArtBas.ModellFarge) THEN 
  RUN opprettModell (trgArtBas.ModellFarge, trgArtBas.ArtikkelNr).
/* MODELL: Ogs modell med ModellFarge = 0, skal legges opp. Dette er artikkler som ikke inngr i noen modell. */
ELSE IF trgArtBas.ModellFarge = 0 AND NOT CAN-FIND(FIRST trgplListeModell WHERE
  trgplListeModell.plListeId   = plListeLinje.plListeId AND
  trgplListeModell.ModellFarge = trgArtBas.ArtikkelNr) THEN 
  RUN opprettModell (trgArtBas.ArtikkelNr, trgArtBas.ArtikkelNr).

/* ARTIKKEL: Artikler som inngr i modell. */
IF trgArtBas.ModellFarge > 0 AND NOT CAN-FIND(FIRST trgplListeArtikkel WHERE
  trgplListeArtikkel.plListeId   = plListeLinje.plListeId AND
  trgplListeArtikkel.ModellFarge = trgArtBas.ModellFarge AND
  trgplListeArtikkel.ArtikkelNr  = trgArtBas.ArtikkelNr) THEN 
  RUN opprettArtikkel (trgArtBas.ModellFarge, trgArtBas.ArtikkelNr).
/* ARTIKKEL: Artikkel som ikke inngr i modell. */
ELSE IF trgArtBas.ModellFarge = 0 AND NOT CAN-FIND(FIRST trgplListeArtikkel WHERE
  trgplListeArtikkel.plListeId   = plListeLinje.plListeId AND
  trgplListeArtikkel.ModellFarge = trgArtBas.ArtikkelNr AND
  trgplListeArtikkel.ArtikkelNr  = trgArtBas.ArtikkelNr) THEN 
  RUN opprettArtikkel (trgArtBas.ArtikkelNr, trgArtBas.ArtikkelNr).

PROCEDURE opprettModell:
  DEFINE INPUT PARAMETER lModellFarge AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER lArtikkelNr  AS DECIMAL NO-UNDO.
  
  CREATE trgplListeModell.
  ASSIGN trgplListeModell.plListeId   = plListeLinje.plListeId
         trgplListeModell.ModellFarge = lModellFarge
         no-error.
END PROCEDURE.

PROCEDURE opprettArtikkel:
  DEFINE INPUT PARAMETER lModellFarge AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER lArtikkelNr  AS DECIMAL NO-UNDO.
  
  CREATE trgplListeArtikkel.
  ASSIGN trgplListeArtikkel.plListeId   = plListeLinje.plListeId
         trgplListeArtikkel.ModellFarge = lModellFarge
         trgplListeArtikkel.ArtikkelNr  = lArtikkelNr
         no-error.         
END PROCEDURE.


