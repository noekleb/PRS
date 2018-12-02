CURRENT-WINDOW:WIDTH = 300.

DEF VAR dFraDato AS DATE NO-UNDO.
DEF VAR dTilDato AS DATE NO-UNDO.

DEF VAR pdLoop  AS DATE NO-UNDO.
DEF VAR cButLst AS CHAR NO-UNDO.

ASSIGN
    dFraDato = 02/16/2011
    dTilDato = 02/16/2011
    cButLst  = '707'
    .

FORM
    WITH FRAME A.

FUNCTION SjekkNonSale RETURNS INTEGER
        ( piArtikkelNr AS DECIMAL ) FORWARD.

BUTIKKER:
FOR EACH Butiker NO-LOCK WHERE
    CAN-DO('707',STRING(Butiker.Butik)):

  DATOBLOKK:
  DO pdLoop = dFraDato TO dTilDato:
      
      DISPLAY
          Butiker.Butik
          pdLoop
          WITH FRAME A.
      PAUSE 0.
      /* Sletter den gamle Akt_Rapp */
      FOR EACH Akt_Rapp EXCLUSIVE-LOCK WHERE
          Akt_Rapp.Dato  = pdLoop AND
          Akt_Rapp.Butik = Butiker.Butik:
          DELETE Akt_Rapp.
      END.

      DATASETT:
      FOR EACH DataSett NO-LOCK WHERE
          DataSett.ButikkNr  = Butiker.Butik AND
          Datasett.GruppeNr  = 1 AND
          DataSett.KasseNr  >= 0 AND
          DataSett.Dato      = pdLoop:

          DISPLAY
              DataSett.KasseNr
              WITH FRAME A.
          PAUSE 0.

          BONGHODER-TRANSACTION:
          FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK WHERE
              BongHode.BongStatus >= 5 AND
              BongHode.BongStatus <= 9:

              /* Oppdaterer Aktivitetersrapport. */
              RUN Aktivitetsrapport.
          END. /* BONGHODER-TRANSACTION */
      END. /* DATASETT */
  END. /* DATOBLOKK */

END. /* BUTIKKER */

PROCEDURE Aktivitetsrapport:
  {Aktivitetsrapport.i}
END. /* PROCEDURE */

FUNCTION SjekkNonSale RETURNS INTEGER
        ( piArtikkelNr AS DECIMAL ):

/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piResult AS INTEGER NO-UNDO.

  DEFINE BUFFER trgArtBas FOR ArtBas.
 
  piResult = 0.
  IF piArtikkelnr > 0 THEN 
  SJEKK: 
  DO:
    FIND trgArtBas NO-LOCK WHERE
      trgArtBas.ArtikkelNr = piArtikkelNr NO-ERROR.
    IF NOT AVAILABLE trgArtBas THEN 
      LEAVE SJEKK.
    IF trgArtBas.Non_Sale = TRUE AND trgArtBas.NegVare = FALSE THEN 
      piResult = 1.
    ELSE  IF trgArtBas.Non_Sale = TRUE AND trgArtBas.NegVare = TRUE THEN 
      piResult = 2.
  END. /* SJEKK */
  
  RETURN piResult.
END FUNCTION.

