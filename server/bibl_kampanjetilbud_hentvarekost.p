/* bibl_kampanjetilbud_hentvarekost.p
   RUN bibl_kampanjetilbud_hentvarekost.p (BongLinje.B_Id, BongLinje.KampId, BongLinje.KampTilbId, OUTPUT lVVareKost).

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM dB_Id       LIKE BongHode.B_Id             NO-UNDO.
DEF INPUT  PARAM dKampId     LIKE KampanjeTilbud.KampId     NO-UNDO.
DEF INPUT  PARAM dKampTilbId LIKE KampanjeTilbud.KamptilbId NO-UNDO.
DEFINE OUTPUT PARAMETER dVVAreKost  AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER dMvaKr      AS DECIMAL NO-UNDO.

DEFINE VARIABLE obOK     AS LOG     NO-UNDO.
DEFINE VARIABLE dPris    AS DECIMAL NO-UNDO.
DEFINE VARIABLE dKost    AS DECIMAL NO-UNDO.
DEFINE VARIABLE h_PrisKo AS HANDLE NO-UNDO.

dVVAreKost = 0.
dMvaKr     = 0.

FIND BongHode NO-LOCK WHERE 
     BongHode.B_Id = dB_Id NO-ERROR.
IF AVAIL BongHode THEN
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = BongHode.Butik NO-ERROR.

    IF AVAILABLE Butiker THEN
    BONG: 
    FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id       = BongHode.B_Id AND 
      BongLinje.KampId     = dKampId AND 
      BongLinje.KampTilbId = dKampTilbId AND 
      BongLinje.Makulert   = FALSE:

      IF BongLinje.TTId = 109 THEN 
        NEXT BONG.  
      IF TRIM(BongLinje.ArtikkelNr) = '' THEN 
        NEXT BONG.
        
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = DEC(ArtBas.ArtikkelNr) AND 
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr. 
      IF NOT AVAILABLE ArtBas AND AVAILABLE ArtPris THEN 
        NEXT BONG.

      dMvaKr = dMvaKr + (dec(BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall))).

      IF ArtBas.OPris THEN 
      DO:      
          IF NOT VALID-HANDLE(h_PrisKo) THEN
              RUN PrisKo.p PERSISTENT SET h_PrisKo.
          dPris = (BongLinje.LinjeSum / BongLinje.Antall).
          IF VALID-HANDLE(h_PrisKo) THEN
            RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                        INPUT BongHode.Butik, 
                                        INPUT (dPris - (dPris - (dPris / (1 + (ArtPris.Mva%[1] / 100))))), 
                                        OUTPUT dKost).      
          dVVareKost = dVVareKost + IF dKost <> ? THEN dKost * BongLinje.Antall ELSE 0.
      END.
      ELSE IF ArtBas.Lager = FALSE THEN /* Fra kalkylen */
        dVVareKost = dVVareKost + IF ArtPris.VareKost[1] <> ? THEN ArtPris.VareKost[1] * BongLinje.Antall ELSE 0.
      ELSE DO:
        FIND Lager NO-LOCK WHERE 
          Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
          Lager.Butik      = BongHode.butikkNr NO-ERROR.
        IF AVAILABLE Lager AND Lager.VVAreKost > 0 AND Lager.VVareKost <> ? THEN
           dVVAreKost = dVVAreKost + (Lager.VVareKost  * BongLinje.Antall).
        ELSE DO:
          IF NOT VALID-HANDLE(h_PrisKo) THEN
              RUN PrisKo.p PERSISTENT SET h_PrisKo.
          dPris = (BongLinje.LinjeSum / BongLinje.Antall).
          IF VALID-HANDLE(h_PrisKo) THEN
            RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                        INPUT BongHode.Butik, 
                                        INPUT (dPris - (dPris - (dPris / (1 + (ArtPris.Mva%[1] / 100))))), 
                                        OUTPUT dKost).      
          dVVareKost = dVVareKost + IF dKost <> ? THEN dKost * BongLinje.Antall ELSE 0.
        END. 
      END.  
    END. /* BONG */ 
END.        

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_prisko.

RETURN.

