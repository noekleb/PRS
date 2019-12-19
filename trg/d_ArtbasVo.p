TRIGGER PROCEDURE FOR DELETE OF ArtbasVo.
/* FIND artbas WHERE artbas.artikkelnr = ArtBasVo.artikkelnr NO-LOCK NO-ERROR.                  */
/* IF AVAIL artbas AND ArtBas.WebButikkArtikkel = TRUE THEN DO:                                 */
/*     IF NOT CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtBas"        AND                   */
/*                                 ELogg.EksterntSystem = "WEBBUTARTINFO" AND                   */
/*                                 ELogg.Verdier        = STRING(ArtbasVo.ArtikkelNr)) THEN DO: */
/*           CREATE ELogg.                                                                      */
/*           ASSIGN ELogg.TabellNavn     = "ArtBas"                                             */
/*                  ELogg.EksterntSystem = "WEBBUTARTINFO"                                      */
/*                  ELogg.Verdier        = STRING(ArtbasVo.ArtikkelNr)                          */
/*                  ELogg.EndringsType = 1                                                      */
/*                  ELogg.Behandlet    = FALSE NO-ERROR.                                        */
/*           IF ERROR-STATUS:ERROR THEN                                                         */
/*               DELETE Elogg.                                                                  */
/*           ELSE                                                                               */
/*               RELEASE ELogg.                                                                 */
/*     END.                                                                                     */
/* END.                                                                                         */
