
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bufArtLAg FOR ArtLAg.

FIXARTLAG:
FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = 1006:

    /* Rydder bort gammal dritt */
    FOR EACH ArtLAg WHERE
        ArtLAg.ArtikkelNr = ArtBas.ArtikkelNr:
        DELETE ArtLAg.
    END.

    TRANSLOGGEN:
    FOR EACH TransLogg NO-LOCK WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
        USE-INDEX OppslagDatoTid:
        /* Er det en gyldig størrelse */
        find first StrTStr no-lock where
          StrTStr.StrTypeId = ArtBas.StrTypeId and
          StrTStr.SoStorl   = TransLogg.Storl no-error.
        if not available StrTStr then
          do:
          end.

        /* Hvis ikke ArtLag posten finnes opprettes den. Men transaksjonen */
        /* flagges med en kode for at lagerpost ble opprettet.             */
        find ArtLag exclusive-lock where
          ArtLag.Butik = TransLogg.Butik and
          ArtLag.Vg    = TransLogg.Vg and
          ArtLag.LopNr = TransLogg.LopNr and
          ArtLag.Storl = TransLogg.Storl no-error no-wait.
        if not available ArtLag then
          do:
            create ArtLag.
            assign
              ArtLag.Butik      = TransLogg.Butik
              ArtLag.Vg         = TransLogg.Vg 
              ArtLag.LopNr      = TransLogg.LopNr 
              ArtLag.Storl      = TransLogg.Storl
              Artlag.ArtikkelNr = TransLogg.ArtikkelNr
              .              
          end.

        case TransLogg.TTId:
          when 1 then /* Varesalg */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt     - TransLogg.Antall
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall
              ArtLag.AntRab   = ArtLAg.AntRab +
                                (if TransLogg.RabKr <> 0
                                   then TransLogg.Antall 
                                   else 0).
          when 2 then /* Brekkasje */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.BrekkAnt = ArtLag.BrekkAnt + TransLogg.Antall.
          when 3 then /* Kundereklamasjon */
            assign
              /* TN 14/5-02 ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall */
              ArtLag.ReklAnt = ArtLag.ReklAnt + TransLogg.Antall
              /* Korrigerer salget ved reklamasjon */
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
          when 4 then /* Lagerreklamasjon */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.ReklLAnt = ArtLag.ReklLAnt + TransLogg.Antall.
          when 5 then /* Varekjøp */
            assign
              ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
              ArtLag.KjopAnt = ArtLag.KjopAnt + TransLogg.Antall.          
          when 6 then /* Overføring */
            do:
              assign  /*Inn i butikk. NB: Translogg.Antall er negativ postert i Translogg. */
                ArtLag.LagAnt  = ArtLag.LagAnt - TransLogg.Antall
                ArtLag.OvAnt   = ArtLag.OvAnt  - TransLogg.Antall.

              /* Justerer fra butikken. */  
              /* NB: i w-gridlager.w lagres fra størrelsen i TransLogg.TilStorl. */
              find bufArtLag exclusive-lock where
                bufArtLag.Butik = TransLogg.OvButik and
                bufArtLag.Vg    = TransLogg.Vg and
                bufArtLag.LopNr = TransLogg.LopNr and
                bufArtLag.Storl = TransLogg.TilStorl no-error no-wait.
              if locked bufArtLag then
                do:
                  return "UNDO".
                end.
              if not available bufArtLag then
                do:
                  create bufArtLag.
                  assign
                    bufArtLag.Butik      = TransLogg.OvButik
                    bufArtLag.Vg         = TransLogg.Vg 
                    bufArtLag.LopNr      = TransLogg.LopNr 
                    bufArtLag.Storl      = TransLogg.TilStorl
                    bufArtLag.ArtikkelNr = TransLogg.ArtikkelNr
                    .              
                end.

              assign  /*Trekke ned i fra butikken. Husk at TransLogg.Antall er negativ. */
                bufArtLag.LagAnt  = bufArtLag.LagAnt + TransLogg.Antall
                bufArtLag.OvAnt   = bufArtLag.OvAnt  + TransLogg.Antall.
            end.
          when 7 then /* Lagerjustering */
            assign
              ArtLag.LagAnt  = ArtLag.LagAnt  - TransLogg.Antall
              ArtLag.JustAnt = ArtLag.JustAnt + TransLogg.Antall.
          when 8 then. /* Nedskrivning - Påvirker ikke ArtLag. */
          when 9 then /* Svinn */        
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.SvinnAnt = ArtLag.SvinnAnt + TransLogg.Antall.
          when 10 then /* Gjennkjøp */        
            assign
              ArtLag.LagAnt      = ArtLag.LagAnt      + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
              ArtLag.GjenkjopAnt = ArtLag.GjenkjopAnt + TransLogg.Antall
              /* Korrigerer rabatter */
              ArtLag.AntRab   = ArtLAg.AntRab +
                                (if TransLogg.RabKr <> 0
                                   then TransLogg.Antall 
                                   else 0)
              /* Korrigerer salget ved gjenkjøp */
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
          when 11 then /* Internt forbruk */        
            assign
              ArtLag.LagAnt = ArtLag.LagAnt - TransLogg.Antall
              ArtLag.IntAnt = ArtLag.IntAnt + TransLogg.Antall.        
        end case.
    END. /* TRANSLOGGEN */

END. /* FIXARTLAG */




