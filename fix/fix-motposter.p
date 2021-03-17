DEF VAR iBatchNr      AS INT  NO-UNDO.
DEF VAR iTransNr      AS INT  NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.
    
DEF BUFFER bufTransLogg FOR TransLogg.

/* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
run batchlogg.p (program-name(1),
               "Balangsering av PLU'er (FIX) " +
               string(today) +
               " " +
               string(time,"HH:MM") +
               " " +
               userid("dictdb"),
               output iBatchNr).

FOR EACH ArtBas WHERE 
    /*ArtBas.ArtikkelNr = 3112 AND*/
    ArtBas.Lager = FALSE:
    FOR EACH TransLogg NO-LOCK WHERE
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND
        CAN-DO("001,003,010",STRING(TransLogg.TTId,"999")):
        
        FIND TransType NO-LOCK WHERE
            TransType.TTId = TransLogg.TTId NO-ERROR.

        FIND FIRST bufTransLogg NO-LOCK WHERE
                        bufTransLogg.ArtikkelNr  = TransLogg.ArtikkelNr  AND
                        bufTransLogg.Butik       = TransLogg.Butik       AND
                        bufTransLogg.Dato        = TransLogg.Dato        AND
                        bufTransLogg.KassaNr     = TransLogg.KassaNr     AND
                        bufTransLogg.BongId      = TransLogg.BongId      AND
                        bufTransLogg.BongLinjeNr = TransLogg.BongLinjeNr AND
                        bufTransLogg.TTID        = 7    /*                 AND
                        bufTransLogg.SEqNr       = 2      */               
                        NO-ERROR.
/*         IF NOT AVAILABLE bufTransLogg THEN                                                       */
/*         DISPLAY                                                                                  */
/*             TransLogg.ArtikkelNr                                                                 */
/*             Translogg.Butik                                                                      */
/*             TransLogg.seqNr                                                                      */
/*             Translogg.TTId COLUMN-LABEL "TTId"                                                   */
/*             TransType.Beskrivelse WHEN AVAILABLE TransType FORMAT "x(10)"                        */
/*             TransLogg.Dato COLUMN-LABEL "DAto"                                                   */
/*             TransLogg.KassaNr COLUMN-LABEL "KAs"                                                 */
/*             Translogg.BongId                                                                     */
/*             TransLogg.BongLinjeNr FORMAT ">9" COLUMN-LABEL "Lin"                                 */
/*             TransLogg.Antal FORMAT "->9"                                                         */
/*             TransLogg.Pris                                                                       */
/*             "|"                                                                                  */
/*             bufTransLogg.Dato WHEN AVAILABLE bufTransLogg COLUMN-LABEL "Dato"                    */
/*             bufTransLogg.KassaNr WHEN AVAILABLE bufTransLogg COLUMN-LABEL "Kas"                  */
/*             bufTranslogg.BongId WHEN AVAILABLE bufTransLogg                                      */
/*             bufTransLogg.BongLinjeNr WHEN AVAILABLE bufTransLogg  FORMAT ">9" COLUMN-LABEL "Lin" */
/*         WITH WIDTH 198                                                                           */
/*         .                                                                                        */
        IF NOT AVAILABLE bufTransLogg THEN
        OPPSTANDELSEN:
        DO:
          /* Setter transaksjonsnummer  */
          if iTransNr = 0 then
            DO:
              find last bufTranslogg where
                bufTranslogg.Butik = TransLogg.Butik
                use-index TransLogg no-error.
              if available bufTranslogg then
                iTransNr = bufTranslogg.TransNr + 1.
              else
                iTransNr = 1.
            END.
          else
            iTransNr = iTransNr + 1.
         
          /* Oppretter TransLogg */
          CREATE bufTranslogg.
          BUFFER-COPY TransLogg 
            EXCEPT BatchNr TransNr SeqNr TTId TBId Antall Postert PostertDato PostertTid Plukket
            TO bufTranslogg
            assign 
              bufTranslogg.BatchNr      = iBatchNr
              bufTranslogg.TransNr      = iTransNr
              bufTranslogg.SeqNr        = 2
              /* Alltid lagerjustering */
              bufTranslogg.TTId         = 7
              bufTranslogg.TBId         = 1
              bufTranslogg.Balangsering = TRUE 
              bufTranslogg.Antall       = TransLogg.Antall * -1
              bufTranslogg.Postert      = FALSE
              bufTranslogg.PostertDato  = ?
              bufTranslogg.PostertTid   = 0
              bufTranslogg.Plukket      = TRUE
              .
         
          /* Slipper posten */
          RELEASE bufTranslogg.
        END. /* OPPSTANDELSEN */
         
         
    END.

END.
