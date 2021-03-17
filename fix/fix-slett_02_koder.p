DEF VAR dDato AS DATE NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF BUFFER bStrekkode FOR Strekkode.

ASSIGN
    cFilNavn = 'slettet02kode' 
                + "_" 
                + REPLACE(STRING(DATE(TODAY)),"/","") 
                + "_" 
                + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + '.txt'.

UPDATE dDato
    .

DEF STREAM Ut.

IF dDato <> ? THEN
DO:
    OUTPUT STREAM Ut TO VALUE(cFilNavn).

    FOR EACH Strekkode EXCLUSIVE-LOCK WHERE
        Strekkode.Kode BEGINS '02' AND 
        LENGTH(TRIM(Strekkode.Kode)) = 13:        

        IF '02' + 
           FILL('0',LENGTH(STRING(Strekkode.ArtikkelNr)) - 7) +  
           STRING(Strekkode.ArtikkelNr) + 
           STRING(Strekkode.StrKode) = SUBSTRING(Strekkode.Kode,1,12) THEN
        SLETTING:
        DO:
          FIND ArtBas OF STrekkode NO-LOCK NO-ERROR.
          
          /* Finnes det en gyldig strekkode på denne størrelsen. */
          FIND FIRST bStrekkode NO-LOCK 
             WHERE bStrekkode.ArtikkelNr = Strekkode.ArtikkelNr
               AND bStrekkode.StrKode    = Strekkode.StrKode
               AND NOT bStrekkode.Kode   BEGINS "02" 
               AND LENGTH(Strekkode.Kode) = 13
             NO-ERROR.
          /* Gjør det ikke det, skal 02 koden ikke slettes hvis det ligger lager på den. */
          IF NOT AVAIL bStrekkode THEN DO:
              /* Ligger det lager påd enne størrelsen */
              FIND FIRST ArtLag NO-LOCK WHERE
                  ArtLag.ArtikkelNr = Strekkode.ArtikkelNr AND 
                  ArtLag.Butik > 0 AND 
                  ArtLag.StrKode = Strekkode.StrKode NO-ERROR.
              /* Er det lager, pos eller neg, skal den ikke slettes */
              IF AVAILABLE ArtLag AND ArtLag.LagAnt <> 0 THEN
                  LEAVE SLETTING.

              /* Det skal ikke ligge salgstransaksjoner på den etter angitt dato */
              IF CAN-FIND(FIRST TransLogg WHERE 
                          TransLogg.ArtikkelNr = Strekkode.ArtikkelNr AND
                          TransLogg.Dato > dDato AND
                          Translogg.TTId = 1) THEN
                  LEAVE SLETTING.
          END.

          PUT STREAM Ut UNFORMATTED
            Strekkode.Kode ';'
            Strekkode.ArtikkelNr ';'
            (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '') ';'
            (IF AVAILABLE ArtBAs THEN TRIM(ArtBas.Beskr) ELSE '') 
            SKIP.
          /* Ta bort komementartegnene på linjen under når du vil slette. */
          /* Delete strekkode. */
        END. /* SLETTING */
    END.
    OUTPUT STREAM Ut CLOSE.
END.
