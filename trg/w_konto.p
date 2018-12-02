TRIGGER PROCEDURE FOR WRITE OF konto.
/* tillsvidare ingen loggning */
/* FIND ELogg WHERE                                                                                                              */
/*      ELogg.TabellNavn     = "konto" AND                                                                                       */
/*      ELogg.EksterntSystem = "HK"    AND                                                                                       */
/*      ELogg.Verdier        = STRING(konto.dato) + CHR(1) + STRING(konto.butikk) + CHR(1) + STRING(konto.kontonummer) NO-ERROR. */
/* IF NOT AVAIL Elogg THEN DO:                                                                                                   */
/*     CREATE Elogg.                                                                                                             */
/*     ASSIGN ELogg.TabellNavn     = "konto"                                                                                     */
/*            ELogg.EksterntSystem = "HK"                                                                                        */
/*            ELogg.Verdier        = STRING(konto.dato) + CHR(1) + STRING(konto.butikk) + CHR(1) + STRING(konto.kontonummer).    */
/* END.                                                                                                                          */
/* ASSIGN ELogg.EndringsType = 1                                                                                                 */
/*        ELogg.Behandlet    = FALSE.                                                                                            */


