/* Sletter bonger for MegaDisk butikker */

CURRENT-WINDOW:WIDTH = 125.

DEF VAR iAnt   AS INT  NO-UNDO.            
DEF VAR cTekst AS CHAR NO-UNDO.
{syspara.i 1 10 30 cTekst}

FORM 
  WITH FRAME G.

/* Filer m/loggposter og linjer. */
FOR EACH Filer EXCLUSIVE-LOCK WHERE
    Filer.FilNavn BEGINS "MD":
    PAUSE 0.
    DISPLAY
        string(Filer.FilId) + " " + 
        Filer.FilNavn @ cTekst FORMAT "x(30)"
        WITH FRAME G WIDTH 123.
/*     FOR EACH FilLinjer OF Filer EXCLUSIVE-LOCK: */
/*         DELETE fillinjer.                       */
/*     END.                                        */
/*     FOR EACH FilLogg OF Filer EXCLUSIVE-LOCK:   */
/*         DELETE fillogg.                         */
/*     END.                                        */
/*     DELETE filer.                               */
END.

FOR EACH Kasse NO-LOCK
    WHERE Kasse.ModellNr = 30:
    iAnt = iAnt + 1.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = Kasse.ButikkNr.
    PAUSE 0.
    DISPLAY
        iAnt COLUMN-LABEL "Antall"
        Kasse.ButikkNr
        Butiker.ButNamn
        Kasse.KasseNr
        Kasse.ModellNr
        cTekst FORMAT "x(15)"
        WITH FRAME G WIDTH 123.

/*     /* Tar bort data */                                   */
/*     FOR EACH DataSett EXCLUSIVE-LOCK WHERE                */
/*         DataSett.ButikkNr = Kasse.ButikkNr AND            */
/*         DataSett.GruppeNr = Kasse.GruppeNr AND            */
/*         DataSett.KasseNr  = Kasse.KasseNr:                */
/*                                                           */
/*         FOR EACH BongHode EXCLUSIVE-LOCK WHERE            */
/*             BongHode.DataSettId = DataSett.DataSettId AND */
/*             BongHode.ButikkNr   = DataSett.ButikkNr AND   */
/*             BongHode.GruppeNr   = Datasett.ButikkNr AND   */
/*             BongHode.KasseNr    = DataSett.KasseNr:       */
/*                                                           */
/*             FOR EACH BongLinje EXCLUSIVE-LOCK WHERE       */
/*                 BongLinje.B_Id = BongHode.B_ID:           */
/*                 DELETE BongLinje.                         */
/*             END.                                          */
/*                                                           */
/*             DELETE BongHode.                              */
/*         END.                                              */
/*                                                           */
/*         DELETE DataSett.                                  */
/*     END.                                                  */
END.
