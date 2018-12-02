TRIGGER PROCEDURE FOR CREATE OF ReklamasjonsLogg.

def var trgReklamasjonsNr as DEC no-undo.

{trg\c_w_trg.i &Fil=SkoTex.Reklamasjonslogg &Type="C"}

/* FIND LAST trgReklamasjonslogg USE-INDEX ReklamasjonsNr NO-LOCK no-error.      */
/* IF AVAILABLE trgReklamasjonslogg THEN                                         */
/*     Reklamasjonslogg.ReklamasjonsNr = trgReklamasjonslogg.Reklamasjonsnr + 1. */
/* ELSE                                                                          */
/*     Reklamasjonslogg.reklamasjonsnr = 1.                                      */

LOOPEN:
DO WHILE TRUE:
    RUN trg/genreklamnr.p (OUTPUT trgReklamasjonsNr).
    assign
      Reklamasjonslogg.ReklamasjonsNr = trgReklamasjonsNr
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.


