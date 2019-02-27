DEF VAR ocReturn AS CHAR no-undo.
DEF VAR obOk     AS LOG  NO-UNDO.

DEF BUFFER bReklamasjonslogg FOR Reklamasjonslogg.
FOR EACH bReklamasjonslogg:

    FOR EACH Reklamasjonslinje OF bReklamasjonslogg:
        ASSIGN
              Reklamasjonslinje.ReklamVerdi = ((Reklamasjonslinje.Pris - Reklamasjonslinje.RabKr) * Reklamasjonslinje.Antall) +
                                                                 Reklamasjonslinje.ReklamUtgifter 
              Reklamasjonslinje.ReklamTotal = (Reklamasjonslinje.VVarekost * Reklamasjonslinje.Antall) +
                                                                Reklamasjonslinje.ReklamUtgifter.
    END.

    /* Oppdaterer totaler i reklamasjonshode. */
    RUN reklamasjonslogg_recalc.p (STRING(bReklamasjonslogg.Reklamasjonsnr),?,'',OUTPUT ocReturn,OUTPUT obOk).

END.
