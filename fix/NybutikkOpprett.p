FOR EACH kjedensbutikker:
    CASE kjedensbutikker.adresse2:
        WHEN "MegaD" THEN
            run NyButikkFraKjede.p (KjedensButikker.ButikkNr,30).
        WHEN "Sweda" THEN
            run NyButikkFraKjede.p (KjedensButikker.ButikkNr,20).
        OTHERWISE
            MESSAGE "Ukjent parameter " STRING(KjedensButikker.ButikkNr)
                KjedensButikker.Adresse2
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END CASE.
END.
    

/*     "for each kjedensbutikker..."                                    */
/*    modellnr läser du från kjedensbutikker.adresse2 där står antingen */
/*     "Sweda" (modellnr = 20) eller "MegaD" (modellnr = 30).           */
