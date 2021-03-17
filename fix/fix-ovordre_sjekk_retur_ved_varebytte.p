DEF BUFFER bufKOrdreLinje FOR KOrdreLinje.

FIND KOrdreHode NO-LOCK WHERE 
    KOrdrEHode.KOrdre_Id = 1190000008.

FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
    KOrdreLinje.Aktiv = TRUE AND
    KOrdreLinje.KopiKOrdreLinjeNr > 0 AND  
    KOrdreLinje.PlukkButikk > 0:



    FIND bufKOrdreLinje NO-LOCK WHERE 
          bufKOrdreLinje.KOrdre_Id     = KOrdreHode.RefKOrdre_Id AND 
          bufKOrdreLinje.KordreLinjeNr = KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
      
    MESSAGE 
    KOrdreHode.KOrdre_Id SKIP
    KOrdreHode.RefKOrdre_Id SKIP
    KOrdreHode.ReturNr SKIP
    KOrdreLinje.Aktiv SKIP
    KOrdreLinje.KopiKOrdreLinjeNr SKIP
    KOrdreLinje.PlukkButikk SKIP
    KOrdreLinje.VareNr SKIP
    KOrdrELinje.Varetekst SKIP(2)
    AVAILABLE bufKOrdreLinje SKIP
        bufKOrdreLinje.KopiKOrdreLinjeNr
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
END.
