DEF VAR bOk AS LOG NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.

{cls\dintero\ttCategory.i}
{cls\dintero\dsCategory.i}

FIND HovedKategori NO-LOCK WHERE 
    Hovedkategori.HovedKatNr = 100 NO-ERROR.

RUN cls\dintero\asCategoryDintero.p( Hovedkategori.HovedKatNr,
                                     OUTPUT bOk, 
                                     OUTPUT ocReturn
                                     ).

MESSAGE bOk ocReturn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                                         
