DEFINE VARIABLE ix         AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDato      AS DATE      NO-UNDO.
DEFINE VARIABLE iTime      AS INTEGER NO-UNDO.
DEFINE VARIABLE cTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeLst            AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rHentConsignorData AS cls.Consignor.HentConsignorData NO-UNDO.
DEFINE VARIABLE rPostPakkeEtikett AS cls.Consignor.PostPakkeEtikett NO-UNDO.

{cls\Consignor\tmpTblKOrdreHode.i}
{ cls\StdFunk\filliste.i }

ASSIGN 
    cLogg    = 'HentConsignorData' + REPLACE(STRING(TODAY),'/','')
    .
rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).
/* Starter med tom linje i loggen. */
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start HentConsignorData.' 
    ).    

/* Starter klassene. */
rHentConsignorData  = NEW cls.Consignor.HentConsignorData( INPUT cLogg ).
rPostPakkeEtikett   = NEW cls.Consignor.PostPakkeEtikett ( INPUT cLogg ).


/* Sjekker og leser inn postpakke etiketter. */
rPostPakkeEtikett:hentFilListe ( OUTPUT TABLE tmpFiler ).
rPostPakkeEtikett:rensFilListe ( INPUT-OUTPUT TABLE tmpFiler ).
rPostPakkeEtikett:importerPdfFiler( INPUT TABLE tmpFiler ).

EMPTY TEMP-TABLE tmpKOrdreHode NO-ERROR.


