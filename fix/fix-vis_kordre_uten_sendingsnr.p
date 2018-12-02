DEF VAR cFil AS CHAR NO-UNDO.
DEF STREAM Ut.

cFil = 'konv\ordre_uten_sendingsnr' + 
       REPLACE(STRING(TODAY),'/','') +  '_' + 
       REPLACE(STRING(TIME,"HH:MM:SS"),':','') + 
       '.csv'.

OUTPUT STREAM Ut TO VALUE(cFil).
    
PUT STREAM Ut UNFORMATTED  
    'EkstORdreNr;'
    'LevStatus;'
    'SendingsNr;'
    'Utsendelsesdato;'
    'KundeNr;'
    'Navn;'
    'ePostadresse;'
    'Mobiltlf'
SKIP.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdreHode.Utsendelsesdato >= 03/01/2017 AND
    KOrdreHode.LevStatus > '30' AND
    KOrdreHode.SendingsNr = ''
    BY KOrdreHode.Utsendelsesdato:
    
    DISPLAY
        KOrdreHode.EkstORdreNr
        KOrdreHode.LevStatus
        KORdreHode.SendingsNr
        KORdreHode.Utsendelsesdato
        KOrdreHode.KundeNr
        KOrdreHode.Navn
        KORdreHode.ePostadresse
        KOrdreHode.Mobiltlf
        WITH WIDTH 350.
    
    PUT STREAM Ut UNFORMATTED  
        KOrdreHode.EkstORdreNr ';'
        KOrdreHode.LevStatus ';'
        KORdreHode.SendingsNr ';'
        KORdreHode.Utsendelsesdato ';'
        KOrdreHode.KundeNr ';'
        KOrdreHode.Navn ';'
        KORdreHode.ePostadresse ';'
        KOrdreHode.Mobiltlf
    SKIP.
END.

OUTPUT STREAM Ut CLOSE.

