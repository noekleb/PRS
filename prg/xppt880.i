DEFINE {&NEW} {&SHARED} TEMP-TABLE tmpPDA
    FIELD Butnr      AS INT  FORMAT ">>>>>9"
    FIELD Ean        AS DEC  FORMAT "->>>>>>>>>>>>9"
    FIELD Dato       AS DATE FORMAT "99/99-99"
    FIELD Tid        AS INT  FORMAT ">>>>9"
    FIELD Loggnr     AS INT  FORMAT ">>>>>9"
    FIELD Transtype  AS INT  FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(30)"
    FIELD Brukerid   AS CHAR FORMAT "x(12)"
    FIELD Antall     AS DEC  FORMAT "->>,>>9.999"   
    FIELD Kostpris   AS DEC  FORMAT "->>>,>>9.99"   
    FIELD Salgssum   AS DEC  FORMAT "->,>>>,>>9.99" 
    FIELD Nylagant   AS DEC  FORMAT "->>>,>>9.999"  
    FIELD Gmlagant   AS DEC  FORMAT "->>>,>>9.999" 
    FIELD LinjeNr    AS INT  FORMAT ">>>>>>>9"
    {&FIELD}
    .
