/*
VAL,,,,E,0200314720162,00001,

Bygg varetellingfil fra translogg.
For simulere varemottak.

1 200156200662 16/10/07 68561 0 7 "" "tomm" 1 0.00 0.00 0 0
1 200156130006 16/10/07 68563 0 7 "" "tomm" 1 0.00 0.00 0 0
*/

DEF TEMP-TABLE tmpVaretran
    FIELD ButikkNr   LIKE Butiker.Butik
    FIELD EANNr      LIKE Strekkode.Kode
    FIELD Dato       AS DATE
    FIELD Tid        AS INT 
    FIELD LoggNr     AS INT FORMAT ">>>>9"
    FIELD TTId       AS INT FORMAT ">9"
    FIELD Transtekst AS CHAR FORMAT "x(40)"
    FIELD BrukerId   AS CHAR
    FIELD Antall     AS DEC
    FIELD KostPris   AS DEC
    FIELD Salgssum   AS DEC
    FIELD NyLagAnt   AS DEC FORMAT ">>>>9.999"
    FIELD GmlLagAnt  AS DEC FORMAT ">>>>9.999"
    .

CURRENT-WINDOW:WIDTH = 100.

DEF VAR cLinje AS CHAR NO-UNDO.    

DEF STREAM ut.

FOR EACH TransLogg NO-LOCK WHERE 
    TransLogg.Butik = 1 AND
    TransLogg.TTId     = 1 :

    CREATE tmpVaretran.
    ASSIGN
        tmpVaretran.ButikkNr   = TransLogg.Butik
        tmpVaretran.EANNr      = TransLogg.Kode
        tmpVaretran.Dato       = TransLogg.Dato
        tmpVaretran.Tid        = TransLogg.Tid
        tmpVaretran.LoggNr     = 0
        tmpVaretran.TTId       = 7
        tmpVaretran.Transtekst = Translogg.Bongtekst
        tmpVaretran.BrukerId   = ""
        tmpVaretran.Antall     = Translogg.Antall
        tmpVaretran.KostPris   = TransLogg.VVAreKost
        tmpVaretran.Salgssum   = TransLogg.Pris
        tmpVaretran.NyLagAnt   = 0
        tmpVaretran.GmlLagAnt  = 0
        .
        .
END.



OUTPUT STREAM ut TO VALUE("c:\home\lindbak\ankommet\varetran.1").
  FOR EACH tmpVareTran:
      EXPORT STREAM Ut tmpVareTran.
  END.
OUTPUT STREAM ut CLOSE.
