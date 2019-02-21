DEF VAR obOk AS LOG NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.

ASSIGN 
    iButNr = 20
    cFil   = 'C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64893.pdf' + 
             ' ' + 
             'C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64034.pdf' + 
             ' ' + 
             'C:\NSoft\Polygon\PRS\utskrift\But_10_PkSdl_180219135646_180219-18660.pdf'  
             
    FILE-INFO:FILE-NAME = cFil.
    .

FIND Butiker NO-LOCK WHERE 
    Butiker.butik = iButNr NO-ERROR.

RUN sendmail_tsl.p ("PAKKSEDDEL",
                    "Faktura " + '20900001' + ' i butikk ' + STRING(Butiker.butik) + 
                         " " + Butiker.ButNamn + ".",
                    FILE-INFO:FULL-PATHNAME,
                    "Faktura  " + '20900001' + " foretatt i butikk " + 
                        STRING(Butiker.butik) + 
                        " " + Butiker.ButNamn + ".  " + 
                        REPLACE('Merknad....',CHR(10),' ') + '  ' + 
                        REPLACE('Melding fra lev...',CHR(10),' '),
                    "",
                    "") 
    NO-ERROR.

