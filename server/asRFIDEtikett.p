
/*------------------------------------------------------------------------
    File        : asRFIDEtikett.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Mon Nov 05 12:51:17 CET 2018
    Notes       : piRFIDEtikett:
                    0 = Ugyldig butikknr
                    1 = RFID etikett
                    2 = Vanlig etikett
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER piButNr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER piRFIDEtikett AS INTEGER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().

ASSIGN 
    bTest = TRUE 
    cLogg = 'asRFIDEtikett' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start asRFIDEtikett. Butikk: ' + STRING(piButNr) + '.'
    ). 

FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = piButNr NO-ERROR.
IF AVAILABLE Butiker THEN 
DO:
    FIND SysPara NO-LOCK WHERE
        SysPara.SysHId = 5 AND
        SysPara.SysGr  = 20 AND
        SysPara.ParaNr = Butiker.BELayout NO-ERROR.
    IF AVAILABLE SysPara THEN 
    DO:
        IF SysPara.Beskrivelse MATCHES '*RFID*' THEN 
            piRFIDEtikett = 1.
        ELSE 
            piRFIDEtikett = 2.

        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   SysPara.Beskrivelse=' + SysPara.Beskrivelse + ' piRFIDEtikett=' + STRING(piButNr) + '.'
            ). 
    END.
END.    

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Slutt asRFIDEtikett.'
    ). 

RETURN.