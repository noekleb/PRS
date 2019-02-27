/* Setter status på VPI korreksjonsposter. */
CURRENT-WINDOW:WIDTH = 350.
                            
DEF VAR iAnt AS INT EXTENT 10 NO-UNDO.
DEF VAR iTotal AS INT NO-UNDO.
DEF VAR iPlu AS INT NO-UNDO.

LOOPEN:
FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE
    VPIArtBas.EkstVPILevNr = 1000016:

    IF DEC(VPIArtBas.VareNr) < 10000 THEN
        iPlu = iPlu + 1.

    iTotal = iTotal + 1.
    CASE VPIArtBas.KorrStatus:
        WHEN  1 THEN iAnt[1] = iAnt[1] + 1.
        WHEN 10 THEN iAnt[2] = iAnt[2] + 1.
        WHEN 11 THEN iAnt[3] = iAnt[3] + 1.
        WHEN 20 THEN iAnt[4] = iAnt[4] + 1.
        WHEN 21 THEN iAnt[5] = iAnt[5] + 1.
        WHEN 79 THEN iAnt[6] = iAnt[6] + 1.
        WHEN 80 THEN iAnt[7] = iAnt[7] + 1.
        WHEN 90 THEN iAnt[8] = iAnt[8] + 1.
        OTHERWISE iAnt[9] = iAnt[9] + 1.
    END CASE.

END.

DISPLAY 
    ' 1           Ubehandlet' iAnt[1] SKIP
    '10       Manuelt koblet' iAnt[2] SKIP
    '11    Automatisk koblet' iAnt[3] SKIP
    '20    Manuelt opprettet' iAnt[4] SKIP
    '21 Automatisk opprettet' iAnt[5] SKIP
    '79       Til behandling' iAnt[6] SKIP
    '80               Avvist' iAnt[7] SKIP
    '90                Sendt' iAnt[8] SKIP
    '                 Øvrigt' iAnt[9] SKIP
    '                 Totalt' iTotal  SKIP
    '(PLUer)' iPlu
    .
