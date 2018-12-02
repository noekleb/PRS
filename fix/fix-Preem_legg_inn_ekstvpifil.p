DEF BUFFER bufEkstVPILev FOR EkstVPILev.
DEF BUFFER bufEkstVPIFil FOR EkstVPIFil.

FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = 907 NO-ERROR.

FOR EACH bufEkstVPILev EXCLUSIVE-LOCK WHERE
    bufEkstVPILev.EkstVPILevNr > 1000999:

    /* Tar bort gammel ekstVPIFil POSVPI */
    FOR EACH EkstVPIFil WHERE
        EkstVPIFil.EkstVPILevNr = bufEkstVPILev.EkstVPILevNr:
        DELETE EkstVPIFil.
    END.
    
    /* Kopierer og modifiserer oppsett fra 907 */
    FOR EACH EkstVPIFil WHERE
        EkstVPIFil.EkstVPILevNr = EkstVPILev.EkstVPILevNr:
        CREATE bufEkstVPIFil.
        BUFFER-COPY EkstVPIFil
            EXCEPT EkstVPIFil.EkstVPILevNr
            TO bufEkstVPIFil
            ASSIGN
            bufEkstVPIFil.EkstVPILevNr = bufEkstVPILev.EkstVPILevNr
            bufEkstVPIFil.VPIFilAktiv  = FALSE
            bufEkstVPIFil.VPIKatalog   = EkstVPIFil.VPIKatalog + '\' + LEFT-TRIM(SUBSTRING(STRING(bufEkstVPILev.EkstVPILevNr),2),'0')
            .
        IF bufEkstVPIFil.VPIFilNavn BEGINS 'GVPI' THEN
            bufEkstVPIFil.VPIFilNavn = 'GVPI' + LEFT-TRIM(SUBSTRING(STRING(bufEkstVPILev.EkstVPILevNr),2),'0').
    END.
    DISPLAY 
        bufEkstVPILev.EkstVPILevNr
        .
END.
