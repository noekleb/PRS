CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cUtFil   AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR FORMAT "x(100)" NO-UNDO.

ASSIGN
    cFilNavn = "Error-Karlson fjernet utgåtte varer.Txt"
    cUtFil   = "Error-Karlson.txt"
    .

DEF STREAM Inn.
DEF STREAM UT.

DEF BUFFER bVPIArtBas FOR VPIArtBas.
DEF BUFFER bVPIStrekkode FOR VPISTrekkode.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
OUTPUT STREAM UT TO VALUE(cUtFil) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    IF cLinje BEGINS "*" THEN
    DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = TRIM(ENTRY(5,cLinje," ")) NO-ERROR.
        IF AVAILABLE Strekkode THEN
            FIND ArtBas OF Strekkode exclusive-lock NO-ERROR.

        FIND VPIStrekkode WHERE
            VPIStrekkode.EkstVPILevNr = 38 AND
            VPIStrekkode.Kode = TRIM(ENTRY(5,cLinje," ")) NO-ERROR.
        IF AVAILABLE VPISTrekkode THEN
            FIND VPIArtBas OF VPISTrekkode NO-ERROR.

        FIND bVPIStrekkode WHERE
            bVPIStrekkode.EkstVPILevNr = 1 AND
            bVPIStrekkode.Kode = TRIM(ENTRY(5,cLinje," ")) NO-ERROR.
        IF AVAILABLE VPISTrekkode THEN
            FIND bVPIArtBas OF VPISTrekkode NO-ERROR.
       
       
        if available ArtBas then
        do:
        assign
        ArtBas.LevKod     = entry(1,ENTRY(2,cLinje,"'"),"+")
        ArtBas.Beskr      = entry(2,ENTRY(2,cLinje,"'"),"+")
        ArtBas.BongTekst  = entry(2,ENTRY(2,cLinje,"'"),"+")
        .
        if num-entries(ENTRY(2,cLinje,"'"),"+") = 3 then
        ArtBas.LevFargKod = entry(3,ENTRY(2,cLinje,"'"),"+")
        .
        end.
        /*    
        DISPLAY 
            (IF AVAILABLE ArtBas
               THEN string(ArtBas.ArtikkelNr)
               ELSE "------------") COLUMN-LABEL "ArtBas" format "x(15)"
            /*
            (IF AVAILABLE VPIArtBas
               THEN string(VPIArtBas.ArtikkelNr)
               ELSE "------------") COLUMN-LABEL "VPIArtBas-038" format "x(15)"
            (IF AVAILABLE bVPIArtBas
               THEN string(bVPIArtBas.ArtikkelNr)
               ELSE "------------") COLUMN-LABEL "VPIArtBas-001" format "x(15)"
            */
            if available ArtBas then ArtBas.LevKod else "" column-label "LevKod"
            if available ArtBas then ArtBas.Beskr  else "" column-label "Beskr"
            if available ArtBas then ArtBas.LevFargKod else "" column-label "LevFargKod"
            entry(1,ENTRY(2,cLinje,"'"),"+")
            entry(2,ENTRY(2,cLinje,"'"),"+")
            entry(3,ENTRY(2,cLinje,"'"),"+")
            ENTRY(5,cLinje," ") FORMAT "x(15)"
            ENTRY(2,cLinje,"'") FORMAT "x(50)"
            ENTRY(4,cLinje,"'") FORMAT "x(50)"
            
            WITH WIDTH 300.
        */
        
        
        PUT STREAM uT UNFORMATTED
            ENTRY(5,cLinje," ") FORMAT "x(15)"
            ENTRY(2,cLinje,"'") FORMAT "x(50)"
            ENTRY(4,cLinje,"'") FORMAT "x(50)"
            SKIP
            .
    END.
END.
OUTPUT STREAM UT CLOSE.
INPUT STREAM Inn CLOSE.
