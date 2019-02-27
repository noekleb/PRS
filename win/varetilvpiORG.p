
 /*

	Last change:  BO    3 Jun 99    3:25 pm
*/

DEFINE INPUT PARAMETER wQY          AS CHAR NO-UNDO.

DEF VAR hField1      AS HANDLE NO-UNDO.
DEF VAR hField2      AS HANDLE NO-UNDO.
DEF VAR hField3      AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR cFilnavn     AS CHAR   NO-UNDO.
DEF VAR iLoop        AS INT    NO-UNDO.
DEF VAR cModellFarge AS CHAR NO-UNDO.
DEF VAR iErstattId   LIKE Erstattningsvare.ErstattId NO-UNDO.
DEFINE BUFFER bArtBas FOR ArtBas.
CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE "ArtBas".

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
hQuery:QUERY-OPEN().

STATUS DEFAULT "".
REPEAT:
    hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
        ASSIGN hField1      = hBuffer:BUFFER-FIELD("Artikkelnr")
               hField2      = hBuffer:BUFFER-FIELD("ModellFarge")
               hField3      = hBuffer:BUFFER-FIELD("Pakke")
             hField3      = hBuffer:BUFFER-FIELD("Pakke")
               cModellFarge = TRIM(hField2:STRING-VALUE())
               iErstattId   = ?.
/*                 hField3 = hBuffer:BUFFER-FIELD("lager"). */
/*        IF hField3:BUFFER-VALUE() = FALSE THEN            */
/*            NEXT.                                         */
        FIND ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                         ELogg.EksterntSystem = "VPI"    AND
                         ELogg.Verdier        = TRIM(hField1:STRING-VALUE()) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtBas"
                   ELogg.EksterntSystem = "VPI"   
                   ELogg.Verdier        = TRIM(hField1:STRING-VALUE())
                   iLoop                = iLoop + 1.
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
        FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR.
        IF AVAIL Erstattningsvare THEN
            ASSIGN iErstattId = Erstattningsvare.ErstattId.
/*         FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR. */
        IF cModellFarge <> "0" THEN DO:
            FOR EACH bArtBas WHERE bArtBas.ModellFarge = DECI(cModellFarge) AND 
                                   bArtBas.ArtikkelNr <> hField1:BUFFER-VALUE():
                FIND ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                                 ELogg.EksterntSystem = "VPI"    AND
                                 ELogg.Verdier        = TRIM(STRING(bArtBas.ArtikkelNr)) NO-ERROR.
                IF NOT AVAIL Elogg THEN DO:
                    CREATE Elogg.
                    ASSIGN ELogg.TabellNavn     = "ArtBas"
                           ELogg.EksterntSystem = "VPI"   
                           ELogg.Verdier        = TRIM(STRING(bArtBas.ArtikkelNr))
                           iLoop                = iLoop + 1.
                END.
                ASSIGN ELogg.EndringsType = 1
                       ELogg.Behandlet    = FALSE
                       iLoop              = iLoop + 1.
                IF iErstattId = ? THEN DO:
                    FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR.
                    IF AVAIL Erstattningsvare THEN
                        ASSIGN iErstattId = Erstattningsvare.ErstattId.
                END.
            END.
        END.
        ELSE IF hField3:BUFFER-VALUE = TRUE THEN DO:

        END.
        IF iErstattId <> ? THEN DO:
            FIND ELogg WHERE ELogg.TabellNavn     = "Erstattningsvare" AND
                             ELogg.EksterntSystem = "VPI"    AND
                             ELogg.Verdier        = STRING(iErstattId) NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "Erstattningsvare"
                       ELogg.EksterntSystem = "VPI"   
                       ELogg.Verdier        = STRING(iErstattId)
/*                        iLoop                = iLoop + 1. */
                    .
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE
/*                    iLoop              = iLoop + 1. */
                .
        END.
/*         FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = */
/*    IF iLoop MODULO 50 = 0 THEN                                              */
/*      STATUS DEFAULT "Antall eksporterte varelinjer " + STRING(iLoop) + ".". */
END.
STATUS DEFAULT "".
MESSAGE "Antall varelinjer til VPI-registeret: " + STRING(iLoop) + "."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
