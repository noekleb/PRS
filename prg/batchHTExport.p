/* batchHTExport.p. -param "HTTYPEID=10,BUTIK=473" */
DEFINE VARIABLE cHTTYPEID AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBUTIK  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hArtbas AS HANDLE      NO-UNDO.

DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"=") = "HTTYPEID" THEN
        cHTTYPEID = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=").
    ELSE IF ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"=") = "BUTIK" THEN
        cBUTIK = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=").
END.

FIND HT-Type WHERE HT-Type.TypeId = INT(cHTTYPEID) NO-LOCK NO-ERROR.
IF AVAIL HT-Type THEN DO:
    FIND butiker WHERE butiker.butik = INT(cBUTIK) NO-LOCK NO-ERROR.
/*     OUTPUT TO c:\tmp\htexTST.txt.                         */
/*           PUT UNFORMATTED "HT AVAIL " AVAIL ht-type SKIP  */
/*                           "BUTIK    " AVAIL butiker SKIP. */
/*     OUTPUT CLOSE.                                         */
    
    IF AVAIL Butiker THEN DO:
        ASSIGN hArtbas = BUFFER Artbas:HANDLE.
        RUN eksportHTfil.p (hArtbas,
                            HT-Type.TypeId,
                            HT-Type.Eksportkatalog,
                            HT-Type.EkspFilPrefix,
                            cBUTIK
                            ).
    END.
END.
QUIT.
