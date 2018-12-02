DEFINE VARIABLE ocBildeFil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wBildepeker AS HANDLE     NO-UNDO.
     DEFINE STREAM stream1.

find varebokhode where vareboknr = 9000008 no-lock.
/* for EACH artbas no-lock.                                                                  */
/*     IF NOT AVAIL artbas  THEN                                                             */
/*         NEXT.                                                                             */
/*     FIND BildeRegister NO-LOCK WHERE                                                      */
/*       BildeRegister.BildNr = Artbas.BildNr NO-ERROR.                                      */
/*     IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:                 */
/*         RUN HentBildePeker  (Artbas.BildNr, 1, BildeRegister.FilNavn, OUTPUT ocBildeFil). */
/*         RUN HentBildePeker  (Artbas.BildNr, 2, BildeRegister.FilNavn, OUTPUT ocBildeFil). */
/*     END.                                                                                  */
/* END.                                                                                      */
for each vareboklinje of varebokhode NO-LOCK /*WHERE
    VarebokLinje.LevNr = 10*/.
    find artbas of vareboklinje NO-LOCK NO-ERROR.
    IF NOT AVAIL artbas  THEN
        NEXT.
    FIND BildeRegister NO-LOCK WHERE
      BildeRegister.BildNr = Artbas.BildNr NO-ERROR.
    IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
        RUN HentBildePeker  (Artbas.BildNr, 1, BildeRegister.FilNavn, OUTPUT ocBildeFil).
        RUN HentBildePeker  (Artbas.BildNr, 2, BildeRegister.FilNavn, OUTPUT ocBildeFil).
    END.



end.

   
    PROCEDURE HentBildePeker:
     DEF INPUT  PARAMETER ipBildNr     as INT  NO-UNDO.
     DEF INPUT  PARAMETER ipMode       as INT  NO-UNDO.
     DEF INPUT PARAMETER  ipBilde      as CHAR NO-UNDO.
     DEF OUTPUT PARAMETER ipBildePeker as CHAR NO-UNDO.

     DEF BUFFER bufBildeData     FOR BildeData.
     DEF BUFFER bufBildeRegister FOR BildeRegister.

     DEF VAR ipBildeKatalog as CHAR NO-UNDO.
     DEF VAR iTeller        AS INTE NO-UNDO.

     /* Katalog */
     ipBildeKatalog = ".\bilder\".

     /*Blankt bilde */

     IF ipMode = 11 THEN DO: /* Vi sletter bilde på disk, pga ny bilde på artikkel */
         ipMode = 1.
         IF SEARCH(ipBildeKatalog + "mini" + ipBilde) <> ? THEN
             OS-DELETE VALUE(ipBildeKatalog + "mini" + ipBilde).
         IF SEARCH(ipBildeKatalog + ipBilde) <> ? THEN
             OS-DELETE VALUE(ipBildeKatalog + ipBilde).
     END.

     if ipBilde <> "" then
     LESUT:
     DO:
       /* Finnes ikke bildet på disk, legges det ut fra DB hvis det finnes der. */
       if SEARCH(ipBildeKatalog + (IF ipMode = 1 THEN "mini" ELSE "") + ipBilde) = ? then
         LES_UT:
         DO:
           FIND bufBildeRegister EXCLUSIVE-LOCK where
             bufBildeRegister.BildNr = ipBildNr NO-ERROR.
           if NOT AVAILABLE bufBildeRegister then
             LEAVE LES_UT.
           IF ipMode = 1 THEN DO:
               if NOT CAN-FIND(FIRST bufBildeData OF bufBildeRegister WHERE bufBildeData.Teller = 200) then
                   ASSIGN ipMode = 3.
               ELSE 
                   ASSIGN ipBilde = "mini" + ipBilde
                          iTeller = 200.

           END.
           if ipMode = 3 AND NOT CAN-FIND(FIRST bufBildeData OF bufBildeRegister) then
             LEAVE LES_UT.

            OUTPUT STREAM Stream1 TO VALUE(ipBildeKatalog + ipBilde) NO-MAP NO-CONVERT.
            FOR EACH bufBildeData FIELDS(RawData) OF bufBildeRegister WHERE bufBildeData.Teller >= iTeller NO-LOCK:
              PUT STREAM Stream1 CONTROL bufBildeData.RawData.
            END.
            OUTPUT STREAM Stream1 CLOSE.
         END. /* LES_UT */
         ELSE ASSIGN ipBilde = (IF ipMode = 1 THEN "mini" ELSE "") + ipBilde.
     END. /* LESUT */
     ELSE ipMode = 2.
     /* Henter bilde.                            */
     /*Er bilde ukjent, skal blankt bilde vises. */
    END PROCEDURE.
