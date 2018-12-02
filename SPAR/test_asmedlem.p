def var lMedlemsNr as dec format ">>>>>>>>>>>>9" no-undo.
DEFINE var cMKortNr      AS CHAR      FORMAT "x(30)" NO-UNDO.
DEFINE var cBongTekst    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE var cNavn         AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE var cStatus       AS CHAR      FORMAT "x(10)" NO-UNDO.
  /* Status = 0, ukjent medlem.                     */
  /* Status = 1, Medelm funnet i SPAR og opprettet. */
  /* Status = 2, Medlemmet funnet lokalt.           */
DEFINE var bOK           AS LOGICAL     NO-UNDO.
DEFINE var cMelding      AS CHARACTER FORMAT "x(40)" NO-UNDO.


run asmedlem.p (2,'6201100010','tomn',output lMedlemsNr,
                output CMKortNr,
                output cBongTekst,
                output cNavn,
                output cStatus,
                output bOk,
                output cMelding).
                
message
lMedlemsNr skip
cMKortNr skip
cBongTekst skip
cNavn skip
cStatus skip
bOk skip
cMelding skip
view-as alert-box.                

         
