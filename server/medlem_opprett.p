/* Opprett nytt medlem for kunde 
   Parametere:  kundenr
   Opprettet: 30.06.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR wMedlemsNr  AS DEC NO-UNDO.
DEF VAR wCL         AS INT NO-UNDO.
{syspara.i 5  1 1 wCl INT}     /* Sentrallager. */

RUN opprettmedlem.p (DEC(icParam), "", wCl, OUTPUT wMedlemsNr).
IF RETURN-VALUE = "AVBRYT" THEN
  ocReturn = "Feil ved opprettelse av medlem".
FIND medlem NO-LOCK 
     WHERE Medlem.MedlemsNr = wMedlemsNr
     NO-ERROR.
IF NOT AVAIL medlem THEN
  ocReturn = "Medlem ble ikke opprettet. Programfeil".

IF ocReturn = "" THEN 
  ASSIGN obOk = TRUE
         ocReturn = STRING(ROWID(medlem))
         .
