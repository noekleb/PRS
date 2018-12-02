/************************************************************
    Program:  bt-klargjorprisko.p
    Created:  TN   10 Jan 1999
Description:  Klargjør priskø.

Last change:  TN   13 Mar 99    6:18 pm
************************************************************/

DEF VAR wPause as int  no-undo.
DEF VAR wStopp as CHAR NO-UNDO.

DEF VAR wLoggFil    as char NO-UNDO.
DEF VAR wLogKatalog as CHAR NO-UNDO.
DEF VAR wTekst      as CHAR NO-UNDO.

run ByggLoggFilNavn.

/* Nullstiller loggen. */
IF SEARCH(wLoggFil) <> ? then
  OS-DELETE VALUE(wLoggFil).

assign
  wTekst = "".
run SkrivTilLogg (wTekst).
assign
  wTekst = "Start av BATCH-Jobb KlargjorPrisKo.".
run SkrivTilLogg (wTekst).
assign
  wTekst = "---------------------------------------------------------------".
run SkrivTilLogg (wTekst).

/* Utfører jobben */
LOOPEN:
do WHILE TRUE:
  assign
    wTekst = "Klargjør priskø: ".
  run SkrivTilLogg (wTekst).

  run x-klargjorprisko.w (?).

  {syspara.i 200 5 2 wPause int}
  if terminal <> "" then
    wait-for "close":U of this-procedure pause wPause.
  else 
    pause wPause.

  {syspara.i 200 5 1 wStopp}
  IF CAN-DO("Stopp,Stop,Avbryt,Quit,End",wStopp)then
    LEAVE LOOPEN.

END. /* LOOPEN */

assign
  wTekst = "---------------------------------------------------------------".
run SkrivTilLogg (wTekst).
assign
  wTekst = "Stopp av BATCH-Jobb KlargjorPrisKo.".
run SkrivTilLogg (wTekst).
assign
  wTekst = "---------------------------------------------------------------".
run SkrivTilLogg (wTekst).

QUIT.

PROCEDURE ByggLoggFilNavn:
assign
  wLoggFil = "batch-klargjorprisko.log".

/* Katalognavn */
if available SysPara then release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 1 100 wLogKatalog}
else {syspara.i 50 1 100 wLogKatalog}

if wLogKatalog = "" then
  wLogKatalog = if opsys = "unix" then "." else ".".
if substring(wLogKatalog,length(wLogKatalog),1) = "/" or
   substring(wLogKatalog,length(wLogKatalog),1) = "\" then
 wLogKatalog = substring(wLogKatalog,1,length(wLogKatalog) - 1).
    
/* Bygger full path til fil */
assign
  wLoggFil = wLogKatalog +
            (if opsys = "unix" 
               then "/"
               else "\") +
            wLoggFil.

return "OK".

END PROCEDURE.

PROCEDURE SkrivTilLogg:
  def INPUT PARAMETER wLoggInfo as CHAR NO-UNDO.

  if wLoggInfo = "" THEN .
  ELSE IF wLoggInfo begins "---" THEN.
  else
    wLoggInfo = wLoggInfo + " " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS").

  OUTPUT TO VALUE(wLoggFil) APPEND.
    PUT unformatted wLoggInfo SKIP.
  output close.

END PROCEDURE.

