DEF NEW SHARED VAR wCurrLng   AS CHAR INITIAL "DES"  NO-UNDO.
DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.

DEF VAR cOsUser AS CHAR NO-UNDO.
cOsUser = OS-GETENV("username").

DEFINE VAR cADFesteBrukerList AS CHAR    NO-UNDO. /* ADBruker;FesteBruker; */
DEFINE VAR ix AS INT NO-UNDO.

cADFesteBrukerList = "tomn;tomn;pub;pub;tny;tomn"
.


ix = LOOKUP(cOsUser,cADFesteBrukerList,";").
IF ix GT 0 AND ix MODULO 2 = 1 THEN
  cOsUser = ENTRY(ix + 1,cADFesteBrukerList,";").

FUNCTION getMainMenuId RETURNS INTEGER():
  RETURN 442. /* Her settes MenuId... 1-SystemMeny, 442-ButikkAdminMeny*/
  /* NAV-BAR - Vanlig meny */
  /* RIBBON - Ribbon meny. */
END FUNCTION.

RUN jboxappstart.p
  (NO,
   "PRS Butikk og kjedestyring",
   "NO",
   "NO",
   "NO",
   "",
   cOsUser,
   "."
   ).
QUIT.

