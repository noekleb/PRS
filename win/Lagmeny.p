/* Program: lagmeny.p (modifisert versjon av Per Ekmans cremenu.p)
            Lagre en komplett meny
   Tilpasset av Sturla Johnsen 21.05.98
   Justert for bruk i SkoTex av TN 3/7-98.

	Last change:  TN   17 Dec 99    0:56 am
*/

DEF INPUT PARAMETER wMdata        AS CHAR   NO-UNDO. /* Streng inneholdende menydata   */
DEF INPUT PARAMETER wParentHandle AS WIDGET NO-UNDO. /* Eier av menyen                 */
DEF INPUT PARAMETER wProcHandle   AS HANDLE NO-UNDO. /* Programmet som menyen ligger i */

DEF VAR wD1          AS CHAR   NO-UNDO INIT ";". /* Skiller hovedelementene */
DEF VAR wD2          AS CHAR   NO-UNDO INIT "|". /* Skiller delelementene   */
DEF VAR wLevelCnt    AS CHAR   NO-UNDO INIT "£". /* "Beskriver" nivået      */
DEF VAR i            AS INTE   NO-UNDO INIT 1.
DEF VAR j            AS INTE   NO-UNDO.
DEF VAR wNesteLevel  AS INTE   NO-UNDO.
DEF VAR wNesteEntry  AS CHAR   NO-UNDO.
DEF VAR wNesteLabel  AS CHAR   NO-UNDO.
DEF VAR wEntry       AS CHAR   NO-UNDO.
DEF VAR wHovedLabel  AS CHAR   NO-UNDO.
DEF VAR wFlagg       AS CHAR   NO-UNDO.
DEF VAR wHurtigtast  AS CHAR   NO-UNDO.
DEF VAR wParametere  AS CHAR   NO-UNDO.
DEF VAR wMenuHandle  AS WIDGET NO-UNDO.
DEF VAR wMainHandle  AS WIDGET NO-UNDO.
DEF VAR wProgram     AS CHAR   NO-UNDO.
DEF VAR wTekst       AS CHAR   NO-UNDO.
DEF VAR wPersistent  as CHAR   NO-UNDO.
DEF VAR wSendParam   as CHAR   NO-UNDO.
DEF VAR wTilgangsniva AS CHAR NO-UNDO. /*BK*/
DEF VAR lProgramTilgang AS LOGICAL INIT FALSE NO-UNDO. /*BK*/

IF wParentHandle:TYPE = "MENU" THEN
     ASSIGN wMainHandle = wParentHandle.
ELSE CREATE MENU wMainHandle
        ASSIGN POPUP-ONLY = wParentHandle:TYPE <> "WINDOW".

/* Lag første nivå */
RUN OpprettMeny(1,wMainHandle).

IF wParentHandle:TYPE =  "WINDOW" THEN ASSIGN wParentHandle:MENU-BAR   = wMainHandle. ELSE
IF wParentHandle:TYPE <> "MENU"   THEN ASSIGN wParentHandle:POPUP-MENU = wMainHandle.

PROCEDURE OpprettMeny:
   DEF INPUT PARAMETER wLevel  AS INTE   NO-UNDO.
   DEF INPUT PARAMETER wParent AS WIDGET NO-UNDO.

   REPEAT WHILE i <= NUM-ENTRIES(wMdata,wD1):
      ASSIGN wEntry      = ENTRY(i,wMdata,wD1)
             wHovedLabel = ENTRY(1,wEntry,wD2)
             wFlagg      = ENTRY(2,wEntry,wD2)
             wHurtigtast = ENTRY(3,wEntry,wD2)
             wParametere = ENTRY(4,wEntry,wD2)
             wProgram    = ENTRY(5,wEntry,wD2)
             /*wTilgangsniva = ENTRY(6, wEntry, wD2) /*BK*/*/
             wTekst      = TRIM(SUBSTR(wHovedLabel,wLevel))
             i           = i + 1
             wPersistent = if INDEX(wFlagg,"P") = 0
                             then "YES":U
                             else "NO":U
             wSendParam  = if INDEX(wFlagg,"F") = 0
                             then "NO":U
                             else "YES":U.

      IF i <= NUM-ENTRIES(wMdata,wD1) THEN
           ASSIGN wNesteEntry = ENTRY(i,wMdata,     wD1)
                  wNesteLabel = ENTRY(1,wNesteEntry,wD2)
                  wNesteLevel = 2 + LENGTH(wNesteLabel) - LENGTH(TRIM(wNesteLabel + "X",wLevelCnt)).
      ELSE ASSIGN wNesteLevel = wLevel - 1.

      IF wNesteLevel = wLevel + 1 OR INDEX(wFlagg,"S") > 0 THEN DO:
         CREATE SUB-MENU wMenuHandle
            ASSIGN PARENT    = wParent
                   LABEL     = wTekst
                   SENSITIVE = INDEX(wFlagg,"D") = 0.

         IF wNesteLevel = wLevel + 1 THEN DO:
            RUN OpprettMeny(wNesteLevel,wMenuHandle).

            IF wNesteLevel < wLevel THEN RETURN.
         END.
      END.
      ELSE DO:

        /*BK*********************************************************************
         Kontroller tilgangsnivå for bruker og setter meny til sensitiv dersom
         tilgang er ok ellers settes meny til insensitiv */
        DEF VAR cFilnavn AS CHAR NO-UNDO.
        cFilnavn = (IF NUM-ENTRIES(wProgram, ".") > 1 
                    THEN ENTRY(1, wProgram, ".") ELSE wProgram).
        IF CAN-DO("startsok",cFilNavn) THEN
            cFilNavn = entry(1,entry(1,wParametere),".").

        FIND Bruker WHERE Bruker.BrukerId = USERID("SkoTex") NO-LOCK NO-ERROR.
        FIND BrukerGrp NO-LOCK WHERE 
            BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-ERROR.
        IF CAN-FIND(FIRST ProgBrGrp 
                    WHERE ProgBrGrp.BrGrpNr = BrukerGrp.BrGrpNr 
                    AND ProgBrGrp.ProgNavn BEGINS cFilnavn ) THEN 
            lProgramTilgang = TRUE.
        ELSE
            lProgramTilgang = FALSE.

        IF wProgram = "Avslutt" THEN
            lProgramTilgang = TRUE.
  /* lProgramTilgang = TRUE.          */
        /***********************************************************************/

         ASSIGN j = INDEX(wFlagg,"R").
         IF j > 0 THEN DO:
            CREATE MENU-ITEM wMenuHandle.
               ASSIGN wMenuHandle:SUBTYPE = "RULE"
                      wMenuHandle:PARENT  = wParent NO-ERROR. /* I tilfelle noen lager rule på nivå 1 */
         END.
         ELSE DO:
            ASSIGN j = INDEX(wFlagg,"C").
            IF j = 0 THEN j = - INDEX(wFlagg,"U").

            CREATE MENU-ITEM wMenuHandle
               ASSIGN PARENT       = wParent
                      SENSITIVE    = ((INDEX(wFlagg,"D") = 0) AND lProgramTilgang)
                      LABEL        = wTekst
                      PRIVATE-DATA = (IF INDEX(wFlagg,"V") > 0
                                        THEN "<LagVindu>"
                                        ELSE "") + wD1 +
                                     (IF INDEX(wFlagg,"L") > 0
                                        THEN "Lib"
                                        ELSE "") + wD1 +
                                     (IF INDEX(wFlagg,"O") > 0
                                        THEN "Lok"
                                        ELSE "") + wD1 +
                                     wParametere + wD1 +
                                     wPersistent + wD1 +
                                     wSendParam
                      TOGGLE-BOX   = j <> 0
               TRIGGERS:
                  ON CHOOSE, VALUE-CHANGED PERSISTENT RUN StartMenyProgram IN wProcHandle (wProgram,wMenuHandle).
               END.
            IF wMenuHandle:TOGGLE-BOX  THEN ASSIGN wMenuHandle:CHECKED = j > 0.
            IF wHurtigtast <> "" AND wLevel > 1 THEN  ASSIGN wMenuHandle:ACCELERATOR = wHurtigtast.

         END.
         IF wNesteLevel = wLevel THEN NEXT. ELSE
         IF wNesteLevel < wLevel THEN RETURN.
      END.
   END.
END PROCEDURE.

