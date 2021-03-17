
/* PROCEDURE forhRab%#1:                                               */
/*   /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/        */
/*   DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.                   */
/*   DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.                   */
/*   DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.                   */
/*                                                                     */
/*   FIND VPIDatasett WHERE ROWID(VPIDatasett) = irRowid NO-LOCK NO-ERROR. */
/*   IF AVAIL VPIDatasett THEN                                           */
/*     ocValue = STRING(VPIDatasett.forhRab%[1]).                        */
/*                                                                     */
/* END PROCEDURE.                                                      */

PROCEDURE chkAktiv:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bAktiv AS LOG NO-UNDO. 
  DEF VAR bFound AS LOG NO-UNDO. 

  ASSIGN 
    bAktiv      = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPIDatasett WHERE ROWID(VPIDatasett) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIDatasett THEN 
  DO:
    bFound = CAN-FIND(FIRST VPIArtBas OF VPIDatasett).
    ocValue = IF bFound THEN 
               (IF bAktiv THEN "TRUE" ELSE "TRUE") 
              ELSE 
                (IF bAktiv THEN "SKIPROW" ELSE "FALSE").
  END.
  
/*   MESSAGE "chkAktiv"  AVAILABLE VPIDatasett string(irRowid) */
/*       SKIP ocValue                                          */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */

END PROCEDURE.

PROCEDURE chkAktivLev:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bAktiv AS LOG NO-UNDO. 
  DEF VAR bFound AS LOG NO-UNDO. 

  ASSIGN
    bAktiv = LOGICAL(ENTRY(1,icParam,'¤'))
    .
  FIND VPIDatasett WHERE ROWID(VPIDatasett) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIDatasett THEN
  DO:
      FIND EkstVPILev where
          EkstVPILev.EkstVPILevNr = VPIDataSett.EkstVPILevNr NO-ERROR.
      if available EkstVPILev then 
      do:
        IF bAktiv THEN
          ocValue = IF EkstVPILev.AktivLev THEN "TRUE" ELSE "SKIPROW".            
        ELSE
          ocValue = IF EkstVPILev.AktivLev THEN "TRUE" ELSE "FALSE".
      end.
      else ocValue = "FALSE".
      
/* MESSAGE EkstVPILev.EkstVPILevNr EkstVPILev.AktivLev SKIP */
/*     ocValue                                              */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */
  END.

/*   MESSAGE  "chkAktivLev" AVAILABLE VPIDatasett string(irRowid) */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */
END PROCEDURE.

/* PROCEDURE chkArtikkel:                                                                                            */
/*   DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.                                                                 */
/*   DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.                                                                 */
/*   DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.                                                                 */
/*   DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.                                                                 */
/*                                                                                                                   */
/*   DEF VAR iVarebokNr      AS INT    NO-UNDO.                                                                      */
/*                                                                                                                   */
/*   iVarebokNr      = INT(ENTRY(1,icParam,'¤')).                                                                    */
/*                                                                                                                   */
/*   FIND VPIDatasett WHERE ROWID(VPIDatasett) = irRowid NO-LOCK NO-ERROR.                                           */
/*   IF AVAIL VPIDatasett THEN                                                                                       */
/*   DO:                                                                                                             */
/*     FIND VareBokHode WHERE VareBokHode.vareboknr = iVarebokNr NO-LOCK NO-ERROR.                                   */
/*     IF AVAIL VareBokHode THEN                                                                                     */
/*     DO:                                                                                                           */
/*       FOR varebokLinje OF VarebokHode WHERE VarebokLinje.ArtikkelNr = VPIDatasett.ArtikkelNr NO-LOCK: LEAVE. END. */
/*       IF NOT AVAIL VarebokLinje THEN                                                                              */
/*       DO:                                                                                                         */
/*         ocValue = 'SKIPROW'.                                                                                      */
/*       END.                                                                                                        */
/*     END.                                                                                                          */
/*   END.                                                                                                            */
/* END PROCEDURE.                                                                                                    */
