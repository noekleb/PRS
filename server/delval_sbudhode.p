/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
  
/* Sletting av budsjettmal, med sletting av alle underliggende poster, er lagt inn */
/* i d_sbudmalhode.p (Trigger procedyren).                                         */
/* Her skal kun validering ligge.                                                */

FIND SBudHode WHERE ROWID(SBudHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.

IF AVAIL SBudHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:

  ASSIGN 
      ocReturn  = "Avbryt".

  ASSIGN
      ocReturn = "". /*Blank hvis ok*/
END. /*Transaction*/
