/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
  
/* Sletting av tellehode, med sletting av alle underliggende psoter, er lagt inn */
/* i d_tellelode.p (Trigger procedyren).                                         */
/* Her skal kun validering ligge.                                                */

FIND TelleHode WHERE ROWID(TelleHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.

IF AVAIL TelleHode THEN 
DO ON ERROR undo, LEAVE TRANSACTION:

  ASSIGN 
      ocReturn  = "Avbryt".

  ASSIGN
      ocReturn = "". /*Blank hvis ok*/
END. /*Transaction*/
