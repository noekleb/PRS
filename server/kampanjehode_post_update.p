/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbunt */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldsParam   AS CHAR  NO-UNDO.   

DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/*cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.*/

ASSIGN 
  bTest = FALSE
  cLogg = 'kampanjehode_post_update' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

/*ASSIGN                                                                                                                                   */
/*  ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE = INT(ENTRY(1,ihBuffer:BUFFER-FIELD("AktiveresTid_time"):BUFFER-VALUE,':')) * 3600 +*/
/*                                                       INT(ENTRY(2,ihBuffer:BUFFER-FIELD("AktiveresTid_time"):BUFFER-VALUE,':')) * 60    */
/*  ihBuffer:BUFFER-FIELD("GyldigTilTid"):BUFFER-VALUE = INT(ENTRY(1,ihBuffer:BUFFER-FIELD("GyldigTilTid_time"):BUFFER-VALUE,':')) * 3600 +*/
/*                                                       INT(ENTRY(2,ihBuffer:BUFFER-FIELD("GyldigTilTid_time"):BUFFER-VALUE,':')) * 60    */
/*  .                                                                                                                                      */

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Verdier fra buffer: ' 
    ).    
/*  rStandardFunksjoner:SkrivTilLogg(cLogg,                                                      */
/*    '    AktiveresTid_time: ' + STRING(ihBuffer:BUFFER-FIELD("AktiveresTid_time"):BUFFER-VALUE)*/
/*    ).                                                                                         */
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    AktiveresTid.....: ' + STRING(ihBuffer:BUFFER-FIELD("AktiveresTid"):BUFFER-VALUE) 
    ).    
/*  rStandardFunksjoner:SkrivTilLogg(cLogg,                                                      */
/*    '    GyldigTilTid_time: ' + STRING(ihBuffer:BUFFER-FIELD("GyldigTilTid_time"):BUFFER-VALUE)*/
/*    ).                                                                                         */
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    GyldigTilTid.....: ' + STRING(ihBuffer:BUFFER-FIELD("GyldigTilTid"):BUFFER-VALUE) 
    ).    
END.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    

ocValue = "".
RETURN.
