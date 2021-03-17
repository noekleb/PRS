
/*------------------------------------------------------------------------
    File        : asCRMKobling.p
    Purpose     : Fristille kassen fra det eksterne CRM systemet, slik at 
                  det i kassen ikke må gjøres endringer ved bytte av CRM 
                  system.

    Syntax      :

    Description : Viderekobling av kall fra POS til eksternt CRM system.  

    Author(s)   : Tom Nøkleby  
    Created     : Fri Oct 30 13:22:17 CET 2020
    Notes       : CrmSystem:
                    1 = Mayflower
                    2 = Dintero
                    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER cMethod     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cFunction   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cSearchType AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cParam      AS CHAR NO-UNDO.
DEF INPUT-OUTPUT  PARAMETER TABLE-HANDLE hParam . 

DEF OUTPUT PARAMETER obOk        AS LOG  NO-UNDO. /* Gikk kall fra POS bra (Bare kallet, ikke svaret)    */
DEF OUTPUT PARAMETER ocReturn    AS CHAR NO-UNDO. /* Eventuelle feilmeldinger                            */
DEF OUTPUT PARAMETER ocRetParam  AS CHAR NO-UNDO. /* Pipe separerte Parameterverdier som kassen skal ha. */


/* ********************  Preprocessor Definitions  ******************** */
DEFINE VARIABLE iCrmSystem AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

/* CRM System: 
   I systemparametrene er eksterne CRM systemer lagt opp. Det er bare 
   et av dem som kan være aktivt ad gangen. Det er det første aktive
   systemet som blir valgt. 
*/   
iCrmSystem = 0.
CRMSYSTEM:
FOR EACH SysPara NO-LOCK WHERE
  SysPara.SysHId =  14 AND
  SysPara.SysGr  =  200:

  ASSIGN iCrmSystem = INT(SysPara.Parameter1).
  IF iCrmSystem >= 1 THEN 
    LEAVE CRMSYSTEM.
END. /* CRMSYSTEM */

CASE iCrmSystem:
  WHEN 1 THEN 
    DO:
      RUN asMayflower.p (cMethod,
                         cFunction,
                         cSearchType,
                         cParam,
                         INPUT-OUTPUT TABLE-HANDLE hParam,
                         OUTPUT obOk,
                         OUTPUT ocReturn,
                         OUTPUT ocRetParam  
                         ).      
    END.
  WHEN 2 THEN 
    DO:
      RUN cls\dintero\asCRMDintero.p (cMethod,
                         cFunction,
                         cSearchType,
                         cParam,
                         INPUT-OUTPUT TABLE-HANDLE hParam,
                         OUTPUT obOk,
                         OUTPUT ocReturn,
                         OUTPUT ocRetParam  
                         ).      
    END.
  OTHERWISE  
    DO:
      ASSIGN 
        obOk = FALSE
        ocReturn = 'Ingen tilkoblede CRM systemer.'
        .
    END. 
END.  
