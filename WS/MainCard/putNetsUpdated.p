/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : putNetsUpdated.p                              ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcSessionId AS CHAR NO-UNDO.    
DEFINE INPUT PARAMETER ipiStorenumber AS INT NO-UNDO. 
DEFINE INPUT PARAMETER TABLE-HANDLE tt_Nets .
DEFINE INPUT PARAMETER TABLE-HANDLE tt_BongHode. 
DEFINE INPUT PARAMETER TABLE-HANDLE tt_BongLinje. 

DEFINE BUFFER dbNets FOR Nets. 
DEFINE VARIABLE hdbNets AS HANDLE NO-UNDO. 
DEFINE BUFFER dbBongHode FOR BongHode. 
DEFINE VARIABLE hdbBongHode AS HANDLE NO-UNDO. 
DEFINE BUFFER dbBongLinje FOR BongLinje. 
DEFINE VARIABLE hdbBongLinje AS HANDLE NO-UNDO. 

DISABLE TRIGGERS FOR LOAD OF dbNets. 
DISABLE TRIGGERS FOR DUMP OF dbNets. 
DISABLE TRIGGERS FOR LOAD OF dbBongHode. 
DISABLE TRIGGERS FOR DUMP OF dbBongHode. 
DISABLE TRIGGERS FOR LOAD OF dbBongLinje. 
DISABLE TRIGGERS FOR DUMP OF dbBongLinje. 
 
DEFINE VARIABLE iError AS INTEGER EXTENT 3 NO-UNDO. 
DEFINE VARIABLE bfNets AS HANDLE NO-UNDO. 
DEFINE VARIABLE bfBonghode AS HANDLE NO-UNDO. 
DEFINE VARIABLE bfBongLinje AS HANDLE NO-UNDO. 
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
DEFINE VARIABLE iCnt   AS INT NO-UNDO.
DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

bfNets = tt_Nets:DEFAULT-BUFFER-HANDLE. 

CREATE QUERY hQuery. 
hQuery:SET-BUFFERS (bfNets) .
hQuery:QUERY-PREPARE ("for each " + bfNets:NAME) .
hQuery:QUERY-OPEN () .

MESSAGE "Receive Data" bfNets:NAME . 

DO WHILE hQuery:GET-NEXT(NO-LOCK)   AND 
     NOT hQuery:QUERY-OFF-END : 
    
    DO TRANSACTION ON ERROR UNDO, NEXT: 
        hdbNets = BUFFER dbNets:HANDLE. 
        hdbNets:BUFFER-CREATE(). 
        lStatus = hdbNets:BUFFER-COPY(bfNets,?,"") . 
        CATCH oneError AS Progress.Lang.SysError:   
            iError[1] = iError[1] + 1.
            /* MESSAGE "Error:" oneError:GetMessage(1).*/   
        END CATCH.
    END.
    iCnt = iCnt + 1. 
END.
MESSAGE "Nets Transactions:" + STRING(iCnt). 
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery. 


bfBongHode = tt_Bonghode:DEFAULT-BUFFER-HANDLE. 
CREATE QUERY hQuery. 
hQuery:SET-BUFFERS (bfBongHode) .
hQuery:QUERY-PREPARE ("for each " + bfBongHode:NAME) .
hQuery:QUERY-OPEN () .
iCnt = 0.
DO WHILE hQuery:GET-NEXT(NO-LOCK)   AND 
    NOT hQuery:QUERY-OFF-END : 

    DO TRANSACTION ON ERROR UNDO, NEXT: 
        hdbBonghode = BUFFER dbBongHode:HANDLE. 
        hdbbonghode:BUFFER-CREATE(). 
        lStatus = hdbbonghode:BUFFER-COPY(bfBonghode,?,""). 
        CATCH oneError AS Progress.Lang.SysError:   
            iError[2] = iError[2] + 1.
            /*MESSAGE "Error:" oneError:GetMessage(1). */
        END CATCH.
    END.
    iCnt = iCnt + 1. 
END.
MESSAGE "BongHode Transactions:" + STRING(iCnt). 
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery. 


bfBongLinje = tt_BongLinje:DEFAULT-BUFFER-HANDLE. 
CREATE QUERY hQuery. 
hQuery:SET-BUFFERS (bfBongLinje) .
hQuery:QUERY-PREPARE ("for each " + bfBongLinje:NAME) .
hQuery:QUERY-OPEN () .
iCnt = 0.
DO WHILE hQuery:GET-NEXT(NO-LOCK)   AND 
    NOT hQuery:QUERY-OFF-END :

    DO TRANSACTION ON ERROR UNDO, NEXT: 
        hdbBongLinje = BUFFER dbBongLinje:HANDLE. 
        hdbBongLinje:BUFFER-CREATE(). 
        lStatus = hdbBongLinje:BUFFER-COPY(bfBongLinje,?,""). 
        CATCH oneError AS Progress.Lang.SysError:   
            iError[3] = iError[3] + 1.
              /* MESSAGE "Error:" oneError:GetMessage(1).*/
        END CATCH.
    END.
    iCnt = iCnt + 1. 
END.
MESSAGE "BongLinje Transactions:" + STRING(iCnt). 
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery. 

IF iError[1] NE 0 THEN
   MESSAGE "Error Nets:" iError[1] .

IF iError[2] NE 0 THEN
   MESSAGE "Error BongHode:" iError[2] .

IF iError[3] NE 0 THEN
   MESSAGE "Error BongLinje:" iError[3] .


RETURN.  
