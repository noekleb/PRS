DEFINE TEMP-TABLE tt_nets NO-UNDO     LIKE nets .
DEFINE TEMP-TABLE tt_bonghode  NO-UNDO LIKE bonghode. 
DEFINE TEMP-TABLE tt_bonglinje NO-UNDO LIKE bonglinje. 
 
DEFINE VARIABLE cSessionid AS CHAR . 

CREATE tt_Nets. 


RUN t:\PRSTrans\putNetsUpdated.p 
    (
     cSessionId, 
     TABLE tt_nets, 
     TABLE tt_bonghode,
     TABLE tt_bonglinje) NO-ERROR. 
