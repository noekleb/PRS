DEF VAR iant AS INT NO-UNDO.
   
RUN InitBBSKortTabell.p.   
    
FOR EACH TelleLinje WHERE
    NOT CAN-FIND(TelleHode OF TelleLinje):
    iAnt = iAnt + 1.
    DELETE Tellelinje.
END.
   
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
