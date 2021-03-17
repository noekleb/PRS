DEFINE VAR cKundnamn  AS CHARACTER   NO-UNDO.
DEFINE VAR dtAskTime  AS DATETIME    NO-UNDO.
DEFINE VAR dtLastTime AS DATETIME    NO-UNDO.
DEFINE VAR deMSgrens  AS DECIMAL     NO-UNDO.
DEFINE VAR lOK        AS LOGICAL     NO-UNDO.
DEFINE VAR cMessage   AS CHARACTER   NO-UNDO.

RUN asWebKommKontroll.p (OUTPUT cKundnamn, 
                         OUTPUT dtAskTime, 
                         OUTPUT dtLastTime,
                         OUTPUT deMSgrens, 
                         OUTPUT lOK,       
                         OUTPUT cMessage).

MESSAGE cKundnamn   skip
        dtAskTime   skip
        dtLastTime  skip
        deMSgrens   skip
        lOK         skip
        cMessage  


    VIEW-AS ALERT-BOX INFO BUTTONS OK.
