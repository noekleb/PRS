DEFINE TEMP-TABLE xx 
    FIELD a AS CHAR. 
DEFINE TEMP-TABLE yy 
    FIELD X AS CHAR. 
DEFINE TEMP-TABLE ff 
    FIELD ff AS CHAR. 


CREATE xx. 
a = "kkk". 

CREATE xx. 
a = "kkk". 


RUN t:\prstrans\putnetsupdated.p 
    (INPUT TABLE xx,
     INPUT TABLE yy, 
     INPUT TABLE ff). 
