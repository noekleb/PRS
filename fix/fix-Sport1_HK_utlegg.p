DEF VAR ocReturn AS CHAR NO-UNDO.

RUN eksportfinans.p (TODAY - 10,TODAY,OUTPUT ocReturn).
RUN eksportsalg.p   (TODAY - 10,TODAY,OUTPUT ocReturn).
RUN eksportlager.p  (DATE(1,1,2009),TODAY,OUTPUT ocReturn).
