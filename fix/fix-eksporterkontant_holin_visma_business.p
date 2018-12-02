DEF VAR lTid AS INT NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DISPLAY 'START' SKIP.
RUN bibl_logg.p ('holin_visma', ' '). 

ASSIGN lTid = TIME.
RUN bibl_logg.p ('holin_visma', 'fix-eksporterkontant_holin_visma_business.p: MANUELL Starter eksporterkontant_holin_visma_business_akkum.p. ' + string(TIME,"HH:MM:SS")).

DISPLAY 'eksporterkontant_holin_visma_business_akkum.p' FORMAT "x(50)" SKIP.

RUN eksporterkontant_holin_visma_business_akkum.p (
    04/01/2010,09/09/2010,OUTPUT ocReturn).

DISPLAY 'FERDIG' FORMAT "x(50)" SKIP.

ASSIGN lTid = TIME.
RUN bibl_logg.p ('holin_visma', 'fix-eksporterkontant_holin_visma_business.p: MANUELL Stopper eksporterkontant_holin_visma_business_akkum.p. ' + string(TIME,"HH:MM:SS")).

DISPLAY 'AVSLUTT'.