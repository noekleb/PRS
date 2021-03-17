DEF VAR lTid AS INT NO-UNDO.
DEF VAR ocRetur AS CHAR NO-UNDO.

RUN bibl_logg.p ('holinFAK_visma', ' ').

ASSIGN lTid = TIME.
RUN bibl_logg.p ('holin_visma', 'fix-2eksporterfaktura_holin_visma_business.p: MANUELL Starter eksporterkontant_holin_visma_business_akkum.p. ' + string(TIME,"HH:MM:SS")).

RUN eksporterfaktura_holin_visma_business.p(06/21/2010, 06/24/2010, OUTPUT ocRetur).

ASSIGN lTid = TIME.
RUN bibl_logg.p ('holin_visma', 'fix-2eksporterfaktura_holin_visma_business.p: MANUELL Ferdig eksporterkontant_holin_visma_business_akkum.p. ' + string(TIME,"HH:MM:SS")).

MESSAGE ocRetur
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
