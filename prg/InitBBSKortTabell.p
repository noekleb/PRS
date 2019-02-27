/* Oppretter poster i TrasBeskr for kreditkorttyper.  */

DEFINE VARIABLE cTekst1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst3      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst4      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTTId        AS INTEGER NO-UNDO.
DEFINE VARIABLE iTBId        AS INTEGER NO-UNDO.
DEFINE VARIABLE cBeskrivelse AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInnloser    AS CHARACTER NO-UNDO.
DEFINE VARIABLE piLoop       AS INTEGER NO-UNDO.

ASSIGN
    cTekst1 = "52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52;52"
    cTekst2 = "1;3;4;5;6;7;11;12;14;15;16;21;22;23;24;25;26;27;28;29;30;32;34;35;36;38;39;40;41;43;45;46;47;48;49;50;51;52;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;77;78;79;80;81;82;83;84;85;86;91"
    cTekst3 = "Kreditkort;Visa;MasterCard;Amex;Diners;Coop Bedrift;JCB;Trumf;Maestro;Lindex;Ikano;NBBL Partner AS;Gavekort Senter;Gavekort Kjede;Esso MasterCard;Sentrumsgavekort;AgriCard|Statoil MasterCard;XponCard;MultiCard;Universal Presentkort;BankAxept;Resurs Bank;NG Bedriftskort;Nets Sentergavekort;Nets Kjedegavekort;Swedbank;Visa Bankkort;MasterCard Bankkort;Reconciliation - Sverige;S&S Medlemskort;Nordea Finans;Handelsbanken Finans;Swebank;SEB Kort;Resurs;DanKort;Coop Visa;Payex Gavekort;Trumf Visa;Gavekort 1;Visa DK;MasterCard DK;Maestro DK;Diners DK;Amex DK;Cashcomspresentkort;Storcash kortet;PBS Kjedekort;Forbrugsforeningen;Ikano Finans;SparXpres;CUP;Rikslunchen;Kjedekort 1;Collector Credit;FDM;Gavekort Senter 2;PBSCenterkort;PBS Handelsstandkort;LIC Kort;Accept Card;Coop MasterCard;Oberthur Gavekort;Bunnpris Bedrift;Rikskortet;KappAhl Club;MediaMarkt;Visa PrePaid No"
    cTekst4 = ";VIS;MAC;AMX;DIN;;JCB;;;;;;;;;;;;;;;;;;;SWE;;;;;;;SWE;;RES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
    .
RUN OpprettTbId.

ASSIGN
    cTekst1 = "52;52;52;52;52;52;52;52"
    cTekst2 = "501;900;901;902;903;904;905;906"
    cTekst3 = "SEQR;Personal;Klarna;Bankgiro;Personalköp;CashCom;Euroline;Fsp"
    cTekst4 = "SEQR;Personal;Klarna;Bankgiro;Personalköp;CCO;EUL;FSP"
    .
RUN OpprettTbId.
    
/* /* SEQR */                                  */
/* DO TRANSACTION:                             */
/*     IF NOT CAN-FIND(TransBeskr WHERE        */
/*         TransBeskr.TTId = 52 AND            */
/*         TransBeskr.TBId = 501) THEN         */
/*     DO:                                     */
/*         CREATE TransBeskr.                  */
/*         ASSIGN                              */
/*             TransBeskr.TTId        = 52     */
/*             TransBeskr.TBId        = 501    */
/*             TransBeskr.Beskrivelse = 'SEQR' */
/*             TransBeskr.Innloser    = 'SEQR' */
/*             .                               */
/*     END.                                    */
/*     RELEASE TransBeskr.                     */
/* END.                                        */

PROCEDURE OpprettTbId:
    DO piLoop = 1 TO NUM-ENTRIES(cTekst1,';') TRANSACTION:
        ASSIGN
            iTTId        = INT(ENTRY(piLoop,cTekst1,';')) 
            iTBId        = INT(ENTRY(piLoop,cTekst2,';'))
            cBeskrivelse = ENTRY(piLoop,cTekst3,';')
            cInnloser    = ENTRY(piLoop,cTekst4,';')
            .
    
        IF NOT CAN-FIND(TransBeskr WHERE 
            TransBeskr.TTId = iTTId AND 
            TransBeskr.TBId = iTBId) THEN 
        DO:
            CREATE TransBeskr.
            ASSIGN
                TransBeskr.TTId        = iTTId
                TransBeskr.TBId        = iTBId
                TransBeskr.Beskrivelse = cBeskrivelse
                TransBeskr.Innloser    = cInnloser
                .
        END.
        RELEASE TransBeskr.
    END. /* TRANSACTION */    
END.
