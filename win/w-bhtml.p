 /*

	Last change:  TN   25 Sep 101    3:57 pm
*/


&Scoped UtStream Stream Ut
DEF STREAM Ut.

DEF VAR wLinje AS CHAR NO-UNDO.
DEF VAR wLnNr  AS INT  NO-UNDO.
DEF VAR wt     AS CHAR NO-UNDO.
DEF VAR j      AS INT  NO-UNDO.
DEF VAR wNr    AS INT  NO-UNDO.
DEF VAR wHit AS LOGI NO-UNDO.
DEF VAR ii AS INTE NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR wHead2       AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER wDokTyp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wSep         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead1Set    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHead     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFields      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHeadForm AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTabell      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wQY          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER hQuery AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER wtmpFileName AS CHAR NO-UNDO.

DEFINE VAR wHead1       AS CHAR NO-UNDO.
DEFINE VAR wTitle       AS CHAR NO-UNDO.

IF VALID-HANDLE(hQuery) THEN DO:
    ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:REPOSITION-TO-ROW(1).
END.
{htmlwrapperdef.i }

ASSIGN wTitle = "Rapport " + wTabell
       wHead1 = wTitle
       wHead2 = STRING(TODAY,"99/99/9999")
       .

Output {&UtStream} to VALUE(wtmpFileName).
IF wDokTyp = "EX" THEN DO:
    PUT {&UtStream} Unformatted IF wSep <> ";" THEN REPLACE(wColHead,wSep,";") ELSE wColHead SKIP.   
END.
ELSE DO:
    PUT {&UtStream} Unformatted
      HTML;Start (wSep,wTitle,"")
      HTML;Head1 (wHead1,ENTRY(1,wHead1Set),ENTRY(2,wHead1Set),INT(ENTRY(3,wHead1Set)),INT(ENTRY(4,wHead1Set)),INT(ENTRY(5,wHead1Set)),INT(ENTRY(6,wHead1Set)))
      HTML;Head2 (wHead2)
      HTML;ColHead (wColHead,wColHeadForm). 
END.
ASSIGN wLnNr = 0.
REPEAT:
    hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    Assign wLnNr  = wLnNr + 1
           wLinje = ""
           wLinje = FILL(wSep,NUM-ENTRIES(wFields,wSep) - 1)
           wHit   = FALSE.
    DO ii = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN hField = hBuffer:BUFFER-FIELD(ii).
        IF wTabell = "Varegrupper" THEN DO:
          IF hField:NAME = "Vg" THEN DO:
             FIND VarGr WHERE VarGr.Vg = INT(hField:STRING-VALUE()) NO-LOCK.
             find HuvGr of VarGr no-lock no-error.
             find Moms of VarGr no-lock no-error.

             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("Vg",wFields,wSep),wLinje,wSep) = STRING(VarGr.Vg)
                    ENTRY(LOOKUP("VgBeskr",wFields,wSep),wLinje,wSep) = TRIM(VarGr.VgBeskr)
                    ENTRY(LOOKUP("Hg",wFields,wSep),wLinje,wSep) = STRING(VarGr.Hg,"zzz9")
                    ENTRY(LOOKUP("wHuvGrBeskr",wFields,wSep),wLinje,wSep) = (IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "")
                    ENTRY(LOOKUP("MomsKod",wFields,wSep),wLinje,wSep) = STRING(VarGr.MomsKod)
                    ENTRY(LOOKUP("wMomsProc",wFields,wSep),wLinje,wSep) = (IF AVAIL Moms THEN STRING(Moms.MomsProc,">9.99") ELSE "")
                    ENTRY(LOOKUP("Kost_Proc",wFields,wSep),wLinje,wSep) = (IF VarGr.Kost_Proc = 0 THEN "" ELSE STRING(VarGr.Kost_Proc,">>9.99")).
             LEAVE.
          END.
        END.
        ELSE IF wTabell = "Hovedgrupper" THEN DO:
          IF hField:NAME = "Hg" THEN DO:
             FIND HuvGr WHERE HuvGr.Hg = INT(hField:STRING-VALUE()) NO-LOCK.
             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("Hg",wFields,wSep),wLinje,wSep) = STRING(HuvGr.Hg)
                    ENTRY(LOOKUP("HgBeskr",wFields,wSep),wLinje,wSep) = STRING(HuvGr.HgBeskr).
             LEAVE.
          END.
        END.
        ELSE IF wTabell = "Butiker" THEN DO:
          IF hField:NAME = "Butik" THEN DO:
             FIND Butiker WHERE Butiker.Butik = INT(hField:STRING-VALUE()) NO-LOCK.
             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("Butik",wFields,wSep),wLinje,wSep) = STRING(Butiker.Butik)
                    ENTRY(LOOKUP("ButNamn",wFields,wSep),wLinje,wSep) = TRIM(Butiker.ButNamn)
                    ENTRY(LOOKUP("BuKon",wFields,wSep),wLinje,wSep) = STRING(Butiker.BuKon)
                    ENTRY(LOOKUP("ProfilNr",wFields,wSep),wLinje,wSep) = STRING(Butiker.ProfilNr).
             LEAVE.
          END.
        END.
        ELSE IF wTabell = "Leverandører" THEN DO:
          IF hField:NAME = "LevNr" THEN DO:
             FIND LevBas WHERE LevBas.LevNr = INT(hField:STRING-VALUE()) NO-LOCK.
             FIND Post WHERE Post.PostNr = LevBas.LevPonr NO-LOCK NO-ERROR.
             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("levnr",wFields,wSep),wLinje,wSep) = STRING(LevBas.LevNr)
                    ENTRY(LOOKUP("levnamn",wFields,wSep),wLinje,wSep) = TRIM(LevBas.LevNamn)
                    ENTRY(LOOKUP("levtel",wFields,wSep),wLinje,wSep) = TRIM(LevBas.Levtel)
                    ENTRY(LOOKUP("levadr",wFields,wSep),wLinje,wSep) = TRIM(LevBas.Levadr)
                    ENTRY(LOOKUP("levponr",wFields,wSep),wLinje,wSep) = STRING(LevBas.levponr)
                    ENTRY(LOOKUP("wPostadresse",wFields,wSep),wLinje,wSep) = (IF AVAIL Post THEN Post.Beskrivelse ELSE "")
                    ENTRY(LOOKUP("levland",wFields,wSep),wLinje,wSep) = TRIM(LevBas.LevLand)
                    ENTRY(LOOKUP("valkod",wFields,wSep),wLinje,wSep) = TRIM(LevBas.ValKod).
             LEAVE.
          END.
        END.
        /*
        ELSE IF wTabell = "Kund" THEN DO:
          IF hField:NAME = "KundNr" THEN DO:
             FIND Kunde WHERE Kunde.KundNr = INT(hField:STRING-VALUE()) NO-LOCK.
            /* find Post where Post.PostNr = Kund.KuPonr no-lock no-error. */
             FIND KundeType OF Kunde NO-LOCK NO-ERROR.
             FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.
             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("KundNr",wFields,wSep),wLinje,wSep) = STRING(Kunde.KundNr)
                    ENTRY(LOOKUP("KundNamn",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KundNamn)
                    ENTRY(LOOKUP("KuTel",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KuTel)
                    ENTRY(LOOKUP("TeleFax",wFields,wSep),wLinje,wSep) = TRIM(Kunde.TeleFax)
                    ENTRY(LOOKUP("KuKon",wFields,wSep),wLinje,wSep) = STRING(Kunde.KuKon)
                    ENTRY(LOOKUP("wTypeBeskr",wFields,wSep),wLinje,wSep) = (IF AVAIL KundeType THEN KundeType.Beskrivelse ELSE "")
                    ENTRY(LOOKUP("wGruppeBeskr",wFields,wSep),wLinje,wSep) = (IF AVAIL KundeGruppe THEN KundeGruppe.Beskrivelse ELSE "")
                    ENTRY(LOOKUP("KuAdr",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KuAdr)
                    ENTRY(LOOKUP("KuPAdr",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KuPAdr)
                    ENTRY(LOOKUP("KuPonr",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KuPonr)
                    ENTRY(LOOKUP("KuLand",wFields,wSep),wLinje,wSep) = TRIM(Kunde.KuLand)
                    ENTRY(LOOKUP("KuSal",wFields,wSep),wLinje,wSep) = STRING(Kunde.KuSal).
             LEAVE.
          END.
        END.
        */
        ELSE IF wTabell = "Selgere" THEN DO:
          IF hField:NAME = "ForsNr" THEN DO:
             FIND Forsalj WHERE Forsalj.ForsNr = INT(hField:STRING-VALUE()) NO-LOCK.
/*            find Post where Post.PostNr = Forsalj.FoPonr no-lock no-error. */
             ASSIGN wHit = TRUE
                    ENTRY(LOOKUP("ForsNr",wFields,wSep),wLinje,wSep) = STRING(Forsalj.ForsNr)
                    ENTRY(LOOKUP("FoNamn",wFields,wSep),wLinje,wSep) = TRIM(Forsalj.FoNamn)
                    ENTRY(LOOKUP("FoAnstNr",wFields,wSep),wLinje,wSep) = STRING(Forsalj.FoAnstNr)
                    ENTRY(LOOKUP("FoPersNr",wFields,wSep),wLinje,wSep) = STRING(Forsalj.FoPersNr)
                    ENTRY(LOOKUP("FoAdr",wFields,wSep),wLinje,wSep) = STRING(Forsalj.FoAdr)
                    ENTRY(LOOKUP("FoPoNr",wFields,wSep),wLinje,wSep) = TRIM(Forsalj.FoPoNr)
                    ENTRY(LOOKUP("FoPadr",wFields,wSep),wLinje,wSep) = TRIM(Forsalj.FoPadr)
                    ENTRY(LOOKUP("FoTel",wFields,wSep),wLinje,wSep) = TRIM(Forsalj.FoTel).
             LEAVE.
          END.
        END.
    END.
    IF wHit THEN
       IF wDokTyp = "EX" THEN
          PUT {&UtStream} Unformatted IF wSep <> ";" THEN REPLACE(wLinje,wSep,";") ELSE wLinje SKIP.
       ELSE
          PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
END.
IF wDokTyp = "HTM" THEN
  PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (20)
      HTML;END     ().





