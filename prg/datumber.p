DEFINE INPUT  PARAMETER datberv AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER datum1  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER datum2  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER antdag  AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER lSvar   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE iLen     AS INTEGER                NO-UNDO.
DEFINE VARIABLE dat1ccaa AS CHARACTER              NO-UNDO.
DEFINE VARIABLE dat1mm   AS CHARACTER              NO-UNDO.
DEFINE VARIABLE dat1dd   AS CHARACTER              NO-UNDO.
DEFINE VARIABLE mtab     AS DECIMAL EXTENT 12
 INIT [0,31,59,90,120,151,181,212,243,273,304,334] NO-UNDO.
DEFINE VARIABLE mlen  AS DECIMAL EXTENT 12
 INIT [31,0,31,30,31,30,31,31,30,31,30,31]         NO-UNDO.
DEFINE VARIABLE datwf4  AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE wslask  AS CHARACTER               NO-UNDO.
DEFINE VARIABLE wskott1 AS CHARACTER               NO-UNDO.
DEFINE VARIABLE indat   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE datwf4a AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE datwf6  AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE wskott  AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE utdat   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE wf8a    AS DECIMAL                 NO-UNDO.

  CASE datberv:
      WHEN 1 THEN     /* Test om datum är ok */
      DO:
        IF LENGTH(datum1) <> 8 THEN
        DO:
          ASSIGN lSvar = FALSE.
          RETURN.
        END.
        ASSIGN dat1ccaa = SUBSTRING(datum1,1,4).
        ASSIGN dat1mm   = SUBSTRING(datum1,5,2).
        ASSIGN dat1dd   = SUBSTRING(datum1,7,2).
        IF INTEGER(dat1ccaa) < 1994 OR INTEGER(dat1ccaa) > 2020 THEN
        DO:
          ASSIGN lSvar = FALSE.
          RETURN.
        END.
        IF INTEGER(dat1mm) > 12 OR INTEGER(dat1mm) < 1 THEN
        DO:
          ASSIGN lSvar = FALSE.
          RETURN.
        END.
        IF INTEGER(dat1dd) > 31 OR INTEGER(dat1dd) < 1 THEN
        DO:
          ASSIGN lSvar = FALSE.
          RETURN.
        END.
        ASSIGN datwf4 = DECIMAL(dat1ccaa).
        RUN skottar.
        IF wskott1 = "00" THEN
          ASSIGN mlen[2] = 29.
        ELSE
          ASSIGN mlen[2] = 28.
        IF INTEGER(dat1dd) > mlen[INTEGER(dat1mm)] THEN
        DO:
          ASSIGN lSvar = FALSE.
          RETURN.
        END.
        ASSIGN lSvar = TRUE.
        RETURN.
      END.

      WHEN 3 THEN     /* +/- antal dagar till visst datum  */
      DO:
        ASSIGN indat = datum1.
        RUN datdag.
        ASSIGN datwf6 = datwf6 + antdag.
        RUN dagdat.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

      WHEN 4 THEN     /* Skillnaden i dagar mellan två datum */
      DO:
        ASSIGN indat = datum1.
        RUN datdag.
        ASSIGN wf8a = datwf6.
        ASSIGN indat = STRING(antdag).
        RUN datdag.
        ASSIGN antdag = datwf6.
        ASSIGN antdag = antdag - wf8a.
        ASSIGN datum2 = STRING(antdag).
        ASSIGN lSvar = TRUE.
      END.

      WHEN 5 THEN     /* Flytta fram till sista i månaden */
      DO:
        ASSIGN indat = datum1.
        RUN sista.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

      WHEN 6 THEN     /* Som 5 + 1 månad */
      DO:
        ASSIGN indat = datum1.
        RUN sistapl.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

      WHEN 7 THEN     /* Som 5 + 2 månader */
      DO:
        ASSIGN indat = datum1.
        RUN sistapl.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

      WHEN 8 THEN     /* Som 5 + 3 månader */
      DO:
        ASSIGN indat = datum1.
        RUN sistapl.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

      WHEN 9 THEN     /* Räkna månader +/- */
      DO:
        ASSIGN indat = datum1.
        RUN manadd.
        ASSIGN datum2 = utdat.
        ASSIGN lSvar = TRUE.
      END.

  END CASE.

PROCEDURE datdag:
  ASSIGN dat1ccaa = SUBSTRING(indat,1,4).
  ASSIGN dat1mm   = SUBSTRING(indat,5,2).
  ASSIGN dat1dd   = SUBSTRING(indat,7,2).

  ASSIGN datwf4a = DECIMAL(dat1ccaa)
         datwf4a = datwf4a - 1994.
  ASSIGN datwf6  = (datwf4a * 365).

  ASSIGN wskott = (datwf4a + 1) / 4.

  ASSIGN datwf6 = datwf6 + wskott.

  RUN skottar.
  IF wskott1 = "00" AND INTEGER(dat1mm) > 2 THEN
      ASSIGN datwf6 = datwf6 + 1.

  ASSIGN datwf6 = datwf6 + mtab[INTEGER(dat1mm)].
  ASSIGN datwf6 = datwf6 + DECIMAL(dat1dd).

END PROCEDURE.


PROCEDURE dagdat:
  ASSIGN datwf4 = 1995.
  ASSIGN datwf6 = datwf6 - 365.

  REPEAT:
    DO:
      RUN skottar.
      IF wskott1 = "00" AND datwf6 < 367 THEN
        LEAVE.
      IF datwf6 < 366 THEN
        LEAVE.
      ASSIGN datwf4 = datwf4 + 1.
      IF wskott1 = "00" THEN
        ASSIGN datwf6 = datwf6 - 366.
      ELSE
        ASSIGN datwf6 = datwf6 - 365.
    END.
  END.

  RUN skottar.

  IF wskott1 = "00" THEN
    ASSIGN mlen[2] = 29.
  ELSE
    ASSIGN mlen[2] = 28.

  DO iLen = 1 TO 12:
    IF datwf6 <= mlen[iLen] THEN
      LEAVE.
    ASSIGN datwf6 = datwf6 - mlen[iLen].
  END.

  datwf6 = TRUNCATE ((datwf6 * 1) / 1 ,0).
  ASSIGN dat1dd = STRING(datwf6,"99").
  ASSIGN dat1mm = STRING(iLen,"99").
  ASSIGN utdat = STRING(datwf4) + dat1mm + dat1dd.

END PROCEDURE.

PROCEDURE sista:
  ASSIGN dat1ccaa = SUBSTRING(indat,1,4).
  ASSIGN dat1mm   = SUBSTRING(indat,5,2).
  ASSIGN dat1dd   = SUBSTRING(indat,7,2).

  ASSIGN datwf4 = DECIMAL(dat1ccaa).
  RUN skottar.
  ASSIGN iLen = INTEGER(dat1mm).
  ASSIGN dat1dd = STRING(mlen[ilen],"99").
  ASSIGN utdat = STRING(datwf4) + dat1mm + dat1dd.
  
END PROCEDURE.

PROCEDURE sistapl:
  ASSIGN dat1ccaa = SUBSTRING(indat,1,4).
  ASSIGN dat1mm   = SUBSTRING(indat,5,2).
  ASSIGN dat1dd   = SUBSTRING(indat,7,2).
  
  ASSIGN datwf4 = DECIMAL(dat1ccaa).
  ASSIGN ilen = INTEGER(dat1mm).

  IF datberv = 6 THEN
    ASSIGN iLen = iLen + 1.
  ELSE IF datberv = 7 THEN
    ASSIGN iLen = iLen + 2.
  ELSE IF datberv = 8 THEN
    ASSIGN iLen = iLen + 3.
  IF ilen > 12 THEN
  DO:
    ASSIGN iLen = iLen - 12.
    ASSIGN datwf4 = datwf4 + 1.
  END.

  ASSIGN dat1ccaa = STRING(datwf4)
         dat1mm   = STRING(ilen,"99")
         indat    = dat1ccaa + dat1mm + dat1dd.
  RUN sista.

END PROCEDURE.

PROCEDURE manadd:
  ASSIGN dat1ccaa = SUBSTRING(indat,1,4).
  ASSIGN dat1mm   = SUBSTRING(indat,5,2).
  ASSIGN dat1dd   = SUBSTRING(indat,7,2).

  ASSIGN datwf4 = DECIMAL(dat1ccaa).
  ASSIGN datwf6 = DECIMAL(dat1mm).
  ASSIGN datwf6 = datwf6 + antdag.

  REPEAT:
    IF datwf6 < 1 THEN
    DO:
      ASSIGN datwf6 = datwf6 + 12.
      ASSIGN datwf4 = datwf4 - 1.
    END.
    ELSE IF datwf6 > 12 THEN
    DO:
      ASSIGN datwf6 = datwf6 - 12.
      ASSIGN datwf4 = datwf4 + 1.
    END.
    ELSE
      LEAVE.
  END.

  ASSIGN dat1mm = STRING(datwf6,"99").
  RUN skottar.
  ASSIGN datwf4a = DECIMAL(dat1dd).
  ASSIGN iLen = INTEGER(dat1mm).

  IF datwf4a > mlen[ilen] THEN
  DO:
    ASSIGN indat = STRING(datwf4,"9999") + dat1mm + dat1dd.
    RUN sista.
  END.
  ELSE
    ASSIGN utdat = STRING(datwf4,"9999") + dat1mm + dat1dd.

END PROCEDURE.

PROCEDURE skottar:
    ASSIGN wslask = TRIM(STRING(datwf4 / 4,">>>9.99")).
    ASSIGN wskott1 = SUBSTRING(wslask,LENGTH(wslask) - 1,2).
    IF wskott1 = "00" THEN
      ASSIGN mlen[2] = 29.
    ELSE
      ASSIGN mlen[2] = 28.

END PROCEDURE.
