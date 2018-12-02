/*----------------------------------------------------------------------------
Program     : log-manager-report.p

Description : Read "4GLTRACE" LOG-MANAGER output
              and report it in a columnar format.

Copyright(c): Taylor Steel Inc. 2006 $n

Author      : Tim Kuehn
              timk@tdkcs.ca

Created     : 2007-03-13 TAYLOR STEEL INC

NOTES       : This was written on an HP-UX system,
              and may require tweaking for other plaforms


PARAMETERS  :

    cur-input-file      LOG-MANAGER input file
    cur-output-file     Where to write the report to

--------Revision History---------------------------------------------------

	Last change: TDK 2007-03-13 10:06:56
             : Brynjar: reformatted time difference reporting
----------------------------------------------------------------------------*/

/****************************************************************************/
/* Procedure Parameters                 */

DEFINE INPUT PARAMETER cur-input-file                   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cur-output-file                  AS CHARACTER NO-UNDO.

/****************************************************************************/
/* Procedure Temp-Tables                */

DEFINE TEMP-TABLE tt-line   NO-UNDO

    FIELD line-no           AS INTEGER 

    FIELD current-line      AS CHARACTER

    FIELD date-string       AS CHARACTER
    FIELD time-string       AS CHARACTER

    FIELD p-string          AS CHARACTER
    FIELD t-string          AS CHARACTER

    FIELD nesting-level     AS CHARACTER
    FIELD trace-type        AS CHARACTER
    FIELD trace-type-2      AS CHARACTER
    FIELD action            AS CHARACTER

    FIELD ret-type          AS CHARACTER

    FIELD event-date        AS DATE
    FIELD event-time        AS DECIMAL
    FIELD time-diff         AS DECIMAL
    FIELD elapsed-time      AS DECIMAL

    INDEX i-ln              line-no
    .

/****************************************************************************/
/* Procedure Buffers                    */

DEFINE BUFFER tt-line-last              FOR tt-line.

/****************************************************************************/
/* Procedure Variables                  */

DEFINE VARIABLE cur-from                            AS CHARACTER    NO-UNDO.

DEFINE VARIABLE cur-line-no                         AS INTEGER      NO-UNDO.
DEFINE VARIABLE cur-elapsed-time                    AS DECIMAL      NO-UNDO.

/****************************************************************************/
/* Initializations                      */

ASSIGN
    cur-elapsed-time = 0.0
    .

INPUT
    FROM VALUE(cur-input-file).

OUTPUT
    TO VALUE(cur-output-file).

/****************************************************************************/

REPEAT WHILE TRUE:

    FIND tt-line-last
        WHERE tt-line-last.line-no = cur-line-no
        EXCLUSIVE-LOCK
        NO-ERROR.

    CREATE tt-line.

    ASSIGN
        cur-line-no         = cur-line-no + 1
        .

    ASSIGN
        tt-line.line-no     = cur-line-no
        .

    IMPORT UNFORMATTED
        tt-line.current-line
        .

    IF LENGTH(tt-line.current-line) < 65 THEN
        NEXT.

    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.date-string).

    ASSIGN
        tt-line.time-string = SUBSTRING(tt-line.date-string, 11, 12)
        tt-line.date-string = SUBSTRING(tt-line.date-string,  2,  8)
        .

    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.p-string).
    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.t-string).
    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.nesting-level).

    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.trace-type).
    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.trace-type-2).

    RUN get-first-string(   INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.action).

    IF tt-line.current-line BEGINS "from" THEN
        DO:

        RUN get-first-string(INPUT-OUTPUT tt-line.current-line, OUTPUT cur-from).

        ASSIGN
            tt-line.action = tt-line.action + " " + cur-from
            .

        IF LOOKUP(ENTRY(1, tt-line.current-line, " "), "main,publish") > 0 THEN
            DO:

            CASE ENTRY(1, tt-line.current-line, " "):
            WHEN "main" THEN
                DO:

                ASSIGN
                    tt-line.ret-type = SUBSTRING(tt-line.current-line, 1, 11)
                    .

                SUBSTRING(tt-line.current-line, 1, 11) = "".
                END.

            WHEN "publish"  THEN

                DO:

                RUN get-first-string(INPUT-OUTPUT tt-line.current-line, OUTPUT tt-line.ret-type).

                END.

            END CASE.

            END.    /* IF LOOKUP() */


        END.


    IF LOOKUP("persist", tt-line.current-line, " ") > 0 THEN
        ASSIGN
            tt-line.ret-type = TRIM(tt-line.ret-type + " PERSIST")
            .

    IF LOOKUP("in", tt-line.current-line, " ") > 0 THEN
        ASSIGN
            tt-line.ret-type = TRIM(tt-line.ret-type + " IN")
            .

    ASSIGN
        tt-line.event-date  =   DATE(SUBSTRING(tt-line.date-string, 2, 10))
        .

    ASSIGN
        tt-line.event-time  =   DECIMAL(ENTRY(1, tt-line.time-string, ":")) * 3600.0 +
                                DECIMAL(ENTRY(2, tt-line.time-string, ":")) *   60.0 +
                                DECIMAL(ENTRY(3, tt-line.time-string, ":"))
        .

    ASSIGN
        tt-line.event-time  =   tt-line.event-time * 1000.0
        .

        /* How long did it take this line to run?   */

    IF AVAILABLE tt-line-last THEN
        DO:

        ASSIGN
            tt-line-last.time-diff      = (tt-line.event-time - tt-line-last.event-time) / 1000.0 WHEN tt-line-last.event-time > 0
            .

        ASSIGN
            cur-elapsed-time            = cur-elapsed-time + tt-line-last.time-diff
            .

        ASSIGN
            tt-line-last.elapsed-time   = cur-elapsed-time
            .

        END.

END.

/****************************************************************************/

FOR EACH tt-line
    NO-LOCK
    BY tt-line.line-no:

    DISPLAY
            tt-line.line-no                                                 FORMAT ">>>,>>9"

            tt-line.date-string + " " +
            tt-line.time-string                                             FORMAT "X(21)"

           (tt-line.time-diff > 0.010)      COLUMN-LABEL " "                FORMAT "*/ "
            tt-line.time-diff               COLUMN-LABEL "TimeDiff!(ms)"    FORMAT ">>>,>>9"   WHEN tt-line.time-diff >= .010

            tt-line.elapsed-time             COLUMN-LABEL "Elapsed!(Sec)"    FORMAT ">,>>>.999"

            tt-line.nesting-level           COLUMN-LABEL "N"                FORMAT "XX"
            tt-line.action                  COLUMN-LABEL "Action"           FORMAT "X(15)"
            tt-line.ret-type                COLUMN-LABEL "Ret"              FORMAT "X(10)"
            tt-line.current-line                                            FORMAT "X(220)"

        WITH WIDTH 600
            DOWN
            STREAM-IO
            .

    DOWN.

END.

/****************************************************************************/

PUT UNFORMATTED         SKIP(1)
    FILL("*", 132)      SKIP
    "Long events  (> 50 ms)  "   SKIP
    FILL("*", 132)      SKIP(1)
    .

FOR EACH tt-line
    WHERE tt-line.time-diff > 50
    NO-LOCK
    BY tt-line.line-no:

    DISPLAY
            tt-line.line-no                                                 FORMAT ">>>,>>9"

            tt-line.date-string + " " +
            tt-line.time-string                                             FORMAT "X(21)"

           (tt-line.time-diff > 0.010)      COLUMN-LABEL " "                FORMAT "*/ "
            tt-line.time-diff               COLUMN-LABEL "TimeDiff!(ms)"    FORMAT ">>>,>>9"   WHEN tt-line.time-diff >= .010

           tt-line.elapsed-time             COLUMN-LABEL "Elapsed!(Sec)"    FORMAT ">,>>>.999"

            tt-line.nesting-level           COLUMN-LABEL "N"                FORMAT "XX"
            tt-line.action                  COLUMN-LABEL "Action"           FORMAT "X(15)"
            tt-line.ret-type                COLUMN-LABEL "Ret"              FORMAT "X(10)"
            tt-line.current-line                                            FORMAT "X(220)"

        WITH FRAME f-long
            WIDTH 600
            DOWN
            STREAM-IO
            .

    DOWN.

END.

/****************************************************************************/
/* Cleanup                              */

INPUT CLOSE.
OUTPUT CLOSE.

/****************************************************************************/
PROCEDURE get-first-string:
/****************************************************************************
PURPOSE:


PARAMETERS:

*****************************************************************************/
/* Local Parameters                     */

DEFINE INPUT-OUTPUT PARAMETER cur-line                  AS CHARACTER NO-UNDO.

    /* OUTPUT   */

DEFINE OUTPUT PARAMETER cur-out-string                  AS CHARACTER NO-UNDO.

/****************************************************************************/

ASSIGN
    cur-out-string = ENTRY(1, cur-line, " ")
    .

ENTRY(1, cur-line, " ") = "".

ASSIGN
    cur-line = TRIM(cur-line)
    .

END PROCEDURE.

