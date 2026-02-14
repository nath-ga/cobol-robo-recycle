       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCCOSTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTS-FILE  ASSIGN TO "..\data\parts.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ROBOTS-FILE ASSIGN TO "..\data\robots.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BOM-FILE    ASSIGN TO "..\data\bom.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARTS-FILE.
       01  PARTS-LINE              PIC X(200).

       FD  ROBOTS-FILE.
       01  ROBOTS-LINE             PIC X(200).

       FD  BOM-FILE.
       01  BOM-LINE                PIC X(200).

       WORKING-STORAGE SECTION.
       77  EOF-FLAG                PIC X VALUE "N".
       77  LINE-NO                 PIC 9(4) VALUE 0.

       77  I                       PIC 9(4) VALUE 0.
       77  PART-COUNT              PIC 9(4) VALUE 0.
       77  ROBOT-COUNT             PIC 9(3) VALUE 0.
       

       01  WS-PARTNO               PIC X(10).
       01  WS-PARTNAME             PIC X(60).
       01  WS-UNITCOST-TXT         PIC X(20).
       01  WS-UNITCOST             PIC 9(9)V99.

       01  WS-ROBOTNO              PIC X(10).
       01  WS-ROBOTNAME            PIC X(60).
       01  WS-TARGETQTY-TXT        PIC X(20).
       01  WS-TARGETQTY            PIC 9(5).
       01  WS-KEY1                PIC X(10).
       01  WS-KEY2                PIC X(10).

       01  WS-BOM-ROBOTNO          PIC X(10).
       01  WS-BOM-PARTNO           PIC X(10).
       01  WS-BOM-QTY-TXT          PIC X(20).
       01  WS-BOM-QTY              PIC 9(5).
       01  WS-LINECOST             PIC 9(9)V99.

       77  FOUND                   PIC X VALUE "N".
       77  ROBOT-IDX               PIC 9(3) VALUE 0.
       77  PART-IDX                PIC 9(4) VALUE 0.

       01  WS-LINE                 PIC X(200).

       01  PART-TABLE.
           05 PART-ENTRY OCCURS 200 TIMES.
              10 T-PARTNO          PIC X(10).
              10 T-PARTNAME        PIC X(60).
              10 T-UNITCOST        PIC 9(9)V99.

       01  ROBOT-TABLE.
           05 ROBOT-ENTRY OCCURS 50 TIMES.
              10 T-ROBOTNO         PIC X(10).
              10 T-ROBOTNAME       PIC X(60).
              10 T-TARGETQTY       PIC 9(5).
              10 T-COST-PER        PIC 9(9)V99.
              10 T-COST-TOTAL      PIC 9(11)V99.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM LOAD-PARTS
           PERFORM LOAD-ROBOTS
           PERFORM PROCESS-BOM
           PERFORM CALC-TOTALS
           PERFORM SHOW-RESULTS
           STOP RUN.

       LOAD-PARTS.
           MOVE "N" TO EOF-FLAG
           MOVE 0 TO LINE-NO
           OPEN INPUT PARTS-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ PARTS-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO LINE-NO
                       IF LINE-NO = 1
                           CONTINUE
                       ELSE
                           PERFORM PARSE-PARTS
                           ADD 1 TO PART-COUNT
                           MOVE WS-PARTNO    TO T-PARTNO(PART-COUNT)
                           MOVE WS-PARTNAME  TO T-PARTNAME(PART-COUNT)
                           MOVE WS-UNITCOST  TO T-UNITCOST(PART-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PARTS-FILE.

       PARSE-PARTS.
           UNSTRING PARTS-LINE
               DELIMITED BY ";"
               INTO WS-PARTNO WS-PARTNAME WS-UNITCOST-TXT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-UNITCOST-TXT) TO WS-UNITCOST.

       LOAD-ROBOTS.
           MOVE "N" TO EOF-FLAG
           MOVE 0 TO LINE-NO
           OPEN INPUT ROBOTS-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ROBOTS-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO LINE-NO
                       IF LINE-NO = 1
                           CONTINUE
                       ELSE
                           PERFORM PARSE-ROBOTS
                           ADD 1 TO ROBOT-COUNT
                           MOVE WS-ROBOTNO TO T-ROBOTNO(ROBOT-COUNT)
                           MOVE WS-ROBOTNAME TO T-ROBOTNAME(ROBOT-COUNT)
                           MOVE WS-TARGETQTY TO T-TARGETQTY(ROBOT-COUNT)
                           MOVE 0 TO T-COST-PER(ROBOT-COUNT)
                           MOVE 0 TO T-COST-TOTAL(ROBOT-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ROBOTS-FILE.

       PARSE-ROBOTS.
           UNSTRING ROBOTS-LINE
               DELIMITED BY ";"
               INTO WS-ROBOTNO WS-ROBOTNAME WS-TARGETQTY-TXT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-TARGETQTY-TXT) TO WS-TARGETQTY.

       PROCESS-BOM.
           MOVE "N" TO EOF-FLAG
           MOVE 0 TO LINE-NO
           OPEN INPUT BOM-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ BOM-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO LINE-NO
                       IF LINE-NO = 1
                           CONTINUE
                       ELSE
                           PERFORM PARSE-BOM
                           PERFORM APPLY-BOM-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BOM-FILE.

       PARSE-BOM.
           UNSTRING BOM-LINE
               DELIMITED BY ";"
               INTO WS-BOM-ROBOTNO WS-BOM-PARTNO WS-BOM-QTY-TXT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-BOM-QTY-TXT) TO WS-BOM-QTY.

       APPLY-BOM-LINE.
           PERFORM FIND-ROBOT-IDX
           PERFORM FIND-PART-IDX
           IF ROBOT-IDX > 0 AND PART-IDX > 0
               COMPUTE WS-LINECOST = T-UNITCOST(PART-IDX) * WS-BOM-QTY
               ADD WS-LINECOST TO T-COST-PER(ROBOT-IDX)
           ELSE
               DISPLAY "WARNUNG: BOM-ZEILE NICHT VERARBEITET -> "
                       WS-BOM-ROBOTNO " / " WS-BOM-PARTNO
           END-IF.

       FIND-ROBOT-IDX.
           MOVE 0 TO ROBOT-IDX
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > ROBOT-COUNT
                   OR ROBOT-IDX > 0
               MOVE FUNCTION TRIM(T-ROBOTNO(I)) TO WS-KEY1
               MOVE FUNCTION TRIM(WS-BOM-ROBOTNO) TO WS-KEY2
               IF WS-KEY1 = WS-KEY2
                   MOVE I TO ROBOT-IDX
               END-IF
           END-PERFORM.

       FIND-PART-IDX.
           MOVE 0 TO PART-IDX

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > PART-COUNT
                   OR PART-IDX > 0

               MOVE FUNCTION TRIM(T-PARTNO(I)) TO WS-KEY1
               MOVE FUNCTION TRIM(WS-BOM-PARTNO) TO WS-KEY2

               IF WS-KEY1 = WS-KEY2
                   MOVE I TO PART-IDX
               END-IF

           END-PERFORM.

       CALC-TOTALS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ROBOT-COUNT
               COMPUTE T-COST-TOTAL(I) = T-COST-PER(I) * T-TARGETQTY(I)
           END-PERFORM.

       SHOW-RESULTS.
           DISPLAY "----- MATERIALKOSTEN PRO ROBOTER -----"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ROBOT-COUNT
               DISPLAY T-ROBOTNO(I) " | " T-ROBOTNAME(I)
               DISPLAY "  Kosten pro Stueck: " T-COST-PER(I)
               DISPLAY "  Kosten gesamt   : " T-COST-TOTAL(I)
           END-PERFORM.
