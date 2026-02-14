       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADROBOTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROBOTS-FILE ASSIGN TO "..\data\robots.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ROBOTS-FILE.
       01  ROBOTS-LINE             PIC X(200).

       WORKING-STORAGE SECTION.
       77  EOF-ROBOTS              PIC X VALUE "N".
       77  LINE-NO                 PIC 9(4) VALUE 0.
       77  ROBOT-COUNT             PIC 9(3) VALUE 0.
       77  I                       PIC 9(3) VALUE 0.


       01  WS-ROBOTNO              PIC X(10).
       01  WS-ROBOTNAME            PIC X(60).
       01  WS-TARGETQTY-TXT        PIC X(20).
       01  WS-TARGETQTY            PIC 9(5).

       01  ROBOT-TABLE.
           05 ROBOT-ENTRY OCCURS 50 TIMES.
              10 T-ROBOTNO         PIC X(10).
              10 T-ROBOTNAME       PIC X(60).
              10 T-TARGETQTY       PIC 9(5).
              10 T-COST-PER        PIC 9(9)V99.
              10 T-COST-TOTAL      PIC 9(11)V99.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT ROBOTS-FILE
           PERFORM UNTIL EOF-ROBOTS = "Y"
               READ ROBOTS-FILE
                   AT END
                       MOVE "Y" TO EOF-ROBOTS
                   NOT AT END
                       ADD 1 TO LINE-NO
                       IF LINE-NO = 1
                           CONTINUE
                       ELSE
                           PERFORM PARSE-ROBOTS-LINE
                           ADD 1 TO ROBOT-COUNT
                           MOVE WS-ROBOTNO   TO T-ROBOTNO(ROBOT-COUNT)
                           MOVE WS-ROBOTNAME TO T-ROBOTNAME(ROBOT-COUNT)
                           MOVE WS-TARGETQTY TO T-TARGETQTY(ROBOT-COUNT)
                           MOVE 0 TO T-COST-PER(ROBOT-COUNT)
                           MOVE 0 TO T-COST-TOTAL(ROBOT-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ROBOTS-FILE

           DISPLAY "GELADENE ROBOTER: " ROBOT-COUNT
           PERFORM SHOW-ALL-ROBOTS
           STOP RUN.

       PARSE-ROBOTS-LINE.
           UNSTRING ROBOTS-LINE
               DELIMITED BY ";"
               INTO WS-ROBOTNO WS-ROBOTNAME WS-TARGETQTY-TXT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-TARGETQTY-TXT) TO WS-TARGETQTY.

       SHOW-ALL-ROBOTS.
           MOVE 1 TO I
           PERFORM UNTIL I > ROBOT-COUNT
               DISPLAY T-ROBOTNO(I)
                       " | "
                       T-ROBOTNAME(I)
                       " | Ziel="
                       T-TARGETQTY(I)
               ADD 1 TO I
           END-PERFORM.


