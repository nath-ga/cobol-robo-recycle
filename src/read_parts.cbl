       IDENTIFICATION DIVISION.
       PROGRAM-ID. READPARTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTS-FILE ASSIGN TO "..\data\parts.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARTS-FILE.
       01  PARTS-LINE              PIC X(200).

       WORKING-STORAGE SECTION.
       77  EOF-PARTS               PIC X VALUE "N".
       77  LINE-NO                 PIC 9(4) VALUE 0.

       01  WS-PARTNO               PIC X(10).
       01  WS-NAME                 PIC X(60).
       01  WS-UNITCOST-TXT         PIC X(20).
       01  WS-UNITCOST             PIC 9(9)V99.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT PARTS-FILE
           PERFORM UNTIL EOF-PARTS = "Y"
               READ PARTS-FILE
                   AT END
                       MOVE "Y" TO EOF-PARTS
                   NOT AT END
                       ADD 1 TO LINE-NO
                       IF LINE-NO = 1
                           CONTINUE
                       ELSE
                           PERFORM PARSE-PARTS-LINE
                           DISPLAY "PARTNO=" WS-PARTNO
                                   " | NAME=" WS-NAME
                                   " | COST=" WS-UNITCOST
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PARTS-FILE
           STOP RUN.

       PARSE-PARTS-LINE.
           UNSTRING PARTS-LINE
               DELIMITED BY ";"
               INTO WS-PARTNO WS-NAME WS-UNITCOST-TXT
           END-UNSTRING

           MOVE FUNCTION NUMVAL(WS-UNITCOST-TXT) TO WS-UNITCOST.
