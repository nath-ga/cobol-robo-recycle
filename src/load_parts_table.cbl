       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADPARTS.

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
       77  PART-COUNT              PIC 9(4) VALUE 0.

       01  WS-PARTNO               PIC X(10).
       01  WS-NAME                 PIC X(60).
       01  WS-UNITCOST-TXT         PIC X(20).
       01  WS-UNITCOST             PIC 9(9)V99.

       01  PART-TABLE.
           05 PART-ENTRY OCCURS 200 TIMES.
              10 T-PARTNO          PIC X(10).
              10 T-NAME            PIC X(60).
              10 T-UNITCOST        PIC 9(9)V99.

       77  I                       PIC 9(4) VALUE 0.
       77  SEARCH-PARTNO           PIC X(10) VALUE SPACES.
       77  FOUND                   PIC X VALUE "N".
       01  FOUND-NAME              PIC X(60).
       01  FOUND-COST              PIC 9(9)V99.

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
                           ADD 1 TO PART-COUNT
                           MOVE WS-PARTNO    TO T-PARTNO(PART-COUNT)
                           MOVE WS-NAME      TO T-NAME(PART-COUNT)
                           MOVE WS-UNITCOST  TO T-UNITCOST(PART-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PARTS-FILE

           DISPLAY "GELADENE PARTS: " PART-COUNT
           PERFORM SHOW-FIRST-THREE
           PERFORM TEST-LOOKUP
           STOP RUN.

       PARSE-PARTS-LINE.
           UNSTRING PARTS-LINE
               DELIMITED BY ";"
               INTO WS-PARTNO WS-NAME WS-UNITCOST-TXT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-UNITCOST-TXT) TO WS-UNITCOST.

       SHOW-FIRST-THREE.
           IF PART-COUNT >= 1
               DISPLAY "1: " T-PARTNO(1)
                       " | " T-NAME(1)
                       " | " T-UNITCOST(1)
           END-IF

           IF PART-COUNT >= 2
               DISPLAY "2: " T-PARTNO(2)
                       " | " T-NAME(2)
                       " | " T-UNITCOST(2)
           END-IF

           IF PART-COUNT >= 3
               DISPLAY "3: " T-PARTNO(3)
                       " | " T-NAME(3)
                       " | " T-UNITCOST(3)
           END-IF.

       TEST-LOOKUP.
           MOVE "PA200" TO SEARCH-PARTNO
           PERFORM FIND-PART
           IF FOUND = "Y"
               DISPLAY "GEFUNDEN: " SEARCH-PARTNO
                       " | " FOUND-NAME
                       " | " FOUND-COST
           ELSE
               DISPLAY "NICHT GEFUNDEN: " SEARCH-PARTNO
           END-IF.

       FIND-PART.
           MOVE "N" TO FOUND
           MOVE SPACES TO FOUND-NAME
           MOVE 0 TO FOUND-COST

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PART-COUNT
               IF T-PARTNO(I) = SEARCH-PARTNO
                   MOVE "Y" TO FOUND
                   MOVE T-NAME(I) TO FOUND-NAME
                   MOVE T-UNITCOST(I) TO FOUND-COST
                   MOVE PART-COUNT TO I
               END-IF
           END-PERFORM.





