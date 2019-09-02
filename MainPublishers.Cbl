       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MainPublishers.
      *Publisher and Supplier are used interchangeably

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIBRARY-DETAILS-FILE ASSIGN TO "LDF.DAT"
               ORGANIZATION IS INDEXED
               FILE STATUS IS LDF-STATUS
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LDF-LIBRARY-NUM
               ALTERNATE RECORD KEY IS LDF-LIBRARY-CODE
                         WITH DUPLICATES.

           SELECT LIBRARY-FILE ASSIGN TO "LIBRARY.DAT"
               ORGANIZATION IS INDEXED
               FILE STATUS IS LIBRARY-STATUS
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LIBRARY-CODE
               ALTERNATE RECORD KEY IS LIBRARY-TITLE
               ALTERNATE RECORD KEY IS LIBRARY-SUPPLIER-CODE
                         WITH DUPLICATES.

           SELECT SUPPLIER-FILE ASSIGN TO "PUBLISHER.DAT"
               ORGANIZATION IS RELATIVE
               FILE STATUS IS SUPPLIER-STATUS
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS SUPPLIER-KEY.

           SELECT REPORT-FILE ASSIGN TO "MainPublishers.rpt".



       DATA DIVISION.
       FILE SECTION.

       FD  LIBRARY-DETAILS-FILE.
       01  LDF-RECORD.
           02  LDF-LIBRARY-NUM         PIC X(5).
           02  LDF-LIBRARY-CODE        PIC 9(5).
           02  LDF-RENTAL-EARNINGS     PIC 9(4)V99.
           02  LDF-PURCHASE-PRICE      PIC 999V99.

       FD  LIBRARY-FILE.
       01  LIBRARY-RECORD.
           02  LIBRARY-CODE            PIC 9(5).
           02  LIBRARY-TITLE           PIC X(29).
           02  FILLER                  PIC X(1).
           02  LIBRARY-SUPPLIER-CODE   PIC 99.


       FD  SUPPLIER-FILE.
       01  SUPPLIER-RECORD.
           02  SUPPLIER-CODE           PIC 99.
           02  SUPPLIER-NAME           PIC X(20).
           02  SUPPLIER-ADDRESS        PIC X(60).

       FD  REPORT-FILE.
       01  PRINT-LINE                  PIC X(74).


       WORKING-STORAGE SECTION.
       01  FILE-STATUS-IDENTIFIERS.
           02  LDF-STATUS              PIC X(2).
           02  LIBRARY-STATUS          PIC X(2).
           02  SUPPLIER-STATUS         PIC X(2).


       01  END-OF-FILE-FLAGS.
           02  FILLER                  PIC 9     VALUE 1.
               88    LIBRARY-FILE-END  VALUE 0.
           02  FILLER                  PIC 9     VALUE 1.
               88    LDF-FILE-END      VALUE 0.
               88    NOT-LDF-END       VALUE 1.


       01  REPORT-HEADING.
           02  FILLER                  PIC X(23) VALUE SPACES.
           02  FILLER                  PIC X(23)
                                       VALUE "MAIN PUBLISHERS REPORT".

       01  UNDER-LINE.
           02  FILLER                  PIC X(23) VALUE SPACES.
           02  FILLER                  PIC X(23) VALUE ALL "-".

       01  ITEM-HEADING.
           02  FILLER                  PIC X(10) VALUE "PUBLISHERS".
           02  FILLER                  PIC X(12) VALUE SPACES.
           02  FILLER                  PIC X(12) VALUE "ALL EARNINGS".
           02  FILLER                  PIC X(3)  VALUE  SPACES.
           02  FILLER                  PIC X(10) VALUE "BOOK TITLE".
           02  FILLER                  PIC X(19) VALUE SPACES.
           02  FILLER                  PIC X(7)  VALUE "AVERAGE".

       01  SUPPLIER-LINE.
           02  PRN-SUPPLIER-NAME       PIC X(19).
           02  PRN-SUPPLIER-EARNINGS   PIC B$$$$,$$9.99.
           02  PRN-LIBRARY-EMPTY       PIC X(4)  VALUE SPACES.
           02  PRN-LIBRARY-TITLE       PIC BBX(26).
           02  PRN-AVERAGE-EARNINGS    PIC B$$,$$9.99.

       01  EMPTY-LINE.
           02  FILLER                  PIC X(56).

       01  EMPTY-FIELDS.
           02  EMPTY-TITLE             PIC X(29) VALUE SPACES.
           02  EMPTY-OTHER             PIC X(27) VALUE SPACES.

       01  PRIMARY-SUPPLIERS.
           02 PRIMARY-SUPPLIER OCCURS 4 TIMES.
              03  PRIMARY-SUPP-CODE        PIC 99.
              03  PRIMARY-SUPP-EARNINGS    PIC 999999V99.
              03  PRIMARY-LIBRARY-CODE     PIC 9(5).
              03  PRIMARY-LIBRARY-TITLE    PIC X(30).
              03  PRIMARY-LIBRARY-EARNINGS PIC 9999V99.


       01  HOLD-SUPP-REC.
           02  HOLD-SUPP-CODE          PIC 99.
           02  HOLD-SUPP-EARNINGS      PIC 999999V99.
           02  HOLD-LIBRARY-CODE       PIC 9(5).
           02  HOLD-LIBRARY-TITLE      PIC X(30).
           02  HOLD-LIBRARY-EARNINGS   PIC 9999V99.

       01  COUNTERS.
           02  TOTAL-LIBRARY-EARNINGS  PIC 9(5)V99.
           02  EDITED-TOTAL-LIBRARY    PIC ZZ,ZZ9.99.
           02  AVERAGE-LIBRARY-EARNINGS
                                       PIC 9(4)V99.
           02  EDITED-AVERAGE-LIBRARY  PIC ZZ,ZZ9.99.
           02  EDITED-SUPP-EARNINGS    PIC ZZZ,ZZ9.99.
           02  LIBRARY-COUNT           PIC 99.
           02  SUPP-RANK               PIC 9.
           02  SUPPLIER-KEY            PIC 99.
           02  FILLER                  PIC 9.
               88  CONTINUE-CHECKING   VALUE 0.
               88  STOP-CHECKING       VALUE 1.


       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT LIBRARY-DETAILS-FILE.
           OPEN INPUT LIBRARY-FILE.
           OPEN INPUT SUPPLIER-FILE.
           OPEN OUTPUT REPORT-FILE.

           PERFORM PRINT-HEADINGS.

           MOVE ZEROS TO PRIMARY-SUPPLIERS.

           MOVE ZEROS TO LIBRARY-SUPPLIER-CODE.
           START LIBRARY-FILE
               KEY IS GREATER THAN LIBRARY-SUPPLIER-CODE
               INVALID KEY DISPLAY "START LIBRARY-FILE STATUS :- "
                                   LIBRARY-STATUS
           END-START.

           READ LIBRARY-FILE NEXT RECORD
               AT END     SET LIBRARY-FILE-END TO TRUE
           END-READ.
           PERFORM FIND-PRIMARY-SUPPLIERS UNTIL LIBRARY-FILE-END.

           PERFORM PRINT-PRIMARY-SUPPLIER-DETAILS

           CLOSE LIBRARY-DETAILS-FILE,
                 LIBRARY-FILE,
                 SUPPLIER-FILE,
                 REPORT-FILE.
           STOP RUN.

       PRINT-HEADINGS.
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER ADVANCING PAGE.

           WRITE PRINT-LINE FROM REPORT-HEADING
               AFTER ADVANCING 1 LINE
           WRITE PRINT-LINE FROM UNDER-LINE
               AFTER ADVANCING 1 LINE

           WRITE PRINT-LINE FROM ITEM-HEADING
               AFTER ADVANCING 2 LINE.

       PRINT-PRIMARY-SUPPLIER-DETAILS.
           PERFORM VARYING SUPP-RANK FROM 1 BY 1 UNTIL SUPP-RANK
                   GREATER THAN 3
               MOVE PRIMARY-SUPP-CODE(SUPP-RANK) TO SUPPLIER-KEY
               READ SUPPLIER-FILE
                    INVALID KEY DISPLAY "PTSD FS= " SUPPLIER-STATUS
               END-READ
               MOVE SUPPLIER-NAME TO PRN-SUPPLIER-NAME
               MOVE PRIMARY-SUPP-EARNINGS(SUPP-RANK) TO
                                                 PRN-SUPPLIER-EARNINGS
               MOVE PRIMARY-LIBRARY-TITLE(SUPP-RANK) TO
                                                 PRN-LIBRARY-TITLE
               MOVE PRIMARY-LIBRARY-EARNINGS(SUPP-RANK) TO
                                                    PRN-AVERAGE-EARNINGS
               WRITE PRINT-LINE FROM SUPPLIER-LINE
                   AFTER ADVANCING 2 LINES
           END-PERFORM.

       FIND-PRIMARY-SUPPLIERS.
           MOVE LIBRARY-SUPPLIER-CODE TO HOLD-SUPP-CODE
           MOVE ZEROS TO  HOLD-LIBRARY-EARNINGS, HOLD-SUPP-EARNINGS
           PERFORM SUM-SUPPLIER-EARNINGS
               UNTIL LIBRARY-SUPPLIER-CODE NOT EQUAL TO HOLD-SUPP-CODE
                     OR LIBRARY-FILE-END

           MOVE HOLD-SUPP-EARNINGS TO EDITED-SUPP-EARNINGS
               DISPLAY "          EARNINGS OF PUBLISHER " HOLD-SUPP-CODE ":"
                   EDITED-SUPP-EARNINGS

           DISPLAY EMPTY-LINE
           DISPLAY EMPTY-LINE
           DISPLAY EMPTY-LINE

           SET CONTINUE-CHECKING TO TRUE
           PERFORM VARYING SUPP-RANK FROM 3 BY -1 UNTIL SUPP-RANK
                   LESS THAN 1
                   OR STOP-CHECKING
               IF HOLD-SUPP-EARNINGS GREATER THAN
                  PRIMARY-SUPP-EARNINGS(SUPP-RANK)
                   MOVE PRIMARY-SUPPLIER(SUPP-RANK) TO
                     PRIMARY-SUPPLIER(SUPP-RANK + 1)
                   MOVE HOLD-SUPP-REC TO PRIMARY-SUPPLIER(SUPP-RANK)
               ELSE
                   SET STOP-CHECKING TO TRUE
               END-IF
           END-PERFORM.

       SUM-SUPPLIER-EARNINGS.
           MOVE LIBRARY-CODE TO LDF-LIBRARY-CODE
           READ LIBRARY-DETAILS-FILE
               KEY IS LDF-LIBRARY-CODE
               INVALID KEY DISPLAY "SUM-SUPP-EARNINGS FS = " LDF-STATUS
           END-READ

           SET NOT-LDF-END TO TRUE
           MOVE ZEROS TO TOTAL-LIBRARY-EARNINGS, LIBRARY-COUNT
           PERFORM SUM-TITLE-EARNINGS UNTIL
               LDF-LIBRARY-CODE NOT EQUAL TO LIBRARY-CODE
                   OR LDF-FILE-END

           DIVIDE TOTAL-LIBRARY-EARNINGS BY LIBRARY-COUNT
                  GIVING AVERAGE-LIBRARY-EARNINGS ROUNDED

           MOVE AVERAGE-LIBRARY-EARNINGS To EDITED-AVERAGE-LIBRARY
           DISPLAY LIBRARY-TITLE "AVERAGE          ="
                   EDITED-AVERAGE-LIBRARY

           MOVE TOTAL-LIBRARY-EARNINGS To EDITED-TOTAL-LIBRARY
           DISPLAY EMPTY-TITLE "ALL EARNINGS     ="
                   EDITED-TOTAL-LIBRARY

           DISPLAY EMPTY-LINE

           IF AVERAGE-LIBRARY-EARNINGS GREATER THAN
                                       HOLD-LIBRARY-EARNINGS
               MOVE AVERAGE-LIBRARY-EARNINGS TO HOLD-LIBRARY-EARNINGS
               MOVE LIBRARY-TITLE TO HOLD-LIBRARY-TITLE
               MOVE LIBRARY-CODE TO HOLD-LIBRARY-CODE
           END-IF

           READ LIBRARY-FILE NEXT RECORD
                AT END SET LIBRARY-FILE-END TO TRUE
           END-READ.


       SUM-TITLE-EARNINGS.
           ADD 1 TO LIBRARY-COUNT
           ADD LDF-RENTAL-EARNINGS TO TOTAL-LIBRARY-EARNINGS,
               HOLD-SUPP-EARNINGS.
           READ LIBRARY-DETAILS-FILE NEXT RECORD
                AT END SET LDF-FILE-END TO TRUE
           END-READ.
