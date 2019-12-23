|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       >>SOURCE FORMAT IS FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NORDAC01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNULINUX.
      *SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NORDCSV-FILE
                   ASSIGN TO NORDCSV-FILENAME
                   ORGANIZATION IS LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   FILE STATUS IS NORDCSV-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  NORDCSV-FILE.
      *    DATA RECORD IS NORDCSV-REC
       01  NORDCSV-REC.
               05  NORDCSV-DATA PIC X(512).

       WORKING-STORAGE SECTION.
       01  NORDCSV-STATUS PIC 99.

      *****************
      * Kirjauspäivä
      * Arvopäivä
      * Maksupäivä
      * Määrä
      * Saaja/Maksaja
      * Tilinumero
      * BIC
      * Tapahtuma
      * Viite
      * Maksajan viite
      * Viesti
      * Kortinnumero
      * Kuitti
      * (empty)
      *****************
       01  NORD-TX-RECORD.
               05  RECORD-DATE PIC X(10).
               05  VALUE-DATE PIC X(10).
               05  PAYMENT-DATE PIC X(10).
               05  AMOUNT PIC S9(9)V99 USAGE COMP-3.
               05  RECIPIENT-AND-PAYER PIC X(64).
               05  ACCOUNT-NUMBER PIC X(24).
               05  BIC-NUMBER PIC X(11).
               05  TRANSACTION PIC X(24).
               05  REFERENCE-NUMBER PIC X(32).
               05  PAYER-REFERENCE-NUMBER PIC X(32).
               05  MSG PIC X(64).
               05  CARD-NUMBER PIC X(16).
               05  RECEIPT PIC X(1).
               05  EMPTY PIC X(1).

       01  SUMMARY.
               05  SUMMARY-CREDIT PIC S9(9)V99 USAGE COMP-3 VALUE 0.00.
               05  SUMMARY-DEBIT PIC S9(9)V99 USAGE COMP-3 VALUE 0.00.

       01  FORMATTED.
               05  FORMATTED-CURRENCY PIC Z(8)9,99.

       01  NORDCSV-FILENAME PIC X(256).

       77  NL PIC X VALUE X"0A".
       77  HT PIC X VALUE X"09".

|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       PROCEDURE DIVISION.
           ACCEPT NORDCSV-FILENAME FROM ENVIRONMENT 'NORDCSV'

           OPEN INPUT NORDCSV-FILE

           PERFORM FOREVER
                   PERFORM READ-NORDCSV-FILE
                   IF NORDCSV-STATUS NOT EQUAL 00
                           EXIT PERFORM
                   END-IF
                   PERFORM UNSTRING-NORDCSV-REC
      *            PERFORM DISPLAY-NORD-TX
                   PERFORM CALCULATE-SUMMARY
           END-PERFORM

           PERFORM DISPLAY-SUMMARY

           CLOSE NORDCSV-FILE

           STOP RUN
           .

       READ-NORDCSV-FILE.
      *    MOVE SPACES TO NORDCSV-REC
           READ NORDCSV-FILE INTO NORDCSV-REC
           EXIT
           .

       UNSTRING-NORDCSV-REC.
      *    MOVE SPACES TO NORD-TX-RECORD
           UNSTRING NORDCSV-DATA
           DELIMITED BY HT
           INTO RECORD-DATE
                VALUE-DATE
                PAYMENT-DATE
                AMOUNT
                RECIPIENT-AND-PAYER
                ACCOUNT-NUMBER
                BIC-NUMBER
                TRANSACTION
                REFERENCE-NUMBER
                PAYER-REFERENCE-NUMBER
                MSG
                CARD-NUMBER
                RECEIPT
                EMPTY
           END-UNSTRING
           EXIT
           .

       DISPLAY-NORD-TX.
           DISPLAY
                    "| " RECORD-DATE
                   " | " VALUE-DATE
                   " | " PAYMENT-DATE
                   " | " AMOUNT
                   " | " RECIPIENT-AND-PAYER
                   " | " ACCOUNT-NUMBER
                   " | " BIC-NUMBER
                   " | " TRANSACTION
                   " | " REFERENCE-NUMBER
                   " | " PAYER-REFERENCE-NUMBER
                   " | " MSG
                   " | " CARD-NUMBER
                   " | " RECEIPT
                   " |"
           END-DISPLAY
           EXIT
           .

       CALCULATE-SUMMARY.
           IF AMOUNT < 0
                   ADD AMOUNT TO SUMMARY-DEBIT
           ELSE
                   ADD AMOUNT TO SUMMARY-CREDIT
           END-IF
           EXIT
           .

       DISPLAY-SUMMARY.
           DISPLAY "SUMMARY:"

           MOVE SUMMARY-DEBIT TO FORMATTED-CURRENCY
           DISPLAY
                   "DEBIT  : "
                   FUNCTION TRIM(FORMATTED-CURRENCY LEADING)
           END-DISPLAY

           MOVE SUMMARY-CREDIT TO FORMATTED-CURRENCY
           DISPLAY
                   "CREDIT : "
                   FUNCTION TRIM(FORMATTED-CURRENCY LEADING)
           END-DISPLAY

           EXIT
           .
