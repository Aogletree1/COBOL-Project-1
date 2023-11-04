       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PIZZA-INVENTORY-PROG.
       AUTHOR.     AUSTIN_OGLETREE.
      **********************************************************
      *  This program is designed to create a running inven-
      *  tory of three pizza trucks for Rolling Pizza Trucks. 
         
      **********************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBMPC.
       OBJECT-COMPUTER.    IBMPC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PR1FA22-Pizza  
               ASSIGN TO 'PR1FA22-Pizza.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PIZZA-TRUCK-OUTPUT-FILE 
               ASSIGN TO 'PIZZA-TRUCK-OUTPUT.TXT'.
 
      *
       DATA DIVISION.
       FILE SECTION.


       FD  PR1FA22-Pizza
           RECORD CONTAINS 40 CHARACTERS.
       01  PIZZA-TRUCK-REC.
           05  TRUCK-ID-IN        PIC X(5).
           05  EMPLOYEE-ID-IN     PIC X(4).
           05  EMPLOYEE-NAME-IN   PIC X(20).
           05  ITEM-ID-IN         PIC A(2).
           05  NUM-IN-STOCK-IN    PIC 9(3).
           05  PURCHASE-IN        PIC 9(3).
           05  SELLING-IN         PIC 9(3).
      *
       FD    PIZZA-TRUCK-OUTPUT-FILE
             RECORD CONTAINS 80 CHARACTERS.

       01    PIZZA-OUTPUT-REC            PIC X(80).
      *********
       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.
             05    ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.

      *************************OUTPUT AREA*****************************
       01 REPORT-HEADER.
          05 FILLER            PIC X(2) VALUE SPACES.
          05 H1-DATE           PIC 9999/99/99       .
          05 FILLER            PIC X(23) VALUE SPACES.
          05 COMPANY-NAME      PIC X(13) VALUE 'ROLLING PIZZA'.
          05 FILLER            PIC X(19) VALUE SPACES.
          05 INITIALS          PIC X(3) VALUE 'AHO'.
      *
       01 REPORT-HEADER-2.
          05 FILLER          PIC X(33) VALUE SPACES.
          05 REPORT-LINE     PIC X(16) VALUE 'INVENTORY REPORT'.
          

       01 ITEM-HEADER.
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 TRUCK           PIC X(5) VALUE 'TRUCK'   .
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 EMPLOYEE        PIC X(8) VALUE 'EMPLOYEE'.
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 ITEM            PIC X(4) VALUE 'ITEM'    .
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 NUM-IN          PIC X(6) VALUE 'NUM IN'  .
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 PURHCASE        PIC X(8) VALUE 'PURCHASE'.
          05 FILLER          PIC X(5) VALUE SPACES    .
          05 SELLING         PIC X(7) VALUE 'SELLING' .
 
       01 ID-LINE.
          05 FILLER          PIC X(6) VALUE SPACES  .
          05 ID1              PIC X(2) VALUE 'ID'    .
          05 FILLER          PIC X(10) VALUE SPACES .
          05 ID2              PIC X(2) VALUE 'ID'    .
          05 FILLER          PIC X(9) VALUE SPACES  .
          05 ID3              PIC X(2) VALUE 'ID'    .
          05 FILLER          PIC X(6) VALUE SPACES  .
          05 STOCK           PIC X(5) VALUE 'STOCK' .
          05 FILLER          PIC X(7) VALUE SPACES  .
          05 PRICE           PIC X(5) VALUE 'PRICE' .
          05 FILLER          PIC X(8) VALUE SPACES  .
          05 PRICE2          PIC X(5) VALUE 'PRICE' .

       01 DETAIL-LINE.
          05 FILLER          PIC X(5) VALUE SPACES   .
          05 TRUCK-ID-OUT     PIC X(5)               .
          
          05 FILLER           PIC X(7) VALUE SPACES  .
          05 EMPLOYEE-ID-OUT  PIC X(4)               .
          
          05 FILLER           PIC X(8) VALUE SPACES  .
          05 ITEM-ID-OUT      PIC AA                 .
          
          05 FILLER           PIC X(7) VALUE SPACES  .
          05 NUM-IN-STOCK-OUT PIC 999                .
          
          05 FILLER           PIC X(9) VALUE SPACES  .
          05 PURCHASE-OUT     PIC 999                .
          
          05 FILLER           PIC X(10) VALUE SPACES  .
          05 SELLING-OUT      PIC 999                .

       PROCEDURE DIVISION.

       100-MAIN-MODULE.

           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-RECORDS
           PERFORM 250-CLOSE-ROUTINE

           .
      *
       125-HOUSEKEEPING.

           OPEN    INPUT     PR1FA22-Pizza
           OPEN    OUTPUT    PIZZA-TRUCK-OUTPUT-FILE
           
           ACCEPT H1-DATE FROM DATE YYYYMMDD           

           MOVE REPORT-HEADER    TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE.

           MOVE REPORT-HEADER-2  TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE.

           MOVE ITEM-HEADER      TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 3 LINE.

           MOVE ID-LINE          TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 1 LINE
            
                  .
      *
       150-READ-RECORDS.

             PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
                  READ PR1FA22-Pizza 
                      AT END
                          MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                          PERFORM 200-PROCESS-RTN
                  END-READ
              END-PERFORM
           .
      *
       200-PROCESS-RTN.

              MOVE TRUCK-ID-IN        TO TRUCK-ID-OUT
              MOVE EMPLOYEE-ID-IN     TO EMPLOYEE-ID-OUT
              MOVE ITEM-ID-IN         TO ITEM-ID-OUT
              MOVE NUM-IN-STOCK-IN    TO NUM-IN-STOCK-OUT
              MOVE PURCHASE-IN        TO PURCHASE-OUT
              MOVE SELLING-IN         TO SELLING-OUT


              MOVE DETAIL-LINE TO PIZZA-OUTPUT-REC
                    
              WRITE PIZZA-OUTPUT-REC 
                     AFTER ADVANCING 1 LINE


           .

       250-CLOSE-ROUTINE.


              CLOSE    PR1FA22-Pizza
                       PIZZA-TRUCK-OUTPUT-FILE

              STOP RUN
           .


