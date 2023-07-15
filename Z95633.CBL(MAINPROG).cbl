       CBL MAP
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE
              ASSIGN TO INPFILE
              FILE STATUS IS INP-ST.
           SELECT OUT-FILE
              ASSIGN TO OUTFILE
              FILE STATUS IS OUT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05 OUT-ID               PIC 9(05).
           05 OUT-DVZ              PIC 9(03).
           05 OUT-FIL              PIC X(01).
           05 OUT-ISLEM-TIPI       PIC X(10).
           05 OUT-FIL2             PIC X(04).
           05 OUT-RETURN-CODE      PIC 9(02).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 OUT-ACIKLAMA         PIC X(30).
           05 OUT-FNAME-FROM       PIC X(15).
           05 OUT-FNAME-TO         PIC X(15).
           05 OUT-LNAME-FROM       PIC X(15).
           05 OUT-LNAME-TO         PIC X(15).

       FD INP-FILE RECORDING MODE F.
       01 INP-REC.
          05 INP-ISLEM-TIPI        PIC X(01).
          05 INP-ID                PIC X(05).
          05 INP-DVZ               PIC X(03).

       WORKING-STORAGE SECTION.
       01  WS-HATA.
           05 WS-GECERSIZ-ISL      PIC X(01).
           05 FILLER PIC X(20)     VALUE ':GECERSIZ ISLEM TIPI'.
       01  WS-WORK-AREA.
           05 WS-SUBPROG           PIC X(07) VALUE 'SUBPROG'.
           05 OUT-ST               PIC 9(02).
              88 OUT-SUCCESS                 VALUE 00 97.
           05 INP-ST               PIC 9(02).
              88 INP-EOF                     VALUE 10.
              88 INP-SUCCESS                 VALUE 00 97.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC       PIC X(01).
                 88 WS-ISLEM-TIPI-VALID      VALUE 'R' 'U' 'W' 'D'.
                 88 WS-FUNC-WRITE            VALUE 'W'.
                 88 WS-FUNC-READ             VALUE 'R'.
                 88 WS-FUNC-UPDATE           VALUE 'U'.
                 88 WS-FUNC-DELETE           VALUE 'D'.
              07 WS-SUB-ID         PIC 9(05).
              07 WS-SUB-DVZ        PIC 9(03).
              07 WS-SUB-RC         PIC 9(02).
              07 WS-SUB-DATA       PIC X(30).
              07 WS-FNAME-FROM     PIC X(15).
              07 WS-FNAME-TO       PIC X(15).
              07 WS-LNAME-FROM     PIC X(15).
              07 WS-LNAME-TO       PIC X(15).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
           STOP RUN.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF NOT INP-SUCCESS
              DISPLAY 'GIRDI DOSYASI ACILMIYOR'
              STOP RUN
           END-IF.
           IF NOT OUT-SUCCESS
              DISPLAY 'CIKTI DOSYASI ACILMIYOR'
              STOP RUN
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
           READ INP-FILE
           NOT AT END
              MOVE INP-ISLEM-TIPI TO   WS-SUB-FUNC
              MOVE INP-ID         TO   WS-SUB-ID
              MOVE INP-DVZ        TO   WS-SUB-DVZ
              PERFORM H300-SUBPROG
           END-READ.

       H300-SUBPROG.
           CALL WS-SUBPROG USING WS-SUB-AREA.
           MOVE SPACES TO OUT-REC.
           MOVE INP-ISLEM-TIPI   TO  OUT-ISLEM-TIPI.
           MOVE INP-ID           TO  OUT-ID.
           MOVE INP-DVZ          TO  OUT-DVZ.
           MOVE WS-SUB-RC        TO  OUT-RETURN-CODE.
      *    MOVE WS-SUB-DATA      TO  OUT-ACIKLAMA.
           MOVE WS-FNAME-FROM    TO  OUT-FNAME-FROM.
           MOVE WS-FNAME-TO      TO  OUT-FNAME-TO.
           MOVE WS-LNAME-FROM    TO  OUT-LNAME-FROM.
           MOVE WS-LNAME-TO      TO  OUT-LNAME-TO.
           MOVE '-'              TO  OUT-FIL.
           MOVE '-RC:'        TO  OUT-FIL2.
           PERFORM H400-ISLEM-KONT.

       H400-ISLEM-KONT.
           IF WS-ISLEM-TIPI-VALID
              EVALUATE WS-SUB-FUNC,
                 WHEN 'R'
                    IF WS-SUB-RC = 23
                       MOVE 'KAYIT BULUNAMADI' TO OUT-ACIKLAMA
                    ELSE
                       MOVE 'KAYIT OKUNDU'     TO OUT-ACIKLAMA
                    END-IF,
                    SET WS-FUNC-READ TO TRUE
                    MOVE 'READ' TO OUT-ISLEM-TIPI,
                 WHEN 'U'
                    IF WS-SUB-RC = 23
                       MOVE 'KAYIT BULUNAMADI' TO OUT-ACIKLAMA
                    ELSE
                       MOVE 'KAYIT GUNCELLENDI' TO OUT-ACIKLAMA
                    END-IF
                    SET WS-FUNC-UPDATE TO TRUE
                    MOVE 'UPDATE' TO OUT-ISLEM-TIPI,
                 WHEN 'W'
                    IF WS-SUB-RC = 23
                       MOVE 'KAYIT ZATEN VAR'  TO OUT-ACIKLAMA
                    ELSE
                       MOVE 'KAYIT EKLENDI'    TO OUT-ACIKLAMA
                    END-IF
                    SET WS-FUNC-WRITE TO TRUE
                    MOVE 'WRITE' TO OUT-ISLEM-TIPI
                 WHEN 'D'
                    IF WS-SUB-RC = 23
                       MOVE 'KAYIT BULUNAMADI' TO OUT-ACIKLAMA
                    ELSE
                       MOVE 'KAYIT SILINDI'    TO OUT-ACIKLAMA
                    END-IF
                    SET WS-FUNC-READ TO TRUE
                    MOVE 'DELETE' TO OUT-ISLEM-TIPI
              END-EVALUATE
              WRITE OUT-REC
           ELSE
              MOVE WS-SUB-FUNC TO WS-GECERSIZ-ISL
              WRITE OUT-REC FROM WS-HATA
           END-IF.
       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
