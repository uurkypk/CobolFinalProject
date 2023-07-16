       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE
              ASSIGN TO IDXFILE
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS IDX-KEY
              FILE STATUS IS IDX-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID            PIC S9(5) COMP-3.
              05 IDX-DVZ           PIC S9(3) COMP.
           03 IDX-FNAME            PIC X(15).
           03 IDX-LNAME            PIC X(15).
           03 IDX-DATE             PIC S9(07) COMP-3.
           03 IDX-BALANCE          PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
       01  WS-IDX-REC.
           03 WS-IDX-KEY.
              05 WS-IDX-ID            PIC S9(5) COMP-3.
              05 WS-IDX-DVZ           PIC S9(3) COMP.
           03 WS-IDX-FNAME            PIC X(15).
           03 WS-IDX-LNAME            PIC X(15).
           03 WS-IDX-DATE             PIC S9(07) COMP-3.
           03 WS-IDX-BALANCE          PIC S9(15) COMP-3.
       01 WS-EOF         PIC X VALUE 'N'.
       01 WS-IDX-FLAG.
          03 IDX-ST               PIC 9(02).
             88 IDX-ST-SUCC       VALUE 00 97.
       01 WS-DISHAT               PIC 9.
          88 WS-DISHATD           VALUE 1.
       01 WS-UPDP.
          03 WS-FNAMET            PIC X(15).
          03 WS-UNSTR             PIC 9(3).
          03 WS-BOYUT             PIC 9(3).
       01 WS-NAMEB                PIC X(35).
       01 WS-STRPOI               PIC 999.
       LINKAGE SECTION.
       01 LS-SUB-AREA.
          05 LS-SUB-FUNC    PIC X(01).
             88 LS-FUNC-WRITE            VALUE 'W'.
             88 LS-FUNC-READ             VALUE 'R'.
             88 LS-FUNC-UPDATE           VALUE 'U'.
             88 LS-FUNC-DELETE           VALUE 'D'.
          05 LS-SUB-ID      PIC 9(05).
          05 LS-SUB-DVZ     PIC 9(03).
          05 LS-SUB-RC      PIC 9(02).
          05 LS-SUB-DATA    PIC X(30).
          05 LS-FNAME-FROM  PIC X(15).
          05 LS-FNAME-TO    PIC X(15).
          05 LS-LNAME-FROM  PIC X(15).
          05 LS-LNAME-TO    PIC X(15).
       PROCEDURE DIVISION USING LS-SUB-AREA.
       0000-MAIN.
           PERFORM INIT-PAR.
           PERFORM OPEN-PAR.
           MOVE     LS-SUB-ID  TO IDX-ID
           MOVE     LS-SUB-DVZ TO IDX-DVZ.
           EVALUATE LS-SUB-FUNC,
              WHEN 'R'
                 PERFORM READ-PAR,
              WHEN 'D'
                 PERFORM DLTE-PAR,
              WHEN 'W'
                 PERFORM WRIT-PAR,
              WHEN 'U'
                 PERFORM UPDT-PAR,
              WHEN OTHER
                 DISPLAY "GECERSIZ"
           END-EVALUATE.
           IF WS-DISHATD
              DISPLAY 'INVALID'
           END-IF.
           IF NOT WS-DISHATD
              DISPLAY 'NOT INVALID'
           END-IF.
           PERFORM CLOSE-FILE.
           GOBACK.

       READ-PAR.
           READ IDX-FILE KEY IS IDX-KEY
              INVALID KEY
                 PERFORM INVALID-PAR
              NOT INVALID
                 PERFORM NOT-INVALID-PAR
           END-READ.

       DLTE-PAR.
           PERFORM READ-PAR.
           DELETE IDX-FILE RECORD
              INVALID KEY
                 PERFORM INVALID-PAR
              NOT INVALID
                 PERFORM NOT-INVALID-PAR
           END-DELETE.
       UPDT-PAR.
           PERFORM READ-PAR.

           MOVE    IDX-FNAME    TO   WS-IDX-FNAME
           MOVE    IDX-LNAME    TO   WS-IDX-LNAME
           MOVE    WS-IDX-FNAME TO   LS-FNAME-FROM.
           MOVE    WS-IDX-LNAME TO   LS-LNAME-FROM.

           INSPECT WS-IDX-LNAME REPLACING ALL 'E' BY 'I'
           INSPECT WS-IDX-LNAME REPLACING ALL 'A' BY 'E'
           COMPUTE WS-BOYUT = LENGTH OF WS-IDX-FNAME
           PERFORM BOSLUK-UPD
                                 UNTIL WS-UNSTR > WS-BOYUT.
           MOVE    WS-NAMEB     TO IDX-FNAME
           MOVE    WS-IDX-LNAME TO IDX-LNAME
           MOVE    WS-NAMEB     TO LS-FNAME-TO
           MOVE    WS-IDX-LNAME TO LS-LNAME-TO.

           REWRITE IDX-REC
              INVALID KEY
                 PERFORM INVALID-PAR
              NOT INVALID
                 MOVE 00 TO LS-SUB-RC
           END-REWRITE.

       BOSLUK-UPD.
           UNSTRING WS-IDX-FNAME
               DELIMITED BY ALL SPACE
               INTO WS-FNAMET
               WITH POINTER WS-UNSTR
           END-UNSTRING.

           STRING WS-FNAMET DELIMITED BY SPACES
               INTO WS-NAMEB
               WITH POINTER WS-STRPOI
           END-STRING.
           MOVE SPACES TO WS-FNAMET.

       WRIT-PAR.
           PERFORM READ-PAR.
           MOVE LS-SUB-ID  TO WS-IDX-ID
           MOVE LS-SUB-DVZ TO WS-IDX-DVZ
           MOVE "UGUR"     TO WS-IDX-FNAME
           MOVE "KAYPAK"   TO WS-IDX-LNAME
           MOVE 06081999   TO WS-IDX-DATE
           MOVE 538        TO WS-IDX-BALANCE
           WRITE IDX-REC FROM WS-IDX-REC
              INVALID KEY
                 PERFORM INVALID-PAR
              NOT INVALID
                 PERFORM NOT-INVALID-PAR
           END-WRITE.

       INIT-PAR.
           MOVE SPACES      TO WS-IDX-REC
           MOVE SPACES      TO WS-NAMEB
           MOVE   1         TO WS-STRPOI
           MOVE   1         TO WS-UNSTR.

       OPEN-PAR.
           OPEN I-O IDX-FILE
           IF NOT IDX-ST-SUCC
              DISPLAY 'DOSYA ACILMADI : ' IDX-ST
              STOP RUN
           END-IF.

       INVALID-PAR.
           SET WS-DISHATD    TO   TRUE.
           MOVE    23        TO   LS-SUB-RC.
           MOVE    SPACES    TO   LS-FNAME-FROM.
           MOVE    SPACES    TO   LS-FNAME-TO.
           MOVE    SPACES    TO   LS-LNAME-FROM.
           MOVE    SPACES    TO   LS-LNAME-TO.

       NOT-INVALID-PAR.
           MOVE    0         TO   WS-DISHAT.
           MOVE    00        TO   LS-SUB-RC.
           MOVE    IDX-FNAME TO   LS-FNAME-FROM.
           MOVE    SPACES    TO   LS-FNAME-TO.
           MOVE    IDX-LNAME TO   LS-LNAME-FROM.
           MOVE    SPACES    TO   LS-LNAME-TO.

       CLOSE-FILE.
           CLOSE IDX-FILE.
