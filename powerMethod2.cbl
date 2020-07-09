       IDENTIFICATION DIVISION.
       PROGRAM-ID. Power2.
       AUTHOR. ChadT.

       DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 WS-TABLE.
             05 WS-A OCCURS 2 TIMES.
                10 WS-B OCCURS 2 TIMES.
                        15 WS-C PIC S9(3)V9(5).
          01 WS-Ztable.
             05 WS-Z OCCURS 2 TIMES.
                        15 WS-X PIC S9(3)V9(5).
          01 temp1 pic S9(3)V9(5).
          01 temp2 pic S9(3)V9(5).
          01 temp3 pic S9(3)V9(5).
          01 temp4 pic S9(3)V9(5).
          01 iter pic 99.
          01 W pic S9(3)V9(5).

       PROCEDURE DIVISION.
           DISPLAY "Please enter 4 numbers: ".
           ACCEPT WS-C(1,1) FROM CONSOLE.
           ACCEPT WS-C(1,2) FROM CONSOLE.
           ACCEPT WS-C(2,1) FROM CONSOLE.
           ACCEPT WS-C(2,2) FROM CONSOLE.
           MOVE 1 to WS-X(1).
           MOVE 1 to WS-X(2).

           DISPLAY "Number of iterations desired:"
           ACCEPT iter FROM CONSOLE.

           DISPLAY WS-B(1,1)" "WS-B(1,2). DISPLAY WS-B(2,1)" "WS-B(2,2).
           DISPLAY " ".
           DISPLAY WS-X(1) " " WS-X(2).
           DISPLAY " ".

           PERFORM POWER-METH iter TIMES.
           DISPLAY "Eigenvalue: " W.

           DISPLAY "Eigenvector: " WS-X(1) " " WS-X(2).

       STOP RUN.

       POWER-METH.
           PERFORM MATRIX-MULTIPLY.
           IF WS-X(1) > WS-X(2) THEN
                   MOVE WS-X(1) to W
           ELSE
                   MOVE WS-X(2) to W
           END-IF.
           DIVIDE WS-X(1) BY W giving WS-X(1).
           DIVIDE WS-X(2) BY W giving WS-X(2).
           DISPLAY WS-X(1) " " WS-X(2).
           DISPLAY " ".

       MATRIX-MULTIPLY.
           MULTIPLY WS-C(1,1) by WS-X(1) giving temp1.
           MULTIPLY WS-C(1,2) by WS-X(2) giving temp2.
           MULTIPLY WS-C(2,1) by WS-X(1) giving temp3.
           MULTIPLY WS-C(2,2) by WS-X(2) giving temp4.
           ADD temp1 to temp2 giving WS-X(1).
           ADD temp3 to temp4 giving WS-X(2).
