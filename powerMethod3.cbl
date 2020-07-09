       IDENTIFICATION DIVISION.
       PROGRAM-ID. Power3.
       AUTHOR. ChadT.

       DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 WS-TABLE.
             05 WS-A OCCURS 3 TIMES.
                10 WS-B OCCURS 3 TIMES.
                        15 WS-C PIC S9(3)V9(5).
          01 WS-Ztable.
             05 WS-Z OCCURS 3 TIMES.
                        15 WS-X PIC S9(3)V9(5).
          01 temp1 pic S9(3)V9(5).
          01 temp2 pic S9(3)V9(5).
          01 temp3 pic S9(3)V9(5).
          01 temp4 pic S9(3)V9(5).
          01 temp5 pic S9(3)V9(5).
          01 temp6 pic S9(3)V9(5).
          01 temp7 pic S9(3)V9(5).
          01 temp8 pic S9(3)V9(5).
          01 temp9 pic S9(3)V9(5).
          01 iter pic 99.
          01 W pic S9(3)V9(5).

       PROCEDURE DIVISION.
           DISPLAY "Please enter 9 numbers: ".
           ACCEPT WS-C(1,1) FROM CONSOLE.
           ACCEPT WS-C(1,2) FROM CONSOLE.
           ACCEPT WS-C(1,3) FROM CONSOLE.
           ACCEPT WS-C(2,1) FROM CONSOLE.
           ACCEPT WS-C(2,2) FROM CONSOLE.
           ACCEPT WS-C(2,3) FROM CONSOLE.
           ACCEPT WS-C(3,1) FROM CONSOLE.
           ACCEPT WS-C(3,2) FROM CONSOLE.
           ACCEPT WS-C(3,3) FROM CONSOLE.
           MOVE 1 to WS-X(1).
           MOVE 1 to WS-X(2).
           MOVE 1 to WS-X(3).

           DISPLAY "Number of iterations desired:"
           ACCEPT iter FROM CONSOLE.

           DISPLAY " ".
           DISPLAY WS-B(1,1)" "WS-B(1,2)" "WS-B(1,3).
           DISPLAY WS-B(2,1)" "WS-B(2,2)" "WS-B(2,3).
           DISPLAY WS-B(3,1)" "WS-B(3,2)" "WS-B(3,3).
           DISPLAY " ".
           DISPLAY WS-X(1) " " WS-X(2) " " WS-X(3).
           DISPLAY " ".

           PERFORM POWER-METH iter TIMES.
           DISPLAY "Eigenvalue: " W.

           DISPLAY "Eigenvector: " WS-X(1) " " WS-X(2) " " WS-X(3).

       STOP RUN.

       POWER-METH.
           PERFORM MATRIX-MULTIPLY.
           IF WS-X(1) IS GREATER THAN OR EQUAL TO WS-X(2) THEN
                   MOVE WS-X(1) to W
           ELSE IF WS-x(2) IS GREATER THAN OR EQUAL TO WS-X(3) THEN
                   MOVE WS-X(2) to W
           ELSE IF WS-X(1) IS GREATER THAN OR EQUAL TO WS-X(3) THEN
                   MOVE WS-X(1) to W
           ELSE
                   MOVE WS-X(3) to W
           END-IF.
           DIVIDE WS-X(1) BY W giving WS-X(1).
           DIVIDE WS-X(2) BY W giving WS-X(2).
           DIVIDE WS-X(3) BY W giving WS-X(3).
           DISPLAY WS-X(1) " " WS-X(2) " " WS-X(3).
           DISPLAY " ".

       MATRIX-MULTIPLY.
           MULTIPLY WS-C(1,1) by WS-X(1) giving temp1.
           MULTIPLY WS-C(1,2) by WS-X(2) giving temp2.
           MULTIPLY WS-C(1,3) by WS-X(3) giving temp3.
           MULTIPLY WS-C(2,1) by WS-X(1) giving temp4.
           MULTIPLY WS-C(2,2) by WS-X(2) giving temp5.
           MULTIPLY WS-C(2,3) by WS-X(3) giving temp6.
           MULTIPLY WS-C(3,1) by WS-X(1) giving temp7.
           MULTIPLY WS-C(3,2) by WS-X(2) giving temp8.
           MULTIPLY WS-C(3,3) by WS-X(3) giving temp9.
           ADD temp1 to temp2 giving WS-X(1).
           ADD temp3 to WS-X(1) giving WS-X(1).
           ADD temp4 to temp5 giving WS-X(2).
           ADD temp6 to WS-X(2) giving WS-X(2).
           ADD temp7 to temp8 giving WS-X(3).
           ADD temp9 to WS-X(3) giving WS-X(3).
