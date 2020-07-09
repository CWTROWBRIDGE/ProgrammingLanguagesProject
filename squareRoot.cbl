       IDENTIFICATION DIVISION.
       PROGRAM-ID. Root.
       AUTHOR. ChadT.

       DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 A PIC S9(4)V9(5).
          01 B PIC S9(4)V9(5).
          01 m PIC S9(4)V9(5).
          01 Ya PIC S9(4)V9(5).
          01 Yb PIC S9(4)V9(5).
          01 Ym PIC S9(4)V9(5).
          01 Original PIC S9(5)V9(5).
          01 Iter PIC 9(2).

       PROCEDURE DIVISION.
           DISPLAY "Please enter a number to find the square root of:".
           ACCEPT Original FROM CONSOLE.
           DISPLAY "Please enter the number of iterations desired:".
           ACCEPT Iter FROM CONSOLE
           MOVE 1 to A.
           DIVIDE Original by 2 giving B.
           PERFORM SquareRoot Iter TIMES.
           DISPLAY "The square root of " Original " is " m.
           STOP RUN.

       SquareRoot.
           ADD A to B giving m.
           DIVIDE m BY 2 GIVING m.
           MULTIPLY A BY A GIVING Ya.
           SUBTRACT Original FROM Ya GIVING Ya.
           MULTIPLY B BY B GIVING Yb.
           SUBTRACT Original FROM Yb GIVING Yb.
           MULTIPLY m BY m GIVING Ym.
           SUBTRACT Original FROM Ym GIVING Ym.
           IF Ym < 0 THEN
				  IF Ya < 0 THEN
				      MOVE m TO A
				  ELSE
				      MOVE m TO B
				  END-IF
           ELSE
		          IF Ya > 0
				      MOVE m TO A
				  ELSE
					  MOVE m TO B
				  END-IF
           END-IF.
