      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  I PIC 9(4) VALUE 0.
       01  J PIC 9(4) VALUE 0.
       01  K PIC 9(4) VALUE ZEROES.
       01  N       PIC 9(4) VALUES 100.*> NUMBER OF PARTICLES
       01  N-VARS  PIC 99 VALUES 2. *> NUMBER OF VARIABLES FOR THE PARTICLE
       01  SWARM   OCCURS 100 TIMES.
           05 PARTICLE PIC S9(4)V9(6) OCCURS 10 TIMES.
           05 VELOCITY PIC S9(4)V9(6) OCCURS 10 TIMES.
           05 PARTICLE-VALUE PIC S9(4)V9(6) VALUE ZEROES.

       01  GLOBAL-BEST.
           05 GLOBAL-BEST-PARTICLE PIC S9(4)V9(6) OCCURS 10 TIMES.
           05 GLOBAL-BEST-VALUE PIC S9(4)V9(6) VALUE 9999.9999.

       01  PERSONAL-BEST OCCURS 100 TIMES.
           05 PERSONAL-BEST-PARTICLE PIC S9(4)V9(6) OCCURS 10 TIMES.
           05 PERSONAL-BEST-VALUE PIC S9(4)V9(6) VALUE ZEROES.

       01  BOUNDS OCCURS 10 TIMES.
           10 UPPER-BOUND PIC 99 VALUES 10.
           10 LOWER-BOUND PIC S99 VALUES -10.
       01  RANDOM-NUMBER PIC S9V9(9).
       01  WS-SEED PIC 9(9).
       01  WS-RANGE PIC 99.
       01  WS-SUM PIC 9(9)V9(6).
       01  WS-RESULT PIC 9(9)V9(6).

       01  OPTIMIZER.
           05 C1 PIC 9 VALUE 2.
           05 C2 PIC 9 VALUE 2.
           05 R1 PIC S9V9(9).
           05 R2 PIC S9V9(9).
           05 W PIC 9V99 VALUE 0.8.
           05 ITER PIC 9(4) VALUE 800.

       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.

           PERFORM 100-INITIALIZE-SWARM.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > ITER

               PERFORM 300-EVALUATE-VELOCITY
               PERFORM 400-UPDATE-POSITION
               PERFORM 700-CHECK-BOUNDARIES
               PERFORM 100-EVALUATE-OBJECTIVE
               PERFORM 500-DETERMINE-PERSONAL-BEST
               PERFORM 600-DETERMINE-GLOBAL-BEST
               PERFORM 999-REPORT
      *>          COMPUTE W = 1 - W*K/ITER
           END-PERFORM.
           DISPLAY "GLOBAL BEST VALUE: " GLOBAL-BEST-VALUE.
           DISPLAY "GLOBAL BEST POSITION: "
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
               DISPLAY GLOBAL-BEST-PARTICLE(J)
           END-PERFORM.
           STOP RUN.
       100-INITIALIZE-SWARM.
           ACCEPT WS-SEED FROM TIME
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM(WS-SEED)
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
                   SUBTRACT LOWER-BOUND(J) FROM UPPER-BOUND(J)
                   GIVING WS-RANGE
                   COMPUTE RANDOM-NUMBER = FUNCTION RANDOM*WS-RANGE
                   + LOWER-BOUND(J)
                   MOVE RANDOM-NUMBER TO PARTICLE(I J)
                   MOVE RANDOM-NUMBER TO PERSONAL-BEST-PARTICLE(I J)
               END-PERFORM
               PERFORM 200-OBJECTIVE 1 TIMES
               MOVE PARTICLE-VALUE(I) TO PERSONAL-BEST-VALUE(I)
            END-PERFORM.


       100-EVALUATE-OBJECTIVE.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM 200-OBJECTIVE 1 TIMES
      *>          DISPLAY SWARM(I)
            END-PERFORM.

       200-OBJECTIVE.
      *>      MOVE 0 TO WS-SUM.
      *>      PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
      *>          COMPUTE WS-RESULT = PARTICLE(I J) ** 2
      *>          ADD WS-RESULT TO WS-SUM
      *>      END-PERFORM.
      *>      MOVE WS-SUM TO PARTICLE-VALUE(I).
      *>   ROSENBROCK FUNCTION
           COMPUTE WS-RESULT = (1 - PARTICLE(I 1)) ** 2 +
               100*(PARTICLE(I 2) - PARTICLE(I 1) ** 2) ** 2.
           MOVE WS-RESULT TO PARTICLE-VALUE(I).

       300-EVALUATE-VELOCITY.
      *>      DETERMINE THE VELOCITY OF THE PARTICLE USING THE PERSONAL AND GLOBAL VALUES
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
                   COMPUTE R1 = FUNCTION RANDOM(WS-SEED)
                   COMPUTE R2 = FUNCTION RANDOM(WS-SEED)
                   COMPUTE VELOCITY(I J) = W*VELOCITY(I J) +
                    C1*R1*(PERSONAL-BEST-PARTICLE(I J) - PARTICLE(I J))+
                    C2*R2*(GLOBAL-BEST-PARTICLE(J) - PARTICLE(I J))
               END-PERFORM
      *>          DISPLAY SWARM(I)
           END-PERFORM.
       400-UPDATE-POSITION.
      *>      USING THE NEW VELOCITY DETERMINE THE PARTICLES POSITIONS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
      *>          NEW POSITION = OLD POISTION + COMPUTED VELOCITY
                   ADD VELOCITY(I J) TO PARTICLE(I J)
                   GIVING PARTICLE(I J)
               END-PERFORM
           END-PERFORM.
       500-DETERMINE-PERSONAL-BEST.
      *>      USE THE CURRENT POSITION TO UPDATE PERSONAL BEST
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF PARTICLE-VALUE(I) < PERSONAL-BEST-VALUE(I)
                   MOVE PARTICLE-VALUE(I) TO PERSONAL-BEST-VALUE(I)
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
                       MOVE PARTICLE(I J) TO PERSONAL-BEST-PARTICLE(I J)
                   END-PERFORM
               END-IF
      *>          DISPLAY "PERSONAL BEST: " PERSONAL-BEST-VALUE(I)
           END-PERFORM.
       600-DETERMINE-GLOBAL-BEST.
      *>      USE THE PERSONAL BEST TO UPDATE GLOBAL BEST
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF PERSONAL-BEST-VALUE(I) < GLOBAL-BEST-VALUE
                   MOVE PERSONAL-BEST-VALUE(I) TO GLOBAL-BEST-VALUE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
                       MOVE PERSONAL-BEST-PARTICLE(I J)
                       TO GLOBAL-BEST-PARTICLE(J)
                   END-PERFORM
               END-IF

           END-PERFORM.
       700-CHECK-BOUNDARIES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N-VARS
                   IF PARTICLE(I J) > UPPER-BOUND(J)
                       COMPUTE VELOCITY(I J) = (-1)*VELOCITY(I J)
                       MOVE UPPER-BOUND(J) TO PARTICLE(I J)
                   END-IF
                   IF PARTICLE(I J) < LOWER-BOUND(J)
                       COMPUTE VELOCITY(I J) = (-1)*VELOCITY(I J)
                       MOVE LOWER-BOUND(J) TO PARTICLE(I J)
               END-PERFORM
           END-PERFORM.
       999-REPORT.
           DISPLAY GLOBAL-BEST-VALUE.
      *>      PERFORM VARYING I FROM 1 BY 1 UNTIL I>N
      *>          DISPLAY PERSONAL-BEST-VALUE(I)
      *>      END-PERFORM.
       END PROGRAM YOUR-PROGRAM-NAME.
