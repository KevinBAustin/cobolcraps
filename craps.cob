       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRAPS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLAYER-FILE-IN ASSIGN TO "player.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT PLAYER-FILE-OUT ASSIGN TO "player.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PLAYER-FILE-IN.
       01 PLAYER-IN.
           05 PAST-PLAYER PIC X(20).
           05 PAST-SCORE PIC Z(6).
       FD PLAYER-FILE-OUT.
       01 PLAYER-OUT.
           05 NEW-PLAYER PIC X(20).
           05 NEW-SCORE PIC 9(6).

       WORKING-STORAGE SECTION.
       01 PLAYERBAL PIC 9(6) VALUE 500.
       01 MINBET PIC 99 VALUE 10.
       01 MAXBET PIC 999 VALUE 200.
       01 BETCOME PIC 999.
       01 BETDONTCOME PIC 999.
       01 BETPASS PIC 999.
       01 BETDONTPASS PIC 999.
       01 ROLL PIC 99.
       01 PUCK-ORIG PIC 99.
       01 PUCK PIC 99.
       01 MAXROUNDS PIC 99 VALUE 10.
       01 PHASE PIC 9 VALUE 0.
       01 ROLLSUM PIC 99.
       01 BETCHECK PIC 9.
       01 BETSUM PIC 999.
       01 SKIP-1 PIC 9.
       01 SKIP-2 PIC 9.
       01 FILE-STATUS PIC 99.
       01 BOARD-COUNTER PIC 99.
       01 PLAYER-NAME PIC X(20).
       01 SLOT PIC 99 VALUE 4.
       01 PLAYER-TABLE.
           05 PLAYER OCCURS 11 TIMES INDEXED BY LEAD-INDEX.
               10 P-NAME-TBL PIC X(20).
               10 SCORE-TBL PIC X(20).

       SCREEN SECTION.
       01 BET-BOARD.
           05 BLANK SCREEN.
           05 LINE 1 COL 24 VALUE "DON'T".
           05 LINE 2 COL 9 VALUE "POINT   COME   COME".
           05 LINE 3 COL 9 VALUE "-----   ----   -----".
           05 LINE 4 COL 13 VALUE 4.
           05 LINE 5 COL 13 VALUE 5.
           05 LINE 6 COL 11 VALUE "SIX".
           05 LINE 7 COL 13 VALUE 8.
           05 LINE 8 COL 10 VALUE "NINE".
           05 LINE 9 COL 12 VALUE 10.
           05 LINE 12 COL 9 VALUE "DON'T COME:".
           05 LINE 12 COL 20 PIC 999 USING BETDONTCOME.
           05 LINE 13 COL 15 VALUE "COME:".
           05 LINE 13 COL 20 PIC 999 USING BETCOME.
           05 LINE 14 COL 9 VALUE "DON'T PASS:".
           05 LINE 14 COL 20 PIC 999 USING BETDONTPASS.
           05 LINE 15 COL 15 VALUE "PASS:".
           05 LINE 15 COL 20 PIC 999 USING BETPASS.
           05 LINE 17 COL 12 VALUE "BALANCE:".
           05 LINE 17 COL 20 PIC Z(6) USING PLAYERBAL.
           05 LINE 19 COL 8 VALUE "MINIMUM BET:".
           05 LINE 19 COL 20 PIC ZZ USING MINBET.
           05 LINE 20 COL 8 VALUE "MAXIMUM BET:".
           05 LINE 20 COL 20 PIC ZZZ USING MAXBET.
       01 COME-BETS.
           05 LINE 23 COL 9 VALUE "ENTER HOW MUCH YOU WANT TO BET FOR".
           05 LINE 23 COL 44 VALUE "THE FIELDS BELOW.".
           05 LINE 24 COL 9 VALUE "PRESS ENTER WHEN DONE (DICE ROLLS".
           05 LINE 24 COL 44 VALUE "AND PROGRAM WILL PAUSE FOR 3s)".
           05 LINE 25 COL 9 VALUE "PRES Q TO QUIT. ".
           05 LINE 27 COL 9 VALUE "DON'T COME:".
           05 LINE 27 COL 20 PIC ZZZ USING BETDONTCOME.
           05 LINE 28 COL 15 VALUE "COME:".
           05 LINE 28 COL 20 PIC ZZZ USING BETCOME.
       01 PASS-BETS.
           05 LINE 23 COL 9 VALUE "ENTER HOW MUCH YOU WANT TO BET FOR".
           05 LINE 23 COL 44 VALUE "THE FIELDS BELOW.".
           05 LINE 24 COL 9 VALUE "PRESS ENTER WHEN DONE (DICE ROLLS).".
           05 LINE 25 COL 9 VALUE "PRESS ALT+F4 TO QUIT.".
           05 LINE 27 COL 9 VALUE "DON'T PASS:".
           05 LINE 27 COL 20 PIC ZZZ USING BETDONTPASS.
           05 LINE 28 COL 15 VALUE "PASS:".
           05 LINE 28 COL 20 PIC ZZZ USING BETPASS.
       01 PUCK-4.
           05 LINE 4 COL 10 VALUE "ON".
       01 PUCK-5.
           05 LINE 5 COL 10 VALUE "ON".
       01 PUCK-6.
           05 LINE 6 COL 8 VALUE "ON".
       01 PUCK-8.
           05 LINE 7 COL 10 VALUE "ON".
       01 PUCK-9.
           05 LINE 8 COL 7 VALUE "ON".
       01 PUCK-10.
           05 LINE 9 COL 9 VALUE "ON".
       01 PLAYER-BROKE.
           05 LINE 22 COL 19 VALUE "YOU DON'T HAVE ENOUGH MONEY".
           05 LINE 22 COL 47 VALUE "TO KEEP PLAYING.".
           05 LINE 23 COL 19 VALUE "GOODBYE.".
       01 DCOME-OUTCOME.
           05 LINE 10 COL 39 VALUE "DON'T COME BETS WIN.".
       01 COME-OUTCOME.
           05 LINE 10 COL 39 VALUE "COME BETS WIN.".
       01 DPASS-OUTCOME.
           05 LINE 10 COL 39 VALUE "DON'T PASS BETS WIN.".
       01 PASS-OUTCOME.
           05 LINE 10 COL 39 VALUE "PASS BETS WIN.".
       01 OUT-BOUNDS.
           05 LINE 22 COL 19 VALUE "YOUR BET OUSTSIDE THE MAX AND MIN".
           05 LINE 22 COL 51 VALUE "BETTING BOUNDS. TRY AGAIN.".
       01 TOO-MUCH.
           05 LINE 22 COL 19 VALUE "YOUR BET WAS MORE THAN WHAT YOU".
           05 LINE 22 COL 51 VALUE "HAVE AVAILABLE. BET SMALLER.".
       01 GAME-OVER.
           05 BLANK SCREEN.
           05 LINE 1 COL 19 VALUE "GAME IS OVER.".
           05 LINE 2 COL 1 VALUE "LEADERBOARD".
           05 LINE 3 COL 1 VALUE "-----------".
       01 GET-NAME.
           05 LINE 15 COL 1 VALUE "YOU MADE THE LEADERBOARD.".
           05 LINE 16 COL 1 VALUE "YOUR NAME:".
           05 LINE 16 COL 11 PIC X(20) USING PLAYER-NAME.
       01 GOODBYE.
           05 LINE 20 COL 1 VALUE "GOODBYE".
       01 DIS-LEAD.
           05 LINE SLOT COL 1 PIC X(20) USING PAST-PLAYER.
           05 LINE SLOT COL 21 PIC Z(6) USING PAST-SCORE.

       PROCEDURE DIVISION.
       THE-GAME.
       PERFORM PLAY-GAME MAXROUNDS TIMES.
           DISPLAY GAME-OVER.

           OPEN INPUT PLAYER-FILE-IN.
           
           IF FILE-STATUS IS NOT EQUAL TO 0
                   DISPLAY "ERROR OPENING FILE, STATUS: " FILE-STATUS
                   STOP RUN
           END-IF.

           SET LEAD-INDEX TO 1.
           PERFORM LEADERBOARD 10 TIMES.
   
           CLOSE PLAYER-FILE-IN.

           SET LEAD-INDEX TO 10.
           IF PLAYERBAL IS GREATER THAN SCORE-TBL(LEAD-INDEX)
                   ACCEPT GET-NAME
                   OPEN OUTPUT PLAYER-FILE-OUT
                   PERFORM SCORE-CHECK 10 TIMES
                   SET LEAD-INDEX TO 1
                   PERFORM NEW-BOARD 10 TIMES
                   CLOSE PLAYER-FILE-OUT
           END-IF.

           DISPLAY GOODBYE.

           STOP RUN.

       NEW-BOARD.
           MOVE P-NAME-TBL(LEAD-INDEX) TO NEW-PLAYER.
           MOVE SCORE-TBL(LEAD-INDEX) TO NEW-SCORE.
           WRITE PLAYER-OUT.
           SET LEAD-INDEX UP BY 1.

       SCORE-CHECK.
           ADD 1 TO LEAD-INDEX GIVING BOARD-COUNTER.
           IF PLAYERBAL IS GREATER THAN SCORE-TBL(LEAD-INDEX)
                   MOVE SCORE-TBL(LEAD-INDEX) TO 
                   SCORE-TBL(BOARD-COUNTER)
                   MOVE P-NAME-TBL(LEAD-INDEX) TO
                   P-NAME-TBL(BOARD-COUNTER)
                   MOVE PLAYER-NAME TO P-NAME-TBL(LEAD-INDEX)
                   MOVE PLAYERBAL TO SCORE-TBL(LEAD-INDEX)
           END-IF.
           SET LEAD-INDEX DOWN BY 1.
                   
       LEADERBOARD.
           READ PLAYER-FILE-IN.
           MOVE PAST-PLAYER TO P-NAME-TBL(LEAD-INDEX).
           MOVE PAST-SCORE TO SCORE-TBL(LEAD-INDEX).
           DISPLAY DIS-LEAD.
           SET SLOT UP BY 1.
           SET LEAD-INDEX UP BY 1.

       PLAY-GAME.
           DISPLAY BET-BOARD.
           MOVE 0 TO BETCHECK.
           MOVE 0 TO PUCK-ORIG.
           MOVE 0 TO SKIP-1.
           MOVE 0 TO SKIP-2.

           IF PLAYERBAL IS LESS THAN MINBET
                   DISPLAY PLAYER-BROKE
                   STOP RUN
           END-IF.

           PERFORM COME-BETTING UNTIL BETCHECK IS EQUAL TO 1.


           SUBTRACT BETSUM FROM PLAYERBAL.
           DISPLAY BET-BOARD.
           CALL "ROLL" USING BY REFERENCE ROLLSUM.
           PERFORM ROLL-CHECK1.

           IF PHASE IS EQUAL TO 1
                   MOVE 0 TO PHASE
                   MOVE 0 TO BETCHECK
                   PERFORM PUCK-CHECK             
                   PERFORM POINT-BETTING UNTIL BETCHECK IS EQUAL TO 1
                   SUBTRACT BETSUM FROM PLAYERBAL
                   PERFORM POINT-ROLL UNTIL PHASE IS EQUAL TO 2
           END-IF.
           MOVE 0 TO PHASE.

       COME-BETTING.
           ACCEPT COME-BETS.
           MOVE 1 TO BETCHECK.
           MOVE 0 TO BETSUM.
           ADD BETDONTCOME TO BETSUM.
           ADD BETCOME TO BETSUM.
           
           IF BETDONTCOME IS GREATER THAN MAXBET
                   MOVE 2 TO BETCHECK.
           IF BETCOME IS GREATER THAN MAXBET
                   MOVE 2 TO BETCHECK.
           IF BETDONTCOME IS LESS THAN MINBET AND IS NOT EQUAL TO 0
                   MOVE 2 TO BETCHECK.
           IF BETCOME IS LESS THAN MINBET AND IS NOT EQUAL TO 0
                   MOVE 2 TO BETCHECK.
           IF BETSUM IS GREATER THAN PLAYERBAL
                   MOVE 3 TO BETCHECK.
           IF BETCHECK IS NOT EQUAL TO 1
                   PERFORM BET-ERROR.

       POINT-BETTING.
           ACCEPT PASS-BETS.
           MOVE 1 TO BETCHECK.
           MOVE 0 TO BETSUM.
           ADD BETDONTPASS TO BETSUM.
           ADD BETPASS TO BETSUM.

           IF BETDONTPASS IS GREATER THAN MAXBET
                   MOVE 2 TO BETCHECK
           ELSE IF BETPASS IS GREATER THAN MAXBET
                   MOVE 2 TO BETCHECK
           ELSE IF BETDONTPASS IS LESS THAN MINBET AND IS NOT EQUAL TO 0
                   MOVE 2 TO BETCHECK
           ELSE IF BETPASS IS LESS THAN MINBET AND IS NOT EQUAL TO 0
                   MOVE 2 TO BETCHECK
           ELSE IF BETSUM IS GREATER THAN PLAYERBAL
                   MOVE 3 TO BETCHECK
           END-IF.
           IF BETCHECK IS NOT EQUAL TO 1
                   PERFORM BET-ERROR.

       BET-ERROR.
           IF BETCHECK IS EQUAL TO 2
                   DISPLAY OUT-BOUNDS.
           IF BETCHECK IS EQUAL TO 3
                   DISPLAY TOO-MUCH.
        
       PUCK-CHECK.
           IF PUCK IS EQUAL TO 4
                   DISPLAY PUCK-4.
           IF PUCK IS EQUAL TO 5
                   DISPLAY PUCK-5.
           IF PUCK IS EQUAL TO 6
                   DISPLAY PUCK-6.
           IF PUCK IS EQUAL TO 8
                   DISPLAY PUCK-8.
           IF PUCK IS EQUAL TO 9
                   DISPLAY PUCK-9.
           IF PUCK IS EQUAL TO 10
                   DISPLAY PUCK-10.
                           

       ROLL-CHECK1.
           IF ROLLSUM IS EQUAL TO 7
                   PERFORM PASS-WINS
           ELSE IF ROLLSUM IS EQUAL TO 11
                   PERFORM PASS-WINS
           ELSE IF ROLLSUM IS EQUAL TO 2
                   PERFORM DPASS-WINS
           ELSE IF ROLLSUM IS EQUAL TO 3
                   PERFORM DPASS-WINS
           ELSE IF ROLLSUM IS EQUAL TO 12
                   PERFORM DPASS-WINS
           ELSE
                   MOVE ROLLSUM TO PUCK-ORIG
                   MOVE ROLLSUM TO PUCK
                   MOVE 1 TO PHASE
           END-IF. 
           CONTINUE AFTER 3 SECONDS.

       ROLL-CHECK2.
           IF SKIP-1 IS EQUAL TO 0
                   IF ROLLSUM IS EQUAL TO PUCK-ORIG
                           PERFORM PASS-WINS
                           MOVE 1 TO SKIP-1
                   ELSE IF ROLLSUM IS EQUAL TO 7
                           PERFORM DPASS-WINS
                           MOVE 1 TO SKIP-1
                   END-IF
           END-IF.
           IF SKIP-2 IS EQUAL TO 0
                   IF PUCK IS EQUAL TO PUCK-ORIG
                           IF ROLLSUM IS EQUAL TO 7
                                   PERFORM COME-WINS
                           ELSE IF ROLLSUM IS EQUAL TO 11
                                   PERFORM COME-WINS
                           ELSE IF ROLLSUM IS EQUAL TO 2
                                   PERFORM DCOME-WINS
                           ELSE IF ROLLSUM IS EQUAL TO 3
                                   PERFORM DCOME-WINS
                           ELSE IF ROLLSUM IS EQUAL TO 12
                                   PERFORM DCOME-WINS
                           ELSE
                                   MOVE ROLLSUM TO PUCK
                   ELSE
                           IF ROLLSUM IS EQUAL TO PUCK
                                   PERFORM COME-WINS
                           ELSE IF ROLLSUM IS EQUAL TO 7
                                   PERFORM DCOME-WINS
                           END-IF
                   END-IF
           END-IF.

       PASS-WINS.
           DISPLAY PASS-OUTCOME.
           ADD BETPASS TO PLAYERBAL.
           MOVE 0 TO BETPASS.
           MOVE 0 TO BETDONTPASS.

       DPASS-WINS.
           DISPLAY DPASS-OUTCOME.
           ADD BETDONTPASS TO PLAYERBAL.
           MOVE 0 TO BETPASS.
           MOVE 0 TO BETDONTPASS.

       COME-WINS.
           DISPLAY COME-OUTCOME.
           ADD BETCOME TO PLAYERBAL.
           MOVE 0 TO BETCOME.
           MOVE 0 TO BETDONTCOME.
           MOVE 1 TO SKIP-2.

       DCOME-WINS.
           DISPLAY DCOME-OUTCOME.
           ADD BETDONTCOME TO PLAYERBAL.
           MOVE 0 TO BETCOME.
           MOVE 0 TO BETDONTCOME.
           MOVE 1 TO SKIP-2.

       POINT-ROLL.
           DISPLAY BET-BOARD.
           CALL "ROLL" USING BY REFERENCE ROLLSUM.
           PERFORM PUCK-CHECK.
           PERFORM ROLL-CHECK2.
           ADD SKIP-1 TO SKIP-2 GIVING PHASE.
           CONTINUE AFTER 3 SECONDS.
