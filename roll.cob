       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROLL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNIX-TIME     PIC 9(10).
       01 RANDOM-NUMBER PIC 9(9).
       01 RANDOM-INITIALIZED PIC X VALUE "N".
       01 RANDOM-INITIALIZED2 PIC X VALUE "N".
       01 DICE-ROLL     PIC 9.
       01 DICE-ROLL2    PIC 9.
       01 LEFTDIE1      PIC X(10) VALUE "|        |".
       01 LEFTDIE2      PIC X(10) VALUE "|        |".
       01 LEFTDIE3      PIC X(10) VALUE "|        |".
       01 LEFTDIE4      PIC X(10) VALUE "|        |".
       01 LEFTDIE5      PIC X(10) VALUE "|        |".
       01 RIGHTDIE1     PIC X(10) VALUE "|        |".
       01 RIGHTDIE2     PIC X(10) VALUE "|        |".
       01 RIGHTDIE3     PIC X(10) VALUE "|        |".
       01 RIGHTDIE4     PIC X(10) VALUE "|        |".
       01 RIGHTDIE5     PIC X(10) VALUE "|        |".

       LINKAGE SECTION.
       01 DICE-SUM      PIC 99.

       SCREEN SECTION.
       01 DIE-BASE.
           05 LINE 3 COL 39 VALUE "*--------*".
           05 LINE 4 COL 39 PIC X(10) USING LEFTDIE1.
           05 LINE 5 COL 39 PIC X(10) USING LEFTDIE2.
           05 LINE 6 COL 39 PIC X(10) USING LEFTDIE3.
           05 LINE 7 COL 39 PIC X(10) USING LEFTDIE4.
           05 LINE 8 COL 39 PIC X(10) USING LEFTDIE5.
           05 LINE 9 COL 39 VALUE "*--------*".
           05 LINE 3 COL 51 VALUE "*--------*".
           05 LINE 4 COL 51 PIC X(10) USING RIGHTDIE1.
           05 LINE 5 COL 51 PIC X(10) USING RIGHTDIE2.
           05 LINE 6 COL 51 PIC X(10) USING RIGHTDIE3.
           05 LINE 7 COL 51 PIC X(10) USING RIGHTDIE4.
           05 LINE 8 COL 51 PIC X(10) USING RIGHTDIE5.
           05 LINE 9 COL 51 VALUE "*--------*".
       01 LEFTDIEDIS.
           05 LINE 4 COL 39 PIC X(10) USING LEFTDIE1.
           05 LINE 5 COL 39 PIC X(10) USING LEFTDIE2.
           05 LINE 6 COL 39 PIC X(10) USING LEFTDIE3.
           05 LINE 7 COL 39 PIC X(10) USING LEFTDIE4.
           05 LINE 8 COL 39 PIC X(10) USING LEFTDIE5.
       01 RIGHTDIEDIS.
           05 LINE 4 COL 51 PIC X(10) USING RIGHTDIE1.
           05 LINE 5 COL 51 PIC X(10) USING RIGHTDIE2.
           05 LINE 6 COL 51 PIC X(10) USING RIGHTDIE3.
           05 LINE 7 COL 51 PIC X(10) USING RIGHTDIE4.
           05 LINE 8 COL 51 PIC X(10) USING RIGHTDIE5.
       
       PROCEDURE DIVISION USING DICE-SUM.
           DISPLAY DIE-BASE.
                  IF RANDOM-INITIALIZED = "N" 
                      MOVE "Y" TO RANDOM-INITIALIZED
                      CALL "time" USING BY REFERENCE UNIX-TIME
                      CALL "srand" USING UNIX-TIME.
       CALL "rand" RETURNING RANDOM-NUMBER.
       
       DIVIDE RANDOM-NUMBER BY 6 GIVING RANDOM-NUMBER 
       REMAINDER DICE-ROLL.
       ADD 1 TO DICE-ROLL.

       IF DICE-ROLL = 1
           MOVE "|        |" TO LEFTDIE1
           MOVE "|        |" TO LEFTDIE2
           MOVE "|    *   |" TO LEFTDIE3
           MOVE "|        |" TO LEFTDIE4
           MOVE "|        |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS
       ELSE IF DICE-ROLL = 2
           MOVE "|        |" TO LEFTDIE1
           MOVE "|        |" TO LEFTDIE2
           MOVE "| *    * |" TO LEFTDIE3
           MOVE "|        |" TO LEFTDIE4
           MOVE "|        |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS
       ELSE IF DICE-ROLL = 3
           MOVE "|        |" TO LEFTDIE1
           MOVE "| *      |" TO LEFTDIE2
           MOVE "|   *    |" TO LEFTDIE3
           MOVE "|     *  |" TO LEFTDIE4
           MOVE "|        |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS
       ELSE IF DICE-ROLL = 4
           MOVE "|        |" TO LEFTDIE1
           MOVE "| *    * |" TO LEFTDIE2
           MOVE "|        |" TO LEFTDIE3
           MOVE "| *    * |" TO LEFTDIE4
           MOVE "|        |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS
       ELSE IF DICE-ROLL = 5
           MOVE "| *    * |" TO LEFTDIE1
           MOVE "|        |" TO LEFTDIE2
           MOVE "|    *   |" TO LEFTDIE3
           MOVE "|        |" TO LEFTDIE4
           MOVE "| *    * |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS
       ELSE
           MOVE "| *    * |" TO LEFTDIE1
           MOVE "|        |" TO LEFTDIE2
           MOVE "| *    * |" TO LEFTDIE3
           MOVE "|        |" TO LEFTDIE4
           MOVE "| *    * |" TO LEFTDIE5
           DISPLAY LEFTDIEDIS.
       
                  IF RANDOM-INITIALIZED2 = "N" 
                      MOVE "Y" TO RANDOM-INITIALIZED2
                      CALL "time" USING BY REFERENCE UNIX-TIME
                      CALL "srand" USING UNIX-TIME.
       CALL "rand" RETURNING RANDOM-NUMBER.
       
        DIVIDE RANDOM-NUMBER BY 6 GIVING RANDOM-NUMBER 
        REMAINDER DICE-ROLL2.
       ADD 1 TO DICE-ROLL2.

       IF DICE-ROLL2 = 1
           MOVE "|        |" TO RIGHTDIE1
           MOVE "|        |" TO RIGHTDIE2
           MOVE "|    *   |" TO RIGHTDIE3
           MOVE "|        |" TO RIGHTDIE4
           MOVE "|        |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS
       ELSE IF DICE-ROLL2 = 2
           MOVE "|        |" TO RIGHTDIE1
           MOVE "|        |" TO RIGHTDIE2
           MOVE "| *    * |" TO RIGHTDIE3
           MOVE "|        |" TO RIGHTDIE4
           MOVE "|        |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS
       ELSE IF DICE-ROLL2 = 3
           MOVE "|        |" TO RIGHTDIE1
           MOVE "|  *     |" TO RIGHTDIE2
           MOVE "|    *   |" TO RIGHTDIE3
           MOVE "|      * |" TO RIGHTDIE4
           MOVE "|        |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS
       ELSE IF DICE-ROLL2 = 4
           MOVE "|        |" TO RIGHTDIE1
           MOVE "| *    * |" TO RIGHTDIE2
           MOVE "|        |" TO RIGHTDIE3
           MOVE "| *    * |" TO RIGHTDIE4
           MOVE "|        |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS
       ELSE IF DICE-ROLL2 = 5
           MOVE "| *    * |" TO RIGHTDIE1
           MOVE "|        |" TO RIGHTDIE2
           MOVE "|    *   |" TO RIGHTDIE3
           MOVE "|        |" TO RIGHTDIE4
           MOVE "| *    * |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS
       ELSE
           MOVE "| *    * |" TO RIGHTDIE1
           MOVE "|        |" TO RIGHTDIE2
           MOVE "| *    * |" TO RIGHTDIE3
           MOVE "|        |" TO RIGHTDIE4
           MOVE "| *    * |" TO RIGHTDIE5
           DISPLAY RIGHTDIEDIS.
       
       ADD DICE-ROLL TO DICE-ROLL2 GIVING DICE-SUM.
