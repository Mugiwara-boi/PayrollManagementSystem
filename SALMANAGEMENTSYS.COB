       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-MANAGEMENT-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE ASSIGN TO "C:\COBOL\EMPREC.txt"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS FS-ID
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE
           RECORD CONTAINS 104 CHARACTERS.
       01 FS-EMPLOYEE.
           05 FS-ID PIC X(5).
           05 FILLER PIC XX.
           05 FS-NAME PIC X(28).
           05 FILLER PIC XX.
           05 FS-DESIG PIC X(26).
           05 FILLER PIC X(7).
           05 FS-MONSAL PIC ZZZZZZ9.99.
           05 FILLER PIC X(3).
           05 FS-PAYPER PIC X(12).
           05 FILLER PIC XX.
           05 FS-STAT PIC X(7).

       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE.
           05 WS-ID PIC X(5).
           05 FILLER PIC XX.
           05 WS-NAME.
              10 WS-FNAME PIC X(12).
              10 FILLER PIC X VALUE SPACE.
              10 WS-MI PIC X(3).
              10 FILLER PIC X VALUE SPACE.
              10 WS-LNAME PIC X(11).
           05 FILLER PIC XX.
           05 WS-DESIG PIC X(26).
           05 FILLER PIC X(3).
           05 WS-PESO PIC X(4) VALUE 'PHP '.
           05 WS-MONSAL PIC 9(7)V99.
           05 FILLER PIC X(3).
           05 WS-PAYPER PIC X(12).
               88 MONTHLY VALUE 'MONTHLY'.
               88 SEMI-MONTHLY VALUE 'SEMI-MONTHLY'.
           05 FILLER PIC XX.
           05 WS-STAT PIC X(7).
       01 WS-HEAD.
           05 FILLER PIC X VALUE '|'.
           05 FILLER PIC X(15) VALUE SPACES.
           05 WS-SIGN PIC X(4) VALUE SPACES.
           05 WS-HEADER PIC X(11).
           05 FILLER PIC X(18) VALUE SPACES.
           05 FILLER PIC X VALUE '|'.
       01 WS-DESCRIP.
           05 FILLER PIC X VALUE '|'.
           05 WS-DESCRIPTION PIC X(23).
           05 FILLER PIC X(3) VALUE '|  '.
           05 WS-UNIT PIC X(5).
           05 FILLER PIC X(5) VALUE '  |  '.
           05 WS-AMOUNT PIC X(10).
           05 FILLER PIC X(3) VALUE '  |'.
       01 WS-DESCRIP2.
           05 FILLER PIC X VALUE '|'.
           05 WS-DESCRIPTION2 PIC X(23).
           05 FILLER PIC X(3) VALUE '|  '.
           05 WS-UNIT2 PIC 99.99.
           05 FILLER PIC X(5) VALUE '  |  '.
           05 WS-AMOUNT2 PIC ZZZZZZ9.99.
           05 FILLER PIC X(3) VALUE '  |'.
       01 WS-DATE.
           05 WS-YEAR PIC 9(4).
           05 WS-MONTH PIC 99.
           05 WS-DAY PIC 99.
       01 WS-CATEG PIC X.
       01 WS-NETPAY PIC 9(7)V99.
       01 WS-DAYSAL PIC 9(7)V99.
       01 WS-HOURSAL PIC 9(7)V99.
       01 WS-BPAY PIC 9(7)V99.
       01 WS-ALLO PIC 9(7)V99.
       01 WS-OTPAY PIC 9(7)V99.
       01 WS-INTAX PIC 9(7)V99.
       01 WS-TAXABLE-INCOME PIC 9(7)V99.
       01 WS-SSS PIC 9(7)V99.
       01 WS-PHIC PIC 9(7)V99.
       01 WS-PAG-IBIG PIC 9(7)V99.
       01 WS-ABSTDEDUC PIC 9(7)V99.
       01 WS-UTLDEDUC PIC 9(7)V99.
       01 WS-LOAN PIC 9(7)V99.
       01 WS-TDEDUC PIC 9(7)V99.
       01 WS-TBENEFITS PIC 9(7)V99.
       01 WS-GROSSPAY PIC 9(7)V99.
       01 WS-OT PIC 99.
       01 WS-ABST PIC 99.
       01 WS-UTL PIC 99.
       01 WS-BPAYUNIT PIC 99V99.
       01 WS-CHOICE PIC 9.
       01 WS-FS PIC XX.
       01 WS-EOF PIC X(4).
       01 WS-TODAY PIC 9(8).
       01 WS-PAST PIC 9(8).
       01 WS-LINE PIC X(25) VALUE ALL '-'.
       01 WS-LINE2 PIC X(25) VALUE ALL '='.
       01 WS-SPACE PIC X(25) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O EMPLOYEE.
           DISPLAY WS-LINE2 "============= SALARY MANAGEMENT SYSTEM"
                  " ==============" WS-LINE2.
           DISPLAY " ".
           PERFORM UNTIL WS-CHOICE = 6
             DISPLAY WS-LINE2 "===================== MAIN MENU "
                                 WITH NO ADVANCING
             DISPLAY "=====================" WS-LINE2
             DISPLAY "|  | CHOOSE FROM THE FOLLOWING OPTIONS:         "
                     WS-SPACE WS-SPACE " |  |"
             PERFORM PRINT-LINE2-PARA
             DISPLAY "|  | (1) ADD NEW EMPLOYEE RECORD." WS-SPACE
                     WS-SPACE "                |  |"
             DISPLAY "|  | (2) UPDATE EMPLOYEE'S RECORD." WS-SPACE
                     WS-SPACE "               |  |"
             DISPLAY "|  | (3) REMOVE EMPLOYEE'S RECORD." WS-SPACE
                     WS-SPACE "               |  |"
             DISPLAY "|  | (4) VIEW LIST OF EMPLOYEES." WS-SPACE
                     WS-SPACE "                 |  |"
             DISPLAY "|  | (5) CREATE PAYSLIP." WS-SPACE WS-SPACE
                     WS-SPACE "|  |"
             DISPLAY "|  | (6) EXIT THE PROGRAM." WS-SPACE WS-SPACE
                     "                       |  |"
             PERFORM PRINT-LINE2-PARA
             DISPLAY " "
             DISPLAY "ENTER YOUR CHOICE: " WITH NO ADVANCING
             ACCEPT WS-CHOICE
             EVALUATE WS-CHOICE
               WHEN 1
                 PERFORM ADD-PARA
               WHEN 2
                 PERFORM UPDATE-PARA
               WHEN 3
                 PERFORM REMOVE-PARA
               WHEN 4
                 PERFORM VIEW-PARA
               WHEN 5
                 PERFORM EVAL-REC-PARA
               WHEN 6
                 MOVE 6 TO WS-CHOICE
               WHEN OTHER
                 DISPLAY " "
                 DISPLAY "INVALID CHOICE."
                 PERFORM FOOT-PARA
             END-EVALUATE
           END-PERFORM.
           CALL 'SYSTEM' USING 'CLS'.
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "|  | THANK YOU FOR USING SALARY MANAGEMENT SYSTEM."
                   " HAVE A NICE DAY!! :) " WS-SPACE "  |  |".
           PERFORM PRINT-LINE2-PARA.
           CLOSE EMPLOYEE.
           STOP RUN.

       ADD-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           DISPLAY WS-LINE2 "==================== NEW EMPLOYEE "
                            "===================" WS-LINE2.
           DISPLAY "|  | ENTER THE FOLLOWING INFORMATION:" WS-SPACE
                   WS-SPACE "            |  |"
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "EMPLOYEE ID             : " WITH NO ADVANCING.
           ACCEPT WS-ID.
           PERFORM INFO-PARA.
           PERFORM MAKE-CAPS-PARA.
           MOVE 'PENDING' TO WS-STAT.
           DISPLAY " ".
           WRITE FS-EMPLOYEE FROM WS-EMPLOYEE
             INVALID KEY DISPLAY "ERROR! THE ID IS ALREADY TAKEN."
             NOT INVALID KEY DISPLAY "NEW EMPLOYEE ADDED."
           END-WRITE.
           PERFORM FOOT-PARA.

       INFO-PARA.
           DISPLAY "EMPLOYEE'S FIRST NAME   : " WITH NO ADVANCING.
           ACCEPT WS-FNAME.
           DISPLAY "EMPLOYEE'S MIDLE INITIAL: " WITH NO ADVANCING.
           ACCEPT WS-MI.
           DISPLAY "EMPLOYEE'S LAST NAME    : " WITH NO ADVANCING.
           ACCEPT WS-LNAME.
           DISPLAY "EMPLOYEE DESIGNATION    : " WITH NO ADVANCING.
           ACCEPT WS-DESIG.
           DISPLAY "MONTHLY BASIC SALARY    : " WITH NO ADVANCING.
           ACCEPT WS-MONSAL.
           DISPLAY "PAY PERIOD              : " WITH NO ADVANCING.
           ACCEPT WS-PAYPER.

       UPDATE-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           DISPLAY WS-LINE2 "=================== UPDATE RECORD "
                   "===================" WS-LINE2.
           DISPLAY "|  | ENTER EMPLOYEE'S NEW INFORMATION: " WS-SPACE
                   WS-SPACE "          |  |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "ENTER EMPLOYEE'S ID TO BE UPDATED: "
                                      WITH NO ADVANCING.
           ACCEPT WS-ID.
           MOVE WS-ID TO FS-ID.
           READ EMPLOYEE
             INVALID KEY DISPLAY " "
               DISPLAY "RECORD DOESN'T EXIST."
             NOT INVALID KEY
               PERFORM INFO-PARA
               PERFORM MAKE-CAPS-PARA
               DISPLAY "PAYMENT STATUS          : " WITH NO ADVANCING
               ACCEPT WS-STAT
               MOVE FUNCTION UPPER-CASE (WS-STAT) TO WS-STAT
               DISPLAY " "
               REWRITE FS-EMPLOYEE FROM WS-EMPLOYEE
                 INVALID KEY DISPLAY "RECORD NOT UPDATED."
                 NOT INVALID KEY DISPLAY "RECORD UPDATED."
               END-REWRITE
           END-READ.
           PERFORM FOOT-PARA.

       REMOVE-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           DISPLAY WS-LINE2 "=================== DELETE RECORD "
                   "===================" WS-LINE2.
           DISPLAY "|  |  " WS-SPACE WS-SPACE WS-SPACE
                   "                  |  |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "ENTER EMPLOYEE'S ID TO BE REMOVED: "
                                      WITH NO ADVANCING.
           ACCEPT WS-ID.
           DISPLAY " ".
           MOVE WS-ID TO FS-ID.
           DELETE EMPLOYEE RECORD
             INVALID KEY DISPLAY "RECORD DOESN'T EXIST."
             NOT INVALID KEY DISPLAY "RECORD DELETED."
           END-DELETE.
           PERFORM FOOT-PARA.

       VIEW-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           INITIALIZE WS-EOF.
           DISPLAY WS-LINE2 "================= LIST OF EMPLOYEES"
                   " =================" WS-LINE2
           DISPLAY "|ID   | NAME                      "
                   " | DESIGNATION                | MNTHLY-SALARY "
                   "| PAY PERIOD  |STATUS |".
           PERFORM PRINT-LINE2-PARA.
           MOVE '00000' TO FS-ID.
           START EMPLOYEE KEY IS > FS-ID
             INVALID KEY DISPLAY "ERROR!"
             NOT INVALID KEY
               PERFORM UNTIL WS-EOF = 'TRUE'
                 READ EMPLOYEE NEXT RECORD INTO WS-EMPLOYEE
                 AT END MOVE 'TRUE' TO WS-EOF
                 NOT AT END
                   MOVE WS-MONSAL TO FS-MONSAL
                   DISPLAY FS-EMPLOYEE
                 END-READ
               END-PERFORM
           END-START.
           PERFORM FOOT-PARA.

       EVAL-REC-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           DISPLAY WS-LINE2 "=================== CREATE PAYLSIP "
                   "==================" WS-LINE2.
           DISPLAY "|  |  " WS-SPACE WS-SPACE WS-SPACE
                   "                  |  |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "ENTER EMPLOYEE'S ID TO BE PROCESSED: "
                                               WITH NO ADVANCING.
           ACCEPT WS-ID.
           MOVE WS-ID TO FS-ID.
           DISPLAY " ".
           READ EMPLOYEE INTO WS-EMPLOYEE
             INVALID KEY DISPLAY "RECORD DOESN'T EXIST."
             NOT INVALID KEY
               EVALUATE WS-STAT
               WHEN 'PAID'
                 CALL 'SYSTEM' USING 'CLS'
                 PERFORM PRINT-LINE2-PARA
                 DISPLAY "|  | EMPLOYEE HAS BEEN PAID ALREADY."
                        " DO YOU WANT "
                        "TO CHANGE THE PAYMENT STATUS AND CONTINUE?"
                        "        |  |"
                 PERFORM PRINT-LINE2-PARA
                 DISPLAY "|  | (1) YES" WS-SPACE WS-SPACE WS-SPACE
                         "            |  |"
                 DISPLAY "|  | (2) NO" WS-SPACE WS-SPACE WS-SPACE
                         "             |  |"
                 PERFORM PRINT-LINE2-PARA
                 DISPLAY " "
                 DISPLAY "ENTER YOUR CHOICE: " WITH NO ADVANCING
                 ACCEPT WS-CHOICE
                 IF WS-CHOICE = 1
                   MOVE 'PENDING' TO WS-STAT
                   PERFORM PAYSLIP-PARA
                 ELSE IF WS-CHOICE = 2
                   CALL 'SYSTEM' USING 'CLS'
                   EXIT PARAGRAPH
                 END-IF
               WHEN OTHER
                 PERFORM PAYSLIP-PARA
               END-EVALUATE
               MOVE 'PAID' TO WS-STAT
               REWRITE FS-EMPLOYEE FROM WS-EMPLOYEE
               END-REWRITE
           END-READ.
           PERFORM FOOT-PARA.

       PAYSLIP-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           DISPLAY WS-LINE2 "==================== DESIGNATION "
                   WS-LINE2 "===================="
           DISPLAY "|  | SPECIFY THE EMPLOYEE DESIGNATION CATEGORY: "
                   WS-SPACE WS-SPACE " |  |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "|  | (1) TOP EXECUTIVE " WS-SPACE WS-SPACE
                   WS-SPACE " |  |".
           DISPLAY "|  | (2) MANAGERIAL" WS-SPACE WS-SPACE WS-SPACE
                   "     |  |".
           DISPLAY "|  | (3) RANK-AND-FILE " WS-SPACE WS-SPACE WS-SPACE
                   " |  |".
           DISPLAY "|  | (4) CONTRACTUAL   " WS-SPACE WS-SPACE WS-SPACE
                   " |  |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY " ".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY "|  | DESIGNATION: " WS-DESIG WS-SPACE WS-SPACE
                   "      | |".
           PERFORM PRINT-LINE2-PARA.
           DISPLAY " ".
           DISPLAY "ENTER CATEGORY        : " WITH NO ADVANCING.
           ACCEPT WS-CATEG.
           COMPUTE WS-DAYSAL = WS-MONSAL / 26.
           COMPUTE WS-HOURSAL = WS-DAYSAL / 8.
           PERFORM COMP-SSS-PARA THRU PRINT-PARA.

       COMP-SSS-PARA.
           EVALUATE WS-MONSAL
             WHEN <= 999.99
               MOVE 0.00 TO WS-SSS
             WHEN 1000.00 THRU 3249.99
               MOVE 135.00 TO WS-SSS
             WHEN 3250.00 THRU 3749.99
               MOVE 157.50 TO WS-SSS
             WHEN 3750.00 THRU 4249.99
               MOVE 180.00 TO WS-SSS
             WHEN 4250.00 THRU 4749.99
               MOVE 202.50 TO WS-SSS
             WHEN 4750.00 THRU 5249.99
               MOVE 225.00 TO WS-SSS
             WHEN 5250.00 THRU 5749.99
               MOVE 247.50 TO WS-SSS
             WHEN 5750.00 THRU 6249.99
               MOVE 270.00 TO WS-SSS
             WHEN 6250.00 THRU 6749.99
               MOVE 292.50 TO WS-SSS
             WHEN 6750.00 THRU 7249.99
               MOVE 315.00 TO WS-SSS
             WHEN 7250.00 THRU 7749.99
               MOVE 337.00 TO WS-SSS
             WHEN 7750.00 THRU 8249.99
               MOVE 360.00 TO WS-SSS
             WHEN 8250.00 THRU 8749.99
               MOVE 382.50 TO WS-SSS
             WHEN 8750.00 THRU 9249.99
               MOVE 405.00 TO WS-SSS
             WHEN 9250.00 THRU 9749.99
               MOVE 427.50 TO WS-SSS
             WHEN 9750.00 THRU 10249.99
               MOVE 450.00 TO WS-SSS
             WHEN 10250.00 THRU 10749.99
               MOVE 472.50 TO WS-SSS
             WHEN 10750.00 THRU 11249.99
               MOVE 495.00 TO WS-SSS
             WHEN 11250.00 THRU 11749.99
               MOVE 517.50 TO WS-SSS
             WHEN 11750.00 THRU 12249.99
               MOVE 540.00 TO WS-SSS
             WHEN 12250.00 THRU 12749.99
               MOVE 562.50 TO WS-SSS
             WHEN 12750.00 THRU 13249.99
               MOVE 585.00 TO WS-SSS
             WHEN 13250.00 THRU 13749.99
               MOVE 607.50 TO WS-SSS
             WHEN 13750.00 THRU 14249.99
               MOVE 630.00 TO WS-SSS
             WHEN 14250.00 THRU 14749.99
               MOVE 652.50 TO WS-SSS
             WHEN 14750.00 THRU 15249.99
               MOVE 675.00 TO WS-SSS
             WHEN 15250.00 THRU 15749.99
               MOVE 697.50 TO WS-SSS
             WHEN 15750.00 THRU 16249.99
               MOVE 720.00 TO WS-SSS
             WHEN 16250.00 THRU 16749.99
               MOVE 742.50 TO WS-SSS
             WHEN 16750.00 THRU 17249.99
               MOVE 765.00 TO WS-SSS
             WHEN 17250.00 THRU 17749.99
               MOVE 787.50 TO WS-SSS
             WHEN 17750.00 THRU 18249.99
               MOVE 810.00 TO WS-SSS
             WHEN 18250.00 THRU 18749.99
               MOVE 832.50 TO WS-SSS
             WHEN 18750.00 THRU 19249.99
               MOVE 855.00 TO WS-SSS
             WHEN 19250.00 THRU 19749.99
               MOVE 877.50 TO WS-SSS
             WHEN 19750.00 THRU 20249.99
               MOVE 900.00 TO WS-SSS
             WHEN 20250.00 THRU 20749.99
               MOVE 922.50 TO WS-SSS
             WHEN 20750.00 THRU 21249.99
               MOVE 945.00 TO WS-SSS
             WHEN 21250.00 THRU 21749.99
               MOVE 967.50 TO WS-SSS
             WHEN 21750.00 THRU 22249.99
               MOVE 990.00 TO WS-SSS
             WHEN 22250.00 THRU 22749.99
               MOVE 1012.50 TO WS-SSS
             WHEN 22750.00 THRU 23249.99
               MOVE 1035.00 TO WS-SSS
             WHEN 23250.00 THRU 23749.99
               MOVE 1057.50 TO WS-SSS
             WHEN 23750.00 THRU 24249.99
               MOVE 1080.00 TO WS-SSS
             WHEN 24250.00 THRU 24749.99
               MOVE 1102.50 TO WS-SSS
             WHEN >= 24750.00
               MOVE 1125.00 TO WS-SSS
           END-EVALUATE.

       COMP-PHIC-PARA.
           EVALUATE WS-MONSAL
             WHEN <= 10000.00
                 MOVE 150.00 TO WS-PHIC
             WHEN 10000.01 THRU 59999.99
               COMPUTE WS-PHIC = (WS-MONSAL * 0.03) / 2
             WHEN >= 60000.00
                  MOVE 1800.00 TO WS-PHIC
           END-EVALUATE.

       COMP-PAG-IBIG-PARA.
           EVALUATE WS-MONSAL
             WHEN <= 1500.00
                COMPUTE WS-PAG-IBIG = WS-MONSAL * 0.01
             WHEN 1500.01 THRU 4999.99
                COMPUTE WS-PAG-IBIG = WS-MONSAL * 0.02
             WHEN >= 5000.00
                  MOVE 100.00 TO WS-PAG-IBIG
           END-EVALUATE.

       COMP-TAX-PARA.
           COMPUTE WS-TBENEFITS = WS-SSS + WS-PHIC + WS-PAG-IBIG.
           COMPUTE WS-TAXABLE-INCOME = WS-MONSAL - WS-TBENEFITS.
           EVALUATE WS-TAXABLE-INCOME
             WHEN <= 20833.00
               MOVE 0.00 TO WS-INTAX
             WHEN 20833.01 THRU 33332.99
               COMPUTE WS-INTAX = ((WS-TAXABLE-INCOME - 20833.00 )
                                * .20)
             WHEN 33333.00 THRU 66666.99
               COMPUTE WS-INTAX = (((WS-TAXABLE-INCOME - 33333.00)
                                * .25) + 2500.00)
             WHEN 66667.00 THRU 166666.99
               COMPUTE WS-INTAX = (((WS-TAXABLE-INCOME - 66667.00)
                                * .30) + 10833.33)
             WHEN 166667.00 THRU 666666.99
               COMPUTE WS-INTAX = (((WS-TAXABLE-INCOME - 166667.00)
                                * .32) + 40833.33)
             WHEN >= 666667.00
               COMPUTE WS-INTAX = (((WS-TAXABLE-INCOME - 666667.00)
                                * .35) + 200833.33)
           END-EVALUATE.

       EVAL-PAYPERIOD-PARA.
           EVALUATE TRUE
             WHEN SEMI-MONTHLY
               COMPUTE WS-BPAY = WS-MONSAL / 2
               COMPUTE WS-SSS = WS-SSS / 2
               COMPUTE WS-PHIC = WS-PHIC / 2
               COMPUTE WS-PAG-IBIG = WS-PAG-IBIG / 2
               COMPUTE WS-INTAX = WS-INTAX / 2
               MOVE 13 TO WS-BPAYUNIT
             WHEN MONTHLY
               COMPUTE WS-BPAY = WS-MONSAL * 1
               COMPUTE WS-SSS = WS-SSS * 1
               COMPUTE WS-PHIC = WS-PHIC * 1
               COMPUTE WS-PAG-IBIG = WS-PAG-IBIG * 1
               COMPUTE WS-INTAX = WS-INTAX * 1
               MOVE 26 TO WS-BPAYUNIT
             WHEN OTHER
               DISPLAY " "
               DISPLAY "INVALID PAY PERIOD! PLEASE UPDATE IT INTO"
                       " MONTHLY OR SEMI-MONTHLY TO PROCEED."
               PERFORM FOOT-PARA
               GO TO MAIN-PROCEDURE
           END-EVALUATE.

       EARN-PARA.
           DISPLAY "OVERTIME HOUR/S       : " WITH NO ADVANCING.
           ACCEPT WS-OT.
           COMPUTE WS-OTPAY = WS-OT * (WS-HOURSAL * 1.25).
           DISPLAY "TOTAL ALLOWANCE AMOUNT: " WITH NO ADVANCING.
           ACCEPT WS-ALLO.
           IF WS-CATEG = 1 OR 2
             MOVE 0 TO WS-OTPAY
           END-IF.

       DEDUC-PARA.
           DISPLAY "UNDERTIME/LATE HOUR/S : " WITH NO ADVANCING.
           ACCEPT WS-UTL.
           COMPUTE WS-UTLDEDUC = WS-UTL * WS-HOURSAL.
           DISPLAY "ABSENCE DAY/S         : " WITH NO ADVANCING.
           ACCEPT WS-ABST.
           COMPUTE WS-ABSTDEDUC = WS-DAYSAL * WS-ABST.
           DISPLAY "TOTAL LOAN AMOUNT     : " WITH NO ADVANCING.
           ACCEPT WS-LOAN.

       NETPAY-PARA.
           COMPUTE WS-GROSSPAY = WS-BPAY + WS-ALLO + WS-OTPAY.
           COMPUTE WS-TDEDUC = WS-UTLDEDUC + WS-ABSTDEDUC + WS-SSS
                         + WS-PHIC + WS-PAG-IBIG + WS-INTAX + WS-LOAN.
           COMPUTE WS-NETPAY = WS-GROSSPAY - WS-TDEDUC.

       PRINT-PARA.
           CALL 'SYSTEM' USING 'CLS'.
           INITIALIZE WS-SIGN.
           DISPLAY WS-LINE2 "====================== PAYSLIP "
                   WS-LINE2 "======================".
           DISPLAY " ".
           DISPLAY "EMPLOYEE ID: " WS-ID WS-SPACE WITH NO ADVANCING.
           DISPLAY "          " "PAYROLL DATE: " WITH NO ADVANCING.
           PERFORM PAYROLL-DATE-PARA.
           DISPLAY "EMPLOYEE NAME: " WS-NAME WITH NO ADVANCING.
           DISPLAY "          " "COVERED DATE: " WITH NO ADVANCING.
           PERFORM COVERED-DATE-PARA.
           DISPLAY "DESIGNATION: " WS-DESIG "              "
                   "PAY PERIOD: " WS-PAYPER.
           PERFORM PRINT-LINE-PARA.
           MOVE 'EARNINGS' TO WS-HEADER.
           DISPLAY WS-HEAD "   " WITH NO ADVANCING.
           MOVE 'DEDUCTIONS' TO WS-HEADER.
           DISPLAY WS-HEAD.
           PERFORM PRINT-LINE-PARA.
           MOVE 'DESCRIPTION' TO WS-DESCRIPTION.
           MOVE 'UNIT' TO WS-UNIT.
           MOVE 'AMOUNT' TO WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WS-DESCRIP.
           PERFORM PRINT-LINE-PARA.
           MOVE 'BASIC PAY' TO WS-DESCRIPTION2.
           MOVE WS-BPAYUNIT TO WS-UNIT2.
           MOVE WS-BPAY TO WS-AMOUNT2.
           DISPLAY WS-DESCRIP2 "   " WITH NO ADVANCING.
           MOVE 'ABSENCE DAY/S' TO WS-DESCRIPTION2.
           MOVE WS-ABST TO WS-UNIT2.
           MOVE WS-ABSTDEDUC TO WS-AMOUNT2.
           DISPLAY WS-DESCRIP2.
           MOVE 'OVERTIME HOUR/S' TO WS-DESCRIPTION2.
           MOVE WS-OT TO WS-UNIT2.
           MOVE WS-OTPAY TO WS-AMOUNT2.
           DISPLAY WS-DESCRIP2 "   " WITH NO ADVANCING.
           MOVE 'UNDERTIME/LATE HOUR/S' TO WS-DESCRIPTION2.
           MOVE WS-UTL TO WS-UNIT2.
           MOVE WS-UTLDEDUC TO WS-AMOUNT2.
           DISPLAY WS-DESCRIP2.
           MOVE SPACE TO WS-DESCRIPTION WS-UNIT WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'SSS' TO WS-DESCRIPTION.
           MOVE WS-SSS TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           MOVE SPACE TO WS-DESCRIPTION WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'PHIC' TO WS-DESCRIPTION.
           MOVE WS-PHIC TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           MOVE SPACE TO WS-DESCRIPTION WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'PAG-IBIG' TO WS-DESCRIPTION.
           MOVE WS-PAG-IBIG TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           MOVE SPACE TO WS-DESCRIPTION WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'INCOME TAX' TO WS-DESCRIPTION.
           MOVE WS-INTAX TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           MOVE 'TOTAL ALLOWANCE' TO WS-DESCRIPTION.
           MOVE WS-ALLO TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'TOTAL LOAN' TO WS-DESCRIPTION.
           MOVE WS-LOAN TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           PERFORM PRINT-LINE-PARA.
           MOVE 'GROSS PAY' TO WS-DESCRIPTION.
           MOVE WS-GROSSPAY TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP "   " WITH NO ADVANCING.
           MOVE 'TOTAL DEDUCTIONS' TO WS-DESCRIPTION.
           MOVE WS-TDEDUC TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-AMOUNT.
           DISPLAY WS-DESCRIP.
           PERFORM PRINT-LINE-PARA.
           DISPLAY WS-SPACE WS-SPACE "   " WS-LINE WS-LINE.
           DISPLAY WS-SPACE WS-SPACE "   " WITH NO ADVANCING.
           MOVE 'NET PAY' TO WS-HEADER.
           DISPLAY WS-HEAD.
           DISPLAY WS-SPACE WS-SPACE "   " WS-LINE WS-LINE.
           DISPLAY WS-SPACE WS-SPACE "   " WITH NO ADVANCING.
           MOVE WS-PESO TO WS-SIGN.
           MOVE WS-NETPAY TO WS-AMOUNT2.
           MOVE WS-AMOUNT2 TO WS-HEADER.
           DISPLAY WS-HEAD.
           DISPLAY WS-SPACE WS-SPACE "   " WS-LINE WS-LINE.

       PAYROLL-DATE-PARA.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE.
           DISPLAY WS-MONTH "/" WS-DAY "/" WS-YEAR.

       COVERED-DATE-PARA.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY.
           EVALUATE TRUE
             WHEN MONTHLY
                COMPUTE WS-PAST = FUNCTION INTEGER-OF-DATE (WS-TODAY)
                              - 30
             WHEN SEMI-MONTHLY
                COMPUTE WS-PAST = FUNCTION INTEGER-OF-DATE (WS-TODAY)
                              - 15
           END-EVALUATE.
           COMPUTE WS-PAST = FUNCTION DATE-OF-INTEGER (WS-PAST).
           MOVE WS-PAST TO WS-DATE.
           DISPLAY WS-MONTH "/" WS-DAY "/" WS-YEAR WITH NO ADVANCING.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE.
           DISPLAY "    " WS-MONTH "/" WS-DAY "/" WS-YEAR.

       MAKE-CAPS-PARA.
           MOVE FUNCTION UPPER-CASE (WS-NAME) TO WS-NAME.
           MOVE FUNCTION UPPER-CASE (WS-DESIG) TO WS-DESIG.
           MOVE FUNCTION UPPER-CASE (WS-PAYPER) TO WS-PAYPER.

       FOOT-PARA.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "PRESS ENTER TO CONTINUE..." WITH NO ADVANCING.
           ACCEPT WS-CHOICE.
           CALL 'SYSTEM' USING 'CLS'.

       PRINT-LINE2-PARA.
           DISPLAY WS-LINE2 WS-LINE2 "===" WS-LINE2 WS-LINE2.

       PRINT-LINE-PARA.
           DISPLAY WS-LINE WS-LINE "   " WS-LINE WS-LINE.
