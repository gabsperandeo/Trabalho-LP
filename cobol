      ******************************************************************
      * PROGRAMA: Informar a situação de um aluno, de acordo com a média
      * AUTORES: FELIPE ROCHA, GABRIELLE SPERANDEO E THAMIRES MARCONDES
      * DATA: 09/05/2020.
      ******************************************************************

       IDENTIFICATION DIVISION.
           PROGRAM-ID. lp.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 NOME-ALUNO PIC X(20).
           77 P1 PIC S99V99.
           77 P2 PIC S99V99.
           77 T1 PIC S99V99.
           77 MEDIA PIC 99V99.
           77 MEDIA-PROVAS PIC 99V99.
           77 PESO-TRAB PIC S99V99.
           77 PESO-MED-PROVAS PIC S99V99.

       PROCEDURE DIVISION.
           DISPLAY 'Digite o nome do aluno: '.
           ACCEPT NOME-ALUNO.

           P1-ENTRADA.
           DISPLAY 'Digite a nota da P1: '.
           ACCEPT P1.
           IF P1<0 OR P1>10 THEN
               GO TO P1-ENTRADA.


           P2-ENTRADA.
           DISPLAY 'Digite a nota da P2: '.
           ACCEPT P2.
           IF P2<0 OR P2>10 THEN
               GO TO P2-ENTRADA.


           T1-ENTRADA.
           DISPLAY 'Digite a nota da T1: '.
           ACCEPT T1.
           IF T1<0 OR T1>10 THEN
               GO TO T1-ENTRADA.

           PESO-TRAB-ENTRADA.
           DISPLAY 'Digite o peso do trabalho de (inteiro de 0 a 10): '.
           ACCEPT PESO-TRAB.
           IF PESO-TRAB<0 OR PESO-TRAB>10 THEN
               GO TO PESO-TRAB-ENTRADA.

           COMPUTE MEDIA-PROVAS = (P1+P2)/2.
           COMPUTE PESO-MED-PROVAS = 10 - PESO-TRAB.
           MULTIPLY PESO-MED-PROVAS BY 0.1 GIVING PESO-MED-PROVAS.
           MULTIPLY PESO-TRAB BY 0.1 GIVING PESO-TRAB.
           COMPUTE MEDIA = T1*PESO-TRAB+MEDIA-PROVAS*PESO-MED-PROVAS.

           IF MEDIA IS GREATER THAN OR EQUAL TO 7 THEN
               DISPLAY 'O(a) aluno(a) ' NOME-ALUNO
               DISPLAY 'foi aprovado(a) com media: ' MEDIA
           ELSE IF MEDIA IS GREATER THAN OR EQUAL TO 3 AND
           IS LESS THAN 7 THEN
               DISPLAY 'O(a) aluno(a) ' NOME-ALUNO
               DISPLAY 'esta de exame com media: ' MEDIA
           ELSE
               DISPLAY 'O(a) aluno(a) ' NOME-ALUNO
               DISPLAY 'foi reprovado(a) com media: ' MEDIA
           END-IF.

           STOP RUN.
       END PROGRAM lp.
