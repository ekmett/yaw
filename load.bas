10 PRINT CHR$(147)+"LOADING";
20 FORI=1TO2
30 POKE $9F61,I
40 F$="BANK-"+MID$(STR$(I),2)+".BIN"
50 PRINT"LOADING "+F$+"...";
60 LOAD F$,8,1,$A000
70 PRINT"DONE"
80 NEXT

